package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{ClassicalAnalysis, IntraControlFlow, SetLattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax.{AnalysisResult, EditorMessage}
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.Labeled1
import org.metaborg.whilelang.scala.ast.{SINTToInt, Utils}

/**
  * Classical forward+may analysis that collects the place of latest write to every variable on each statement
  */
object ReachingDefinitions extends ClassicalAnalysis {
  override type Label = Int
  override type Prop = (String, Option[Int])
  override type Start = Labeled1
  override type RelevantNode = SLabeledStatement
  override type Result = AnalysisResult

  override def prepareProp(prop: Prop): String =
    (prop._1, prop._2.map(_.toString).getOrElse("?")).toString

  override def labelMap(ast: Start): Map[Label, RelevantNode] = Utils.labelToAstMap(ast)

  override def lattice(ast: Start): SetLattice[Prop] = {
    val refs = Utils.collectRefs(ast)
    val labels = Utils.labelToAstMap(ast).keySet
    val top = Utils.cartesianProduct(refs, labels.map(Some)).union(refs.map((_, None)))
    SetLattice.mayAnalysis(top)
  }

  override def flow(ast: Start): IntraControlFlow[Label] = Utils.flow(ast)

  override def extremalValue(ast: Start): Set[Prop] = {
    val refs = Utils.collectRefs(ast)
    refs.map((_, None))
  }

  override def run(ast: Start): Result = {
    val analysisResult = analyze(ast)
    val labelToOrigin: Map[Label, Origin] = Utils.labelToOriginMap(ast)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  override def kill(nodes: RelevantNode, property: Set[Prop]): Set[Prop] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => property.filter(p => id1.string == p._1)
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }

  override def gen(nodes: RelevantNode): Set[Prop] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => Set((id1.string, Some(SINTToInt(int3))))
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }
}
