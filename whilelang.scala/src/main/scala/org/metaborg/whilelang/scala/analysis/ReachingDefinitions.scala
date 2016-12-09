package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{IntraControlFlow, SetLattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax.{AnalysisResult, EditorMessage}
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.Labeled1
import org.metaborg.whilelang.scala.ast.Utils
import org.metaborg.whilelang.scala.ast.SINTToInt
import org.strategoxt.lang.Context

/**
  * Classical analysis which is a forward, may analysis
  */
object ReachingDefinitions extends ClassicalAnalysis[Int, (String, Option[Int]), Labeled1, SLabeledStatement] {
  override def prepareProp(prop: (String, Option[Int])): String = (prop._1, prop._2.map(_.toString).getOrElse("?"))
    .toString

  override def labelMap(ast: Labeled1): Map[Int, SLabeledStatement] = Utils.labelToAstMap(ast)

  override def lattice(ast: Labeled1): SetLattice[(String, Option[Int])] = {
    val refs = Utils.collectRefs(ast)
    val labels = Utils.labelToAstMap(ast).keySet
    val top = Utils.cartesianProduct(refs, labels.map(Some)).union(refs.map((_, None)))
    SetLattice.mayAnalysis(top)
  }

  override def flow(ast: Labeled1): IntraControlFlow[Int] = Utils.flow(ast)

  override def extremalValue(ast: Labeled1): Set[(String, Option[Int])] = {
    val refs = Utils.collectRefs(ast)
    refs.map((_, None))
  }

  override def run(ast: Labeled1): AnalysisResult = {
    val analysisResult = analyze(ast)
    val labelToOrigin: Map[Int, Origin] = Utils.labelToOriginMap(ast)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  override def kill(nodes: SLabeledStatement,
                    property: Set[(String, Option[Int])]): Set[(String, Option[Int])] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => property.filter(p => id1.string == p._1)
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }

  override def gen(nodes: SLabeledStatement): Set[(String, Option[Int])] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => Set((id1.string, Some(SINTToInt(int3))))
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }
}
