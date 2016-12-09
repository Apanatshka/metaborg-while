package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{IntraControlFlow, SetLattice, _}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax.{AnalysisResult, EditorMessage}
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.Labeled1
import org.metaborg.whilelang.scala.ast._

/**
  * Classical backward+may analysis that collects variables that may be live after each statement.
  */
object LiveVariables extends ClassicalAnalysis {
  override type Label = Int
  override type Prop = String
  override type Start = Labeled1
  override type RelevantNode = SLabeledStatement
  override type Result = AnalysisResult

  override def labelMap(ast: Start): Map[Label, RelevantNode] = Utils.labelToAstMap(ast)

  override def lattice(ast: Start): SetLattice[Prop] = SetLattice.mayAnalysis(Utils.collectRefs(ast))

  override def flow(ast: Start): IntraControlFlow[Label] = Utils.flow(ast).reverse

  override def extremalValue(ast: Start): Set[Prop] = Set.empty

  override def run(ast: Start): Result = {
    val analysisResult = analyze(ast)
    val labelToOrigin: Map[Label, Origin] = Utils.labelToOriginMap(ast)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  override def kill(nodes: RelevantNode, property: Set[Prop]): Set[Prop] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => Set(id1.string)
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }

  override def gen(nodes: RelevantNode): Set[Prop] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => Utils.collectRefs(expr2)
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Utils.collectRefs(expr1)
    case LabeledWhile3(expr1, int2, labeledstatement3) => Utils.collectRefs(expr1)
  }

}
