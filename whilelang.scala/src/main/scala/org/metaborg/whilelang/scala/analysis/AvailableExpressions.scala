package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{IntraControlFlow, SetLattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax.{AnalysisResult, EditorMessage}
import org.metaborg.whilelang.scala.ast.MExpr.SExpr
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart.Labeled1
import org.metaborg.whilelang.scala.ast.Utils
import org.strategoxt.lang.Context

/**
  * Classical analysis which is a forward, must analysis
  */
object AvailableExpressions extends ClassicalAnalysis[Int, SExpr, Labeled1, SLabeledStatement] {
  override def run(ast: Labeled1): AnalysisResult = {
    val analysisResult = analyze(ast)
    val labelToOrigin: Map[Int, Origin] = Utils.labelToOriginMap(ast)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  override def prepareProp(prop: SExpr): String = Utils.toString(prop)

  override def labelMap(ast: Labeled1): Map[Int, SLabeledStatement] = Utils.labelToAstMap(ast)

  override def lattice(ast: Labeled1): SetLattice[SExpr] = SetLattice.mustAnalysis[SExpr](Utils.collectAExprs(ast))

  override def flow(ast: Labeled1): IntraControlFlow[Int] = Utils.flow(ast)

  override def extremalValue(ast: Labeled1): Set[SExpr] = Set.empty

  override def kill(nodes: SLabeledStatement,
                    property: Set[SExpr]): Set[SExpr] = nodes match {
    case LabeledAssign3(id1, expr2, int3) => property.filter(p => Utils.collectRefs(p).contains(id1.string))
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
    case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
  }

  override def gen(nodes: SLabeledStatement): Set[SExpr] = nodes match {
    case LabeledAssign3(id1, expr2, int3) =>
      Utils.collectAExprs(expr2).filterNot(p => Utils.collectRefs(p).contains(id1.string))
    case LabeledSkip1(int1) => Set.empty
    case Seq2(labeledstatement1, labeledstatement2) => Set.empty
    case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) =>
      Utils.collectAExprs(expr1)
    case LabeledWhile3(expr1, int2, labeledstatement3) => Utils.collectAExprs(expr1)
  }
}
