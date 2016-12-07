package org.metaborg.whilelang.scala.analysis

import org.metaborg.popa.mfp.{IntraMaximalFixedPoint, SetLattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax._
import org.metaborg.whilelang.scala.ast.MExpr.SExpr
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement
import org.metaborg.whilelang.scala.ast.MStatement.SLabeledStatement._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart
import org.metaborg.whilelang.scala.ast.Utils
import org.strategoxt.lang.Context

import scala.language.implicitConversions


/**
  * `editor-analyze` implementation
  */
object EditorServicesImpl extends EditorServices {
  type LabelAE = Int
  type PropAE = SExpr

  override def editorAnalyze(generalStrategyInput: GeneralStrategyInput)
                            (implicit context: Context): AnalysisResult = {
    SStart.fromSTerm(generalStrategyInput.ast) match {
      case None =>
        val startOfFile = generalStrategyInput.ast.origin.get.zero
        generalStrategyInput
          .makeAnalysisResult(List(EditorMessage("Unlabeled ast cannot be analysed", startOfFile)), List(), List())
      case Some(ast) => availableExpressions(ast)
    }
  }

  def availableExpressions(ast: SStart)(implicit context: Context): AnalysisResult = {

    val labeledAst = Utils.label(ast)
    implicit val labelToAstNode: Map[LabelAE, SLabeledStatement] = Utils.labelToAstMap(labeledAst)
    val allExprs = Utils.collectAExprs(ast)

    val intraMaximalFixedPoint =
      new IntraMaximalFixedPoint[LabelAE, Set[PropAE]](SetLattice.mustAnalysis[PropAE](allExprs),
                                                       IntraMaximalFixedPoint.classicalTransfer(killAE, genAE),
                                                       Utils.flow(labeledAst),
                                                       extremalValue = Set.empty)

    val analysisResult: Map[LabelAE, (Set[PropAE], Set[PropAE])] = intraMaximalFixedPoint.result
    val labelToOrigin: Map[LabelAE, Origin] = Utils.labelToOriginMap(labeledAst)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  def prepareProperties[Prop](res: (Set[Prop], Set[Prop])): (String, String) =
    (prepareProperty(res._1), prepareProperty(res._2))

  def prepareProperty[Prop](property: Set[Prop]): String = property.mkString("{", ", ", "}")

  def killAE(label: LabelAE, prop: Set[PropAE])(implicit labelToAstNode: Map[LabelAE, SLabeledStatement]): Set[PropAE] =
    labelToAstNode(label) match {
      case LabeledAssign3(id1, expr2, int3) => prop.filter(p => Utils.collectRefs(p).contains(id1.string))
      case LabeledSkip1(int1) => Set.empty
      case Seq2(labeledstatement1, labeledstatement2) => Set.empty
      case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) => Set.empty
      case LabeledWhile3(expr1, int2, labeledstatement3) => Set.empty
    }

  def genAE(label: LabelAE, prop: Set[PropAE])(implicit labelToAstNode: Map[LabelAE, SLabeledStatement]): Set[PropAE] =
    labelToAstNode(label) match {
      case LabeledAssign3(id1, expr2, int3) =>
        Utils.collectAExprs(expr2).filterNot(p => Utils.collectRefs(p).contains(id1.string))
      case LabeledSkip1(int1) => Set.empty
      case Seq2(labeledstatement1, labeledstatement2) => Set.empty
      case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4) =>
        Utils.collectAExprs(expr1)
      case LabeledWhile3(expr1, int2, labeledstatement3) => Utils.collectAExprs(expr1)
    }
}