package org.metaborg.lang.whilelang.analysis

import org.metaborg.lang.whilelang.ast.MExpr.SExpr
import org.metaborg.lang.whilelang.ast.MStatement.SLabeledStatement
import org.metaborg.lang.whilelang.ast.MStatement.SLabeledStatement._
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart
import org.metaborg.lang.whilelang.ast.Utils
import org.metaborg.popa.mfp.{IntraMaximalFixedPoint, SetLattice}
import org.metaborg.scalaterms.spoofax.Debug.{print => debug}
import org.metaborg.scalaterms.spoofax._
import org.metaborg.scalaterms.{Origin, STerm, TermLike}
import org.strategoxt.lang.Context

import scala.language.implicitConversions


/**
  * `editor-analyze` implementation
  */
object EditorServicesImpl extends EditorServices {
  type LabelAE = Int

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
      new IntraMaximalFixedPoint[LabelAE, Set[AEProp]](SetLattice.mustAnalysis[AEProp](allExprs),
                                                       IntraMaximalFixedPoint.classicalTransfer(killAE, genAE),
                                                       Utils.flow(labeledAst),
                                                       extremalValue = Set.empty)

    val analysisResult: Map[LabelAE, (Set[AEProp], Set[AEProp])] = intraMaximalFixedPoint.result
    val labelToOrigin: Map[LabelAE, Origin] = Utils.labelToOriginMap(labeledAst)
    val notes: List[EditorMessage] = analysisResult.toList
      .map({ case (lbl, res) => EditorMessage.from(prepareProperties(res), labelToOrigin(lbl)) })

    AnalysisResult(ast.toSTerm, errors = List(), warnings = List(), notes)
  }

  def prepareProperties[Prop](res: (Set[Prop], Set[Prop])): (String, String) =
    (prepareProperty(res._1), prepareProperty(res._2))

  def prepareProperty[Prop](property: Set[Prop]): String = property.mkString("{", ", ", "}")

  def killAE(label: LabelAE, prop: Set[AEProp])(implicit labelToAstNode: Map[LabelAE, SLabeledStatement]): Set[AEProp] =
    labelToAstNode(label) match {
      case LabeledAssign3(id1, expr2, int3, origin) => prop.filter(p => Utils.collectRefs(p.sexpr).contains(id1.string))
      case LabeledSkip1(int1, origin) => Set.empty
      case Seq2(labeledstatement1, labeledstatement2, origin) => Set.empty
      case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) => Set.empty
      case LabeledWhile3(expr1, int2, labeledstatement3, origin) => Set.empty
    }

  def genAE(label: LabelAE, prop: Set[AEProp])(implicit labelToAstNode: Map[LabelAE, SLabeledStatement]): Set[AEProp] =
    labelToAstNode(label) match {
      case LabeledAssign3(id1, expr2, int3, origin) =>
        Utils.collectAExprs(expr2).filterNot(p => Utils.collectRefs(p).contains(id1.string))
      case LabeledSkip1(int1, origin) => Set.empty
      case Seq2(labeledstatement1, labeledstatement2, origin) => Set.empty
      case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) =>
        Utils.collectAExprs(expr1)
      case LabeledWhile3(expr1, int2, labeledstatement3, origin) => Utils.collectAExprs(expr1)
    }

  implicit def sExprsToProperty(sExprs: Set[SExpr]): Set[AEProp] = sExprs.map(new AEProp(_))
}

class AEProp(val sexpr: SExpr) extends TermLike {
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: AEProp => Utils.equals(this.sexpr, that.sexpr)
    case _ => false
  }

  override def toString: String = Utils.toString(this.sexpr)

  override def hashCode(): Int = Utils.hashCode(this.sexpr)

  /**
    * @return equivalent Scala ATerm representation
    */
  override def toSTerm: STerm = sexpr.toSTerm
}
