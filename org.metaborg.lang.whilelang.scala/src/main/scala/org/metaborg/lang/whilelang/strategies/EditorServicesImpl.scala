package org.metaborg.lang.whilelang.strategies

import org.metaborg.lang.whilelang.ast.MExpr.SExpr
import org.metaborg.lang.whilelang.ast.MStatement.SLabeledStatement._
import org.metaborg.lang.whilelang.ast.MWhilelang.SStart
import org.metaborg.lang.whilelang.ast.Utils
import org.metaborg.popa.mfp.{IntraControlFlow, IntraMaximalFixedPoint, Lattice}
import org.metaborg.scalaterms.Origin
import org.metaborg.scalaterms.spoofax._
import org.spoofax.interpreter.core.Context
import org.spoofax.terms.util.NotImplementedException


/**
  * Created by jeff on 28/11/16.
  */
class EditorServicesImpl extends EditorServices {
  override def editorAnalyze(generalStrategyInput: GeneralStrategyInput)
                            (implicit context: Context): AnalysisResult = {
    val startOfFile = generalStrategyInput.ast.origin.zero

    type Label = Int
    type Property = Set[SExpr]

    SStart.fromSTerm(generalStrategyInput.ast) match {
      case None => generalStrategyInput
        .makeAnalysisResult(List(EditorMessage("Unlabeled ast cannot be analysed", startOfFile)), List(), List())
      case Some(ast) =>
        val labeledAst = Utils.label(ast)
        val labelToAstNode = Utils.labelToAstMap(labeledAst)
        val flow: IntraControlFlow[Int] = Utils.flow(labeledAst)
        val allExprs = Utils.collectExprs(ast)

        val lattice = new Lattice[Property] {
          override val top: Set[SExpr] = Set.empty
          override val bottom: Set[SExpr] = allExprs

          override def lte(one: Set[SExpr],
                           other: Set[SExpr]): Boolean = other.subsetOf(one)

          override def glb(one: Set[SExpr],
                           other: Set[SExpr]): Set[SExpr] = one.union(other)

          override def lub(one: Set[SExpr],
                           other: Set[SExpr]): Set[SExpr] = one.intersect(other)
        }

        def kill(label: Label): Property = labelToAstNode(label) match {
          case LabeledAssign3(id1, expr2, int3, origin) => allExprs.filter(Utils.collectRefs(_).contains(id1.string))
          case LabeledSkip1(int1, origin) => Set.empty
          case Seq2(labeledstatement1, labeledstatement2, origin) => Set.empty
          case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) => Set.empty
          case LabeledWhile3(expr1, int2, labeledstatement3, origin) => Set.empty
        }
        def gen(label: Label): Property = labelToAstNode(label) match {
          case LabeledAssign3(id1, expr2, int3, origin) => Utils.collectExprs(expr2)
          case LabeledSkip1(int1, origin) => Set.empty
          case Seq2(labeledstatement1, labeledstatement2, origin) => Set.empty
          case LabeledIfThenElse4(expr1, int2, labeledstatement3, labeledstatement4, origin) =>
            Utils.collectExprs(expr1)
          case LabeledWhile3(expr1, int2, labeledstatement3, origin) => Utils.collectExprs(expr1)
        }

        val intraMaximalFixedPoint =
          new IntraMaximalFixedPoint[Label, Property](lattice,
                                                      IntraMaximalFixedPoint.transfer(kill, gen),
                                                      flow,
                                                      extremalLabels = Set(flow.init),
                                                      extremalValue = Set.empty)
        val analysisResult: Map[Label, (Property, Property)] = intraMaximalFixedPoint.result
        val labelToOrigin: Map[Label, Origin] = Utils.labelToOriginMap(labeledAst)
        val notes: List[EditorMessage] = analysisResult.toList
          .map({ case (lbl, res) => EditorMessage.from(res, labelToOrigin(lbl)) })

        generalStrategyInput.makeAnalysisResult(List(), List(), notes)
    }
  }

  override def editorResolve(focusedStrategyInput: FocusedStrategyInput)
                            (implicit context: Context): ResolutionResult = throw new NotImplementedException()

  override def editorHover(focusedStrategyInput: FocusedStrategyInput)
                          (implicit context: Context): HoverResult = throw new NotImplementedException()
}
