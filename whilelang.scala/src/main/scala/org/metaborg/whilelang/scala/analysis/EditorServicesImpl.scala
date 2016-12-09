package org.metaborg.whilelang.scala.analysis

import org.metaborg.scalaterms.spoofax._
import org.metaborg.whilelang.scala.ast.MWhilelang.SStart
import org.metaborg.whilelang.scala.ast.Utils
import org.strategoxt.lang.Context

import scala.language.implicitConversions


/**
  * `editor-analyze` implementation
  */
object EditorServicesImpl extends EditorServices {
  override def editorAnalyze(generalStrategyInput: GeneralStrategyInput)
                            (implicit context: Context): AnalysisResult = {
    SStart.fromSTerm(generalStrategyInput.ast) match {
      case None =>
        val startOfFile = generalStrategyInput.ast.origin.get.zero
        generalStrategyInput
          .makeAnalysisResult(List(EditorMessage("Unlabeled ast cannot be analysed", startOfFile)), List(), List())
      case Some(ast) =>
        val labeledAst = Utils.label(ast)
        val ae = AvailableExpressions.run(labeledAst)
        val rd = ReachingDefinitions.run(labeledAst)
        AnalysisResult(rd.ast, ae.errors ++ rd.errors, ae.warnings ++ rd.warnings, ae.notes ++ rd.notes)
    }
  }
}