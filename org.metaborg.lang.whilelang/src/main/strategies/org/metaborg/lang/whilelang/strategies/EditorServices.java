package org.metaborg.lang.whilelang.strategies;

import org.spoofax.interpreter.terms.IStrategoTerm;
import org.strategoxt.lang.Context;
import org.strategoxt.lang.Strategy;
import org.metaborg.lang.whilelang.analysis.*;

public class EditorServices {
	public static final editor_analyze_0_0 editor_analyze_0_0 = new editor_analyze_0_0();
	
	static class editor_analyze_0_0 extends Strategy {
		@Override
		public IStrategoTerm invoke(Context context, IStrategoTerm current) {
			return EditorServicesImpl$.MODULE$.editorAnalyze(context, current);
		}
	}
}
