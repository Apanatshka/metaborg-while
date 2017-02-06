package whilelang.lang.strategies;

import org.strategoxt.lang.JavaInteropRegisterer;
import org.strategoxt.lang.Strategy;

public class InteropRegisterer extends JavaInteropRegisterer {
	public InteropRegisterer() {
		super(new Strategy[] {
//				EditorServices.editor_analyze_0_0,
//				EditorServices.editor_hover_0_0,
//				EditorServices.editor_resolve_0_0
		});
	}
}
