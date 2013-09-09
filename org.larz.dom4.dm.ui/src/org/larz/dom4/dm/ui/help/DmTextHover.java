/* This file is part of dom4Editor.
 *
 * dom4Editor is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * dom4Editor is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with dom4Editor.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.larz.dom4.dm.ui.help;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Point;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.resource.EObjectAtOffsetHelper;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.hover.ProblemAnnotationHover;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;

public class DmTextHover implements ITextHover, ITextHoverExtension2 {
	/**
	 * As pointed out in the forum, we should put problem messages first.
	 */
	private ProblemAnnotationHover problem;

	public DmTextHover(ISourceViewer sourceViewer) {
		problem = new ProblemAnnotationHover() {
			@Override
			protected boolean isHandled(Annotation annotation) {
				return true;
			}
			
		};
		problem.setSourceViewer(sourceViewer);
	}

	@Override
	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		//This method is deprecated and replaced by ITextHoverExtension2.getHoverInfo2.
		return getHoverInfo2(textViewer, hoverRegion);
	}

	/**
	 * Simply a copy & paste from AbstractHover.getHoverRegion.
	 */
	@Override
	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		Point selection = textViewer.getSelectedRange();
		//x is the offset of the selection, y is the length of the selection.
		//If the given offset falls in between, return the selected range.
		if (selection.x <= offset && offset < selection.x + selection.y) {
			return new Region(selection.x, selection.y);
		}
		//Otherwise returns a zero-length region.
		return new Region(offset, 0);
	}

	@Override
	public String getHoverInfo2(ITextViewer textViewer, IRegion hoverRegion) {
		String problemString = (String) problem.getHoverInfo2(textViewer, hoverRegion);
		String ourString = getDmHover(textViewer, hoverRegion);
		//If there are problems, return them first.
		if(problemString!=null && !problemString.isEmpty()) {
			return problemString;//+"\n"+ourString;
		}
		//Otherwise, return our hover string.
		return ourString;
	}

	private String getDmHover(ITextViewer textViewer, final IRegion hoverRegion) {
		IXtextDocument doc = (IXtextDocument)textViewer.getDocument();
		return doc.readOnly(new IUnitOfWork<String, XtextResource>() {
			@Override
			public String exec(XtextResource state) throws Exception {
				String text = null;
				IParseResult parseResult = state.getParseResult();
				if (parseResult != null && parseResult.getRootNode() != null) {
					INode node = NodeModelUtils.findLeafNodeAtOffset(parseResult.getRootNode(), hoverRegion.getOffset());
					text = ((ILeafNode)node).getText();
				}

				EObjectAtOffsetHelper helper = new EObjectAtOffsetHelper();
				return HelpTextHelper.getText(
						helper.resolveElementAt(
								state, 
								hoverRegion.getOffset()), 
						text);
			}
		});
	}
}
