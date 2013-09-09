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
package org.larz.dom4.dm.ui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.parser.antlr.ISyntaxErrorMessageProvider;
import org.eclipse.xtext.parsetree.reconstr.impl.DefaultCommentAssociater;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.larz.dom4.dm.ui.comment.DmCommentAssociater;
import org.larz.dom4.dm.ui.editor.DmXtextEditor;
import org.larz.dom4.dm.ui.help.DmSourceViewerConfiguration;
import org.larz.dom4.dm.ui.syntax.DmSyntaxErrorMessageProvider;

/**
 * Use this class to register components to be used within the IDE.
 */
public class DmUiModule extends org.larz.dom4.dm.ui.AbstractDmUiModule {
	public DmUiModule(AbstractUIPlugin plugin) {
		super(plugin);
	}
	public Class<? extends XtextEditor> bindEditor() {
		return DmXtextEditor.class;
	}
	public Class<? extends DefaultCommentAssociater> bindCommentAssociater() {
		return DmCommentAssociater.class;
	}
	public Class<? extends ISyntaxErrorMessageProvider> bindISyntaxErrorMessageProvider() {
		return DmSyntaxErrorMessageProvider.class;
	}
	public Class<? extends org.eclipse.xtext.ui.editor.XtextSourceViewerConfiguration> bindSourceViewerConfiguration() {  
	    return DmSourceViewerConfiguration.class;  
    } 
}
