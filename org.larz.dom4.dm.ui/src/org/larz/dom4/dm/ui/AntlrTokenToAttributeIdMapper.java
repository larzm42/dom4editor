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

import org.eclipse.jface.text.IDocument;
import org.eclipse.xtext.ui.editor.model.ITokenTypeToPartitionTypeMapper;
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultAntlrTokenToAttributeIdMapper;

public class AntlrTokenToAttributeIdMapper extends DefaultAntlrTokenToAttributeIdMapper implements ITokenTypeToPartitionTypeMapper {

	private static final String[] SUPPORTED_PARTITION_TYPES = { 
		IDocument.DEFAULT_CONTENT_TYPE
	};
	
	// see org.eclipse.xtext.ui.editor.model.TerminalsTokenTypeToPartitionMapper
	public String getPartitionType(int antlrTokenType) {
		return getMappedValue(antlrTokenType);
	}
	
	public String[] getSupportedPartitionTypes() {
		return SUPPORTED_PARTITION_TYPES;
	}

}
