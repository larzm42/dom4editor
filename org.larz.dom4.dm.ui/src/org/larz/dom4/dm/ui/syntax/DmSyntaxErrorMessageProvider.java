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
package org.larz.dom4.dm.ui.syntax;

import java.text.MessageFormat;

import org.antlr.runtime.RecognitionException;
import org.eclipse.xtext.conversion.ValueConverterException;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage;
import org.eclipse.xtext.parser.antlr.ISyntaxErrorMessageProvider;


public class DmSyntaxErrorMessageProvider implements ISyntaxErrorMessageProvider {

	@Override
	public SyntaxErrorMessage getSyntaxErrorMessage(IParserErrorContext context) {
		String errorMessage = context.getDefaultMessage();
		RecognitionException recognitionException = context.getRecognitionException();
		if (recognitionException != null && recognitionException.token != null && recognitionException.token.getText() != null) {
			errorMessage = recognitionException.token.getText();
		}
		return new SyntaxErrorMessage(MessageFormat.format("Unexpected value: {0}", errorMessage), Diagnostic.SYNTAX_DIAGNOSTIC);
		
	}

	@Override
	public SyntaxErrorMessage getSyntaxErrorMessage(IValueConverterErrorContext context) {
		String errorMessage = context.getDefaultMessage();
		ValueConverterException valueConverterException = context.getValueConverterException();
		if (valueConverterException != null && valueConverterException.getMessage() != null) {
			errorMessage = valueConverterException.getMessage();
		}
		return new SyntaxErrorMessage(errorMessage, Diagnostic.SYNTAX_DIAGNOSTIC);
	}

}
