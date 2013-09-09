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
package org.larz.dom4.dm.parser;

import java.lang.reflect.Field;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CharStream;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.parser.antlr.XtextTokenStream;
import org.larz.dom4.dm.services.DmGrammarAccess;

import com.google.inject.Inject;

public class DmParser extends org.eclipse.xtext.parser.antlr.AbstractAntlrParser {
	
	@Inject
	private DmGrammarAccess grammarAccess;
	
	@Override
	protected IParseResult parse(String ruleName, CharStream in) {
		if ("Dom4Mod".equals(ruleName)) {
			try {
				Field data = ANTLRStringStream.class.getDeclaredField("data");
				Field n = ANTLRStringStream.class.getDeclaredField("n");
				data.setAccessible(true);
				n.setAccessible(true);
				Object theData = data.get(in);
				if (theData instanceof char[]) {
					char[] chars = ((char[])theData);
					if (chars[0]!= '#' && chars[0] != '\n') {
						char[] newChars = new char[chars.length];
						System.arraycopy(chars, 0, newChars, 0, chars.length);
						newChars[0] = '\n';
						data.set(in, newChars);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			} 
		}
		return super.parse(ruleName, in);
	}
	
	@Override
	protected void setInitialHiddenTokens(XtextTokenStream tokenStream) {
		tokenStream.setInitialHiddenTokens("RULE_ML_COMMENT", "RULE_SL_COMMENT", "RULE_WS");
	}
	
	@Override
	protected org.larz.dom4.dm.parser.antlr.internal.InternalDmParser createParser(XtextTokenStream stream) {
		return new org.larz.dom4.dm.parser.antlr.internal.InternalDmParser(stream, getGrammarAccess());
	}
	
	@Override 
	protected String getDefaultRuleName() {
		return "Dom4Mod";
	}
	
	public DmGrammarAccess getGrammarAccess() {
		return this.grammarAccess;
	}
	
	public void setGrammarAccess(DmGrammarAccess grammarAccess) {
		this.grammarAccess = grammarAccess;
	}
	
}
