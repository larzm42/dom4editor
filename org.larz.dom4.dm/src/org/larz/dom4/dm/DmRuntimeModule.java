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
package org.larz.dom4.dm;

import org.eclipse.xtext.conversion.IValueConverterService;
import org.eclipse.xtext.parser.IParser;
import org.larz.dom4.dm.converter.DmValueConverter;

/**
 * Use this class to register components to be used at runtime / without the Equinox extension registry.
 */
public class DmRuntimeModule extends org.larz.dom4.dm.AbstractDmRuntimeModule {
	@Override
	public Class<? extends IParser> bindIParser() {
		return org.larz.dom4.dm.parser.DmParser.class;
	}

	@Override
	public Class<? extends IValueConverterService> bindIValueConverterService() {
	  return DmValueConverter.class;
	}

}
