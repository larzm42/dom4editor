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
package org.larz.dom4.dm.ui.editor;

import java.util.Locale;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * We use an auxiliary class just to do a 'better' equals check
 */
public class LinkedFileEditorInput extends FileEditorInput
{
	public LinkedFileEditorInput(IFile file)
	{
		super(file);
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj) {
			return true;
		}
		String targetUri = ""; //$NON-NLS-1$
		if (!(obj instanceof IFileEditorInput))
		{
			if (obj instanceof FileStoreEditorInput)
			{
				FileStoreEditorInput other = (FileStoreEditorInput)obj;
				targetUri = other.getURI().toString();
			}
		}
		else
		{
			IFileEditorInput other = (IFileEditorInput) obj;
			targetUri = other.getFile().getLocationURI().toString();
		}
		if (targetUri.isEmpty())
			return false;
		return getFile().getLocationURI().toString().toLowerCase(Locale.ENGLISH).
				equals(targetUri.toLowerCase(Locale.ENGLISH));
	}
}
