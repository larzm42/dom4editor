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
package org.larz.dom4;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.larz.dom4.editor.DmEditor;
import org.larz.dom4.editor.Messages;

public class FixIds extends AbstractHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		MessageDialog overwriteDialog = new MessageDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), Messages.getString("FixIDsDialog.title"), null, 
				Messages.getString("FixIDsDialog.text"), MessageDialog.INFORMATION, new String[] {
			IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL }, 1); // 'No' is the default
		if(overwriteDialog.open() != Window.OK) {
			return null;
		}
		IWorkbenchPage page = HandlerUtil.getActiveWorkbenchWindow(event).getActivePage();
		((DmEditor)page.getActiveEditor()).fixIdNumbers();
		return null;
	}
} 