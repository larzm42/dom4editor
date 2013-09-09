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

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.larz.dom4.dm.ui.editor.DmXtextEditor;
import org.larz.dom4.dm.ui.editor.LinkedFileEditorInput;

public class Open extends AbstractHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		BusyIndicator.showWhile(Display.getCurrent(), new Runnable() {
			@Override
			public void run() {
				IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				FileDialog dialog = new FileDialog(window.getShell(), SWT.OPEN);
				dialog.setFilterExtensions(new String[]{"*.dm"});
				dialog.open();
				String[] names = dialog.getFileNames();
				String filterPath = System.getProperty("user.home"); //$NON-NLS-1$

				if (names != null) {
					filterPath =  dialog.getFilterPath();

					for (int i =  0; i < names.length; i++) {
						IFileStore fileStore =  EFS.getLocalFileSystem().getStore(new Path(filterPath));
						if (!names[i].endsWith(".dm")) {
							names[i] += ".dm";
						}
						fileStore =  fileStore.getChild(names[i]);

						try {
							// open the editor on the file
							window.getActivePage().openEditor(getEditorInput(fileStore), getEditorId(fileStore));

						} catch (PartInitException e) {
							MessageDialog.open(MessageDialog.ERROR, window.getShell(), Messages.getString("OpenFileError.title"), Messages.format("OpenFileError.message", fileStore.getName()), SWT.SHEET);
						}
					}

				}
			}
		});
		return null;
	}
	private static IEditorInput getEditorInput(IFileStore fileStore) {
		IFile workspaceFile = getWorkspaceFile(fileStore);
		if (workspaceFile != null) {
			return new FileEditorInput(workspaceFile);
		}

		URI uri = fileStore.toURI();

		// Check if this is linkable input
		if(uri.getScheme().equals("file")) { //$NON-NLS-1$
			return new LinkedFileEditorInput(DmXtextEditor.obtainLink(uri));
		}
		return new FileStoreEditorInput(fileStore);
	}
	private static IFile getWorkspaceFile(IFileStore fileStore) {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IFile[] files = root.findFilesForLocationURI(fileStore.toURI());
		files = filterNonExistentFiles(files);
		if (files == null || files.length == 0)
			return null;

		// for now only return the first file
		return files[0];
	}
	private static IFile[] filterNonExistentFiles(IFile[] files) {
		if (files == null)
			return null;

		int length = files.length;
		ArrayList<IFile> existentFiles = new ArrayList<IFile>(length);
		for (int i = 0; i < length; i++) {
			if (files[i].exists())
				existentFiles.add(files[i]);
		}
		return (IFile[]) existentFiles.toArray(new IFile[existentFiles.size()]);
	}
	private static String getEditorId(IFileStore fileStore) throws PartInitException {
		String name = fileStore.fetchInfo().getName();
		if (name == null) {
			throw new IllegalArgumentException();
		}

		IContentType contentType= null;
		try {
			InputStream is = null;
			try {
				is = fileStore.openInputStream(EFS.NONE, null);
				contentType= Platform.getContentTypeManager().findContentTypeFor(is, name);
			} finally {
				if (is != null) {
					is.close();
				}
			}
		} catch (CoreException ex) {
			// continue without content type
		} catch (IOException ex) {
			// continue without content type
		}

		IEditorRegistry editorReg= PlatformUI.getWorkbench().getEditorRegistry();

		return getEditorDescriptor(name, editorReg, editorReg.getDefaultEditor(name, contentType)).getId();
	}
	private static IEditorDescriptor getEditorDescriptor(String name,
			IEditorRegistry editorReg, IEditorDescriptor defaultDescriptor)
					throws PartInitException {

		if (defaultDescriptor != null) {
			return defaultDescriptor;
		}

		IEditorDescriptor editorDesc = defaultDescriptor;

		// next check the OS for in-place editor (OLE on Win32)
		if (editorReg.isSystemInPlaceEditorAvailable(name)) {
			editorDesc = editorReg
					.findEditor(IEditorRegistry.SYSTEM_INPLACE_EDITOR_ID);
		}

		// next check with the OS for an external editor
		if (editorDesc == null
				&& editorReg.isSystemExternalEditorAvailable(name)) {
			editorDesc = editorReg
					.findEditor(IEditorRegistry.SYSTEM_EXTERNAL_EDITOR_ID);
		}

		// next lookup the default text editor
		if (editorDesc == null) {
			editorDesc = editorReg
					.findEditor("org.eclipse.ui.DefaultTextEditor"/*IDEWorkbenchPlugin.DEFAULT_TEXT_EDITOR_ID*/);
		}

		// if no valid editor found, bail out
		if (editorDesc == null) {
			throw new PartInitException(
					Messages.getString("NoEditorFound.message")/*IDEWorkbenchMessages.IDE_noFileEditorFound*/);
		}

		return editorDesc;
	}

} 