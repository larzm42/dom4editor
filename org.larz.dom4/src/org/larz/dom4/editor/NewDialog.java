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
package org.larz.dom4.editor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

public class NewDialog extends Dialog {
	public boolean select;
	public Text modName;
	public Text descText;
	public Text versionText;
	
	
	/**
	 * @param parentShell
	 */
	public NewDialog(Shell parentShell) {
		super(parentShell);
        setShellStyle(getShellStyle() | SWT.RESIZE); 
	}

	/**
	 * @param parentShell
	 */
	public NewDialog(IShellProvider parentShell) {
		super(parentShell);
        setShellStyle(getShellStyle() | SWT.RESIZE); 
	}
	
	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(Messages.getString("NewDialog.title"));
	}

	@Override
	protected Point getInitialSize() {
		return new Point(500, 200);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
		
		final Composite composite = new Composite(comp, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		Label name = new Label(composite, SWT.NONE);
		name.setText(Messages.getString("NewDialog.name"));
		modName = new Text(composite, SWT.BORDER);
		modName.setLayoutData(new GridData(100, SWT.DEFAULT));
		modName.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				setOKEnablement();
			}
		});
		Label desc = new Label(composite, SWT.NONE);
		desc.setText(Messages.getString("NewDialog.desc"));
		descText = new Text(composite, SWT.BORDER | SWT.MULTI | SWT.WRAP);
		descText.setLayoutData(new GridData(400, SWT.DEFAULT));
		descText.addListener(SWT.Modify, new Listener() {			
			@Override
			public void handleEvent(Event event) {
				int currentHeight = descText.getSize().y;
				int preferredHeight = descText.computeSize(500, SWT.DEFAULT).y;
				if (currentHeight != preferredHeight) {
					GridData data = (GridData)descText.getLayoutData();
					data.heightHint = preferredHeight;
					composite.pack();
				}
				setOKEnablement();
			}
		});
		Label version = new Label(composite, SWT.NONE);
		version.setText(Messages.getString("NewDialog.version"));
		versionText = new Text(composite, SWT.BORDER);
		versionText.setLayoutData(new GridData(100, SWT.DEFAULT));
		versionText.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				setOKEnablement();
			}
		});

		return composite;
	}
		
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		getButton(IDialogConstants.OK_ID).setEnabled(false);
	}

	private void setOKEnablement() {
		if (modName.getText().equals("") || descText.getText().equals("") || versionText.getText().equals("")) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		} else {
			getButton(IDialogConstants.OK_ID).setEnabled(true);
		}
	}

	@Override
	protected void okPressed() {
		final String name = modName.getText();
		final String description = descText.getText();
		final String version = versionText.getText();
		super.okPressed();
		FileDialog dialog = new FileDialog(getParentShell(), SWT.SAVE);
		dialog.setFilterExtensions(new String[]{"*.dm"});
		dialog.open();
		String[] names =  dialog.getFileNames();
		String filterPath =  System.getProperty("user.home"); //$NON-NLS-1$

		if (names != null) {
			filterPath =  dialog.getFilterPath();

			for (int i =  0; i < names.length; i++) {
				IFileStore fileStore =  EFS.getLocalFileSystem().getStore(new Path(filterPath));
				if (!names[i].endsWith(".dm")) {
					names[i] += ".dm";
				}
				fileStore = fileStore.getChild(names[i]);
				IFileInfo fetchInfo = fileStore.fetchInfo();
				if (fetchInfo.exists()) {
					MessageDialog overwriteDialog = new MessageDialog(getParentShell(), Messages.getString("NewDialog.save.fileExists.title"), null, 
							Messages.format("NewDialog.save.fileExists.fmt", fetchInfo.getName()), MessageDialog.WARNING, new String[] {
							IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL }, 1); // 'No' is the default
					if(overwriteDialog.open() != Window.OK) {
						return;
					}
				}
				if (!fetchInfo.isDirectory()) {
					IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
					try {
						File newFile = fileStore.toLocalFile(0, null);

						newFile.createNewFile();
						FileWriter out = new FileWriter(newFile);
						out.write("#modname \"" + name + "\"\n");
						out.write("#description \"" + description + "\"\n");
						out.write("#version " + version + "\n");
						out.flush();
						out.close();

						IDE.openEditorOnFileStore(page, fileStore);
					} catch (PartInitException e) {
						MessageDialog.open(MessageDialog.ERROR, getParentShell(), Messages.getString("NewDialog.save.openError.title"), Messages.format("NewDialog.save.openError.fmt", fileStore.getName()), SWT.SHEET);
					} catch (IOException e) {
						e.printStackTrace();
					} catch (CoreException e) {
						e.printStackTrace();
					}
				}
			}

		}

	}

	

}
