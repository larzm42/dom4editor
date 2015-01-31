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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.custom.TableEditor;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.NameInst1;
import org.larz.dom4.dm.dm.NameInst2;
import org.larz.dom4.dm.dm.NameMods;
import org.larz.dom4.dm.dm.SelectName;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

public class NameDetailsPage extends AbstractDetailsPage {
	private TableViewer tv;
	private TableEditor editor;

	private Text id;
	private Button clear;

	/**
	 * @param doc
	 * @param viewer
	 */
	public NameDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.forms.IDetailsPage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	public void createContents(Composite parent) {
		TableWrapLayout layout = new TableWrapLayout();
		layout.topMargin = 5;
		layout.leftMargin = 5;
		layout.rightMargin = 2;
		layout.bottomMargin = 2;
		parent.setLayout(layout);

		FormToolkit toolkit = mform.getToolkit();
		Section s1 = toolkit.createSection(parent, Section.DESCRIPTION|Section.TITLE_BAR);
		s1.marginWidth = 10;
		s1.setText(Messages.getString("NameDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 3;
		client.setLayout(glayout);
		
		Composite nameComp = toolkit.createComposite(client);
		nameComp.setLayout(new GridLayout(2, false));
		GridData gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.horizontalSpan = 3;
		nameComp.setLayoutData(gd);

		toolkit.createLabel(nameComp, Messages.getString("NameDetailsSection.mod.name")); //$NON-NLS-1$
		
		id = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		id.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setNameId(doc, id.getText());
			}			
		});
		id.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setNameId(doc, id.getText());
				}
			}
			
		});
		
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 200;
		id.setLayoutData(gd);
		
		clear = toolkit.createButton(nameComp, "clear", SWT.CHECK);
		setToolTip(clear, HelpTextHelper.getText(HelpTextHelper.NAME_CATEGORY, "clear"));
		clear.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (clear.getSelection()) {
					addClear(doc, input);
					clear.setFont(boldFont);
				} else {
					removeClear(doc);
					clear.setFont(normalFont);
				}
			}

		});
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.horizontalSpan = 3;
		clear.setLayoutData(gd);

		tv = new TableViewer(nameComp, SWT.BORDER | SWT.FULL_SELECTION);
		gd = new GridData(GridData.FILL_BOTH);
		gd.horizontalSpan = 2;
		gd.heightHint = 400;
		tv.getTable().setLayoutData(gd);
		tv.setContentProvider(new ArrayContentProvider());
		tv.getTable().setLinesVisible(true);
		tv.getTable().setHeaderVisible(false); 
		
		TableViewerColumn column = new TableViewerColumn(tv,SWT.NONE);
		column.getColumn().setWidth(200);
		column.setLabelProvider(new ColumnLabelProvider(){
			@Override
			public String getText(Object element) {
				return ((NameInst1)element).getValue();
			}
		});
		
		tv.setInput(input != null ? getNames((SelectName)input).toArray() : null);

		editor = new TableEditor(tv.getTable());
		//The editor must have the same size as the cell and must
		//not be any smaller than 50 pixels.
		editor.horizontalAlignment = SWT.LEFT;
		editor.grabHorizontal = true;
		editor.minimumWidth = 50;
		// editing the second column
		final int EDITABLECOLUMN = 0;
		
		tv.getTable().addSelectionListener(new SelectionAdapter() {
			public void widgetDefaultSelected(SelectionEvent e) {
				// Clean up any previous editor control
				Control oldEditor = editor.getEditor();
				
				if (oldEditor != null) {
					oldEditor.dispose();
				}
		
				// Identify the selected row
				final TableItem item = (TableItem)e.item;
				if (item == null) return;
		
				// The control that will be the editor must be a child of the Table
				final Text newEditor = new Text(tv.getTable(), SWT.NONE);
				newEditor.setText(item.getText(EDITABLECOLUMN));
				newEditor.addModifyListener(new ModifyListener() {
					public void modifyText(ModifyEvent me) {
						Text text = (Text)editor.getEditor();
						editor.getItem().setText(EDITABLECOLUMN, text.getText());
						int row = tv.getTable().getSelectionIndex();
						setInst2(doc, row, newEditor.getText());
					}
				});
				newEditor.selectAll();
				newEditor.setFocus();
				editor.setEditor(newEditor, item, EDITABLECOLUMN);
				
			}
		});
		
		Button addName = toolkit.createButton(nameComp, "Add Name", SWT.PUSH);
		addName.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				NameInst1 newName = addInst2(doc, input, "");
				tv.setSelection(new StructuredSelection(newName));
			}
			
		});
		
		MenuManager menuManager = new MenuManager();
		Menu menu = menuManager.createContextMenu(tv.getTable());
		menuManager.add(new Action("Delete") {
			@Override
			public void run() {
				removeInst2(doc, tv.getTable().getSelectionIndex());
				tv.setInput(input != null ? getNames((SelectName)input).toArray() : null);
				tv.refresh();
			}
			
		});
		tv.getTable().setMenu(menu);

	}

	class MasterLabelProvider extends LabelProvider implements ITableLabelProvider {
		public String getColumnText(Object obj, int index) {
			return (String)obj;
		}

		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}
		
	}

	public void update() {
		if (input != null) {
			String str = getNameId((SelectName)input);
			id.setText(str!=null?str:"");
			
			clear.setSelection(getClear((SelectName)input));
			clear.setFont(getClear((SelectName)input)? boldFont : normalFont);

			tv.setInput(input != null ? getNames((SelectName)input).toArray() : null);
		}
	}
	
	private String getNameId(SelectName name) {
		return Integer.toString(name.getValue());
	}
	
	private void setNameId(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				SelectName armorToEdit = (SelectName)input;
				armorToEdit.setValue(Integer.parseInt(newName));
			}  
		});

		updateSelection();
	}

	private Boolean getClear(SelectName armor) {
		EList<NameMods> list = armor.getMods();
		for (NameMods mod : list) {
			if (mod instanceof NameInst2) {
				if (((NameInst2)mod).isClear()) {
					return Boolean.TRUE;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private void addClear(final XtextEditor editor, final Object armor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<NameMods> mods = ((SelectName)input).getMods();
						NameInst2 type = DmFactory.eINSTANCE.createNameInst2();
						type.setClear(true);
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void removeClear(final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						NameMods modToRemove = null;
						EList<NameMods> mods = ((SelectName)input).getMods();
						for (NameMods mod : mods) {
							if (mod instanceof NameInst2) {
								if (((NameInst2)mod).isClear()) {
									modToRemove = mod;
								}
								break;
							}
						}
						if (modToRemove != null) {
							mods.remove(modToRemove);
						}
					}  
				});

				updateSelection();
			}
		});
	}

	
	private List<NameInst1> getNames(SelectName name) {
		List<NameInst1> nameList = new ArrayList<NameInst1>();
		EList<NameMods> list = name.getMods();
		for (NameMods mod : list) {
			if (mod instanceof NameInst1) {
				if (((NameInst1)mod).isName()) {
					nameList.add((NameInst1)mod);
				}
			}
		}
		return nameList;
	}
	
	private void setInst2(final XtextEditor editor, final int row, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				SelectName armorToEdit = (SelectName)input;
				int counter = 0;
				EList<NameMods> mods = armorToEdit.getMods();
				for (NameMods mod : mods) {
					if (mod instanceof NameInst1) {
						if (((NameInst1)mod).isName()) {
							if (counter == row) {
								((NameInst1)mod).setValue(newName);
								break;
							}
							counter++;
						}
					}
				}

			}  
		});

		updateSelection();
	}

	private NameInst1 addInst2(final XtextEditor editor, final Object armor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		NameInst1 inst1 = myDocument.modify(new IUnitOfWork<NameInst1, XtextResource>() {     
			@Override
			public NameInst1 exec(XtextResource resource) {
				SelectName armorToEdit = (SelectName)input;
				EList<NameMods> mods = armorToEdit.getMods();
				NameInst1 type = DmFactory.eINSTANCE.createNameInst1();
				type.setName(true);
				type.setValue(newName);
				mods.add(type);
				return type;
			}  
		});

		tv.setInput(input != null ? getNames((SelectName)input).toArray() : null);

		updateSelection();
		return inst1;
	}
	
	private void removeInst2(final XtextEditor editor, final int row) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				SelectName armorToEdit = (SelectName)input;
				NameMods modToRemove = null;
				int counter = 0;
				EList<NameMods> mods = armorToEdit.getMods();
				for (NameMods mod : mods) {
					if (mod instanceof NameInst1) {
						if (((NameInst1)mod).isName()) {
							if (counter == row) {
								modToRemove = mod;
								break;
							}
							counter++;
						}
					}
				}
				if (modToRemove != null) {
					mods.remove(modToRemove);
				}
			}  
		});

		updateSelection();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.forms.IDetailsPage#inputChanged(org.eclipse.jface.viewers.IStructuredSelection)
	 */
	public void selectionChanged(IFormPart part, ISelection selection) {
		Control oldEditor = editor.getEditor();
		
		if (oldEditor != null) {
			oldEditor.dispose();
		}
		tv.cancelEditing();
		IStructuredSelection ssel = (IStructuredSelection)selection;
		if (ssel.size()==1) {
			input = (SelectName)((AbstractElementWrapper)ssel.getFirstElement()).getElement();
		} else {
			input = null;
		}
		update();
	}
	
}
