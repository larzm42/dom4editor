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

import java.util.EnumMap;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.larz.dom4.db.Database;
import org.larz.dom4.db.PoptypeDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.PoptypeInst1;
import org.larz.dom4.dm.dm.PoptypeInst2;
import org.larz.dom4.dm.dm.PoptypeMods;
import org.larz.dom4.dm.dm.SelectPoptype;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class PoptypeDetailsPage extends AbstractDetailsPage {
	private Text name;

	enum Inst {
		CLEARREC (Messages.getString("PoptypeDetailsSection.mod.clearrec")), 
		CLEARDEF (Messages.getString("PoptypeDetailsSection.mod.cleardef")), 
		ADDRECRUIT (Messages.getString("PoptypeDetailsSection.mod.addrecunit"), "1"),
		ADDRECCOM (Messages.getString("PoptypeDetailsSection.mod.addreccom"), "1");
		
		private String label;
		private String defaultValue;
		
		Inst(String label) {
			this.label = label;
		}

		Inst(String label, String defaultValue) {
			this.label = label;
			this.defaultValue = defaultValue;
		}
		
	}
	
	interface InstFields {}
	
	class Inst1Fields implements InstFields {
		private Button check;
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst2Fields implements InstFields {
		private Button check;
	}
	
	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);

	public PoptypeDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.CLEARREC, new Inst2Fields());
		instMap.put(Inst.CLEARDEF, new Inst2Fields());
		instMap.put(Inst.ADDRECRUIT, new Inst1Fields());
		instMap.put(Inst.ADDRECCOM, new Inst1Fields());
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
		s1.setText(Messages.getString("PoptypeDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 3;
		glayout.verticalSpacing = 0;
		client.setLayout(glayout);
		
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			final InstFields field = fields.getValue();
			final Button check = toolkit.createButton(client, key.label, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.POPTYPE_CATEGORY, key.label));
			Text myValue = null;
			Label defaultLabel = null;
			if (field instanceof Inst1Fields) {
				final Text value = toolkit.createText(client, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue = value;
				name = value;
				defaultLabel = toolkit.createLabel(client, "");
				defaultLabel.setEnabled(false);

				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							value.setText(key.defaultValue);
							if (field instanceof Inst1Fields) {
								addInst1(key, doc, key.defaultValue);
							} else {
								addInst2(key, doc);
							}
							check.setFont(boldFont);
						} else {
							value.setEnabled(false);
							value.setText("");
							removeInst(key, doc);
							check.setFont(normalFont);
						}
					}

				});
				value.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						setInst1(key, doc, value.getText());
					}			
				});
				value.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst1(key, doc, value.getText());
						}
					}

				});
				value.setEnabled(false);
				GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
			} else {
				createSpacer(toolkit, client, 2);
			}
			if (field instanceof Inst1Fields) {
				((Inst1Fields)fields.getValue()).check = check;
				((Inst1Fields)fields.getValue()).value = myValue;
				((Inst1Fields)fields.getValue()).defaultLabel = defaultLabel;
			} else {
				((Inst2Fields)fields.getValue()).check = check;
			}
		}

		createSpacer(toolkit, client, 2);
	}
	
	public void update() {
		PoptypeDB poptypeDB = new PoptypeDB();
		if (input instanceof SelectPoptype) {
			poptypeDB = Database.getPoptype(((SelectPoptype)input).getValue());
		}
		for (Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			String val1 = getInst1(fields.getKey(), (SelectPoptype)input);
			if (val1 != null) {
				if (fields.getValue() instanceof Inst1Fields) {
					((Inst1Fields)fields.getValue()).value.setText(val1);
					((Inst1Fields)fields.getValue()).value.setEnabled(true);
					((Inst1Fields)fields.getValue()).check.setSelection(true);
					((Inst1Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst1Fields) {
					((Inst1Fields)fields.getValue()).value.setText("");
					((Inst1Fields)fields.getValue()).value.setEnabled(false);
					((Inst1Fields)fields.getValue()).check.setSelection(false);
					((Inst1Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Boolean isVal = getInst2(fields.getKey(), (SelectPoptype)input);
			if (isVal != null) {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).check.setSelection(isVal);
				}
			}
			if (input instanceof SelectPoptype) {
				switch (fields.getKey()) {
				case ADDRECCOM:
					if (poptypeDB.addreccom != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", poptypeDB.addreccom.toString()));
						Inst.ADDRECCOM.defaultValue = poptypeDB.addreccom.toString();
					}
					break;
				case ADDRECRUIT:
					if (poptypeDB.addrecunit != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", poptypeDB.addrecunit.toString()));
						Inst.ADDRECRUIT.defaultValue = poptypeDB.addrecunit.toString();
					}
					break;
				}
			}
		}
		name.getParent().getParent().layout(true);
	}
	
	private String getInst1(Inst inst, SelectPoptype poptype) {
		EList<PoptypeMods> list = poptype.getMods();
		for (PoptypeMods mod : list) {
			if (mod instanceof PoptypeInst1) {
				switch (inst) {
				case ADDRECCOM:
					if (((PoptypeInst1)mod).isAddreccom()){
						return ((PoptypeInst1)mod).getValue();
					}
					break;
				case ADDRECRUIT:
					if (((PoptypeInst1)mod).isAddrecunit()){
						return ((PoptypeInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}

	private Boolean getInst2(Inst inst, SelectPoptype poptype) {
		EList<PoptypeMods> list = poptype.getMods();
		for (PoptypeMods mod : list) {
			if (mod instanceof PoptypeInst2) {
				switch (inst) {
				case CLEARDEF:
					if (((PoptypeInst2)mod).isCleardef()) {
						return Boolean.TRUE;
					}
					break;
				case CLEARREC:
					if (((PoptypeInst2)mod).isClearrec()) {
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private void setInst1(final Inst inst, final XtextEditor editor, final String newName) {
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				SelectPoptype poptypeToEdit = (SelectPoptype)input;
				EList<PoptypeMods> mods = poptypeToEdit.getMods();
				for (PoptypeMods mod : mods) {
					if (mod instanceof PoptypeInst1) {
						switch (inst) {
						case ADDRECCOM:
							if (((PoptypeInst1)mod).isAddreccom()) {
								((PoptypeInst1)mod).setValue(newName);
							}
							break;
						case ADDRECRUIT:
							if (((PoptypeInst1)mod).isAddrecunit()) {
								((PoptypeInst1)mod).setValue(newName);
							}
							break;
						}
					}
				}
			}
		});
		updateSelection();
	}
	
	private void addInst1(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<PoptypeMods> mods = ((SelectPoptype)input).getMods();
						PoptypeInst1 type = DmFactory.eINSTANCE.createPoptypeInst1();
						switch (inst) {
						case ADDRECCOM:
							type.setAddreccom(true);
							break;
						case ADDRECRUIT:
							type.setAddrecunit(true);
							break;
						}
						type.setValue(newName);
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}

	private void addInst2(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<PoptypeMods> mods = ((SelectPoptype)input).getMods();
						PoptypeInst2 type = DmFactory.eINSTANCE.createPoptypeInst2();
						switch (inst) {
						case CLEARDEF:
							type.setCleardef(true);
							break;
						case CLEARREC:
							type.setClearrec(true);
							break;
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void removeInst(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						PoptypeMods modToRemove = null;
						EList<PoptypeMods> mods = ((SelectPoptype)input).getMods();
						for (PoptypeMods mod : mods) {
							if (mod instanceof PoptypeInst1) {
								switch (inst) {
								case ADDRECCOM:
									if (((PoptypeInst1)mod).isAddreccom()) {
										modToRemove = mod;
									}
									break;
								case ADDRECRUIT:
									if (((PoptypeInst1)mod).isAddrecunit()) {
										modToRemove = mod;
									}
									break;
								}
							} else if (mod instanceof PoptypeInst2) {
								switch (inst) {
								case CLEARDEF:
									if (((PoptypeInst2)mod).isCleardef()) {
										modToRemove = mod;
									}
									break;
								case CLEARREC:
									if (((PoptypeInst2)mod).isClearrec()) {
										modToRemove = mod;
									}
									break;
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
		});
	}
	
}
