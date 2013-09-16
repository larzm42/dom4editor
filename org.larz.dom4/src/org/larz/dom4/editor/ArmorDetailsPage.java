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
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
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
import org.larz.dom4.db.ArmorDB;
import org.larz.dom4.db.Database;
import org.larz.dom4.dm.dm.Armor;
import org.larz.dom4.dm.dm.ArmorInst1;
import org.larz.dom4.dm.dm.ArmorInst2;
import org.larz.dom4.dm.dm.ArmorMods;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.SelectArmorById;
import org.larz.dom4.dm.dm.SelectArmorByName;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class ArmorDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;

	enum Inst2 {
		NAME (Messages.getString("ArmorDetailsSection.mod.name"), ""), 
		TYPE (Messages.getString("ArmorDetailsSection.mod.type"), "4"), 
		PROT (Messages.getString("ArmorDetailsSection.mod.prot"), "1"),
		DEF (Messages.getString("ArmorDetailsSection.mod.def"), "1"), 
		ENC (Messages.getString("ArmorDetailsSection.mod.enc"), "0"), 
		RCOST (Messages.getString("ArmorDetailsSection.mod.rcost"), "10"); 
		
		private String label;
		private String defaultValue;
		
		Inst2(String label, String defaultValue) {
			this.label = label;
			this.defaultValue = defaultValue;
		}
		
	}
	
	class Inst2Fields {
		private Text value;
		private Button check;
		private Label defaultLabel;
	}
	
	private EnumMap<Inst2, Inst2Fields> inst2Map = new EnumMap<Inst2, Inst2Fields>(Inst2.class);
	
	public ArmorDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		inst2Map.put(Inst2.TYPE, new Inst2Fields());
		inst2Map.put(Inst2.PROT, new Inst2Fields());
		inst2Map.put(Inst2.DEF, new Inst2Fields());
		inst2Map.put(Inst2.ENC, new Inst2Fields());
		inst2Map.put(Inst2.RCOST, new Inst2Fields());
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
		s1.setText(Messages.getString("ArmorDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 3;
		glayout.verticalSpacing = 0;
		client.setLayout(glayout);
		
		Composite nameComp = toolkit.createComposite(client);
		glayout = new GridLayout(2, false);
		glayout.marginWidth = 0;
		nameComp.setLayout(glayout);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.horizontalSpan = 3;
		nameComp.setLayoutData(gd);

		nameCheck = toolkit.createButton(nameComp, Messages.getString("ArmorDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.ARMOR_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setArmorname(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setArmorname(doc, name.getText());
				}
			}
			
		});
		
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 200;
		name.setLayoutData(gd);
		nameCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (nameCheck.getSelection()) {
					addInst1(Inst2.NAME, doc, "");
					name.setEnabled(true);
					name.setText("");
					nameCheck.setFont(boldFont);
				} else {
					removeInst(Inst2.NAME, doc);
					name.setEnabled(false);
					if (input instanceof SelectArmorById || input instanceof SelectArmorByName) {
						name.setText(getSelectArmorname((Armor)input));
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		for (final Map.Entry<Inst2, Inst2Fields> fields : inst2Map.entrySet()) {
			final Inst2 key = fields.getKey();
			final Button check = toolkit.createButton(client, key.label, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.ARMOR_CATEGORY, key.label));
			final Text value = toolkit.createText(client, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
			Label defaultLabel = toolkit.createLabel(client, "");
			defaultLabel.setEnabled(false);
			
			value.addVerifyListener(new VerifyListener() {
				@Override
				public void verifyText(VerifyEvent e) {
					if (Character.isLetter(e.character)) {
						e.doit = false;
					}
				}
			});
			check.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (check.getSelection()) {
						value.setEnabled(true);
						value.setText(key.defaultValue);
						addInst2(key, doc, key.defaultValue);
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
					setInst2(key, doc, value.getText());
				}			
			});
			value.addKeyListener(new KeyAdapter() {
				@Override
				public void keyPressed(KeyEvent e) {
					if (e.character == '\r') {
						setInst2(key, doc, value.getText());
					}
				}
				
			});
			value.setEnabled(false);
			gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
			gd.widthHint = DEFAULT_VALUE_WIDTH;
			value.setLayoutData(gd);
			fields.getValue().check = check;
			fields.getValue().value = value;
			fields.getValue().defaultLabel = defaultLabel;
		}

		createSpacer(toolkit, client, 2);
	}
	
	public void update() {
		if (input != null) {
			String nameString = getInst1(Inst2.NAME, (Armor)input);
			if (nameString != null) {
				name.setText(nameString);
				name.setEnabled(true);
				nameCheck.setSelection(true);
				nameCheck.setFont(boldFont);
			} else {
				if (input instanceof SelectArmorByName || input instanceof SelectArmorById) {
					String str = getSelectArmorname((Armor)input);
					name.setText(str!= null?str:"");
					name.setEnabled(false);
				} else {
					String str = getArmorname((Armor)input);
					name.setText(str!=null?str:"");
					nameCheck.setEnabled(false);
				}
				name.setEnabled(false);
				nameCheck.setSelection(false);
				nameCheck.setFont(normalFont);
			}
		}
		ArmorDB armorDB = new ArmorDB();
		if (input instanceof SelectArmorById) {
			armorDB = Database.getArmor(((SelectArmorById)input).getValue());
		} else if (input instanceof SelectArmorByName) {
			armorDB = Database.getArmor(((SelectArmorByName)input).getValue());
		}
		for (Map.Entry<Inst2, Inst2Fields> fields : inst2Map.entrySet()) {
			Integer val = getInst2(fields.getKey(), (Armor)input);
			if (val != null) {
				fields.getValue().value.setText(val.toString());
				fields.getValue().value.setEnabled(true);
				fields.getValue().check.setSelection(true);
				fields.getValue().check.setFont(boldFont);
			} else {
				fields.getValue().value.setText("");
				fields.getValue().value.setEnabled(false);
				fields.getValue().check.setSelection(false);
				fields.getValue().check.setFont(normalFont);
			}
			if (input instanceof SelectArmorByName || input instanceof SelectArmorById) {
				switch (fields.getKey()) {
				case DEF:
					if (armorDB.def != null) {
						fields.getValue().defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", armorDB.def.toString()));
						Inst2.DEF.defaultValue = armorDB.def.toString();
					}
					break;
				case ENC:
					if (armorDB.enc != null) {
						fields.getValue().defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", armorDB.enc.toString()));
						Inst2.ENC.defaultValue = armorDB.enc.toString();
					}
					break;
				case PROT:
					if (armorDB.prot != null) {
						fields.getValue().defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", armorDB.prot.toString()));
						Inst2.PROT.defaultValue = armorDB.prot.toString();
					}
					break;
				case RCOST:
					if (armorDB.rcost != null) {
						fields.getValue().defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", armorDB.rcost.toString()));
						Inst2.RCOST.defaultValue = armorDB.rcost.toString();
					}
					break;
				case TYPE:
					if (armorDB.type != null) {
						fields.getValue().defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", armorDB.type.toString()));
						Inst2.TYPE.defaultValue = armorDB.type.toString();
					}
					break;
				}
			}
		}
		name.getParent().getParent().layout(true);
	}
	
	private String getArmorname(Armor armor) {
		EList<ArmorMods> list = armor.getMods();
		for (ArmorMods mod : list) {
			if (mod instanceof ArmorInst1) {
				if (((ArmorInst1)mod).isName()) {
					return ((ArmorInst1)mod).getValue();
				}
			}
		}
		return null;
	}
	
	private String getSelectArmorname(Armor armor) {
		if (armor instanceof SelectArmorByName) {
			return ((SelectArmorByName)armor).getValue();
		} else {
			int id = ((SelectArmorById)armor).getValue();
			return Database.getArmorName(id);
		}
	}
	
	private void setArmorname(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Armor armorToEdit = (Armor)input;
				EList<ArmorMods> mods = armorToEdit.getMods();
				boolean nameSet = false;
				for (ArmorMods mod : mods) {
					if (mod instanceof ArmorInst1) {
						if (((ArmorInst1)mod).isName()) {
							((ArmorInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					ArmorInst1 nameInst = DmFactory.eINSTANCE.createArmorInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}
			
		});

		updateSelection();
	}
	
	private String getInst1(Inst2 inst2, Armor armor) {
		EList<ArmorMods> list = (armor).getMods();
		for (ArmorMods mod : list) {
			if (mod instanceof ArmorInst1) {
				switch (inst2) {
				case NAME:
					if (((ArmorInst1)mod).isName()){
						return ((ArmorInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}


	private Integer getInst2(Inst2 inst2, Armor armor) {
		EList<ArmorMods> list = armor.getMods();
		for (ArmorMods mod : list) {
			if (mod instanceof ArmorInst2) {
				switch (inst2) {
				case TYPE:
					if (((ArmorInst2)mod).isType()) {
						return Integer.valueOf(((ArmorInst2)mod).getValue());
					}
					break;
				case PROT:
					if (((ArmorInst2)mod).isProt()) {
						return Integer.valueOf(((ArmorInst2)mod).getValue());
					}
					break;
				case DEF:
					if (((ArmorInst2)mod).isDef()) {
						return Integer.valueOf(((ArmorInst2)mod).getValue());
					}
					break;
				case ENC:
					if (((ArmorInst2)mod).isEnc()) {
						return Integer.valueOf(((ArmorInst2)mod).getValue());
					}
					break;
				case RCOST:
					if (((ArmorInst2)mod).isRcost()) {
						return Integer.valueOf(((ArmorInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private void setInst2(final Inst2 inst2, final XtextEditor editor, final String newName) 
	{
		try {
			// If this is not an int, return
			Integer.parseInt(newName);
		} catch (NumberFormatException e) {
			return;
		}
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				Armor armorToEdit = (Armor)input;
				EList<ArmorMods> mods = armorToEdit.getMods();
				for (ArmorMods mod : mods) {
					if (mod instanceof ArmorInst2) {
						switch (inst2) {
						case TYPE:
							if (((ArmorInst2)mod).isType()) {
								((ArmorInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PROT:
							if (((ArmorInst2)mod).isProt()) {
								((ArmorInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEF:
							if (((ArmorInst2)mod).isDef()) {
								((ArmorInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ENC:
							if (((ArmorInst2)mod).isEnc()) {
								((ArmorInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RCOST:
							if (((ArmorInst2)mod).isRcost()) {
								((ArmorInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						}
					}
				}
			}
			
		});

		updateSelection();
	}
	
	private void addInst1(final Inst2 inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<ArmorMods> mods = ((Armor)input).getMods();
						ArmorInst1 type = DmFactory.eINSTANCE.createArmorInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
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

	private void addInst2(final Inst2 inst2, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<ArmorMods> mods = ((Armor)input).getMods();
						ArmorInst2 type = DmFactory.eINSTANCE.createArmorInst2();
						switch (inst2) {
						case TYPE:
							type.setType(true);
							break;
						case PROT:
							type.setProt(true);
							break;
						case DEF:
							type.setDef(true);
							break;
						case ENC:
							type.setEnc(true);
							break;
						case RCOST:
							type.setRcost(true);
							break;
						}
						type.setValue(Integer.valueOf(newName));
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void removeInst(final Inst2 inst2, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						ArmorMods modToRemove = null;
						EList<ArmorMods> mods = ((Armor)input).getMods();
						for (ArmorMods mod : mods) {
							if (mod instanceof ArmorInst1) {
								switch (inst2) {
								case NAME:
									if (((ArmorInst1)mod).isName()) {
										modToRemove = mod;
									}
									break;
								}
							} else if (mod instanceof ArmorInst2) {
								switch (inst2) {
								case TYPE:
									if (((ArmorInst2)mod).isType()) {
										modToRemove = mod;
									}
									break;
								case PROT:
									if (((ArmorInst2)mod).isProt()) {
										modToRemove = mod;
									}
									break;
								case DEF:
									if (((ArmorInst2)mod).isDef()) {
										modToRemove = mod;
									}
									break;
								case ENC:
									if (((ArmorInst2)mod).isEnc()) {
										modToRemove = mod;
									}
									break;
								case RCOST:
									if (((ArmorInst2)mod).isRcost()) {
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
