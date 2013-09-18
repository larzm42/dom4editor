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
import org.larz.dom4.db.MercenaryDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.MercenaryInst1;
import org.larz.dom4.dm.dm.MercenaryInst2;
import org.larz.dom4.dm.dm.MercenaryMods;
import org.larz.dom4.dm.dm.NewMercenary;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class MercenaryDetailsPage extends AbstractDetailsPage {
	private Text name;
	
	enum Inst {
		NAME (Messages.getString("MercenaryDetailsSection.mod.name"), "0"),
		BOSSNAME (Messages.getString("MercenaryDetailsSection.mod.bossname"), "0"),
		COM (Messages.getString("MercenaryDetailsSection.mod.com"), "0"),
		UNIT (Messages.getString("MercenaryDetailsSection.mod.unit"), "0"),
		NRUNITS (Messages.getString("MercenaryDetailsSection.mod.nrunits"), "0"),
		LEVEL (Messages.getString("MercenaryDetailsSection.mod.level"), "0"),
		MINMEN (Messages.getString("MercenaryDetailsSection.mod.minmen"), "0"),
		MINPAY (Messages.getString("MercenaryDetailsSection.mod.minpay"), "0"),
		XP (Messages.getString("MercenaryDetailsSection.mod.xp"), "0"),
		RANDEQUIP (Messages.getString("MercenaryDetailsSection.mod.randequip"), "0"),
		RECRATE (Messages.getString("MercenaryDetailsSection.mod.recrate"), "0"),
		ITEM (Messages.getString("MercenaryDetailsSection.mod.item"), "0"),
		ERAMASK (Messages.getString("MercenaryDetailsSection.mod.eramask"), "0");
		
		private String label;
		private String defaultValue;
		
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
		private Text value;
		private Label defaultLabel;
	}
	
	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);

	public MercenaryDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.NAME, new Inst1Fields());
		instMap.put(Inst.BOSSNAME, new Inst1Fields());
		instMap.put(Inst.COM, new Inst1Fields());
		instMap.put(Inst.UNIT, new Inst1Fields());
		instMap.put(Inst.NRUNITS, new Inst2Fields());
		instMap.put(Inst.LEVEL, new Inst2Fields());
		instMap.put(Inst.MINMEN, new Inst2Fields());
		instMap.put(Inst.MINPAY, new Inst2Fields());
		instMap.put(Inst.XP, new Inst2Fields());
		instMap.put(Inst.RANDEQUIP, new Inst2Fields());
		instMap.put(Inst.RECRATE, new Inst2Fields());
		instMap.put(Inst.ITEM, new Inst1Fields());
		instMap.put(Inst.ERAMASK, new Inst2Fields());
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
		s1.setText(Messages.getString("MercenaryDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 4;
		glayout.verticalSpacing = 0;
		client.setLayout(glayout);
		
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			final InstFields field = fields.getValue();
			final Button check = toolkit.createButton(client, key.label, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MERCENARY_CATEGORY, key.label));
			final Text value = toolkit.createText(client, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
			name = value;
			Label defaultLabel = toolkit.createLabel(client, "");
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
							addInst2(key, doc, key.defaultValue);
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
			if (field instanceof Inst2Fields) {
				value.addVerifyListener(new VerifyListener() {
					@Override
					public void verifyText(VerifyEvent e) {
						if (Character.isLetter(e.character)) {
							e.doit = false;
						}
					}
				});
			}
			value.addFocusListener(new FocusAdapter() {
				@Override
				public void focusLost(FocusEvent e) {
					if (field instanceof Inst1Fields) {
						setInst1(key, doc, value.getText());
					} else {
						setInst2(key, doc, value.getText());
					}
				}			
			});
			value.addKeyListener(new KeyAdapter() {
				@Override
				public void keyPressed(KeyEvent e) {
					if (e.character == '\r') {
						if (field instanceof Inst1Fields) {
							setInst1(key, doc, value.getText());
						} else {
							setInst2(key, doc, value.getText());
						}
					}
				}

			});

			value.setEnabled(false);
			GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
			if (field instanceof Inst1Fields) {
				gd.widthHint = 160;
				gd.horizontalSpan = 2;
			} else {
				gd.widthHint = DEFAULT_VALUE_WIDTH;
			}
			value.setLayoutData(gd);
			if (field instanceof Inst1Fields) {
				((Inst1Fields)fields.getValue()).check = check;
				((Inst1Fields)fields.getValue()).value = value;
				((Inst1Fields)fields.getValue()).defaultLabel = defaultLabel;
			} else {
				((Inst2Fields)fields.getValue()).check = check;
				((Inst2Fields)fields.getValue()).value = value;
				((Inst2Fields)fields.getValue()).defaultLabel = defaultLabel;
				createSpacer(toolkit, client, 1);
			}
		}

		createSpacer(toolkit, client, 2);
	}
	
	public void update() {
		MercenaryDB mercenaryDB = new MercenaryDB();
		//TODO right now no default merc values?
		//mercenaryDB = Database.getPoptype(((Mercenary)input).getValue());
		for (Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			String val1 = getInst1(fields.getKey(), (NewMercenary)input);
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
			Integer val2 = getInst2(fields.getKey(), (NewMercenary)input);
			if (val2 != null) {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText(Integer.toString(val2));
					((Inst2Fields)fields.getValue()).value.setEnabled(true);
					((Inst2Fields)fields.getValue()).check.setSelection(true);
					((Inst2Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText("");
					((Inst2Fields)fields.getValue()).value.setEnabled(false);
					((Inst2Fields)fields.getValue()).check.setSelection(false);
					((Inst2Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			if (input instanceof NewMercenary) {
				switch (fields.getKey()) {
				case NAME:
					if (mercenaryDB.name != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.name.toString()));
						Inst.NAME.defaultValue = mercenaryDB.name.toString();
					}
					break;
				case BOSSNAME:
					if (mercenaryDB.bossname != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.bossname.toString()));
						Inst.BOSSNAME.defaultValue = mercenaryDB.bossname.toString();
					}
					break;
				case COM:
					if (mercenaryDB.com != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.com.toString()));
						Inst.COM.defaultValue = mercenaryDB.com.toString();
					}
					break;
				case UNIT:
					if (mercenaryDB.unit != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.unit.toString()));
						Inst.UNIT.defaultValue = mercenaryDB.unit.toString();
					}
					break;
				case NRUNITS:
					if (mercenaryDB.nrunits != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.nrunits.toString()));
						Inst.NRUNITS.defaultValue = mercenaryDB.nrunits.toString();
					}
					break;
				case LEVEL:
					if (mercenaryDB.level != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.level.toString()));
						Inst.LEVEL.defaultValue = mercenaryDB.level.toString();
					}
					break;
				case MINMEN:
					if (mercenaryDB.minmen != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.minmen.toString()));
						Inst.MINMEN.defaultValue = mercenaryDB.minmen.toString();
					}
					break;
				case MINPAY:
					if (mercenaryDB.minpay != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.minpay.toString()));
						Inst.MINPAY.defaultValue = mercenaryDB.minpay.toString();
					}
					break;
				case XP:
					if (mercenaryDB.xp != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.xp.toString()));
						Inst.XP.defaultValue = mercenaryDB.xp.toString();
					}
					break;
				case RANDEQUIP:
					if (mercenaryDB.randequip != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.randequip.toString()));
						Inst.RANDEQUIP.defaultValue = mercenaryDB.randequip.toString();
					}
					break;
				case RECRATE:
					if (mercenaryDB.recrate != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.recrate.toString()));
						Inst.RECRATE.defaultValue = mercenaryDB.recrate.toString();
					}
					break;
				case ITEM:
					if (mercenaryDB.item != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.item.toString()));
						Inst.ITEM.defaultValue = mercenaryDB.item.toString();
					}
					break;
				case ERAMASK:
					if (mercenaryDB.eramask != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", mercenaryDB.eramask.toString()));
						Inst.ERAMASK.defaultValue = mercenaryDB.eramask.toString();
					}
					break;
				}
			}
		}
		name.getParent().getParent().layout(true);
	}
	
	private String getInst1(Inst inst, NewMercenary merc) {
		EList<MercenaryMods> list = merc.getMods();
		for (MercenaryMods mod : list) {
			if (mod instanceof MercenaryInst1) {
				switch (inst) {
				case NAME:
					if (((MercenaryInst1)mod).isName()){
						return ((MercenaryInst1)mod).getValue();
					}
					break;
				case BOSSNAME:
					if (((MercenaryInst1)mod).isBossname()){
						return ((MercenaryInst1)mod).getValue();
					}
					break;
				case COM:
					if (((MercenaryInst1)mod).isCom()){
						return ((MercenaryInst1)mod).getValue();
					}
					break;
				case UNIT:
					if (((MercenaryInst1)mod).isUnit()){
						return ((MercenaryInst1)mod).getValue();
					}
					break;
				case ITEM:
					if (((MercenaryInst1)mod).isItem()){
						return ((MercenaryInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}

	private Integer getInst2(Inst inst, NewMercenary merc) {
		EList<MercenaryMods> list = merc.getMods();
		for (MercenaryMods mod : list) {
			if (mod instanceof MercenaryInst2) {
				switch (inst) {
				case NRUNITS:
					if (((MercenaryInst2)mod).isNrunits()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case LEVEL:
					if (((MercenaryInst2)mod).isLevel()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case MINMEN:
					if (((MercenaryInst2)mod).isMinmen()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case MINPAY:
					if (((MercenaryInst2)mod).isMinpay()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case XP:
					if (((MercenaryInst2)mod).isXp()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case RANDEQUIP:
					if (((MercenaryInst2)mod).isRandequip()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case RECRATE:
					if (((MercenaryInst2)mod).isRecrate()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				case ERAMASK:
					if (((MercenaryInst2)mod).isEramask()) {
						return ((MercenaryInst2)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}
	
	private void setInst1(final Inst inst, final XtextEditor editor, final String newName) {
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				NewMercenary mercToEdit = (NewMercenary)input;
				EList<MercenaryMods> mods = mercToEdit.getMods();
				for (MercenaryMods mod : mods) {
					if (mod instanceof MercenaryInst1) {
						switch (inst) {
						case NAME:
							if (((MercenaryInst1)mod).isName()) {
								((MercenaryInst1)mod).setValue(newName);
							}
							break;
						case BOSSNAME:
							if (((MercenaryInst1)mod).isBossname()) {
								((MercenaryInst1)mod).setValue(newName);
							}
							break;
						case COM:
							if (((MercenaryInst1)mod).isCom()) {
								((MercenaryInst1)mod).setValue(newName);
							}
							break;
						case UNIT:
							if (((MercenaryInst1)mod).isUnit()) {
								((MercenaryInst1)mod).setValue(newName);
							}
							break;
						case ITEM:
							if (((MercenaryInst1)mod).isItem()) {
								((MercenaryInst1)mod).setValue(newName);
							}
							break;
						}
					}
				}
			}
		});
		updateSelection();
	}
	
	private void setInst2(final Inst inst, final XtextEditor editor, final String newName) {
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				NewMercenary mercToEdit = (NewMercenary)input;
				EList<MercenaryMods> mods = mercToEdit.getMods();
				for (MercenaryMods mod : mods) {
					if (mod instanceof MercenaryInst2) {
						switch (inst) {
						case NRUNITS:
							if (((MercenaryInst2)mod).isNrunits()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LEVEL:
							if (((MercenaryInst2)mod).isLevel()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MINMEN:
							if (((MercenaryInst2)mod).isMinmen()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MINPAY:
							if (((MercenaryInst2)mod).isMinpay()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case XP:
							if (((MercenaryInst2)mod).isXp()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RANDEQUIP:
							if (((MercenaryInst2)mod).isRandequip()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RECRATE:
							if (((MercenaryInst2)mod).isRecrate()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ERAMASK:
							if (((MercenaryInst2)mod).isEramask()) {
								((MercenaryInst2)mod).setValue(Integer.parseInt(newName));
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
						EList<MercenaryMods> mods = ((NewMercenary)input).getMods();
						MercenaryInst1 type = DmFactory.eINSTANCE.createMercenaryInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
							break;
						case BOSSNAME:
							type.setBossname(true);
							break;
						case COM:
							type.setCom(true);
							break;
						case UNIT:
							type.setUnit(true);
							break;
						case ITEM:
							type.setItem(true);
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

	private void addInst2(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<MercenaryMods> mods = ((NewMercenary)input).getMods();
						MercenaryInst2 type = DmFactory.eINSTANCE.createMercenaryInst2();
						switch (inst) {
						case NRUNITS:
							type.setNrunits(true);
							break;
						case LEVEL:
							type.setLevel(true);
							break;
						case MINMEN:
							type.setMinmen(true);
							break;
						case MINPAY:
							type.setMinpay(true);
							break;
						case XP:
							type.setXp(true);
							break;
						case RANDEQUIP:
							type.setRandequip(true);
							break;
						case RECRATE:
							type.setRecrate(true);
							break;
						case ERAMASK:
							type.setEramask(true);
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
						MercenaryMods modToRemove = null;
						EList<MercenaryMods> mods = ((NewMercenary)input).getMods();
						for (MercenaryMods mod : mods) {
							if (mod instanceof MercenaryInst1) {
								switch (inst) {
								case NAME:
									if (((MercenaryInst1)mod).isName()) {
										modToRemove = mod;
									}
									break;
								case BOSSNAME:
									if (((MercenaryInst1)mod).isBossname()) {
										modToRemove = mod;
									}
									break;
								case COM:
									if (((MercenaryInst1)mod).isCom()) {
										modToRemove = mod;
									}
									break;
								case UNIT:
									if (((MercenaryInst1)mod).isUnit()) {
										modToRemove = mod;
									}
									break;
								case ITEM:
									if (((MercenaryInst1)mod).isItem()) {
										modToRemove = mod;
									}
									break;
								}
							} else if (mod instanceof MercenaryInst2) {
								switch (inst) {
								case NRUNITS:
									if (((MercenaryInst2)mod).isNrunits()) {
										modToRemove = mod;
									}
									break;
								case LEVEL:
									if (((MercenaryInst2)mod).isLevel()) {
										modToRemove = mod;
									}
									break;
								case MINMEN:
									if (((MercenaryInst2)mod).isMinmen()) {
										modToRemove = mod;
									}
									break;
								case MINPAY:
									if (((MercenaryInst2)mod).isMinpay()) {
										modToRemove = mod;
									}
									break;
								case XP:
									if (((MercenaryInst2)mod).isXp()) {
										modToRemove = mod;
									}
									break;
								case RANDEQUIP:
									if (((MercenaryInst2)mod).isRandequip()) {
										modToRemove = mod;
									}
									break;
								case RECRATE:
									if (((MercenaryInst2)mod).isRecrate()) {
										modToRemove = mod;
									}
									break;
								case ERAMASK:
									if (((MercenaryInst2)mod).isEramask()) {
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
