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
import java.util.EnumMap;
import java.util.List;
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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
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
import org.larz.dom4.db.SpellDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.SelectSpellById;
import org.larz.dom4.dm.dm.SelectSpellByName;
import org.larz.dom4.dm.dm.Spell;
import org.larz.dom4.dm.dm.SpellInst1;
import org.larz.dom4.dm.dm.SpellInst2;
import org.larz.dom4.dm.dm.SpellInst3;
import org.larz.dom4.dm.dm.SpellInst4;
import org.larz.dom4.dm.dm.SpellInst5;
import org.larz.dom4.dm.dm.SpellMods;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

public class SpellDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;
	private Text descr;
	private Button descCheck;

	enum Inst {
		NAME (Messages.getString("SpellDetailsSection.mod.name"), ""),
		DESCR (Messages.getString("SpellDetailsSection.mod.descr"), ""),
		CLEAR (Messages.getString("SpellDetailsSection.mod.clear"), ""),
		COPYSPELL (Messages.getString("SpellDetailsSection.mod.copyspell"), ""),
		SCHOOL (Messages.getString("SpellDetailsSection.mod.school"), "0"),
		RESEARCHLEVEL (Messages.getString("SpellDetailsSection.mod.researchlevel"), "0"),
		PATH1 (Messages.getString("SpellDetailsSection.mod.path"), "0", "0"),
		PATH2 (Messages.getString("SpellDetailsSection.mod.path"), "1", "0"),
		PATHLEVEL1 (Messages.getString("SpellDetailsSection.mod.pathlevel"), "0", "0"),
		PATHLEVEL2 (Messages.getString("SpellDetailsSection.mod.pathlevel"), "1", "0"),
		AOE (Messages.getString("SpellDetailsSection.mod.aoe"), "1"),
		DAMAGE (Messages.getString("SpellDetailsSection.mod.damage"), "1"),
		DAMAGEMON (Messages.getString("SpellDetailsSection.mod.damagemon"), "1"),
		EFFECT (Messages.getString("SpellDetailsSection.mod.effect"), "0"),
		FATIGUECOST (Messages.getString("SpellDetailsSection.mod.fatiguecost"), "0"),
		FLIGHTSPR (Messages.getString("SpellDetailsSection.mod.flightspr"), "10000"),
		EXPLSPR (Messages.getString("SpellDetailsSection.mod.explspr"), "10001"),
		NEXTSPELL (Messages.getString("SpellDetailsSection.mod.nextspell"), ""),
		NREFF (Messages.getString("SpellDetailsSection.mod.nreff"), "1"),
		RANGE (Messages.getString("SpellDetailsSection.mod.range"), "1"),
		PRECISION (Messages.getString("SpellDetailsSection.mod.precision"), "10"),
		SOUND (Messages.getString("SpellDetailsSection.mod.sound"), "0"),
		SPEC (Messages.getString("SpellDetailsSection.mod.spec"), "0"),
		RESTRICTED1 (Messages.getString("SpellDetailsSection.mod.restricted"), "0"),
		RESTRICTED2 (Messages.getString("SpellDetailsSection.mod.restricted"), "0"),
		RESTRICTED3 (Messages.getString("SpellDetailsSection.mod.restricted"), "0"),
		PROVRANGE (Messages.getString("SpellDetailsSection.mod.provrange"), "0"),
		ONLYGEOSRC (Messages.getString("SpellDetailsSection.mod.onlygeosrc"), "0"),
		ONLYGEODST (Messages.getString("SpellDetailsSection.mod.onlygeodst"), "0"),
		ONLYFRIENDLYDST (Messages.getString("SpellDetailsSection.mod.onlyfriendlydst"), "0"),
		ONLYOWNDST (Messages.getString("SpellDetailsSection.mod.onlyowndst"), "0"),
		NOWATERTRACE (Messages.getString("SpellDetailsSection.mod.nowatertrace"), "0"),
		NOLANDTRACE (Messages.getString("SpellDetailsSection.mod.nolandtrace"), "0"),
		WALKABLE (Messages.getString("SpellDetailsSection.mod.walkable"), "0");

		private String label;
		private String defaultValue;
		private String defaultValue2;
		
		Inst(String label, String defaultValue) {
			this.label = label;
			this.defaultValue = defaultValue;
		}
		
		Inst(String label, String defaultValue, String defaultValue2) {
			this.label = label;
			this.defaultValue = defaultValue;
			this.defaultValue2 = defaultValue2;
		}
		
		Inst(String label) {
			this.label = label;
		}
	}
	
	interface InstFields {}
	
	class Inst1Fields implements InstFields {
		private Button check;
		private Text value;
	}
	
	class Inst2Fields implements InstFields {
		private Button check;
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst3Fields implements InstFields {
		private Button check;
		private Text value1;
		private Label defaultLabel1;
		private Text value2;
		private Label defaultLabel2;
	}
	
	class Inst4Fields implements InstFields {
		private Button check;
	}

	class Inst5Fields implements InstFields {
		private Button check;
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst6Fields implements InstFields {
		private Button check;
		private MappedDynamicCombo value;
		private Label defaultLabel;
	}
	
	class Inst7Fields implements InstFields {
		private Button check;
		private Text value1;
		private Label defaultLabel1;
		private MappedDynamicCombo value2;
		private Label defaultLabel2;
	}

	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	
	public SpellDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.SCHOOL, new Inst6Fields());
		instMap.put(Inst.RESEARCHLEVEL, new Inst2Fields());
		instMap.put(Inst.AOE, new Inst2Fields());
		instMap.put(Inst.DAMAGE, new Inst2Fields());
		instMap.put(Inst.DAMAGEMON, new Inst1Fields());
		instMap.put(Inst.EFFECT, new Inst2Fields());
		instMap.put(Inst.FATIGUECOST, new Inst2Fields());
		instMap.put(Inst.FLIGHTSPR, new Inst2Fields());
		instMap.put(Inst.EXPLSPR, new Inst2Fields());
		instMap.put(Inst.NREFF, new Inst2Fields());
		instMap.put(Inst.RANGE, new Inst2Fields());
		instMap.put(Inst.PRECISION, new Inst2Fields());
		instMap.put(Inst.SOUND, new Inst2Fields());
		instMap.put(Inst.SPEC, new Inst2Fields());
		instMap.put(Inst.RESTRICTED1, new Inst2Fields());	
		instMap.put(Inst.RESTRICTED2, new Inst2Fields());	
		instMap.put(Inst.RESTRICTED3, new Inst2Fields());	
		instMap.put(Inst.PATH1, new Inst7Fields());	
		instMap.put(Inst.PATH2, new Inst7Fields());	
		instMap.put(Inst.PATHLEVEL1, new Inst3Fields());	
		instMap.put(Inst.PATHLEVEL2, new Inst3Fields());	
		instMap.put(Inst.CLEAR, new Inst4Fields());	
		instMap.put(Inst.COPYSPELL, new Inst5Fields());	
		instMap.put(Inst.NEXTSPELL, new Inst5Fields());	
		instMap.put(Inst.PROVRANGE, new Inst2Fields());
		instMap.put(Inst.ONLYGEOSRC, new Inst2Fields());
		instMap.put(Inst.ONLYGEODST, new Inst2Fields());
		instMap.put(Inst.ONLYFRIENDLYDST, new Inst2Fields());
		instMap.put(Inst.ONLYOWNDST, new Inst2Fields());
		instMap.put(Inst.NOWATERTRACE, new Inst2Fields());
		instMap.put(Inst.NOLANDTRACE, new Inst2Fields());
		instMap.put(Inst.WALKABLE, new Inst2Fields());
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

		final FormToolkit toolkit = mform.getToolkit();
		Section s1 = toolkit.createSection(parent, Section.DESCRIPTION|Section.TITLE_BAR);
		s1.marginWidth = 10;
		s1.setText(Messages.getString("SpellDetailsSection.name"));
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		final Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 2;
		glayout.makeColumnsEqualWidth = true;
		client.setLayout(glayout);
		
		final Composite nameComp = toolkit.createComposite(client);
		glayout = new GridLayout(2, false);
		glayout.marginWidth = 0;
		nameComp.setLayout(glayout);
		GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, false);
		gd.horizontalSpan = 2;
		nameComp.setLayoutData(gd);
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("SpellDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.SPELL_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setSpellname(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setSpellname(doc, name.getText());
				}
			}
			
		});
		
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 500;
		name.setLayoutData(gd);
		nameCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (nameCheck.getSelection()) {
					addInst1(Inst.NAME, doc, "");
					name.setEnabled(true);
					name.setText("");
					nameCheck.setFont(boldFont);
				} else {
					removeInst(Inst.NAME, doc);
					name.setEnabled(false);
					if (input instanceof SelectSpellById || input instanceof SelectSpellByName) {
						name.setText(getSelectSpellname(input));
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		descCheck = toolkit.createButton(nameComp, Messages.getString("SpellDetailsSection.mod.descr"), SWT.CHECK);
		descCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.SPELL_CATEGORY, "descr"));

		descr = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		descr.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setSpelldescr(doc, descr.getText());
			}			
		});
		descr.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setSpelldescr(doc, descr.getText());
				}
			}
			
		});
		descr.setLayoutData(new GridData(500, SWT.DEFAULT));
		descr.addListener(SWT.Modify, new Listener() {
			
			@Override
			public void handleEvent(Event event) {
				int currentHeight = descr.getSize().y;
				int preferredHeight = descr.computeSize(500, SWT.DEFAULT).y;
				if (currentHeight != preferredHeight) {
					GridData data = (GridData)descr.getLayoutData();
					data.heightHint = preferredHeight;
					client.pack();
				}
			}
		});
		descr.setEnabled(false);
		descr.setBackground(toolkit.getColors().getInactiveBackground());
		descCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (descCheck.getSelection()) {
					addInst1(Inst.DESCR, doc, "");
					descr.setEnabled(true);
					descr.setBackground(toolkit.getColors().getBackground());
					descr.setText(getSelectSpelldescr(input));
					descCheck.setFont(boldFont);
				} else {
					removeInst(Inst.DESCR, doc);
					descr.setEnabled(false);
					descr.setBackground(toolkit.getColors().getInactiveBackground());
					descr.setText(getSelectSpelldescr(input));
					descCheck.setFont(normalFont);
				}
			}
		});

		Composite leftColumn = toolkit.createComposite(client);
		glayout = new GridLayout(5, false);
		glayout.marginHeight = 0;
		glayout.marginWidth = 0;
		glayout.verticalSpacing = 0;
		leftColumn.setLayout(glayout);
		leftColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		Composite rightColumn = toolkit.createComposite(client);
		glayout = new GridLayout(5, false);
		glayout.marginHeight = 0;
		glayout.marginWidth = 0;
		glayout.verticalSpacing = 0;
		rightColumn.setLayout(glayout);
		rightColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		boolean isRight = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			final InstFields field = fields.getValue();
			final Button check = toolkit.createButton(isRight?rightColumn:leftColumn, key.label, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.SPELL_CATEGORY, key.label));

			check.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (check.getSelection()) {
						check.setFont(boldFont);
						if (field instanceof Inst1Fields) {
							addInst1(key, doc, key.defaultValue);
						} else if (field instanceof Inst2Fields) {
							addInst2(key, doc, key.defaultValue);
						} else if (field instanceof Inst3Fields) {
							addInst3(key, doc, key.defaultValue, key.defaultValue2);
						} else if (field instanceof Inst4Fields) {
							addInst4(key, doc);
						} else if (field instanceof Inst5Fields) {
							addInst5(key, doc, key.defaultValue);
						} else if (field instanceof Inst6Fields) {
							addInst2(key, doc, key.defaultValue);
						} else if (field instanceof Inst7Fields) {
							addInst3(key, doc, key.defaultValue, key.defaultValue2);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}

			});

			if (field instanceof Inst4Fields) {
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.horizontalSpan = 2;
				check.setLayoutData(gd);
			}

			Text myValue1 = null;
			Text myValue2 = null;
			if (field instanceof Inst1Fields ||	field instanceof Inst2Fields ||	field instanceof Inst3Fields ||	field instanceof Inst5Fields ||	field instanceof Inst7Fields) {
				final Text value = toolkit.createText(isRight?rightColumn:leftColumn, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue1 = value;
				
				if (field instanceof Inst2Fields ||	field instanceof Inst3Fields ||	field instanceof Inst7Fields) {
					value.addVerifyListener(new VerifyListener() {
						
						@Override
						public void verifyText(VerifyEvent e) {
							if (Character.isLetter(e.character)) {
								e.doit = false;
							}
						}
					});
				}
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							value.setText(key.defaultValue);
						} else {
							value.setEnabled(false);
							value.setText("");
						}
					}

				});
				value.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						if (field instanceof Inst1Fields) {
							setInst1(key, doc, value.getText());
						} else if (field instanceof Inst2Fields) {
							setInst2(key, doc, value.getText());
						} else if (field instanceof Inst3Fields) {
							setInst3(key, doc, value.getText(), null);
						} else if (field instanceof Inst5Fields) {
							setInst5(key, doc, value.getText());
						} else if (field instanceof Inst7Fields) {
							setInst3(key, doc, value.getText(), null);
						}
					}			
				});
				value.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							if (field instanceof Inst1Fields) {
								setInst1(key, doc, value.getText());
							} else if (field instanceof Inst2Fields) {
								setInst2(key, doc, value.getText());
							} else if (field instanceof Inst3Fields) {
								setInst3(key, doc, value.getText(), null);
							} else if (field instanceof Inst5Fields) {
								setInst5(key, doc, value.getText());
							} else if (field instanceof Inst7Fields) {
								setInst3(key, doc, value.getText(), null);
							}
						}
					}
				});
				value.setEnabled(false);
				if (field instanceof Inst1Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = 160;
					gd.horizontalSpan = 4;
				} else if (field instanceof Inst2Fields ||field instanceof Inst3Fields ||field instanceof Inst7Fields) {
					gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
					gd.widthHint = DEFAULT_VALUE_WIDTH;
				} else if (field instanceof Inst5Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.horizontalSpan = 3;
				}
				value.setLayoutData(gd);
				
			}
				
			MappedDynamicCombo myInst6Value1 = null;
			if (field instanceof Inst6Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst6Value1 = value;

				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							value.setItems(new String[]{
									"cannot be researched", "Conjuration", "Alteration", "Evocation", "Construction", "Enchantment", "Thaumaturgy", "Blood", "Divine"},
									new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7});
							int selection = Integer.parseInt(key.defaultValue);
							value.select(selection);
						} else {
							value.setEnabled(false);
							value.removeAll();
						}
					}

				});
				value.addSelectionListener(new SelectionListener() {
					
					@Override
					public void widgetSelected(SelectionEvent e) {
						int val = value.getSelectedValue();
						setInst2(key, doc, Integer.toString(val));
					}
					
					@Override
					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});
				value.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
				
			}

			Label defaultLabel1 = null;
			
			if (field instanceof Inst2Fields || field instanceof Inst3Fields || field instanceof Inst4Fields || field instanceof Inst5Fields || field instanceof Inst6Fields || field instanceof Inst7Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
			}
			if (field instanceof Inst2Fields || field instanceof Inst6Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst3Fields || field instanceof Inst7Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst4Fields) {
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
			} else if (field instanceof Inst5Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
			}

			Label defaultLabel2 = null;
			if (field instanceof Inst3Fields) {
				final Text value = toolkit.createText(isRight?rightColumn:leftColumn, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue2 = value;
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
							value.setText(key.defaultValue2);
						} else {
							value.setEnabled(false);
							value.setText("");
						}
					}

				});
				value.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						setInst3(key, doc, null, value.getText());
					}			
				});
				value.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst3(key, doc, null, value.getText());
						}
					}
				});
				value.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
				
				defaultLabel2 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel2.setEnabled(false);
			}
			
			MappedDynamicCombo myInst7Value2 = null;
			if (field instanceof Inst7Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst7Value2 = value;

				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							value.setItems(new String[]{
									"none", "Fire", "Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood", "Holy"},
									new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7, 8});
							int selection = Integer.parseInt(key.defaultValue2);
							value.select(selection);
						} else {
							value.setEnabled(false);
							value.removeAll();
						}
					}

				});
				value.addSelectionListener(new SelectionListener() {
					
					@Override
					public void widgetSelected(SelectionEvent e) {
						int val = value.getSelectedValue();
						setInst3(key, doc, null, Integer.toString(val));
					}
					
					@Override
					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});
				value.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
				
				defaultLabel2 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel2.setEnabled(false);
			}

			if (field instanceof Inst1Fields) {
				((Inst1Fields)field).check = check;
				((Inst1Fields)field).value = myValue1;
			} else if (field instanceof Inst2Fields) {
				((Inst2Fields)field).check = check;
				((Inst2Fields)field).value = myValue1;
				((Inst2Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst3Fields) {
				((Inst3Fields)field).check = check;
				((Inst3Fields)field).value1 = myValue1;
				((Inst3Fields)field).defaultLabel1 = defaultLabel1;
				((Inst3Fields)field).value2 = myValue2;
				((Inst3Fields)field).defaultLabel2 = defaultLabel2;
			} else if (field instanceof Inst4Fields) {
				((Inst4Fields)field).check = check;
				//((Inst4Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst5Fields) {
				((Inst5Fields)field).check = check;
				((Inst5Fields)field).value = myValue1;
				((Inst5Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst6Fields) {
				((Inst6Fields)field).check = check;
				((Inst6Fields)field).value = myInst6Value1;
				((Inst6Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst7Fields) {
				((Inst7Fields)field).check = check;
				((Inst7Fields)field).value1 = myValue1;
				((Inst7Fields)field).defaultLabel1 = defaultLabel1;
				((Inst7Fields)field).value2 = myInst7Value2;
				((Inst7Fields)field).defaultLabel2 = defaultLabel2;
			}

			isRight = !isRight;
		}

		createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
	}
	
	public void update() {
		if (input != null) {
			String nameString = getInst1(Inst.NAME, input);
			if (nameString != null) {
				name.setText(nameString);
				name.setEnabled(true);
				nameCheck.setSelection(true);
				nameCheck.setFont(boldFont);
			} else {
				if (input instanceof SelectSpellByName || input instanceof SelectSpellById) {
					String str = getSelectSpellname(input);
					name.setText(str!= null?str:"");
					name.setEnabled(false);
				} else {
					String str = getSpellname((Spell)input);
					name.setText(str!=null?str:"");
					nameCheck.setEnabled(false);
				}
				name.setEnabled(false);
				nameCheck.setSelection(false);
				nameCheck.setFont(normalFont);
			}

			String description = getInst1(Inst.DESCR, input);
			final FormToolkit toolkit = mform.getToolkit();
			if (description != null) {
				descr.setText(description);
				descr.setEnabled(true);
				descr.setBackground(toolkit.getColors().getBackground());
				descCheck.setSelection(true);
				descCheck.setFont(boldFont);
			} else {
				descr.setText(getSelectSpelldescr(input));
				descr.setEnabled(false);
				descr.setBackground(toolkit.getColors().getInactiveBackground());
				descCheck.setSelection(false);
				descCheck.setFont(normalFont);
			}

		}
		SpellDB spellDB = new SpellDB();
		if (input instanceof SelectSpellById) {
			spellDB = Database.getSpell(((SelectSpellById)input).getValue());
		} else if (input instanceof SelectSpellByName) {
			spellDB = Database.getSpell(((SelectSpellByName)input).getValue());
		}
		for (Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			String val1 = getInst1(fields.getKey(), input);
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
			Integer val = getInst2(fields.getKey(), input);
			if (val != null) {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText(val.toString());
					((Inst2Fields)fields.getValue()).value.setEnabled(true);
					((Inst2Fields)fields.getValue()).check.setSelection(true);
					((Inst2Fields)fields.getValue()).check.setFont(boldFont);
				}
				if (fields.getValue() instanceof Inst6Fields) {
					((Inst6Fields)fields.getValue()).value.setEnabled(true);
					((Inst6Fields)fields.getValue()).value.setItems(new String[]{
							"cannot be researched", "Conjuration", "Alteration", "Evocation", "Construction", "Enchantment", "Thaumaturgy", "Blood", "Divine"},
							new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7});
					int selection = Integer.parseInt(val.toString());
					((Inst6Fields)fields.getValue()).value.select(selection);
					((Inst6Fields)fields.getValue()).value.setEnabled(true);
					((Inst6Fields)fields.getValue()).check.setSelection(true);
					((Inst6Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText("");
					((Inst2Fields)fields.getValue()).value.setEnabled(false);
					((Inst2Fields)fields.getValue()).check.setSelection(false);
					((Inst2Fields)fields.getValue()).check.setFont(normalFont);
				}
				if (fields.getValue() instanceof Inst6Fields) {
					((Inst6Fields)fields.getValue()).value.removeAll();
					((Inst6Fields)fields.getValue()).value.setEnabled(false);
					((Inst6Fields)fields.getValue()).check.setSelection(false);
					((Inst6Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Integer[] vals = getInst3(fields.getKey(), input);
			if (vals != null) {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value1.setText(vals[0].toString());
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					((Inst3Fields)fields.getValue()).value2.setText(vals[1].toString());
					((Inst3Fields)fields.getValue()).value2.setEnabled(true);
					((Inst3Fields)fields.getValue()).check.setSelection(true);
					((Inst3Fields)fields.getValue()).check.setFont(boldFont);
				}
				if (fields.getValue() instanceof Inst7Fields) {
					((Inst7Fields)fields.getValue()).value1.setText(vals[0].toString());
					((Inst7Fields)fields.getValue()).value1.setEnabled(true);
					((Inst7Fields)fields.getValue()).value2.setItems(new String[]{
							"None", "Fire",	"Air", "Water", "Earth", "Astral", "Death",
							"Nature", "Blood", "Holy"
							},
							new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7, 8});
					int selection = Integer.parseInt(vals[1].toString());
					((Inst7Fields)fields.getValue()).value2.select(selection);
					((Inst7Fields)fields.getValue()).value2.setEnabled(true);
					((Inst7Fields)fields.getValue()).check.setSelection(true);
					((Inst7Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value1.setText("");
					((Inst3Fields)fields.getValue()).value1.setEnabled(false);
					((Inst3Fields)fields.getValue()).value2.setText("");
					((Inst3Fields)fields.getValue()).value2.setEnabled(false);
					((Inst3Fields)fields.getValue()).check.setSelection(false);
					((Inst3Fields)fields.getValue()).check.setFont(normalFont);
				}
				if (fields.getValue() instanceof Inst7Fields) {
					((Inst7Fields)fields.getValue()).value1.setText("");
					((Inst7Fields)fields.getValue()).value1.setEnabled(false);
					((Inst7Fields)fields.getValue()).value2.removeAll();
					((Inst7Fields)fields.getValue()).value2.setEnabled(false);
					((Inst7Fields)fields.getValue()).check.setSelection(false);
					((Inst7Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Boolean isVal = getInst4(fields.getKey(), input);
			if (isVal != null) {
				if (fields.getValue() instanceof Inst4Fields) {
					((Inst4Fields)fields.getValue()).check.setSelection(isVal);
					((Inst4Fields)fields.getValue()).check.setFont(isVal ? boldFont : normalFont);
				}
			}
			Object val5 = getInst5(fields.getKey(), input);
			if (val5 != null) {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value.setText(val5.toString());
					((Inst5Fields)fields.getValue()).value.setEnabled(true);
					((Inst5Fields)fields.getValue()).check.setSelection(true);
					((Inst5Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value.setText("");
					((Inst5Fields)fields.getValue()).value.setEnabled(false);
					((Inst5Fields)fields.getValue()).check.setSelection(false);
					((Inst5Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			if (input instanceof SelectSpellByName || input instanceof SelectSpellById) {
				switch (fields.getKey()) {
				case SCHOOL:
					if (spellDB.school != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getSchoolName(spellDB.school)));
						Inst.SCHOOL.defaultValue = spellDB.school.toString();
					}
					break;
				case RESEARCHLEVEL:
					if (spellDB.researchlevel != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.researchlevel));
						Inst.RESEARCHLEVEL.defaultValue = spellDB.researchlevel.toString();
					}
					break;
				case PATH1:
					if (spellDB.path1 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 0));
						Inst.PATH1.defaultValue = "0";
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(spellDB.path1)));
						Inst.PATH1.defaultValue2 = spellDB.path1.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 0));
						Inst.PATH1.defaultValue = "0";
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.PATH1.defaultValue2 = "0";
					}
					break;
				case PATH2:
					if (spellDB.path2 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 1));
						Inst.PATH2.defaultValue = "1";
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(spellDB.path2)));
						Inst.PATH2.defaultValue2 = spellDB.path2.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 1));
						Inst.PATH2.defaultValue = "1";
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.PATH2.defaultValue2 = "0";
					}
					break;
				case PATHLEVEL1:
					if (spellDB.pathlevel1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 0));
						Inst.PATHLEVEL1.defaultValue = "0";
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.pathlevel1));
						Inst.PATHLEVEL1.defaultValue2 = spellDB.pathlevel1.toString();
					}
					break;
				case PATHLEVEL2:
					if (spellDB.pathlevel2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", 1));
						Inst.PATHLEVEL2.defaultValue = "1";
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.pathlevel2));
						Inst.PATHLEVEL2.defaultValue2 = spellDB.pathlevel2.toString();
					}
					break;
				case AOE:
					if (spellDB.aoe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.aoe));
						Inst.AOE.defaultValue = spellDB.aoe.toString();
					}
					break;
				case DAMAGE:
					if (spellDB.damage != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.damage));
						Inst.DAMAGE.defaultValue = spellDB.damage.toString();
					}
					break;
				case DAMAGEMON:
					if (spellDB.damagemon != null) {
						Inst.DAMAGEMON.defaultValue = spellDB.damagemon.toString();
					}
					break;
				case EFFECT:
					if (spellDB.effect != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.effect));
						Inst.EFFECT.defaultValue = spellDB.effect.toString();
					}
					break;
				case FATIGUECOST:
					if (spellDB.fatiguecost != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.fatiguecost));
						Inst.FATIGUECOST.defaultValue = spellDB.fatiguecost.toString();
					}
					break;
				case NREFF:
					if (spellDB.nreff != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.nreff));
						Inst.NREFF.defaultValue = spellDB.nreff.toString();
					}
					break;
				case RANGE:
					if (spellDB.range != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.range));
						Inst.RANGE.defaultValue = spellDB.range.toString();
					}
					break;
				case PRECISION:
					if (spellDB.precision != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.precision));
						Inst.PRECISION.defaultValue = spellDB.precision.toString();
					}
					break;
				case SPEC:
					if (spellDB.spec != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.spec));
						Inst.SPEC.defaultValue = spellDB.spec.toString();
					}
					break;
				case NEXTSPELL:
					if (spellDB.nextspell != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.nextspell));
						Inst.NEXTSPELL.defaultValue = spellDB.nextspell.toString();
					}
					break;
				case RESTRICTED1:
					if (spellDB.restricted1 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.restricted1));
						Inst.RESTRICTED1.defaultValue = spellDB.restricted1.toString();
					}
					break;
				case RESTRICTED2:
					if (spellDB.restricted2 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.restricted2));
						Inst.RESTRICTED2.defaultValue = spellDB.restricted2.toString();
					}
					break;
				case RESTRICTED3:
					if (spellDB.restricted3 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.restricted3));
						Inst.RESTRICTED3.defaultValue = spellDB.restricted3.toString();
					}
					break;
				case PROVRANGE:
					if (spellDB.provrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.provrange));
						Inst.PROVRANGE.defaultValue = spellDB.provrange.toString();
					}
					break;
				case ONLYGEOSRC:
					if (spellDB.onlygeosrc != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.onlygeosrc));
						Inst.ONLYGEOSRC.defaultValue = spellDB.onlygeosrc.toString();
					}
					break;
				case ONLYGEODST:
					if (spellDB.onlygeodst != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.onlygeodst));
						Inst.ONLYGEODST.defaultValue = spellDB.onlygeodst.toString();
					}
					break;
				case ONLYFRIENDLYDST:
					if (spellDB.onlyfriendlydst != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.onlyfriendlydst));
						Inst.ONLYFRIENDLYDST.defaultValue = spellDB.onlyfriendlydst.toString();
					}
					break;
				case ONLYOWNDST:
					if (spellDB.onlyowndst != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.onlyowndst));
						Inst.ONLYOWNDST.defaultValue = spellDB.onlyowndst.toString();
					}
					break;
				case NOWATERTRACE:
					if (spellDB.nowatertrace != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.nowatertrace));
						Inst.NOWATERTRACE.defaultValue = spellDB.nowatertrace.toString();
					}
					break;
				case NOLANDTRACE:
					if (spellDB.nolandtrace != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.nolandtrace));
						Inst.NOLANDTRACE.defaultValue = spellDB.nolandtrace.toString();
					}
					break;
				case WALKABLE:
					if (spellDB.walkable != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", spellDB.walkable));
						Inst.WALKABLE.defaultValue = spellDB.walkable.toString();
					}
					break;				}
			}
		}
		name.getParent().getParent().getParent().layout(true, true);
	}
	
	private String getSelectSpellname(Object spell) {
		if (spell instanceof SelectSpellByName) {
			return ((SelectSpellByName)spell).getValue();
		} else {
			int id = ((SelectSpellById)spell).getValue();
			return Database.getSpellName(id);
		}
	}
	
	private String getSelectSpelldescr(Object spell) {
		if (spell instanceof SelectSpellByName) {
			return Database.getSpellDescr(((SelectSpellByName)spell).getValue());
		} else if (spell instanceof SelectSpellById) {
			int id = ((SelectSpellById)spell).getValue();
			return Database.getSpellDescr(Database.getSpellName(id));
		}
		return "";
	}
	
	private String getSpellname(Spell spell) {
		EList<SpellMods> list = spell.getMods();
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst1) {
				if (((SpellInst1)mod).isName()) {
					return ((SpellInst1)mod).getValue();
				}
			}
		}
		return null;
	}
	
	private void setSpellname(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Spell spellToEdit = (Spell)input;
				EList<SpellMods> mods = spellToEdit.getMods();
				boolean nameSet = false;
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst1) {
						if (((SpellInst1)mod).isName()) {
							((SpellInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					SpellInst1 nameInst = DmFactory.eINSTANCE.createSpellInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private void setSpelldescr(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Spell spellToEdit = (Spell)input;
				EList<SpellMods> mods = spellToEdit.getMods();
				boolean nameSet = false;
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst1) {
						if (((SpellInst1)mod).isDescr()) {
							((SpellInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					SpellInst1 nameInst = DmFactory.eINSTANCE.createSpellInst1();
					nameInst.setDescr(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private String getPathName(int id) {
		switch (id) {
		case -1:
			return "none";
		case 0:
			return "Fire";
		case 1:
			return "Air";
		case 2:
			return "Water";
		case 3:
			return "Earth";
		case 4:
			return "Astral";
		case 5:
			return "Death";
		case 6:
			return "Nature";
		case 7:
			return "Blood";
		case 8:
			return "Holy";
		}
		return "Unknown";
	}

	private String getSchoolName(int id) {
		switch (id) {
		case -1:
			return "cannot be researched";
		case 0:
			return "Conjuration";
		case 1:
			return "Alteration";
		case 2:
			return "Evocation";
		case 3:
			return "Construction";
		case 4:
			return "Enchantment";
		case 5:
			return "Thaumaturgy";
		case 6:
			return "Blood";
		case 7:
			return "Divine";
		}
		return "Unknown";
	}

	private String getInst1(Inst inst2, Object spell) {
		EList<SpellMods> list = ((Spell)spell).getMods();
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst1) {
				switch (inst2) {
				case NAME:
					if (((SpellInst1)mod).isName()){
						return ((SpellInst1)mod).getValue();
					}
					break;
				case DESCR:
					if (((SpellInst1)mod).isDescr()){
						return ((SpellInst1)mod).getValue();
					}
					break;
				case DAMAGEMON:
					if (((SpellInst1)mod).isDamagemon()){
						return ((SpellInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer getInst2(Inst inst2, Object spell) {
		int restrictedCount = 0;
		EList<SpellMods> list = ((Spell)spell).getMods();
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst2) {
				switch (inst2) {
				case SCHOOL:
					if (((SpellInst2)mod).isSchool()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case RESEARCHLEVEL:
					if (((SpellInst2)mod).isResearchlevel()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case AOE:
					if (((SpellInst2)mod).isAoe()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case DAMAGE:
					if (((SpellInst2)mod).isDamage()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case EFFECT:
					if (((SpellInst2)mod).isEffect()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case FATIGUECOST:
					if (((SpellInst2)mod).isFatiguecost()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case FLIGHTSPR:
					if (((SpellInst2)mod).isFlightspr()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case EXPLSPR:
					if (((SpellInst2)mod).isExplspr()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case NREFF:
					if (((SpellInst2)mod).isNreff()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case RANGE:
					if (((SpellInst2)mod).isRange()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case PRECISION:
					if (((SpellInst2)mod).isPrecision()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case SOUND:
					if (((SpellInst2)mod).isSound()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case SPEC:
					if (((SpellInst2)mod).isSpec()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case RESTRICTED1:
					if (((SpellInst2)mod).isRestricted()) {
						restrictedCount++;
						if (restrictedCount == 1) {
							return Integer.valueOf(((SpellInst2)mod).getValue());
						}
					}
					break;
				case RESTRICTED2:
					if (((SpellInst2)mod).isRestricted()) {
						restrictedCount++;
						if (restrictedCount == 2) {
							return Integer.valueOf(((SpellInst2)mod).getValue());
						}
					}
					break;
				case RESTRICTED3:
					if (((SpellInst2)mod).isRestricted()) {
						restrictedCount++;
						if (restrictedCount == 3) {
							return Integer.valueOf(((SpellInst2)mod).getValue());
						}
					}
					break;
				case PROVRANGE:
					if (((SpellInst2)mod).isProvrange()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case ONLYGEOSRC:
					if (((SpellInst2)mod).isOnlygeosrc()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case ONLYGEODST:
					if (((SpellInst2)mod).isOnlygeodst()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case ONLYFRIENDLYDST:
					if (((SpellInst2)mod).isOnlyfriendlydst()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case ONLYOWNDST:
					if (((SpellInst2)mod).isOnlyowndst()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case NOWATERTRACE:
					if (((SpellInst2)mod).isNowatertrace()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case NOLANDTRACE:
					if (((SpellInst2)mod).isNolandtrace()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;
				case WALKABLE:
					if (((SpellInst2)mod).isWalkable()){
						return Integer.valueOf(((SpellInst2)mod).getValue());
					}
					break;				}
			}
		}
		return null;
	}
	
	private Integer[] getInst3(Inst inst3, Object spell) {
		EList<SpellMods> list = ((Spell)spell).getMods();
		int pathCount = 0;
		int pathLevelCount = 0;
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst3) {
				switch (inst3) {
				case PATH1:
					if (((SpellInst3)mod).isPath()) {
						pathCount++;
						if (pathCount == 1) {
							return new Integer[]{Integer.valueOf(((SpellInst3)mod).getValue1()), Integer.valueOf(((SpellInst3)mod).getValue2())};
						}
					}
					break;
				case PATH2:
					if (((SpellInst3)mod).isPath()) {
						pathCount++;
						if (pathCount == 2) {
							return new Integer[]{Integer.valueOf(((SpellInst3)mod).getValue1()), Integer.valueOf(((SpellInst3)mod).getValue2())};
						}
					}
					break;
				case PATHLEVEL1:
					if (((SpellInst3)mod).isPathlevel()) {
						pathLevelCount++;
						if (pathLevelCount == 1) {
							return new Integer[]{Integer.valueOf(((SpellInst3)mod).getValue1()), Integer.valueOf(((SpellInst3)mod).getValue2())};
						}
					}
					break;
				case PATHLEVEL2:
					if (((SpellInst3)mod).isPathlevel()) {
						pathLevelCount++;
						if (pathLevelCount == 2) {
							return new Integer[]{Integer.valueOf(((SpellInst3)mod).getValue1()), Integer.valueOf(((SpellInst3)mod).getValue2())};
						}
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Boolean getInst4(Inst inst4, Object spell) {
		EList<SpellMods> list = ((Spell)spell).getMods();
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst4) {
				switch (inst4) {
				case CLEAR:
					if (((SpellInst4)mod).isClear()){
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private Object getInst5(Inst inst2, Object spell) {
		EList<SpellMods> list = ((Spell)spell).getMods();
		for (SpellMods mod : list) {
			if (mod instanceof SpellInst5) {
				switch (inst2) {
				case COPYSPELL:
					if (((SpellInst5)mod).isCopyspell()){
						String strVal = ((SpellInst5)mod).getValue1();
						Integer intVal = ((SpellInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case NEXTSPELL:
					if (((SpellInst5)mod).isNextspell()){
						String strVal = ((SpellInst5)mod).getValue1();
						Integer intVal = ((SpellInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				}
			}
		}
		return null;
	}
	
	private void setInst1(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Spell spellToEdit = (Spell)input;
				EList<SpellMods> mods = spellToEdit.getMods();				
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst1) {
						switch (inst2) {
						case NAME:
							if (((SpellInst1)mod).isName()) {
								((SpellInst1)mod).setValue(newName);
							}
							break;
						case DESCR:
							if (((SpellInst1)mod).isDescr()) {
								((SpellInst1)mod).setValue(newName);
							}
							break;
						case DAMAGEMON:
							if (((SpellInst1)mod).isDamagemon()) {
								((SpellInst1)mod).setValue(newName);
							}
							break;
						}
					}
				}

			}  
		});

		updateSelection();
	}

	private void setInst2(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				int restrictedCount = 0;
				Spell spellToEdit = (Spell)input;
				EList<SpellMods> mods = spellToEdit.getMods();
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst2) {
						switch (inst2) {
						case SCHOOL:
							if (((SpellInst2)mod).isSchool()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESEARCHLEVEL:
							if (((SpellInst2)mod).isResearchlevel()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AOE:
							if (((SpellInst2)mod).isAoe()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DAMAGE:
							if (((SpellInst2)mod).isDamage()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EFFECT:
							if (((SpellInst2)mod).isEffect()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FATIGUECOST:
							if (((SpellInst2)mod).isFatiguecost()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FLIGHTSPR:
							if (((SpellInst2)mod).isFlightspr()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EXPLSPR:
							if (((SpellInst2)mod).isExplspr()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NREFF:
							if (((SpellInst2)mod).isNreff()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RANGE:
							if (((SpellInst2)mod).isRange()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PRECISION:
							if (((SpellInst2)mod).isPrecision()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SOUND:
							if (((SpellInst2)mod).isSound()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPEC:
							if (((SpellInst2)mod).isSpec()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESTRICTED1:
							if (((SpellInst2)mod).isRestricted()) {
								restrictedCount++;
								if (restrictedCount == 1) {
									((SpellInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case RESTRICTED2:
							if (((SpellInst2)mod).isRestricted()) {
								restrictedCount++;
								if (restrictedCount == 2) {
									((SpellInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case RESTRICTED3:
							if (((SpellInst2)mod).isRestricted()) {
								restrictedCount++;
								if (restrictedCount == 3) {
									((SpellInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case PROVRANGE:
							if (((SpellInst2)mod).isProvrange()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ONLYGEOSRC:
							if (((SpellInst2)mod).isOnlygeosrc()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ONLYGEODST:
							if (((SpellInst2)mod).isOnlygeodst()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ONLYFRIENDLYDST:
							if (((SpellInst2)mod).isOnlyfriendlydst()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ONLYOWNDST:
							if (((SpellInst2)mod).isOnlyowndst()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NOWATERTRACE:
							if (((SpellInst2)mod).isNowatertrace()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NOLANDTRACE:
							if (((SpellInst2)mod).isNolandtrace()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WALKABLE:
							if (((SpellInst2)mod).isWalkable()){
								((SpellInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;						}
					}
				}

			}  
		});

		updateSelection();
	}

	private void setInst3(final Inst inst3, final XtextEditor editor, final String value1, final String value2) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Spell spellToEdit = (Spell)input;
				int pathCount = 0;
				int pathLevelCount = 0;
				EList<SpellMods> mods = spellToEdit.getMods();
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst3) {
						switch (inst3) {
						case PATH1:
							if (((SpellInst3)mod).isPath()) {
								pathCount++;
								if (pathCount == 1) {
									if (value1 != null) {
										((SpellInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SpellInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case PATH2:
							if (((SpellInst3)mod).isPath()) {
								pathCount++;
								if (pathCount == 2) {
									if (value1 != null) {
										((SpellInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SpellInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case PATHLEVEL1:
							if (((SpellInst3)mod).isPathlevel()) {
								pathLevelCount++;
								if (pathLevelCount == 1) {
									if (value1 != null) {
										((SpellInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SpellInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case PATHLEVEL2:
							if (((SpellInst3)mod).isPathlevel()) {
								pathLevelCount++;
								if (pathLevelCount == 2) {
									if (value1 != null) {
										((SpellInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SpellInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						}
					}
				}

			}  
		});

		updateSelection();
	}
	
	private void setInst5(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Spell spellToEdit = (Spell)input;
				List<SpellMods> modsToRemove = new ArrayList<SpellMods>();
				List<SpellMods> modsToAdd = new ArrayList<SpellMods>();
				EList<SpellMods> mods = spellToEdit.getMods();
				for (SpellMods mod : mods) {
					if (mod instanceof SpellInst5) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst2) {
						case COPYSPELL:
							if (((SpellInst5)mod).isCopyspell()){
								modsToRemove.add(mod);
								SpellInst5 newMod = DmFactory.eINSTANCE.createSpellInst5();
								newMod.setCopyspell(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case NEXTSPELL:
							if (((SpellInst5)mod).isNextspell()){
								modsToRemove.add(mod);
								SpellInst5 newMod = DmFactory.eINSTANCE.createSpellInst5();
								newMod.setNextspell(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						}
					}
				}
				mods.removeAll(modsToRemove);
				mods.addAll(modsToAdd);
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
					public void process(XtextResource resource) throws Exception {
						EList<SpellMods> mods = ((Spell)input).getMods();
						SpellInst1 type = DmFactory.eINSTANCE.createSpellInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
							break;
						case DESCR:
							type.setDescr(true);
							break;
						case DAMAGEMON:
							type.setDamagemon(true);
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
					public void process(XtextResource resource) throws Exception {
						EList<SpellMods> mods = ((Spell)input).getMods();
						SpellInst2 type = DmFactory.eINSTANCE.createSpellInst2();
						switch (inst) {
						case SCHOOL:
							type.setSchool(true);
							break;
						case RESEARCHLEVEL:
							type.setResearchlevel(true);
							break;
						case AOE:
							type.setAoe(true);
							break;
						case DAMAGE:
							type.setDamage(true);
							break;
						case EFFECT:
							type.setEffect(true);
							break;
						case FATIGUECOST:
							type.setFatiguecost(true);
							break;
						case FLIGHTSPR:
							type.setFlightspr(true);
							break;
						case EXPLSPR:
							type.setExplspr(true);
							break;
						case NREFF:
							type.setNreff(true);
							break;
						case RANGE:
							type.setRange(true);
							break;
						case PRECISION:
							type.setPrecision(true);
							break;
						case SOUND:
							type.setSound(true);
							break;
						case SPEC:
							type.setSpec(true);
							break;
						case RESTRICTED1:
							type.setRestricted(true);
							break;
						case RESTRICTED2:
							type.setRestricted(true);
							break;
						case RESTRICTED3:
							type.setRestricted(true);
							break;
						case PROVRANGE:
							type.setProvrange(true);
							break;
						case ONLYGEOSRC:
							type.setOnlygeosrc(true);
							break;
						case ONLYGEODST:
							type.setOnlygeodst(true);
							break;
						case ONLYFRIENDLYDST:
							type.setOnlyfriendlydst(true);
							break;
						case ONLYOWNDST:
							type.setOnlyowndst(true);
							break;
						case NOWATERTRACE:
							type.setNowatertrace(true);
							break;
						case NOLANDTRACE:
							type.setNolandtrace(true);
							break;
						case WALKABLE:
							type.setWalkable(true);
							break;						}
						type.setValue(Integer.valueOf(newName));
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void addInst3(final Inst inst, final XtextEditor editor, final String newName1, final String newName2) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<SpellMods> mods = ((Spell)input).getMods();
						SpellInst3 type = DmFactory.eINSTANCE.createSpellInst3();
						switch (inst) {
						case PATH1:
							type.setPath(true);
							break;
						case PATH2:
							type.setPath(true);
							break;
						case PATHLEVEL1:
							type.setPathlevel(true);
							break;
						case PATHLEVEL2:
							type.setPathlevel(true);
							break;
						}
						type.setValue1(Integer.valueOf(newName1));
						type.setValue2(Integer.valueOf(newName2));
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void addInst4(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<SpellMods> mods = ((Spell)input).getMods();
						SpellInst4 type = DmFactory.eINSTANCE.createSpellInst4();
						switch (inst) {
						case CLEAR:
							type.setClear(true);
							break;
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void addInst5(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<SpellMods> mods = ((Spell)input).getMods();
						SpellInst5 type = DmFactory.eINSTANCE.createSpellInst5();
						switch (inst) {
						case COPYSPELL:
							type.setCopyspell(true);
							break;
						case NEXTSPELL:
							type.setNextspell(true);
							break;
						}
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}
						if (newValue != null) {
							type.setValue2(Integer.valueOf(newName));
						} else {
							type.setValue1(newName);
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void removeInst(final Inst inst2, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						SpellMods modToRemove = null;
						int pathCount = 0;
						int pathLevelCount = 0;
						EList<SpellMods> mods = ((Spell)input).getMods();
						for (SpellMods mod : mods) {
							if (mod instanceof SpellInst1) {
								switch (inst2) {
								case NAME:
									if (((SpellInst1)mod).isName()){
										modToRemove = mod;
									}
									break;
								case DESCR:
									if (((SpellInst1)mod).isDescr()){
										modToRemove = mod;
									}
									break;
								case DAMAGEMON:
									if (((SpellInst1)mod).isDamagemon()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof SpellInst2) {
								switch (inst2) {
								case SCHOOL:
									if (((SpellInst2)mod).isSchool()){
										modToRemove = mod;
									}
									break;
								case RESEARCHLEVEL:
									if (((SpellInst2)mod).isResearchlevel()){
										modToRemove = mod;
									}
									break;
								case AOE:
									if (((SpellInst2)mod).isAoe()){
										modToRemove = mod;
									}
									break;
								case DAMAGE:
									if (((SpellInst2)mod).isDamage()){
										modToRemove = mod;
									}
									break;
								case EFFECT:
									if (((SpellInst2)mod).isEffect()){
										modToRemove = mod;
									}
									break;
								case FATIGUECOST:
									if (((SpellInst2)mod).isFatiguecost()){
										modToRemove = mod;
									}
									break;
								case FLIGHTSPR:
									if (((SpellInst2)mod).isFlightspr()){
										modToRemove = mod;
									}
									break;
								case EXPLSPR:
									if (((SpellInst2)mod).isExplspr()){
										modToRemove = mod;
									}
									break;
								case NREFF:
									if (((SpellInst2)mod).isNreff()){
										modToRemove = mod;
									}
									break;
								case RANGE:
									if (((SpellInst2)mod).isRange()){
										modToRemove = mod;
									}
									break;
								case PRECISION:
									if (((SpellInst2)mod).isPrecision()){
										modToRemove = mod;
									}
									break;
								case SOUND:
									if (((SpellInst2)mod).isSound()){
										modToRemove = mod;
									}
									break;
								case SPEC:
									if (((SpellInst2)mod).isSpec()){
										modToRemove = mod;
									}
									break;
								case RESTRICTED1:
									if (((SpellInst2)mod).isRestricted()){
										modToRemove = mod;
									}
									break;
								case RESTRICTED2:
									if (((SpellInst2)mod).isRestricted()){
										modToRemove = mod;
									}
									break;
								case RESTRICTED3:
									if (((SpellInst2)mod).isRestricted()){
										modToRemove = mod;
									}
									break;
								case PROVRANGE:
									if (((SpellInst2)mod).isProvrange()){
										modToRemove = mod;
									}
									break;
								case ONLYGEOSRC:
									if (((SpellInst2)mod).isOnlygeosrc()){
										modToRemove = mod;
									}
									break;
								case ONLYGEODST:
									if (((SpellInst2)mod).isOnlygeodst()){
										modToRemove = mod;
									}
									break;
								case ONLYFRIENDLYDST:
									if (((SpellInst2)mod).isOnlyfriendlydst()){
										modToRemove = mod;
									}
									break;
								case ONLYOWNDST:
									if (((SpellInst2)mod).isOnlyowndst()){
										modToRemove = mod;
									}
									break;
								case NOWATERTRACE:
									if (((SpellInst2)mod).isNowatertrace()){
										modToRemove = mod;
									}
									break;
								case NOLANDTRACE:
									if (((SpellInst2)mod).isNolandtrace()){
										modToRemove = mod;
									}
									break;
								case WALKABLE:
									if (((SpellInst2)mod).isWalkable()){
										modToRemove = mod;
									}
									break;								}
							}
							if (mod instanceof SpellInst3) {
								switch (inst2) {
								case PATH1:
									if (((SpellInst3)mod).isPath()){
										pathCount++;
										if (pathCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case PATH2:
									if (((SpellInst3)mod).isPath()){
										pathCount++;
										if (pathCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case PATHLEVEL1:
									if (((SpellInst3)mod).isPathlevel()){
										pathLevelCount++;
										if (pathLevelCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case PATHLEVEL2:
									if (((SpellInst3)mod).isPathlevel()){
										pathLevelCount++;
										if (pathLevelCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								}
							}
							if (mod instanceof SpellInst4) {
								switch (inst2) {
								case CLEAR:
									if (((SpellInst4)mod).isClear()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof SpellInst5) {
								switch (inst2) {
								case COPYSPELL:
									if (((SpellInst5)mod).isCopyspell()){
										modToRemove = mod;
									}
									break;
								case NEXTSPELL:
									if (((SpellInst5)mod).isNextspell()){
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
