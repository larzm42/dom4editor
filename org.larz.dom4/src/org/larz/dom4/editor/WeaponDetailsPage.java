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
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.larz.dom4.db.Database;
import org.larz.dom4.db.WeaponDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.SelectWeaponById;
import org.larz.dom4.dm.dm.SelectWeaponByName;
import org.larz.dom4.dm.dm.Weapon;
import org.larz.dom4.dm.dm.WeaponInst1;
import org.larz.dom4.dm.dm.WeaponInst2;
import org.larz.dom4.dm.dm.WeaponInst3;
import org.larz.dom4.dm.dm.WeaponInst4;
import org.larz.dom4.dm.dm.WeaponMods;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class WeaponDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;

	enum Inst {
		NAME (Messages.getString("WeaponDetailsSection.mod.name"), ""), 
		DMG (Messages.getString("WeaponDetailsSection.mod.dmg"), "3"), 
		NRATT (Messages.getString("WeaponDetailsSection.mod.nratt"), "1"),
		ATT (Messages.getString("WeaponDetailsSection.mod.att"), "0"), 
		DEF (Messages.getString("WeaponDetailsSection.mod.def"), "0"), 
		LEN (Messages.getString("WeaponDetailsSection.mod.len"), "4"),
		RANGE (Messages.getString("WeaponDetailsSection.mod.range"), "30"),
		AMMO (Messages.getString("WeaponDetailsSection.mod.ammo"), "12"),
		RCOST (Messages.getString("WeaponDetailsSection.mod.rcost"), "0"),
		TWOHANDED (Messages.getString("WeaponDetailsSection.mod.twohanded")),
		SOUND (Messages.getString("WeaponDetailsSection.mod.sound"), "12"),
		SLASH (Messages.getString("WeaponDetailsSection.mod.slash")),
		PIERCE (Messages.getString("WeaponDetailsSection.mod.pierce")),
		BLUNT (Messages.getString("WeaponDetailsSection.mod.blunt")),
		COLD (Messages.getString("WeaponDetailsSection.mod.cold")),
		FIRE (Messages.getString("WeaponDetailsSection.mod.fire")),
		SHOCK (Messages.getString("WeaponDetailsSection.mod.shock")),
		MAGIC (Messages.getString("WeaponDetailsSection.mod.magic")),
		POISON (Messages.getString("WeaponDetailsSection.mod.poison")),
		ACID (Messages.getString("WeaponDetailsSection.mod.acid")),
		DT_NORMAL (Messages.getString("WeaponDetailsSection.mod.dt_normal")),
		DT_STUN (Messages.getString("WeaponDetailsSection.mod.dt_stun")),
		DT_PARALYZE (Messages.getString("WeaponDetailsSection.mod.dt_paralyze")),
		DT_POISON (Messages.getString("WeaponDetailsSection.mod.dt_poison")),
		ARMORPIERCING (Messages.getString("WeaponDetailsSection.mod.armorpiercing")),
		ARMORNEGATING (Messages.getString("WeaponDetailsSection.mod.armornegating")),
		NOSTR (Messages.getString("WeaponDetailsSection.mod.nostr")),
		MRNEGATES (Messages.getString("WeaponDetailsSection.mod.mrnegates")),
		MRNEGATESEASILY (Messages.getString("WeaponDetailsSection.mod.mrnegateseasily")),
		HARDMRNEG (Messages.getString("WeaponDetailsSection.mod.hardmrneg")),
		SIZERESIST (Messages.getString("WeaponDetailsSection.mod.sizeresist")),
		MIND (Messages.getString("WeaponDetailsSection.mod.mind")),
		UNDEADIMMUNE (Messages.getString("WeaponDetailsSection.mod.undeadimmune")),
		INANIMATEIMMUNE (Messages.getString("WeaponDetailsSection.mod.inanimateimmune")),
		FLYINGIMMUNE (Messages.getString("WeaponDetailsSection.mod.flyingimmune")),
		ENEMYIMMUNE (Messages.getString("WeaponDetailsSection.mod.enemyimmune")),
		FRIENDLYIMMUNE (Messages.getString("WeaponDetailsSection.mod.friendlyimmune")),
		UNDEADONLY (Messages.getString("WeaponDetailsSection.mod.undeadonly")),
		DT_CAP (Messages.getString("WeaponDetailsSection.mod.dt_cap")),
		DT_DEMON (Messages.getString("WeaponDetailsSection.mod.dt_demon")),
		DT_DEMONONLY (Messages.getString("WeaponDetailsSection.mod.dt_demononly")),
		DT_HOLY (Messages.getString("WeaponDetailsSection.mod.dt_holy")),
		DT_MAGIC (Messages.getString("WeaponDetailsSection.mod.dt_magic")),
		DT_SMALL (Messages.getString("WeaponDetailsSection.mod.dt_small")),
		DT_LARGE (Messages.getString("WeaponDetailsSection.mod.dt_large")),
		DT_CONSTRUCTONLY (Messages.getString("WeaponDetailsSection.mod.dt_constructonly")),
		DT_RAISE (Messages.getString("WeaponDetailsSection.mod.dt_raise")),
		AOE (Messages.getString("WeaponDetailsSection.mod.aoe"), "10"),
		BONUS (Messages.getString("WeaponDetailsSection.mod.bonus")),
		SECONDARYEFFECT (Messages.getString("WeaponDetailsSection.mod.secondaryeffect"), "0"),
		SECONDARYEFFECTALWAYS (Messages.getString("WeaponDetailsSection.mod.secondaryeffectalways"), "0"),
		CHARGE (Messages.getString("WeaponDetailsSection.mod.charge")),
		FLAIL (Messages.getString("WeaponDetailsSection.mod.flail")),
		NOREPEL (Messages.getString("WeaponDetailsSection.mod.norepel")),
		UNREPEL (Messages.getString("WeaponDetailsSection.mod.unrepel")),
		BEAM (Messages.getString("WeaponDetailsSection.mod.beam")),
		RANGE050 (Messages.getString("WeaponDetailsSection.mod.range050")),
		RANGE0 (Messages.getString("WeaponDetailsSection.mod.range0")),
		MELEE50 (Messages.getString("WeaponDetailsSection.mod.melee50")),
		SKIP (Messages.getString("WeaponDetailsSection.mod.skip")),
		SKIP2 (Messages.getString("WeaponDetailsSection.mod.skip2")),
		FLYSPR (Messages.getString("WeaponDetailsSection.mod.flyspr"), "100", "1"),
		EXPLSPR (Messages.getString("WeaponDetailsSection.mod.explspr"), "10001");

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
	
	class Inst2Fields implements InstFields {
		private Button check;
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst3Fields implements InstFields {
		private Button check;
		private Text value1;
		private Text value2;
		private Label defaultLabel1;
		private Label defaultLabel2;
	}
	
	class Inst4Fields implements InstFields {
		private Button check;
		private Label defaultLabel;
	}

	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	
	public WeaponDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.DMG, new Inst2Fields());
		instMap.put(Inst.NRATT, new Inst2Fields());
		instMap.put(Inst.ATT, new Inst2Fields());
		instMap.put(Inst.DEF, new Inst2Fields());
		instMap.put(Inst.LEN, new Inst2Fields());
		instMap.put(Inst.RANGE, new Inst2Fields());
		instMap.put(Inst.AMMO, new Inst2Fields());
		instMap.put(Inst.RCOST, new Inst2Fields());
		instMap.put(Inst.TWOHANDED, new Inst4Fields());
		instMap.put(Inst.SOUND, new Inst2Fields());
		instMap.put(Inst.SLASH, new Inst4Fields());
		instMap.put(Inst.PIERCE, new Inst4Fields());
		instMap.put(Inst.BLUNT, new Inst4Fields());
		instMap.put(Inst.COLD, new Inst4Fields());
		instMap.put(Inst.FIRE, new Inst4Fields());
		instMap.put(Inst.SHOCK, new Inst4Fields());
		instMap.put(Inst.MAGIC, new Inst4Fields());
		instMap.put(Inst.POISON, new Inst4Fields());
		instMap.put(Inst.ACID, new Inst4Fields());
		instMap.put(Inst.DT_NORMAL, new Inst4Fields());
		instMap.put(Inst.DT_STUN, new Inst4Fields());
		instMap.put(Inst.DT_PARALYZE, new Inst4Fields());
		instMap.put(Inst.DT_POISON, new Inst4Fields());
		instMap.put(Inst.ARMORPIERCING, new Inst4Fields());
		instMap.put(Inst.ARMORNEGATING, new Inst4Fields());
		instMap.put(Inst.NOSTR, new Inst4Fields());
		instMap.put(Inst.MRNEGATES, new Inst4Fields());
		instMap.put(Inst.MRNEGATESEASILY, new Inst4Fields());
		instMap.put(Inst.HARDMRNEG, new Inst4Fields());
		instMap.put(Inst.SIZERESIST, new Inst4Fields());
		instMap.put(Inst.MIND, new Inst4Fields());
		instMap.put(Inst.UNDEADIMMUNE, new Inst4Fields());
		instMap.put(Inst.INANIMATEIMMUNE, new Inst4Fields());
		instMap.put(Inst.FLYINGIMMUNE, new Inst4Fields());
		instMap.put(Inst.ENEMYIMMUNE, new Inst4Fields());
		instMap.put(Inst.FRIENDLYIMMUNE, new Inst4Fields());
		instMap.put(Inst.UNDEADONLY, new Inst4Fields());
		instMap.put(Inst.DT_CAP, new Inst4Fields());
		instMap.put(Inst.DT_DEMON, new Inst4Fields());
		instMap.put(Inst.DT_DEMONONLY, new Inst4Fields());
		instMap.put(Inst.DT_HOLY, new Inst4Fields());
		instMap.put(Inst.DT_MAGIC, new Inst4Fields());
		instMap.put(Inst.DT_SMALL, new Inst4Fields());
		instMap.put(Inst.DT_LARGE, new Inst4Fields());
		instMap.put(Inst.DT_CONSTRUCTONLY, new Inst4Fields());
		instMap.put(Inst.DT_RAISE, new Inst4Fields());
		instMap.put(Inst.AOE, new Inst2Fields());
		instMap.put(Inst.BONUS, new Inst4Fields());
		instMap.put(Inst.SECONDARYEFFECT, new Inst2Fields());
		instMap.put(Inst.SECONDARYEFFECTALWAYS, new Inst2Fields());		
		instMap.put(Inst.CHARGE, new Inst4Fields());
		instMap.put(Inst.FLAIL, new Inst4Fields());
		instMap.put(Inst.NOREPEL, new Inst4Fields());
		instMap.put(Inst.UNREPEL, new Inst4Fields());
		instMap.put(Inst.BEAM, new Inst4Fields());
		instMap.put(Inst.RANGE050, new Inst4Fields());
		instMap.put(Inst.RANGE0, new Inst4Fields());
		instMap.put(Inst.MELEE50, new Inst4Fields());
		instMap.put(Inst.SKIP, new Inst4Fields());
		instMap.put(Inst.SKIP2, new Inst4Fields());
		instMap.put(Inst.FLYSPR, new Inst3Fields());
		instMap.put(Inst.EXPLSPR, new Inst2Fields());

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
		s1.setText(Messages.getString("WeaponDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 2;
		glayout.makeColumnsEqualWidth = true;
		client.setLayout(glayout);
		
		Composite nameComp = toolkit.createComposite(client);
		glayout = new GridLayout(2, false);
		glayout.marginWidth = 0;
		nameComp.setLayout(glayout);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.horizontalSpan = 2;
		nameComp.setLayoutData(gd);
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("WeaponDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.WEAPON_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setWeaponname(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setWeaponname(doc, name.getText());
				}
			}
			
		});
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
					if (input instanceof SelectWeaponById || input instanceof SelectWeaponByName) {
						name.setText(getSelectWeaponname(input));
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 400;
		name.setLayoutData(gd);
		
		Composite leftColumn = null;
		Composite rightColumn = null;
		boolean isRight = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			
			if (key.equals(Inst.DMG) || 
				key.equals(Inst.SLASH) || 
				key.equals(Inst.ARMORPIERCING) || 
				key.equals(Inst.AOE)) {

				final Section expandable = toolkit.createSection(client, ExpandableComposite.TWISTIE | ExpandableComposite.TITLE_BAR);
				switch (key) {
				case DMG:
					expandable.setText(Messages.getString("WeaponDetailsSection.mod.section.basic"));
					break;
				case SLASH:
					expandable.setText(Messages.getString("WeaponDetailsSection.mod.section.damage"));
					break;
				case ARMORPIERCING:
					expandable.setText(Messages.getString("WeaponDetailsSection.mod.section.qualifiers"));
					break;
				case AOE:
					expandable.setText(Messages.getString("WeaponDetailsSection.mod.section.properties"));
					break;
				}
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.horizontalSpan = 2;
				expandable.setLayoutData(gd);
				expandable.addExpansionListener(new ExpansionAdapter() {
					public void expansionStateChanged(ExpansionEvent e) {
						mform.getForm().reflow(true);
					}
				});

				Composite header1 = toolkit.createComposite(expandable, SWT.BORDER);
				header1.setLayout(new GridLayout(2, true));
				expandable.setClient(header1);
				if (key.equals(Inst.DMG)) {
					expandable.setExpanded(true);
				}

				leftColumn = toolkit.createComposite(header1);
				glayout = new GridLayout(5, false);
				glayout.marginHeight = 0;
				glayout.marginWidth = 0;
				glayout.verticalSpacing = 0;
				leftColumn.setLayout(glayout);
				leftColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

				rightColumn = toolkit.createComposite(header1);
				glayout = new GridLayout(5, false);
				glayout.marginHeight = 0;
				glayout.marginWidth = 0;
				glayout.verticalSpacing = 0;
				rightColumn.setLayout(glayout);
				rightColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
				isRight = false;
			}
						
			final InstFields field = fields.getValue();
			final Button check = toolkit.createButton(isRight?rightColumn:leftColumn, key.label, SWT.CHECK);
			check.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (check.getSelection()) {
						check.setFont(boldFont);
						if (field instanceof Inst2Fields) {
							addInst2(key, doc, key.defaultValue);
						} else if (field instanceof Inst3Fields) {
							addInst3(key, doc, key.defaultValue, key.defaultValue2);
						} else if (field instanceof Inst4Fields) {
							addInst4(key, doc);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}

			});
			if (key.label.equals("2ndFx")) {
				check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.WEAPON_CATEGORY, "secondaryeffect"));
			} else if (key.label.equals("2ndFxAll")) {
				check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.WEAPON_CATEGORY, "secondaryeffectalways"));
			} else {
				check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.WEAPON_CATEGORY, key.label));
			}

			Text myValue1 = null;
			Text myValue2 = null;
			if (field instanceof Inst2Fields ||	field instanceof Inst3Fields) {
				final Text value = toolkit.createText(isRight?rightColumn:leftColumn, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue1 = value;
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
						} else {
							value.setEnabled(false);
							value.setText("");
						}
					}

				});
				value.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
				
			}
				
			Label defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
			defaultLabel1.setEnabled(false);
			if (field instanceof Inst2Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst4Fields) {
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.horizontalSpan = 2;
				gd.heightHint=20;
				check.setLayoutData(gd);
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
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
							value.setText(key.defaultValue);
						} else {
							value.setEnabled(false);
							value.setText("");
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
			
			final Text finalValue1 = myValue1;
			final Text finalValue2 = myValue2;
			if (myValue1 != null) {
				myValue1.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						if (field instanceof Inst2Fields) {
							setInst2(key, doc, finalValue1.getText());
						} else if (field instanceof Inst3Fields) {
							setInst3(key, doc, finalValue1.getText(), finalValue2.getText());
						}
					}			
				});
				myValue1.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							if (field instanceof Inst2Fields) {
								setInst2(key, doc, finalValue1.getText());
							} else if (field instanceof Inst3Fields) {
								setInst3(key, doc, finalValue1.getText(), finalValue2.getText());
							}
						}
					}
				});
			}
			if (myValue2 != null) {
				myValue2.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						setInst3(key, doc, finalValue1.getText(), finalValue2.getText());
					}			
				});
				myValue2.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst3(key, doc, finalValue1.getText(), finalValue2.getText());
						}
					}
				});
			}

			if (field instanceof Inst2Fields) {
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
				((Inst4Fields)field).defaultLabel = defaultLabel1;
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
				if (input instanceof SelectWeaponByName || input instanceof SelectWeaponById) {
					String str = getSelectWeaponname(input);
					name.setText(str!= null?str:"");
					name.setEnabled(false);
				} else {
					String str = getWeaponname(input);
					name.setText(str!=null?str:"");
					nameCheck.setEnabled(false);
				}
				name.setEnabled(false);
				nameCheck.setSelection(false);
				nameCheck.setFont(normalFont);
			}
		}
		WeaponDB weaponDB = new WeaponDB();
		if (input instanceof SelectWeaponById) {
			weaponDB = Database.getWeapon(((SelectWeaponById)input).getValue());
		} else if (input instanceof SelectWeaponByName) {
			weaponDB = Database.getWeapon(((SelectWeaponByName)input).getValue());
		}
		for (Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			Integer val = getInst2(fields.getKey(), input);
			if (val != null) {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText(val.toString());
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
			Integer[] vals = getInst3(fields.getKey(), input);
			if (vals != null) {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value1.setText(vals[0].toString());
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					if (vals[1] != null) {
						((Inst3Fields)fields.getValue()).value2.setText(vals[1].toString());
					} else {
						((Inst3Fields)fields.getValue()).value2.setText("");
					}
					((Inst3Fields)fields.getValue()).value2.setEnabled(true);
					((Inst3Fields)fields.getValue()).check.setSelection(true);
					((Inst3Fields)fields.getValue()).check.setFont(boldFont);
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
			}
			Boolean isVal = getInst4(fields.getKey(), input);
			if (isVal != null) {
				if (fields.getValue() instanceof Inst4Fields) {
					((Inst4Fields)fields.getValue()).check.setSelection(isVal);
					((Inst4Fields)fields.getValue()).check.setFont(isVal? boldFont : normalFont);
				}
			}
			if (input instanceof SelectWeaponByName || input instanceof SelectWeaponById) {
				switch (fields.getKey()) {
				case DMG:
					if (weaponDB.dmg != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dmg));
						Inst.DMG.defaultValue = weaponDB.dmg.toString();
					}
					break;
				case NRATT:
					if (weaponDB.nratt != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.nratt));
						Inst.NRATT.defaultValue = weaponDB.nratt.toString();
					}
					break;
				case ATT:
					if (weaponDB.att != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.att));
						Inst.ATT.defaultValue = weaponDB.att.toString();
					}
					break;
				case DEF:
					if (weaponDB.def != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.def));
						Inst.DEF.defaultValue = weaponDB.def.toString();
					}
					break;
				case LEN:
					if (weaponDB.len != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.len));
						Inst.LEN.defaultValue = weaponDB.len.toString();
					}
					break;
				case RANGE:
					if (weaponDB.range != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.range));
						Inst.RANGE.defaultValue = weaponDB.range.toString();
					}
					break;
				case AMMO:
					if (weaponDB.ammo != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.ammo));
						Inst.AMMO.defaultValue = weaponDB.ammo.toString();
					}
					break;
				case RCOST:
					if (weaponDB.rcost != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.rcost));
						Inst.RCOST.defaultValue = weaponDB.rcost.toString();
					}
					break;
				case SOUND:
					if (weaponDB.sound != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.sound));
						Inst.SOUND.defaultValue = weaponDB.sound.toString();
					}
					break;
				case AOE:
					if (weaponDB.aoe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.aoe));
						Inst.AOE.defaultValue = weaponDB.aoe.toString();
					}
					break;
				case SECONDARYEFFECT:
					if (weaponDB.secondaryeffect != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.secondaryeffect));
						Inst.SECONDARYEFFECT.defaultValue = weaponDB.secondaryeffect.toString();
					}
					break;
				case SECONDARYEFFECTALWAYS:
					if (weaponDB.secondaryeffectalways != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.secondaryeffectalways));
						Inst.SECONDARYEFFECTALWAYS.defaultValue = weaponDB.secondaryeffectalways.toString();
					}
					break;
				case EXPLSPR:
					if (weaponDB.explspr != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.explspr));
						Inst.EXPLSPR.defaultValue = weaponDB.explspr.toString();
					}
					break;
				case FLYSPR:
					((Inst3Fields)fields.getValue()).defaultLabel1.setText(weaponDB.flyspr1 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.flyspr1) : "");
					((Inst3Fields)fields.getValue()).defaultLabel2.setText(weaponDB.flyspr2 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.flyspr2) : "");
					break;
				case TWOHANDED:
					if (weaponDB.twohanded != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.twohanded));
						Inst.TWOHANDED.defaultValue = weaponDB.twohanded.toString();
					}
					break;
				case ARMORPIERCING:
					if (weaponDB.armorpiercing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.armorpiercing));
						Inst.ARMORPIERCING.defaultValue = weaponDB.armorpiercing.toString();
					}
					break;
				case ARMORNEGATING:
					if (weaponDB.armornegating != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.armornegating));
						Inst.ARMORNEGATING.defaultValue = weaponDB.armornegating.toString();
					}
					break;
				case MAGIC:
					if (weaponDB.magic != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.magic));
						Inst.MAGIC.defaultValue = weaponDB.magic.toString();
					}
					break;
				case DT_NORMAL:
					if (weaponDB.dt_normal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_normal));
						Inst.DT_NORMAL.defaultValue = weaponDB.dt_normal.toString();
					}
					break;
				case DT_STUN:
					if (weaponDB.dt_stun != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_stun));
						Inst.DT_STUN.defaultValue = weaponDB.dt_stun.toString();
					}
					break;
				case DT_PARALYZE:
					if (weaponDB.dt_paralyze != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_paralyze));
						Inst.DT_PARALYZE.defaultValue = weaponDB.dt_paralyze.toString();
					}
					break;
				case DT_POISON:
					if (weaponDB.dt_poison != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_poison));
						Inst.DT_POISON.defaultValue = weaponDB.dt_poison.toString();
					}
					break;
				case DT_CAP:
					if (weaponDB.dt_cap != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_cap));
						Inst.DT_CAP.defaultValue = weaponDB.dt_cap.toString();
					}
					break;
				case DT_DEMON:
					if (weaponDB.dt_demon != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_demon));
						Inst.DT_DEMON.defaultValue = weaponDB.dt_demon.toString();
					}
					break;
				case DT_DEMONONLY:
					if (weaponDB.dt_demononly != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_demononly));
						Inst.DT_DEMONONLY.defaultValue = weaponDB.dt_demononly.toString();
					}
					break;
				case DT_HOLY:
					if (weaponDB.dt_holy != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_holy));
						Inst.DT_HOLY.defaultValue = weaponDB.dt_holy.toString();
					}
					break;
				case DT_MAGIC:
					if (weaponDB.dt_magic != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_magic));
						Inst.DT_MAGIC.defaultValue = weaponDB.dt_magic.toString();
					}
					break;
				case DT_SMALL:
					if (weaponDB.dt_small != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_small));
						Inst.DT_SMALL.defaultValue = weaponDB.dt_small.toString();
					}
					break;
				case DT_LARGE:
					if (weaponDB.dt_large != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_large));
						Inst.DT_LARGE.defaultValue = weaponDB.dt_large.toString();
					}
					break;
				case DT_CONSTRUCTONLY:
					if (weaponDB.dt_constructonly != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_constructonly));
						Inst.DT_CONSTRUCTONLY.defaultValue = weaponDB.dt_constructonly.toString();
					}
					break;
				case DT_RAISE:
					if (weaponDB.dt_raise != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.dt_raise));
						Inst.DT_RAISE.defaultValue = weaponDB.dt_raise.toString();
					}
					break;
				case MIND:
					if (weaponDB.mind != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.mind));
						Inst.MIND.defaultValue = weaponDB.mind.toString();
					}
					break;
				case COLD:
					if (weaponDB.cold != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.cold));
						Inst.COLD.defaultValue = weaponDB.cold.toString();
					}
					break;
				case FIRE:
					if (weaponDB.fire != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.fire));
						Inst.FIRE.defaultValue = weaponDB.fire.toString();
					}
					break;
				case SHOCK:
					if (weaponDB.shock != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.shock));
						Inst.SHOCK.defaultValue = weaponDB.shock.toString();
					}
					break;
				case POISON:
					if (weaponDB.poison != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.poison));
						Inst.POISON.defaultValue = weaponDB.poison.toString();
					}
					break;
				case BONUS:
					if (weaponDB.bonus != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.bonus));
						Inst.BONUS.defaultValue = weaponDB.bonus.toString();
					}
					break;
				case CHARGE:
					if (weaponDB.charge != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.charge));
						Inst.CHARGE.defaultValue = weaponDB.charge.toString();
					}
					break;
				case FLAIL:
					if (weaponDB.flail != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.flail));
						Inst.FLAIL.defaultValue = weaponDB.flail.toString();
					}
					break;
				case NOSTR:
					if (weaponDB.nostr != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.nostr));
						Inst.NOSTR.defaultValue = weaponDB.nostr.toString();
					}
					break;
				case MRNEGATES:
					if (weaponDB.mrnegates != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.mrnegates));
						Inst.MRNEGATES.defaultValue = weaponDB.mrnegates.toString();
					}
					break;
				case MRNEGATESEASILY:
					if (weaponDB.mrnegateseasily != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.mrnegateseasily));
						Inst.MRNEGATESEASILY.defaultValue = weaponDB.mrnegateseasily.toString();
					}
					break;	
				case SLASH:
					if (weaponDB.slash != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.slash));
						Inst.SLASH.defaultValue = weaponDB.slash.toString();
					}
					break;
				case PIERCE:
					if (weaponDB.pierce != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.pierce));
						Inst.PIERCE.defaultValue = weaponDB.pierce.toString();
					}
					break;
				case BLUNT:
					if (weaponDB.blunt != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.blunt));
						Inst.BLUNT.defaultValue = weaponDB.blunt.toString();
					}
					break;
				case ACID:
					if (weaponDB.acid != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.acid));
						Inst.ACID.defaultValue = weaponDB.acid.toString();
					}
					break;
				case HARDMRNEG:
					if (weaponDB.hardmrneg != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.hardmrneg));
						Inst.HARDMRNEG.defaultValue = weaponDB.hardmrneg.toString();
					}
					break;
				case SIZERESIST:
					if (weaponDB.sizeresist != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.sizeresist));
						Inst.SIZERESIST.defaultValue = weaponDB.sizeresist.toString();
					}
					break;
				case UNDEADIMMUNE:
					if (weaponDB.undeadimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.undeadimmune));
						Inst.UNDEADIMMUNE.defaultValue = weaponDB.undeadimmune.toString();
					}
					break;
				case INANIMATEIMMUNE:
					if (weaponDB.inanimateimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.inanimateimmune));
						Inst.INANIMATEIMMUNE.defaultValue = weaponDB.inanimateimmune.toString();
					}
					break;
				case FLYINGIMMUNE:
					if (weaponDB.flyingimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.flyingimmune));
						Inst.FLYINGIMMUNE.defaultValue = weaponDB.flyingimmune.toString();
					}
					break;
				case ENEMYIMMUNE:
					if (weaponDB.enemyimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.enemyimmune));
						Inst.ENEMYIMMUNE.defaultValue = weaponDB.enemyimmune.toString();
					}
					break;
				case FRIENDLYIMMUNE:
					if (weaponDB.friendlyimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.friendlyimmune));
						Inst.FRIENDLYIMMUNE.defaultValue = weaponDB.friendlyimmune.toString();
					}
					break;
				case UNDEADONLY:
					if (weaponDB.undeadonly != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.undeadonly));
						Inst.UNDEADONLY.defaultValue = weaponDB.undeadonly.toString();
					}
					break;
				case NOREPEL:
					if (weaponDB.norepel != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.norepel));
						Inst.NOREPEL.defaultValue = weaponDB.norepel.toString();
					}
					break;
				case UNREPEL:
					if (weaponDB.unrepel != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.unrepel));
						Inst.UNREPEL.defaultValue = weaponDB.unrepel.toString();
					}
					break;
				case BEAM:
					if (weaponDB.beam != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.beam));
						Inst.BEAM.defaultValue = weaponDB.beam.toString();
					}
					break;
				case RANGE050:
					if (weaponDB.range050 != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.range050));
						Inst.RANGE050.defaultValue = weaponDB.range050.toString();
					}
					break;
				case RANGE0:
					if (weaponDB.range0 != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.range0));
						Inst.RANGE0.defaultValue = weaponDB.range0.toString();
					}
					break;
				case MELEE50:
					if (weaponDB.melee50 != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.melee50));
						Inst.MELEE50.defaultValue = weaponDB.melee50.toString();
					}
					break;
				case SKIP:
					if (weaponDB.skip != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.skip));
						Inst.SKIP.defaultValue = weaponDB.skip.toString();
					}
					break;
				case SKIP2:
					if (weaponDB.skip2 != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", weaponDB.skip2));
						Inst.SKIP2.defaultValue = weaponDB.skip2.toString();
					}
					break;
				}
			}
		}
		name.getParent().getParent().getParent().layout(true, true);
	}
	
	private String getWeaponname(Object weapon) {
		EList<WeaponMods> list = ((Weapon)weapon).getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst1) {
				if (((WeaponInst1)mod).isName()) {
					return ((WeaponInst1)mod).getValue();
				}
			}
		}
		return null;
	}
	
	private String getSelectWeaponname(Object weapon) {
		if (weapon instanceof SelectWeaponByName) {
			return ((SelectWeaponByName)weapon).getValue();
		} else {
			int id = ((SelectWeaponById)weapon).getValue();
			return Database.getWeaponName(id);
		}
	}
	
	private void setWeaponname(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Weapon weaponToEdit = (Weapon)input;
				EList<WeaponMods> mods = weaponToEdit.getMods();
				boolean nameSet = false;
				for (WeaponMods mod : mods) {
					if (mod instanceof WeaponInst1) {
						if (((WeaponInst1)mod).isName()) {
							((WeaponInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					WeaponInst1 nameInst = DmFactory.eINSTANCE.createWeaponInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private String getInst1(Inst inst2, Object weapon) {
		EList<WeaponMods> list = ((Weapon)weapon).getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst1) {
				switch (inst2) {
				case NAME:
					if (((WeaponInst1)mod).isName()){
						return ((WeaponInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}

	private Integer getInst2(Inst inst2, Object weapon) {
		EList<WeaponMods> list = ((Weapon)weapon).getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst2) {
				switch (inst2) {
				case DMG:
					if (((WeaponInst2)mod).isDmg()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case NRATT:
					if (((WeaponInst2)mod).isNratt()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case ATT:
					if (((WeaponInst2)mod).isAtt()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case DEF:
					if (((WeaponInst2)mod).isDef()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case LEN:
					if (((WeaponInst2)mod).isLen()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case RANGE:
					if (((WeaponInst2)mod).isRange()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case AMMO:
					if (((WeaponInst2)mod).isAmmo()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case RCOST:
					if (((WeaponInst2)mod).isRcost()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case SOUND:
					if (((WeaponInst2)mod).isSound()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case AOE:
					if (((WeaponInst2)mod).isAoe()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case SECONDARYEFFECT:
					if (((WeaponInst2)mod).isSecondaryeffect()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case SECONDARYEFFECTALWAYS:
					if (((WeaponInst2)mod).isSecondaryeffectalways()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				case EXPLSPR:
					if (((WeaponInst2)mod).isExplspr()) {
						return Integer.valueOf(((WeaponInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer[] getInst3(Inst inst3, Object weapon) {
		EList<WeaponMods> list = ((Weapon)weapon).getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst3) {
				switch (inst3) {
				case FLYSPR:
					if (((WeaponInst3)mod).isFlyspr()) {
						return new Integer[]{Integer.valueOf(((WeaponInst3)mod).getValue1()), ((WeaponInst3)mod).getValue2() != 0 ? Integer.valueOf(((WeaponInst3)mod).getValue2()) : null};
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Boolean getInst4(Inst inst4, Object weapon) {
		EList<WeaponMods> list = ((Weapon)weapon).getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst4) {
				switch (inst4) {
				case TWOHANDED:
					if (((WeaponInst4)mod).isTwohanded()) {
						return Boolean.TRUE;
					}
					break;
				case ARMORPIERCING:
					if (((WeaponInst4)mod).isArmorpiercing()) {
						return Boolean.TRUE;
					} else {
					}
				case ARMORNEGATING:
					if (((WeaponInst4)mod).isArmornegating()) {
						return Boolean.TRUE;
					}
					break;
				case MAGIC:
					if (((WeaponInst4)mod).isMagic()) {
						return Boolean.TRUE;
					}
					break;
				case DT_NORMAL:
					if (((WeaponInst4)mod).isDt_normal()) {
						return Boolean.TRUE;
					}
					break;
				case DT_STUN:
					if (((WeaponInst4)mod).isDt_stun()) {
						return Boolean.TRUE;
					}
					break;
				case DT_PARALYZE:
					if (((WeaponInst4)mod).isDt_paralyze()) {
						return Boolean.TRUE;
					}
					break;
				case DT_POISON:
					if (((WeaponInst4)mod).isDt_poison()) {
						return Boolean.TRUE;
					}
					break;
				case DT_CAP:
					if (((WeaponInst4)mod).isDt_cap()) {
						return Boolean.TRUE;
					}
					break;
				case DT_DEMON:
					if (((WeaponInst4)mod).isDt_demon()) {
						return Boolean.TRUE;
					}
					break;
				case DT_DEMONONLY:
					if (((WeaponInst4)mod).isDt_demononly()) {
						return Boolean.TRUE;
					}
					break;
				case DT_HOLY:
					if (((WeaponInst4)mod).isDt_holy()) {
						return Boolean.TRUE;
					}
					break;
				case DT_MAGIC:
					if (((WeaponInst4)mod).isDt_magic()) {
						return Boolean.TRUE;
					}
					break;
				case DT_SMALL:
					if (((WeaponInst4)mod).isDt_small()) {
						return Boolean.TRUE;
					}
					break;
				case DT_LARGE:
					if (((WeaponInst4)mod).isDt_large()) {
						return Boolean.TRUE;
					}
					break;
				case DT_CONSTRUCTONLY:
					if (((WeaponInst4)mod).isDt_constructonly()) {
						return Boolean.TRUE;
					}
					break;
				case DT_RAISE:
					if (((WeaponInst4)mod).isDt_raise()) {
						return Boolean.TRUE;
					}
					break;
				case MIND:
					if (((WeaponInst4)mod).isMind()) {
						return Boolean.TRUE;
					}
					break;
				case COLD:
					if (((WeaponInst4)mod).isCold()) {
						return Boolean.TRUE;
					}
					break;
				case FIRE:
					if (((WeaponInst4)mod).isFire()) {
						return Boolean.TRUE;
					}
					break;
				case SHOCK:
					if (((WeaponInst4)mod).isShock()) {
						return Boolean.TRUE;
					}
					break;
				case POISON:
					if (((WeaponInst4)mod).isPoison()) {
						return Boolean.TRUE;
					}
					break;
				case BONUS:
					if (((WeaponInst4)mod).isBonus()) {
						return Boolean.TRUE;
					}
					break;
				case CHARGE:
					if (((WeaponInst4)mod).isCharge()) {
						return Boolean.TRUE;
					}
					break;
				case FLAIL:
					if (((WeaponInst4)mod).isFlail()) {
						return Boolean.TRUE;
					}
					break;
				case NOSTR:
					if (((WeaponInst4)mod).isNostr()) {
						return Boolean.TRUE;
					}
					break;
				case MRNEGATES:
					if (((WeaponInst4)mod).isMrnegates()) {
						return Boolean.TRUE;
					}
					break;
				case MRNEGATESEASILY:
					if (((WeaponInst4)mod).isMrnegateseasily()) {
						return Boolean.TRUE;
					}
					break;
				case BLUNT:
					if (((WeaponInst4)mod).isBlunt()) {
						return Boolean.TRUE;
					}
					break;
				case ACID:
					if (((WeaponInst4)mod).isAcid()) {
						return Boolean.TRUE;
					}
					break;
				case HARDMRNEG:
					if (((WeaponInst4)mod).isHardmrneg()) {
						return Boolean.TRUE;
					}
					break;
				case SIZERESIST:
					if (((WeaponInst4)mod).isSizeresist()) {
						return Boolean.TRUE;
					}
					break;
				case UNDEADIMMUNE:
					if (((WeaponInst4)mod).isUndeadimmune()) {
						return Boolean.TRUE;
					}
					break;
				case INANIMATEIMMUNE:
					if (((WeaponInst4)mod).isInanimateimmune()) {
						return Boolean.TRUE;
					}
					break;
				case FLYINGIMMUNE:
					if (((WeaponInst4)mod).isFlyingimmune()) {
						return Boolean.TRUE;
					}
					break;
				case ENEMYIMMUNE:
					if (((WeaponInst4)mod).isEnemyimmune()) {
						return Boolean.TRUE;
					}
					break;
				case FRIENDLYIMMUNE:
					if (((WeaponInst4)mod).isFriendlyimmune()) {
						return Boolean.TRUE;
					}
					break;
				case UNDEADONLY:
					if (((WeaponInst4)mod).isUndeadonly()) {
						return Boolean.TRUE;
					}
					break;
				case NOREPEL:
					if (((WeaponInst4)mod).isNorepel()) {
						return Boolean.TRUE;
					}
					break;
				case UNREPEL:
					if (((WeaponInst4)mod).isUnrepel()) {
						return Boolean.TRUE;
					}
					break;
				case BEAM:
					if (((WeaponInst4)mod).isBeam()) {
						return Boolean.TRUE;
					}
					break;
				case RANGE050:
					if (((WeaponInst4)mod).isRange050()) {
						return Boolean.TRUE;
					}
					break;
				case RANGE0:
					if (((WeaponInst4)mod).isRange0()) {
						return Boolean.TRUE;
					}
					break;
				case MELEE50:
					if (((WeaponInst4)mod).isMelee50()) {
						return Boolean.TRUE;
					}
					break;
				case SKIP:
					if (((WeaponInst4)mod).isSkip()) {
						return Boolean.TRUE;
					}
					break;
				case SKIP2:
					if (((WeaponInst4)mod).isSkip2()) {
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private void setInst2(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Weapon weaponToEdit = (Weapon)input;
				EList<WeaponMods> mods = weaponToEdit.getMods();
				for (WeaponMods mod : mods) {
					if (mod instanceof WeaponInst2) {
						switch (inst2) {
						case DMG:
							if (((WeaponInst2)mod).isDmg()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NRATT:
							if (((WeaponInst2)mod).isNratt()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ATT:
							if (((WeaponInst2)mod).isAtt()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEF:
							if (((WeaponInst2)mod).isDef()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LEN:
							if (((WeaponInst2)mod).isLen()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RANGE:
							if (((WeaponInst2)mod).isRange()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AMMO:
							if (((WeaponInst2)mod).isAmmo()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RCOST:
							if (((WeaponInst2)mod).isRcost()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SOUND:
							if (((WeaponInst2)mod).isSound()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AOE:
							if (((WeaponInst2)mod).isAoe()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SECONDARYEFFECT:
							if (((WeaponInst2)mod).isSecondaryeffect()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SECONDARYEFFECTALWAYS:
							if (((WeaponInst2)mod).isSecondaryeffectalways()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EXPLSPR:
							if (((WeaponInst2)mod).isExplspr()) {
								((WeaponInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						}
					}
				}

			}  
		});

		updateSelection();
	}

	private void setInst3(final Inst inst3, final XtextEditor editor, final String value1, final String value2)  {
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Weapon weaponToEdit = (Weapon)input;
				List<WeaponMods> modsToRemove = new ArrayList<WeaponMods>();
				List<WeaponMods> modsToAdd = new ArrayList<WeaponMods>();
				EList<WeaponMods> mods = weaponToEdit.getMods();
				for (WeaponMods mod : mods) {
					if (mod instanceof WeaponInst3) {
						Integer newValue1 = null;
						Integer newValue2 = null;
						try {
							newValue1 = Integer.valueOf(value1);
						} catch (NumberFormatException e) {
							// is not a number
						}
						try {
							newValue2 = Integer.valueOf(value2);
						} catch (NumberFormatException e) {
							// is not a number
						}
						switch (inst3) {
						case FLYSPR:
							if (((WeaponInst3)mod).isFlyspr()) {
								modsToRemove.add(mod);
								WeaponInst3 newMod = DmFactory.eINSTANCE.createWeaponInst3();
								newMod.setFlyspr(true);
								if (newValue1 != null) {
									newMod.setValue1(newValue1);
								}
								if (newValue2 != null) {
									newMod.setValue2(newValue2);
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
						EList<WeaponMods> mods = ((Weapon)input).getMods();
						WeaponInst1 type = DmFactory.eINSTANCE.createWeaponInst1();
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

	private void addInst2(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<WeaponMods> mods = ((Weapon)input).getMods();
						WeaponInst2 type = DmFactory.eINSTANCE.createWeaponInst2();
						switch (inst) {
						case DMG:
							type.setDmg(true);
							break;
						case NRATT:
							type.setNratt(true);
							break;
						case ATT:
							type.setAtt(true);
							break;
						case DEF:
							type.setDef(true);
							break;
						case LEN:
							type.setLen(true);
							break;
						case RANGE:
							type.setRange(true);
							break;
						case AMMO:
							type.setAmmo(true);
							break;
						case RCOST:
							type.setRcost(true);
							break;
						case SOUND:
							type.setSound(true);
							break;
						case AOE:
							type.setAoe(true);
							break;
						case SECONDARYEFFECT:
							type.setSecondaryeffect(true);
							break;
						case SECONDARYEFFECTALWAYS:
							type.setSecondaryeffectalways(true);
							break;
						case EXPLSPR:
							type.setExplspr(true);
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
	
	private void addInst3(final Inst inst, final XtextEditor editor, final String newName1, final String newName2) {
		try {
			// If this is not an int, return
			Integer.valueOf(newName1);
		} catch (NumberFormatException e) {
			return;
		}
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<WeaponMods> mods = ((Weapon)input).getMods();
						WeaponInst3 type = DmFactory.eINSTANCE.createWeaponInst3();
						switch (inst) {
						case FLYSPR:
							type.setFlyspr(true);
							break;
						}
						type.setValue1(Integer.valueOf(newName1));
						try {
							type.setValue2(Integer.valueOf(newName2));
						} catch (NumberFormatException e) {
							// Optional parm
						}
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
						EList<WeaponMods> mods = ((Weapon)input).getMods();
						WeaponInst4 type = DmFactory.eINSTANCE.createWeaponInst4();
						switch (inst) {
						case TWOHANDED:
							type.setTwohanded(true);
							break;
						case ARMORPIERCING:
							type.setArmorpiercing(true);
							break;
						case ARMORNEGATING:
							type.setArmornegating(true);
							break;
						case MAGIC:
							type.setMagic(true);
							break;
						case DT_NORMAL:
							type.setDt_normal(true);
							break;
						case DT_STUN:
							type.setDt_stun(true);
							break;
						case DT_PARALYZE:
							type.setDt_paralyze(true);
							break;
						case DT_POISON:
							type.setDt_poison(true);
							break;
						case DT_CAP:
							type.setDt_cap(true);
							break;
						case DT_DEMON:
							type.setDt_demon(true);
							break;
						case DT_DEMONONLY:
							type.setDt_demononly(true);
							break;
						case DT_HOLY:
							type.setDt_holy(true);
							break;
						case DT_MAGIC:
							type.setDt_magic(true);
							break;
						case DT_SMALL:
							type.setDt_small(true);
							break;
						case DT_LARGE:
							type.setDt_large(true);
							break;
						case DT_CONSTRUCTONLY:
							type.setDt_constructonly(true);
							break;
						case DT_RAISE:
							type.setDt_raise(true);
							break;
						case MIND:
							type.setMind(true);
							break;
						case COLD:
							type.setCold(true);
							break;
						case FIRE:
							type.setFire(true);
							break;
						case SHOCK:
							type.setShock(true);
							break;
						case POISON:
							type.setPoison(true);
							break;
						case BONUS:
							type.setBonus(true);
							break;
						case CHARGE:
							type.setCharge(true);
							break;
						case FLAIL:
							type.setFlail(true);
							break;
						case NOSTR:
							type.setNostr(true);
							break;
						case MRNEGATES:
							type.setMrnegates(true);
							break;
						case MRNEGATESEASILY:
							type.setMrnegateseasily(true);
							break;
						case BLUNT:
							type.setBlunt(true);
							break;
						case ACID:
							type.setAcid(true);
							break;
						case HARDMRNEG:
							type.setHardmrneg(true);
							break;
						case SIZERESIST:
							type.setSizeresist(true);
							break;
						case UNDEADIMMUNE:
							type.setUndeadimmune(true);
							break;
						case INANIMATEIMMUNE:
							type.setInanimateimmune(true);
							break;
						case FLYINGIMMUNE:
							type.setFlyingimmune(true);
							break;
						case ENEMYIMMUNE:
							type.setEnemyimmune(true);
							break;
						case FRIENDLYIMMUNE:
							type.setFriendlyimmune(true);
							break;
						case UNDEADONLY:
							type.setUndeadonly(true);
							break;
						case NOREPEL:
							type.setNorepel(true);
							break;
						case UNREPEL:
							type.setUnrepel(true);
							break;
						case BEAM:
							type.setBeam(true);
							break;
						case RANGE050:
							type.setRange050(true);
							break;
						case RANGE0:
							type.setRange0(true);
							break;
						case MELEE50:
							type.setMelee50(true);
							break;
						case SKIP:
							type.setSkip(true);
							break;
						case SKIP2:
							type.setSkip2(true);
							break;
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
						WeaponMods modToRemove = null;
						EList<WeaponMods> mods = ((Weapon)input).getMods();
						for (WeaponMods mod : mods) {
							if (mod instanceof WeaponInst1) {
								switch (inst2) {
								case NAME:
									if (((WeaponInst1)mod).isName()) {
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof WeaponInst2) {
								switch (inst2) {
								case DMG:
									if (((WeaponInst2)mod).isDmg()) {
										modToRemove = mod;
									}
									break;
								case NRATT:
									if (((WeaponInst2)mod).isNratt()) {
										modToRemove = mod;
									}
									break;
								case ATT:
									if (((WeaponInst2)mod).isAtt()) {
										modToRemove = mod;
									}
									break;
								case DEF:
									if (((WeaponInst2)mod).isDef()) {
										modToRemove = mod;
									}
									break;
								case LEN:
									if (((WeaponInst2)mod).isLen()) {
										modToRemove = mod;
									}
									break;
								case RANGE:
									if (((WeaponInst2)mod).isRange()) {
										modToRemove = mod;
									}
									break;
								case AMMO:
									if (((WeaponInst2)mod).isAmmo()) {
										modToRemove = mod;
									}
									break;
								case RCOST:
									if (((WeaponInst2)mod).isRcost()) {
										modToRemove = mod;
									}
									break;
								case SOUND:
									if (((WeaponInst2)mod).isSound()) {
										modToRemove = mod;
									}
									break;
								case AOE:
									if (((WeaponInst2)mod).isAoe()) {
										modToRemove = mod;
									}
									break;
								case SECONDARYEFFECT:
									if (((WeaponInst2)mod).isSecondaryeffect()) {
										modToRemove = mod;
									}
									break;
								case SECONDARYEFFECTALWAYS:
									if (((WeaponInst2)mod).isSecondaryeffectalways()) {
										modToRemove = mod;
									}
									break;
								case EXPLSPR:
									if (((WeaponInst2)mod).isExplspr()) {
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof WeaponInst3) {
								switch (inst2) {
								case FLYSPR:
									if (((WeaponInst3)mod).isFlyspr()) {
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof WeaponInst4) {
								switch (inst2) {
								case TWOHANDED:
									if (((WeaponInst4)mod).isTwohanded()) {
										modToRemove = mod;
									}
									break;
								case ARMORPIERCING:
									if (((WeaponInst4)mod).isArmorpiercing()) {
										modToRemove = mod;
									}
									break;
								case ARMORNEGATING:
									if (((WeaponInst4)mod).isArmornegating()) {
										modToRemove = mod;
									}
									break;
								case MAGIC:
									if (((WeaponInst4)mod).isMagic()) {
										modToRemove = mod;
									}
									break;
								case DT_NORMAL:
									if (((WeaponInst4)mod).isDt_normal()) {
										modToRemove = mod;
									}
									break;
								case DT_STUN:
									if (((WeaponInst4)mod).isDt_stun()) {
										modToRemove = mod;
									}
									break;
								case DT_PARALYZE:
									if (((WeaponInst4)mod).isDt_paralyze()) {
										modToRemove = mod;
									}
									break;
								case DT_POISON:
									if (((WeaponInst4)mod).isDt_poison()) {
										modToRemove = mod;
									}
									break;
								case DT_CAP:
									if (((WeaponInst4)mod).isDt_cap()) {
										modToRemove = mod;
									}
									break;
								case DT_DEMON:
									if (((WeaponInst4)mod).isDt_demon()) {
										modToRemove = mod;
									}
									break;
								case DT_DEMONONLY:
									if (((WeaponInst4)mod).isDt_demononly()) {
										modToRemove = mod;
									}
									break;
								case DT_HOLY:
									if (((WeaponInst4)mod).isDt_holy()) {
										modToRemove = mod;
									}
									break;
								case DT_MAGIC:
									if (((WeaponInst4)mod).isDt_magic()) {
										modToRemove = mod;
									}
									break;
								case DT_SMALL:
									if (((WeaponInst4)mod).isDt_small()) {
										modToRemove = mod;
									}
									break;
								case DT_LARGE:
									if (((WeaponInst4)mod).isDt_large()) {
										modToRemove = mod;
									}
									break;
								case DT_CONSTRUCTONLY:
									if (((WeaponInst4)mod).isDt_constructonly()) {
										modToRemove = mod;
									}
									break;
								case DT_RAISE:
									if (((WeaponInst4)mod).isDt_raise()) {
										modToRemove = mod;
									}
									break;
								case MIND:
									if (((WeaponInst4)mod).isMind()) {
										modToRemove = mod;
									}
									break;
								case COLD:
									if (((WeaponInst4)mod).isCold()) {
										modToRemove = mod;
									}
									break;
								case FIRE:
									if (((WeaponInst4)mod).isFire()) {
										modToRemove = mod;
									}
									break;
								case SHOCK:
									if (((WeaponInst4)mod).isShock()) {
										modToRemove = mod;
									}
									break;
								case POISON:
									if (((WeaponInst4)mod).isPoison()) {
										modToRemove = mod;
									}
									break;
								case BONUS:
									if (((WeaponInst4)mod).isBonus()) {
										modToRemove = mod;
									}
									break;
								case CHARGE:
									if (((WeaponInst4)mod).isCharge()) {
										modToRemove = mod;
									}
									break;
								case FLAIL:
									if (((WeaponInst4)mod).isFlail()) {
										modToRemove = mod;
									}
									break;
								case NOSTR:
									if (((WeaponInst4)mod).isNostr()) {
										modToRemove = mod;
									}
									break;
								case MRNEGATES:
									if (((WeaponInst4)mod).isMrnegates()) {
										modToRemove = mod;
									}
									break;
								case MRNEGATESEASILY:
									if (((WeaponInst4)mod).isMrnegateseasily()) {
										modToRemove = mod;
									}
									break;
								case BLUNT:
									if (((WeaponInst4)mod).isBlunt()) {
										modToRemove = mod;
									}
									break;
								case ACID:
									if (((WeaponInst4)mod).isAcid()) {
										modToRemove = mod;
									}
									break;
								case HARDMRNEG:
									if (((WeaponInst4)mod).isHardmrneg()) {
										modToRemove = mod;
									}
									break;
								case SIZERESIST:
									if (((WeaponInst4)mod).isSizeresist()) {
										modToRemove = mod;
									}
									break;
								case UNDEADIMMUNE:
									if (((WeaponInst4)mod).isUndeadimmune()) {
										modToRemove = mod;
									}
									break;
								case INANIMATEIMMUNE:
									if (((WeaponInst4)mod).isInanimateimmune()) {
										modToRemove = mod;
									}
									break;
								case FLYINGIMMUNE:
									if (((WeaponInst4)mod).isFlyingimmune()) {
										modToRemove = mod;
									}
									break;
								case ENEMYIMMUNE:
									if (((WeaponInst4)mod).isEnemyimmune()) {
										modToRemove = mod;
									}
									break;
								case FRIENDLYIMMUNE:
									if (((WeaponInst4)mod).isFriendlyimmune()) {
										modToRemove = mod;
									}
									break;
								case UNDEADONLY:
									if (((WeaponInst4)mod).isUndeadonly()) {
										modToRemove = mod;
									}
									break;
								case NOREPEL:
									if (((WeaponInst4)mod).isNorepel()) {
										modToRemove = mod;
									}
									break;
								case UNREPEL:
									if (((WeaponInst4)mod).isUnrepel()) {
										modToRemove = mod;
									}
									break;
								case BEAM:
									if (((WeaponInst4)mod).isBeam()) {
										modToRemove = mod;
									}
									break;
								case RANGE050:
									if (((WeaponInst4)mod).isRange050()) {
										modToRemove = mod;
									}
									break;
								case RANGE0:
									if (((WeaponInst4)mod).isRange0()) {
										modToRemove = mod;
									}
									break;
								case MELEE50:
									if (((WeaponInst4)mod).isMelee50()) {
										modToRemove = mod;
									}
									break;
								case SKIP:
									if (((WeaponInst4)mod).isSkip()) {
										modToRemove = mod;
									}
									break;
								case SKIP2:
									if (((WeaponInst4)mod).isSkip2()) {
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
