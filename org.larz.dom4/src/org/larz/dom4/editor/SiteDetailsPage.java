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
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
import org.larz.dom4.db.SiteDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.SelectSiteById;
import org.larz.dom4.dm.dm.SelectSiteByName;
import org.larz.dom4.dm.dm.Site;
import org.larz.dom4.dm.dm.SiteInst1;
import org.larz.dom4.dm.dm.SiteInst2;
import org.larz.dom4.dm.dm.SiteInst3;
import org.larz.dom4.dm.dm.SiteInst4;
import org.larz.dom4.dm.dm.SiteInst5;
import org.larz.dom4.dm.dm.SiteMods;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class SiteDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;

	enum Inst {
		NAME (Messages.getString("SiteDetailsSection.mod.name")),
		CLEAR (Messages.getString("SiteDetailsSection.mod.clear")),
		PATH (Messages.getString("SiteDetailsSection.mod.path"), "0"),
		LEVEL (Messages.getString("SiteDetailsSection.mod.level"), "0"),
		RARITY (Messages.getString("SiteDetailsSection.mod.rarity"), "0"),
		LOC (Messages.getString("SiteDetailsSection.mod.loc"), "735"),
		GOLD (Messages.getString("SiteDetailsSection.mod.gold"), "0"),
		RES (Messages.getString("SiteDetailsSection.mod.res"), "0"),
		GEMS1 (Messages.getString("SiteDetailsSection.mod.gems"), "0", "0"),
		DECUNREST (Messages.getString("SiteDetailsSection.mod.decunrest"), "0"),
		GEMS2 (Messages.getString("SiteDetailsSection.mod.gems"), "0", "0"),
		SUPPLY (Messages.getString("SiteDetailsSection.mod.supply"), "0"),
		GEMS3 (Messages.getString("SiteDetailsSection.mod.gems"), "0", "0"),
		HOMEMON1 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM1 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON2 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM2 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON3 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM3 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON4 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM4 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON5 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM5 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON6 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM6 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON7 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM7 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		HOMEMON8 (Messages.getString("SiteDetailsSection.mod.homemon"), "0"),
		HOMECOM8 (Messages.getString("SiteDetailsSection.mod.homecom"), "0"),
		MON1 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM1 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON2 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM2 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON3 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM3 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON4 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM4 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON5 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM5 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON6 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM6 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON7 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM7 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		MON8 (Messages.getString("SiteDetailsSection.mod.mon"), "0"),
		COM8 (Messages.getString("SiteDetailsSection.mod.com"), "0"),
		SUMMON (Messages.getString("SiteDetailsSection.mod.summon"), "0"),
		VOIDGATE (Messages.getString("SiteDetailsSection.mod.voidgate"), "0"),
		LAB (Messages.getString("SiteDetailsSection.mod.lab")),		
		TEMPLE (Messages.getString("SiteDetailsSection.mod.temple")),
		FORT (Messages.getString("SiteDetailsSection.mod.fort"), "0"),
		FORTPART (Messages.getString("SiteDetailsSection.mod.fortpart"), "0"),
		INCSCALE1 (Messages.getString("SiteDetailsSection.mod.incscale"), "0"),
		INCSCALE2 (Messages.getString("SiteDetailsSection.mod.incscale"), "0"),
		DECSCALE1 (Messages.getString("SiteDetailsSection.mod.decscale"), "0"),
		DECSCALE2 (Messages.getString("SiteDetailsSection.mod.decscale"), "0"),
		CONJCOST (Messages.getString("SiteDetailsSection.mod.conjcost"), "10"),
		ALTCOST (Messages.getString("SiteDetailsSection.mod.altcost"), "10"),
		EVOCOST (Messages.getString("SiteDetailsSection.mod.evocost"), "10"),
		CONSTCOST (Messages.getString("SiteDetailsSection.mod.constcost"), "10"),
		ENCHCOST (Messages.getString("SiteDetailsSection.mod.enchcost"), "10"),
		THAUCOST (Messages.getString("SiteDetailsSection.mod.thaucost"), "10"),
		BLOODCOST (Messages.getString("SiteDetailsSection.mod.bloodcost"), "10"),
		SCRY (Messages.getString("SiteDetailsSection.mod.scry"), "0"),
		FIRERANGE (Messages.getString("SiteDetailsSection.mod.firerange"), "0"),
		AIRRANGE (Messages.getString("SiteDetailsSection.mod.airrange"), "0"),
		WATERRANGE (Messages.getString("SiteDetailsSection.mod.waterrange"), "0"),
		EARTHRANGE (Messages.getString("SiteDetailsSection.mod.earthrange"), "0"),
		ASTRALRANGE (Messages.getString("SiteDetailsSection.mod.astralrange"), "0"),
		DEATHRANGE (Messages.getString("SiteDetailsSection.mod.deathrange"), "0"),
		NATURERANGE (Messages.getString("SiteDetailsSection.mod.naturerange"), "0"),
		BLOODRANGE (Messages.getString("SiteDetailsSection.mod.bloodrange"), "0"),
		ELEMENTRANGE (Messages.getString("SiteDetailsSection.mod.elementrange"), "0"),
		SORCERYRANGE (Messages.getString("SiteDetailsSection.mod.sorceryrange"), "0"),
		ALLRANGE (Messages.getString("SiteDetailsSection.mod.allrange"), "0"),
		HEAL (Messages.getString("SiteDetailsSection.mod.heal"), "10"),
		CURSE (Messages.getString("SiteDetailsSection.mod.curse"), "10"),
		DISEASE (Messages.getString("SiteDetailsSection.mod.disease"), "10"),
		HORRORMARK (Messages.getString("SiteDetailsSection.mod.horrormark"), "10"),
		HOLYFIRE (Messages.getString("SiteDetailsSection.mod.holyfire"), "10"),
		HOLYPOWER (Messages.getString("SiteDetailsSection.mod.holypower"), "10"),
		XP (Messages.getString("SiteDetailsSection.mod.xp"), "0"),
		ADVENTURERUIN (Messages.getString("SiteDetailsSection.mod.adventureruin"), "0"),
		CLAIM (Messages.getString("SiteDetailsSection.mod.claim")),
		CLUSTER (Messages.getString("SiteDetailsSection.mod.cluster"), "0"),
		DOMINION (Messages.getString("SiteDetailsSection.mod.dominion"), "0"),
		GODDOMCHAOS (Messages.getString("SiteDetailsSection.mod.goddomchaos"), "0"),
		GODDOMLAZY (Messages.getString("SiteDetailsSection.mod.goddomlazy"), "0"),
		GODDOMCOLD (Messages.getString("SiteDetailsSection.mod.goddomcold"), "0"),
		GODDOMDEATH (Messages.getString("SiteDetailsSection.mod.goddomdeath"), "0"),
		GODDOMMISFORTUNE (Messages.getString("SiteDetailsSection.mod.goddommisfortune"), "0"),
		GODDOMDRAIN (Messages.getString("SiteDetailsSection.mod.goddomdrain"), "0"),
		BLESSHP (Messages.getString("SiteDetailsSection.mod.blesshp"), "0"),
		BLESSMR (Messages.getString("SiteDetailsSection.mod.blessmr"), "0"),
		BLESSMOR (Messages.getString("SiteDetailsSection.mod.blessmor"), "0"),
		BLESSSTR (Messages.getString("SiteDetailsSection.mod.blessstr"), "0"),
		BLESSATT (Messages.getString("SiteDetailsSection.mod.blessatt"), "0"),
		BLESSDEF (Messages.getString("SiteDetailsSection.mod.blessdef"), "0"),
		BLESSPREC (Messages.getString("SiteDetailsSection.mod.blessprec"), "0"),
		BLESSFIRERES (Messages.getString("SiteDetailsSection.mod.blessfireres"), "0"),
		BLESSCOLDRES (Messages.getString("SiteDetailsSection.mod.blesscoldres"), "0"),
		BLESSSHOCKRES (Messages.getString("SiteDetailsSection.mod.blessshockres"), "0"),
		BLESSPOISRES (Messages.getString("SiteDetailsSection.mod.blesspoisres"), "0"),
		BLESSAIRSHLD (Messages.getString("SiteDetailsSection.mod.blessairshld"), "0"),
		BLESSREINVIG (Messages.getString("SiteDetailsSection.mod.blessreinvig"), "0"),
		BLESSDTV (Messages.getString("SiteDetailsSection.mod.blessdtv"), "0"),
		BLESSANIMAWE (Messages.getString("SiteDetailsSection.mod.blessanimawe"), "0"),
		BLESSAWE (Messages.getString("SiteDetailsSection.mod.blessawe"), "0"),
		BLESSDARKVIS (Messages.getString("SiteDetailsSection.mod.blessdarkvis"), "0"),
		EVIL (Messages.getString("SiteDetailsSection.mod.evil")),
		WILD (Messages.getString("SiteDetailsSection.mod.wild"));

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
		private MappedDynamicCombo value1;
		private Text value2;
		private Label defaultLabel1;
		private Label defaultLabel2;
	}
	
	class Inst4Fields implements InstFields {
		private Button check;
		private Label defaultLabel;
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
	
	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	private Set<List<Inst>> dynamicFields = new HashSet<List<Inst>>();

	public SiteDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.PATH, new Inst6Fields());
		instMap.put(Inst.LEVEL, new Inst2Fields());
		instMap.put(Inst.RARITY, new Inst2Fields());
		instMap.put(Inst.LOC, new Inst2Fields());
		instMap.put(Inst.HOMEMON1, new Inst5Fields());
		instMap.put(Inst.HOMEMON2, new Inst5Fields());
		instMap.put(Inst.HOMEMON3, new Inst5Fields());
		instMap.put(Inst.HOMEMON4, new Inst5Fields());
		instMap.put(Inst.HOMEMON5, new Inst5Fields());
		instMap.put(Inst.HOMEMON6, new Inst5Fields());
		instMap.put(Inst.HOMEMON7, new Inst5Fields());
		instMap.put(Inst.HOMEMON8, new Inst5Fields());
		instMap.put(Inst.HOMECOM1, new Inst5Fields());
		instMap.put(Inst.HOMECOM2, new Inst5Fields());
		instMap.put(Inst.HOMECOM3, new Inst5Fields());
		instMap.put(Inst.HOMECOM4, new Inst5Fields());
		instMap.put(Inst.HOMECOM5, new Inst5Fields());
		instMap.put(Inst.HOMECOM6, new Inst5Fields());
		instMap.put(Inst.HOMECOM7, new Inst5Fields());
		instMap.put(Inst.HOMECOM8, new Inst5Fields());
		instMap.put(Inst.MON1, new Inst5Fields());
		instMap.put(Inst.MON2, new Inst5Fields());
		instMap.put(Inst.MON3, new Inst5Fields());
		instMap.put(Inst.MON4, new Inst5Fields());
		instMap.put(Inst.MON5, new Inst5Fields());
		instMap.put(Inst.MON6, new Inst5Fields());
		instMap.put(Inst.MON7, new Inst5Fields());
		instMap.put(Inst.MON8, new Inst5Fields());
		instMap.put(Inst.COM1, new Inst5Fields());
		instMap.put(Inst.COM2, new Inst5Fields());
		instMap.put(Inst.COM3, new Inst5Fields());
		instMap.put(Inst.COM4, new Inst5Fields());
		instMap.put(Inst.COM5, new Inst5Fields());
		instMap.put(Inst.COM6, new Inst5Fields());
		instMap.put(Inst.COM7, new Inst5Fields());
		instMap.put(Inst.COM8, new Inst5Fields());
		instMap.put(Inst.GOLD, new Inst2Fields());
		instMap.put(Inst.RES, new Inst2Fields());
		instMap.put(Inst.INCSCALE1, new Inst6Fields());
		instMap.put(Inst.INCSCALE2, new Inst6Fields());
		instMap.put(Inst.DECSCALE1, new Inst6Fields());
		instMap.put(Inst.DECSCALE2, new Inst6Fields());
		instMap.put(Inst.HEAL, new Inst2Fields());
		instMap.put(Inst.CURSE, new Inst2Fields());
		instMap.put(Inst.DISEASE, new Inst2Fields());
		instMap.put(Inst.HORRORMARK, new Inst2Fields());
		instMap.put(Inst.HOLYFIRE, new Inst2Fields());
		instMap.put(Inst.HOLYPOWER, new Inst2Fields());
		instMap.put(Inst.CONJCOST, new Inst2Fields());
		instMap.put(Inst.ALTCOST, new Inst2Fields());
		instMap.put(Inst.EVOCOST, new Inst2Fields());
		instMap.put(Inst.CONSTCOST, new Inst2Fields());
		instMap.put(Inst.ENCHCOST, new Inst2Fields());
		instMap.put(Inst.THAUCOST, new Inst2Fields());
		instMap.put(Inst.BLOODCOST, new Inst2Fields());
		instMap.put(Inst.GEMS1, new Inst3Fields());
		instMap.put(Inst.GEMS2, new Inst3Fields());
		instMap.put(Inst.GEMS3, new Inst3Fields());
		instMap.put(Inst.CLEAR, new Inst4Fields());
		instMap.put(Inst.LAB, new Inst4Fields());
		instMap.put(Inst.DECUNREST, new Inst2Fields());
		instMap.put(Inst.SUPPLY, new Inst2Fields());
		instMap.put(Inst.SUMMON, new Inst5Fields());
		instMap.put(Inst.VOIDGATE, new Inst2Fields());
		instMap.put(Inst.TEMPLE, new Inst4Fields());
		instMap.put(Inst.FORT, new Inst2Fields());
		instMap.put(Inst.FORTPART, new Inst2Fields());
		instMap.put(Inst.SCRY, new Inst2Fields());
		instMap.put(Inst.FIRERANGE, new Inst2Fields());
		instMap.put(Inst.AIRRANGE, new Inst2Fields());
		instMap.put(Inst.WATERRANGE, new Inst2Fields());
		instMap.put(Inst.EARTHRANGE, new Inst2Fields());
		instMap.put(Inst.ASTRALRANGE, new Inst2Fields());
		instMap.put(Inst.DEATHRANGE, new Inst2Fields());
		instMap.put(Inst.NATURERANGE, new Inst2Fields());
		instMap.put(Inst.BLOODRANGE, new Inst2Fields());
		instMap.put(Inst.ELEMENTRANGE, new Inst2Fields());
		instMap.put(Inst.SORCERYRANGE, new Inst2Fields());
		instMap.put(Inst.ALLRANGE, new Inst2Fields());
		instMap.put(Inst.XP, new Inst2Fields());
		instMap.put(Inst.ADVENTURERUIN, new Inst2Fields());
		instMap.put(Inst.CLAIM, new Inst4Fields());
		instMap.put(Inst.CLUSTER, new Inst2Fields());
		instMap.put(Inst.DOMINION, new Inst2Fields());
		instMap.put(Inst.GODDOMCHAOS, new Inst2Fields());
		instMap.put(Inst.GODDOMLAZY, new Inst2Fields());
		instMap.put(Inst.GODDOMCOLD, new Inst2Fields());
		instMap.put(Inst.GODDOMDEATH, new Inst2Fields());
		instMap.put(Inst.GODDOMMISFORTUNE, new Inst2Fields());
		instMap.put(Inst.GODDOMDRAIN, new Inst2Fields());
		instMap.put(Inst.BLESSHP, new Inst2Fields());
		instMap.put(Inst.BLESSMR, new Inst2Fields());
		instMap.put(Inst.BLESSMOR, new Inst2Fields());
		instMap.put(Inst.BLESSSTR, new Inst2Fields());
		instMap.put(Inst.BLESSATT, new Inst2Fields());
		instMap.put(Inst.BLESSDEF, new Inst2Fields());
		instMap.put(Inst.BLESSPREC, new Inst2Fields());
		instMap.put(Inst.BLESSFIRERES, new Inst2Fields());
		instMap.put(Inst.BLESSCOLDRES, new Inst2Fields());
		instMap.put(Inst.BLESSSHOCKRES, new Inst2Fields());
		instMap.put(Inst.BLESSPOISRES, new Inst2Fields());
		instMap.put(Inst.BLESSAIRSHLD, new Inst2Fields());
		instMap.put(Inst.BLESSREINVIG, new Inst2Fields());
		instMap.put(Inst.BLESSDTV, new Inst2Fields());
		instMap.put(Inst.BLESSANIMAWE, new Inst2Fields());
		instMap.put(Inst.BLESSAWE, new Inst2Fields());
		instMap.put(Inst.BLESSDARKVIS, new Inst2Fields());
		instMap.put(Inst.EVIL, new Inst4Fields());
		instMap.put(Inst.WILD, new Inst4Fields());
		
		List<Inst> homeMonList = new ArrayList<Inst>();
		homeMonList.add(Inst.HOMEMON1);
		homeMonList.add(Inst.HOMEMON2);
		homeMonList.add(Inst.HOMEMON3);
		homeMonList.add(Inst.HOMEMON4);
		homeMonList.add(Inst.HOMEMON5);
		homeMonList.add(Inst.HOMEMON6);
		homeMonList.add(Inst.HOMEMON7);
		homeMonList.add(Inst.HOMEMON8);
		dynamicFields.add(homeMonList);
		
		List<Inst> homeComList = new ArrayList<Inst>();
		homeComList.add(Inst.HOMECOM1);
		homeComList.add(Inst.HOMECOM2);
		homeComList.add(Inst.HOMECOM3);
		homeComList.add(Inst.HOMECOM4);
		homeComList.add(Inst.HOMECOM5);
		homeComList.add(Inst.HOMECOM6);
		homeComList.add(Inst.HOMECOM7);
		homeComList.add(Inst.HOMECOM8);
		dynamicFields.add(homeComList);

		List<Inst> monList = new ArrayList<Inst>();
		monList.add(Inst.MON1);
		monList.add(Inst.MON2);
		monList.add(Inst.MON3);
		monList.add(Inst.MON4);
		monList.add(Inst.MON5);
		monList.add(Inst.MON6);
		monList.add(Inst.MON7);
		monList.add(Inst.MON8);
		dynamicFields.add(monList);
		
		List<Inst> comList = new ArrayList<Inst>();
		comList.add(Inst.COM1);
		comList.add(Inst.COM2);
		comList.add(Inst.COM3);
		comList.add(Inst.COM4);
		comList.add(Inst.COM5);
		comList.add(Inst.COM6);
		comList.add(Inst.COM7);
		comList.add(Inst.COM8);
		dynamicFields.add(comList);

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
		s1.setText(Messages.getString("SiteDetailsSection.name"));
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
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("SiteDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.SITE_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setSitename(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setSitename(doc, name.getText());
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
					if (input instanceof SelectSiteById || input instanceof SelectSiteByName) {
						name.setText(getSelectSitename(input));
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		Composite leftColumn = null;
		Composite rightColumn = null;
		boolean isRight = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			
			if (key.equals(Inst.CLEAR) || 
				key.equals(Inst.HOMEMON1) || 
				key.equals(Inst.LAB) || 
				key.equals(Inst.INCSCALE1) ||
				key.equals(Inst.CONJCOST) ||
				key.equals(Inst.FIRERANGE) ||
				key.equals(Inst.HEAL) ||
				key.equals(Inst.CLAIM)) {

				final Section expandable = toolkit.createSection(client, ExpandableComposite.TWISTIE | ExpandableComposite.TITLE_BAR);
				switch (key) {
				case CLEAR:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.basic"));
					break;
				case HOMEMON1:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.monsters"));
					break;
				case LAB:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.buildings"));
					break;
				case INCSCALE1:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.scale"));
					break;
				case CONJCOST:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.rituals"));
					break;
				case FIRERANGE:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.range"));
					break;
				case HEAL:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.effects"));
					break;
				case CLAIM:
					expandable.setText(Messages.getString("SiteDetailsSection.mod.section.throne"));
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
				if (key.equals(Inst.CLEAR)) {
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
			final Button check = new DynamicButton(isRight?rightColumn:leftColumn, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.SITE_CATEGORY, key.label));

			check.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (check.getSelection()) {
						check.setFont(boldFont);
						if (field instanceof Inst1Fields) {
							addInst1(key, doc, key.defaultValue);
						} else if (field instanceof Inst2Fields || field instanceof Inst6Fields) {
							addInst2(key, doc, key.defaultValue);
						} else if (field instanceof Inst3Fields) {
							addInst3(key, doc, key.defaultValue, key.defaultValue2);
						} else if (field instanceof Inst4Fields) {
							addInst4(key, doc);
						} else if (field instanceof Inst5Fields) {
							addInst5(key, doc, key.defaultValue);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}
			});
			check.setText(key.label);

			if (field instanceof Inst4Fields) {
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.horizontalSpan = 2;
				check.setLayoutData(gd);
			}

			Text myValue1 = null;
			Text myValue2 = null;
			if (field instanceof Inst1Fields ||	field instanceof Inst2Fields ||	field instanceof Inst5Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue1 = value;
				
				if (field instanceof Inst2Fields ||	field instanceof Inst5Fields) {
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
							for (List<Inst> dynamic : dynamicFields) {
								if (dynamic.contains(key)) {
									for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
										if (dynamic.contains(fields.getKey())) {
											if (fields.getValue() instanceof Inst2Fields) {
												if (Boolean.FALSE.equals(((Inst2Fields)fields.getValue()).value.getData())) {
													((Inst2Fields)fields.getValue()).value.setData(Boolean.TRUE);
													((Inst2Fields)fields.getValue()).check.setData(Boolean.TRUE);
													((Inst2Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
													client.getParent().layout(true, true);
													break;
												}
											} else if (fields.getValue() instanceof Inst5Fields) {
												if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value.getData())) {
													((Inst5Fields)fields.getValue()).value.setData(Boolean.TRUE);
													((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
													((Inst5Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
													client.getParent().layout(true, true);
													break;
												}
											}
										}
									}
									update();
									mform.fireSelectionChanged(mform.getParts()[0], viewer.getSelection());
								}
							}
						} else {
							value.setEnabled(false);
							value.setText("");
							for (List<Inst> dynamic : dynamicFields) {
								if (dynamic.contains(key)) {
									@SuppressWarnings("rawtypes")
									List<Map.Entry> entries = Arrays.asList(instMap.entrySet().toArray(new Map.Entry[instMap.entrySet().size()]));
									Collections.reverse(entries);
									for (final Map.Entry<Inst, InstFields> fields : entries) {
										if (!key.equals(fields.getKey()) && dynamic.contains(fields.getKey())) {
											if (fields.getValue() instanceof Inst2Fields) {
												if (Boolean.TRUE.equals(((Inst2Fields)fields.getValue()).value.getData()) && !((Inst2Fields)fields.getValue()).value.isEnabled()) {
													((Inst2Fields)fields.getValue()).value.setData(Boolean.FALSE);
													((Inst2Fields)fields.getValue()).check.setData(Boolean.FALSE);
													((Inst2Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
													client.getParent().layout(true, true);
													break;
												}
											} else if (fields.getValue() instanceof Inst5Fields) {
												if (Boolean.TRUE.equals(((Inst5Fields)fields.getValue()).value.getData()) && !((Inst5Fields)fields.getValue()).value.isEnabled()) {
													((Inst5Fields)fields.getValue()).value.setData(Boolean.FALSE);
													((Inst5Fields)fields.getValue()).check.setData(Boolean.FALSE);
													((Inst5Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
													client.getParent().layout(true, true);
													break;
												}
											}
										}
									}
									update();
									mform.fireSelectionChanged(mform.getParts()[0], viewer.getSelection());
								}
							}
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
						} else if (field instanceof Inst5Fields) {
							setInst5(key, doc, value.getText());
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
							} else if (field instanceof Inst5Fields) {
								setInst5(key, doc, value.getText());
							}
						}
					}
				});
				value.setEnabled(false);
				if (field instanceof Inst1Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = 160;
					gd.horizontalSpan = 4;
				} else if (field instanceof Inst2Fields || field instanceof Inst5Fields) {
					gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
					gd.widthHint = DEFAULT_VALUE_WIDTH;
				}
				value.setLayoutData(gd);
				
			}
				
			MappedDynamicCombo myInst3Value1 = null;
			if (field instanceof Inst3Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst3Value1 = value;
				
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							value.setItems(new String[]{
									"Fire", "Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood"},
									new int[]{0, 1, 2, 3, 4, 5, 6, 7});
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

			MappedDynamicCombo myInst5Value1 = null;
			if (field instanceof Inst6Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst5Value1 = value;
				
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							if (key == Inst.PATH) {
								value.setItems(new String[]{
										"Fire", "Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood", "Holy"},
										new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8});
							} else if (key == Inst.INCSCALE1 || key == Inst.INCSCALE2 || key == Inst.DECSCALE1 || key == Inst.DECSCALE2) {
								value.setItems(new String[]{
										"Turmoil", "Sloth", "Cold", "Death", "Misfortune", "Drain"},
										new int[]{0, 1, 2, 3, 4, 5});
							}
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
			
			if (field instanceof Inst2Fields || field instanceof Inst3Fields || field instanceof Inst4Fields || field instanceof Inst5Fields || field instanceof Inst6Fields) {
				defaultLabel1 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel1.setEnabled(false);
			}
			if (field instanceof Inst2Fields || field instanceof Inst5Fields || field instanceof Inst6Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst4Fields) {
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
			
			if (field instanceof Inst1Fields) {
				((Inst1Fields)field).check = check;
				((Inst1Fields)field).value = myValue1;
			} else if (field instanceof Inst2Fields) {
				((Inst2Fields)field).check = check;
				((Inst2Fields)field).value = myValue1;
				((Inst2Fields)field).defaultLabel = defaultLabel1;
				for (List<Inst> list : dynamicFields) {
					boolean firstElement = true;
					for (Inst inst : list) {
						if (key.equals(inst)) {
							if (firstElement) {
								myValue1.setData(Boolean.TRUE);
								check.setData(Boolean.TRUE);
								defaultLabel1.setData(Boolean.TRUE);
							} else {
								myValue1.setData(Boolean.FALSE);
								check.setData(Boolean.FALSE);
								defaultLabel1.setData(Boolean.FALSE);
							}
						}
						firstElement = false;
					}
				}
			} else if (field instanceof Inst3Fields) {
				((Inst3Fields)field).check = check;
				((Inst3Fields)field).value1 = myInst3Value1;
				((Inst3Fields)field).defaultLabel1 = defaultLabel1;
				((Inst3Fields)field).value2 = myValue2;
				((Inst3Fields)field).defaultLabel2 = defaultLabel2;
			} else if (field instanceof Inst4Fields) {
				((Inst4Fields)field).check = check;
				((Inst4Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst5Fields) {
				((Inst5Fields)field).check = check;
				((Inst5Fields)field).value = myValue1;
				((Inst5Fields)field).defaultLabel = defaultLabel1;
				for (List<Inst> list : dynamicFields) {
					boolean firstElement = true;
					for (Inst inst : list) {
						if (key.equals(inst)) {
							if (firstElement) {
								myValue1.setData(Boolean.TRUE);
								check.setData(Boolean.TRUE);
								defaultLabel1.setData(Boolean.TRUE);
							} else {
								myValue1.setData(Boolean.FALSE);
								check.setData(Boolean.FALSE);
								defaultLabel1.setData(Boolean.FALSE);
							}
						}
						firstElement = false;
					}
				}
			} else if (field instanceof Inst6Fields) {
				((Inst6Fields)field).check = check;
				((Inst6Fields)field).value = myInst5Value1;
				((Inst6Fields)field).defaultLabel = defaultLabel1;
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
				if (input instanceof SelectSiteByName || input instanceof SelectSiteById) {
					String str = getSelectSitename((Site)input);
					name.setText(str!= null?str:"");
					name.setEnabled(false);
				} else {
					String str = getSitename((Site)input);
					name.setText(str!=null?str:"");
					nameCheck.setEnabled(false);
				}
				name.setEnabled(false);
				nameCheck.setSelection(false);
				nameCheck.setFont(normalFont);
			}
		}
		SiteDB siteDB = new SiteDB();
		if (input instanceof SelectSiteById) {
			siteDB = Database.getSite(((SelectSiteById)input).getValue());
		} else if (input instanceof SelectSiteByName) {
			siteDB = Database.getSite(((SelectSiteByName)input).getValue());
		}
		Set<List<Inst>> dynamicFirstEmpty = new HashSet<List<Inst>>();
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
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst2Fields)fields.getValue()).value.getData())) {
								((Inst2Fields)fields.getValue()).value.setData(Boolean.TRUE);
								((Inst2Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst2Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
								break;
							}
						}
					}
				}
				if (fields.getValue() instanceof Inst6Fields) {
					if (fields.getKey() == Inst.PATH) {
						((Inst6Fields)fields.getValue()).value.setItems(new String[]{
								"Fire", "Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood", "Holy"},
								new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8});
					} else if (fields.getKey() == Inst.INCSCALE1 || fields.getKey() == Inst.INCSCALE2 || fields.getKey() == Inst.DECSCALE1 || fields.getKey() == Inst.DECSCALE2) {
						((Inst6Fields)fields.getValue()).value.setItems(new String[]{
								"Turmoil", "Sloth", "Cold", "Death", "Misfortune", "Drain"},
								new int[]{0, 1, 2, 3, 4, 5});
					}
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
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic)) {
								if (Boolean.TRUE.equals(((Inst2Fields)fields.getValue()).value.getData())) {
									((Inst2Fields)fields.getValue()).value.setData(Boolean.FALSE);
									((Inst2Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst2Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
									break;
								}
							} else {
								dynamicFirstEmpty.add(dynamic);
								if (Boolean.FALSE.equals(((Inst2Fields)fields.getValue()).value.getData())) {
									((Inst2Fields)fields.getValue()).value.setData(Boolean.TRUE);
									((Inst2Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst2Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
									break;
								}
							}
						}
					}
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
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					((Inst3Fields)fields.getValue()).value1.setItems(new String[]{
							"Fire", "Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood"},
							new int[]{0, 1, 2, 3, 4, 5, 6, 7});
					int selection = Integer.parseInt(vals[0].toString());
					((Inst3Fields)fields.getValue()).value1.select(selection);
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					((Inst3Fields)fields.getValue()).value2.setText(vals[1].toString());
					((Inst3Fields)fields.getValue()).value2.setEnabled(true);
					((Inst3Fields)fields.getValue()).check.setSelection(true);
					((Inst3Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value1.removeAll();
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
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value.getData())) {
								((Inst5Fields)fields.getValue()).value.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
								break;
							}
						}
					}
				}
			} else {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value.setText("");
					((Inst5Fields)fields.getValue()).value.setEnabled(false);
					((Inst5Fields)fields.getValue()).check.setSelection(false);
					((Inst5Fields)fields.getValue()).check.setFont(normalFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic)) {
								if (Boolean.TRUE.equals(((Inst5Fields)fields.getValue()).value.getData())) {
									((Inst5Fields)fields.getValue()).value.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
									break;
								}
							} else {
								dynamicFirstEmpty.add(dynamic);
								if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value.getData())) {
									((Inst5Fields)fields.getValue()).value.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
									break;
								}
							}
						}
					}
				}
			}

			if (input instanceof SelectSiteByName || input instanceof SelectSiteById) {
				switch (fields.getKey()) {
				case PATH:
					if (siteDB.path != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(siteDB.path)));
						Inst.PATH.defaultValue = siteDB.path.toString();
					}
					break;
				case LEVEL:
					if (siteDB.level != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.level));
						Inst.LEVEL.defaultValue = siteDB.level.toString();
					}
					break;
				case RARITY:
					if (siteDB.rarity != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.rarity));
						Inst.RARITY.defaultValue = siteDB.rarity.toString();
					}
					break;
				case LOC:
					if (siteDB.loc != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.loc));
						Inst.LOC.defaultValue = siteDB.loc.toString();
					}
					break;
//				case HOMEMON:
//					if (siteDB.homemon != null) {
//						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.homemon));
//						Inst.HOMEMON.defaultValue = siteDB.homemon.toString();
//					}
//					break;
//				case HOMECOM:
//					if (siteDB.homecom != null) {
//						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.homecom));
//						Inst.HOMECOM.defaultValue = siteDB.homecom.toString();
//					}
//					break;
				case MON1:
					if (siteDB.mon1 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.mon1));
						Inst.MON1.defaultValue = siteDB.mon1.toString();
					}
					break;
				case MON2:
					if (siteDB.mon2 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.mon2));
						Inst.MON2.defaultValue = siteDB.mon2.toString();
					}
					break;
				case MON3:
					if (siteDB.mon3 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.mon3));
						Inst.MON3.defaultValue = siteDB.mon3.toString();
					}
					break;
				case MON4:
					if (siteDB.mon4 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.mon4));
						Inst.MON4.defaultValue = siteDB.mon4.toString();
					}
					break;
				case MON5:
					if (siteDB.mon5 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.mon5));
						Inst.MON5.defaultValue = siteDB.mon5.toString();
					}
					break;
				case COM1:
					if (siteDB.com1 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.com1));
						Inst.COM1.defaultValue = siteDB.com1.toString();
					}
					break;
				case COM2:
					if (siteDB.com2 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.com2));
						Inst.COM2.defaultValue = siteDB.com2.toString();
					}
					break;
				case COM3:
					if (siteDB.com3 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.com3));
						Inst.COM3.defaultValue = siteDB.com3.toString();
					}
					break;
				case COM4:
					if (siteDB.com4 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.com4));
						Inst.COM4.defaultValue = siteDB.com4.toString();
					}
					break;
				case COM5:
					if (siteDB.com5 != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.com5));
						Inst.COM5.defaultValue = siteDB.com5.toString();
					}
					break;
				case GOLD:
					if (siteDB.gold != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.gold));
						Inst.GOLD.defaultValue = siteDB.gold.toString();
					}
					break;
				case RES:
					if (siteDB.res != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.res));
						Inst.RES.defaultValue = siteDB.res.toString();
					}
					break;
				case INCSCALE1:
					if (siteDB.incscale1 != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getScaleName(siteDB.incscale1)));
						Inst.INCSCALE1.defaultValue = siteDB.incscale1.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCSCALE1.defaultValue = "0";
					}
					break;
				case INCSCALE2:
					if (siteDB.incscale2 != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getScaleName(siteDB.incscale2)));
						Inst.INCSCALE2.defaultValue = siteDB.incscale2.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCSCALE2.defaultValue = "0";
					}
					break;
				case DECSCALE1:
					if (siteDB.decscale1 != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getScaleName(siteDB.decscale1)));
						Inst.DECSCALE1.defaultValue = siteDB.decscale1.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DECSCALE1.defaultValue = "0";
					}
					break;
				case DECSCALE2:
					if (siteDB.decscale2 != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getScaleName(siteDB.decscale2)));
						Inst.DECSCALE2.defaultValue = siteDB.decscale2.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DECSCALE2.defaultValue = "0";
					}
					break;
				case GEMS1:
					if (siteDB.gemspath1 != null && siteDB.gemsamt1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(siteDB.gemspath1)));
						Inst.GEMS1.defaultValue = siteDB.gemspath1.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.gemsamt1));
						Inst.GEMS1.defaultValue2 = siteDB.gemsamt1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.GEMS1.defaultValue = "0";
						Inst.GEMS1.defaultValue2 = "1";
					}
					break;
				case GEMS2:
					if (siteDB.gemspath2 != null && siteDB.gemsamt2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(siteDB.gemspath2)));
						Inst.GEMS2.defaultValue = siteDB.gemspath2.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.gemsamt2));
						Inst.GEMS2.defaultValue2 = siteDB.gemsamt2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.GEMS2.defaultValue = "0";
						Inst.GEMS2.defaultValue2 = "1";
					}
					break;
				case GEMS3:
					if (siteDB.gemspath3 != null && siteDB.gemsamt3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(siteDB.gemspath3)));
						Inst.GEMS3.defaultValue = siteDB.gemspath3.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", siteDB.gemsamt3));
						Inst.GEMS3.defaultValue2 = siteDB.gemsamt3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.GEMS3.defaultValue = "0";
						Inst.GEMS3.defaultValue2 = "1";
					}
					break;
				}
			}
		}
		name.getParent().getParent().getParent().layout(true, true);
	}
	
	private String getSelectSitename(Object site) {
		if (site instanceof SelectSiteByName) {
			return ((SelectSiteByName)site).getValue();
		} else {
			int id = ((SelectSiteById)site).getValue();
			return Database.getSiteName(id);
		}
	}
	
	private String getSitename(Site site) {
		EList<SiteMods> list = site.getMods();
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst1) {
				if (((SiteInst1)mod).isName()) {
					return ((SiteInst1)mod).getValue();
				}
			}
		}
		return null;
	}
	
	private void setSitename(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Site siteToEdit = (Site)input;
				EList<SiteMods> mods = siteToEdit.getMods();
				boolean nameSet = false;
				for (SiteMods mod : mods) {
					if (mod instanceof SiteInst1) {
						if (((SiteInst1)mod).isName()) {
							((SiteInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					SiteInst1 nameInst = DmFactory.eINSTANCE.createSiteInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private String getPathName(int id) {
		switch (id) {
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

	private String getScaleName(int id) {
		switch (id) {
		case 0:
			return "Turmoil";
		case 1:
			return "Sloth";
		case 2:
			return "Cold";
		case 3:
			return "Death";
		case 4:
			return "Misfortune";
		case 5:
			return "Drain";
		}
		return "Unknown";
	}

	private String getInst1(Inst inst2, Object site) {
		EList<SiteMods> list = ((Site)site).getMods();
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst1) {
				switch (inst2) {
				case NAME:
					if (((SiteInst1)mod).isName()){
						return ((SiteInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer getInst2(Inst inst2, Object site) {
		EList<SiteMods> list = ((Site)site).getMods();
		int inscaleCount = 0;
		int descaleCount = 0;
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst2) {
				switch (inst2) {
				case PATH:
					if (((SiteInst2)mod).isPath()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case LEVEL:
					if (((SiteInst2)mod).isLevel()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case RARITY:
					if (((SiteInst2)mod).isRarity()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case LOC:
					if (((SiteInst2)mod).isLoc()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GOLD:
					if (((SiteInst2)mod).isGold()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case RES:
					if (((SiteInst2)mod).isRes()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case INCSCALE1:
					if (((SiteInst2)mod).isIncscale()){
						inscaleCount++;
						if (inscaleCount == 1) {
							return Integer.valueOf(((SiteInst2)mod).getValue());
						}
					}
					break;
				case INCSCALE2:
					if (((SiteInst2)mod).isIncscale()){
						inscaleCount++;
						if (inscaleCount == 2) {
							return Integer.valueOf(((SiteInst2)mod).getValue());
						}
					}
					break;
				case DECSCALE1:
					if (((SiteInst2)mod).isDecscale()){
						descaleCount++;
						if (descaleCount == 1) {
							return Integer.valueOf(((SiteInst2)mod).getValue());
						}
					}
					break;
				case DECSCALE2:
					if (((SiteInst2)mod).isDecscale()){
						descaleCount++;
						if (descaleCount == 2) {
							return Integer.valueOf(((SiteInst2)mod).getValue());
						}
					}
					break;
				case HEAL:
					if (((SiteInst2)mod).isHeal()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case CURSE:
					if (((SiteInst2)mod).isCurse()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case DISEASE:
					if (((SiteInst2)mod).isDisease()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case HORRORMARK:
					if (((SiteInst2)mod).isHorrormark()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case HOLYFIRE:
					if (((SiteInst2)mod).isHolyfire()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case HOLYPOWER:
					if (((SiteInst2)mod).isHolypower()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case CONJCOST:
					if (((SiteInst2)mod).isConjcost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ALTCOST:
					if (((SiteInst2)mod).isAltcost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case EVOCOST:
					if (((SiteInst2)mod).isEvocost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case CONSTCOST:
					if (((SiteInst2)mod).isConstcost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ENCHCOST:
					if (((SiteInst2)mod).isEnchcost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case THAUCOST:
					if (((SiteInst2)mod).isThaucost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLOODCOST:
					if (((SiteInst2)mod).isBloodcost()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case DECUNREST:
					if (((SiteInst2)mod).isDecunrest()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case SUPPLY:
					if (((SiteInst2)mod).isSupply()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case VOIDGATE:
					if (((SiteInst2)mod).isVoidgate()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case FORT:
					if (((SiteInst2)mod).isFort()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case FORTPART:
					if (((SiteInst2)mod).isFortpart()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case SCRY:
					if (((SiteInst2)mod).isScry()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case FIRERANGE:
					if (((SiteInst2)mod).isFirerange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case AIRRANGE:
					if (((SiteInst2)mod).isAirrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case WATERRANGE:
					if (((SiteInst2)mod).isWaterrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case EARTHRANGE:
					if (((SiteInst2)mod).isEarthrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ASTRALRANGE:
					if (((SiteInst2)mod).isAstralrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case DEATHRANGE:
					if (((SiteInst2)mod).isDeathrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case NATURERANGE:
					if (((SiteInst2)mod).isNaturerange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLOODRANGE:
					if (((SiteInst2)mod).isBloodrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ELEMENTRANGE:
					if (((SiteInst2)mod).isElementrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case SORCERYRANGE:
					if (((SiteInst2)mod).isSorceryrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ALLRANGE:
					if (((SiteInst2)mod).isAllrange()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case XP:
					if (((SiteInst2)mod).isXp()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case ADVENTURERUIN:
					if (((SiteInst2)mod).isAdventureruin()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case CLUSTER:
					if (((SiteInst2)mod).isCluster()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case DOMINION:
					if (((SiteInst2)mod).isDominion()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMCHAOS:
					if (((SiteInst2)mod).isGoddomchaos()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMLAZY:
					if (((SiteInst2)mod).isGoddomlazy()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMCOLD:
					if (((SiteInst2)mod).isGoddomcold()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMDEATH:
					if (((SiteInst2)mod).isGoddomdeath()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMMISFORTUNE:
					if (((SiteInst2)mod).isGoddommisfortune()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case GODDOMDRAIN:
					if (((SiteInst2)mod).isGoddomdrain()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSHP:
					if (((SiteInst2)mod).isBlesshp()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSMR:
					if (((SiteInst2)mod).isBlessmr()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSMOR:
					if (((SiteInst2)mod).isBlessmor()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSSTR:
					if (((SiteInst2)mod).isBlessstr()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSATT:
					if (((SiteInst2)mod).isBlessatt()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSDEF:
					if (((SiteInst2)mod).isBlessdef()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSPREC:
					if (((SiteInst2)mod).isBlessprec()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSFIRERES:
					if (((SiteInst2)mod).isBlessfireres()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSCOLDRES:
					if (((SiteInst2)mod).isBlesscoldres()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSSHOCKRES:
					if (((SiteInst2)mod).isBlessshockres()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSPOISRES:
					if (((SiteInst2)mod).isBlesspoisres()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSAIRSHLD:
					if (((SiteInst2)mod).isBlessairshld()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSREINVIG:
					if (((SiteInst2)mod).isBlessreinvig()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSDTV:
					if (((SiteInst2)mod).isBlessdtv()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSANIMAWE:
					if (((SiteInst2)mod).isBlessanimawe()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSAWE:
					if (((SiteInst2)mod).isBlessawe()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				case BLESSDARKVIS:
					if (((SiteInst2)mod).isBlessdarkvis()){
						return Integer.valueOf(((SiteInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer[] getInst3(Inst inst3, Object site) {
		EList<SiteMods> list = ((Site)site).getMods();
		int gemCount = 0;
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst3) {
				switch (inst3) {
				case GEMS1:
					if (((SiteInst3)mod).isGems()) {
						gemCount++;
						if (gemCount == 1) {
							return new Integer[]{Integer.valueOf(((SiteInst3)mod).getValue1()), Integer.valueOf(((SiteInst3)mod).getValue2())};
						}
					}
					break;
				case GEMS2:
					if (((SiteInst3)mod).isGems()) {
						gemCount++;
						if (gemCount == 2) {
							return new Integer[]{Integer.valueOf(((SiteInst3)mod).getValue1()), Integer.valueOf(((SiteInst3)mod).getValue2())};
						}
					}
					break;
				case GEMS3:
					if (((SiteInst3)mod).isGems()) {
						gemCount++;
						if (gemCount == 3) {
							return new Integer[]{Integer.valueOf(((SiteInst3)mod).getValue1()), Integer.valueOf(((SiteInst3)mod).getValue2())};
						}
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Boolean getInst4(Inst inst4, Object site) {
		EList<SiteMods> list = ((Site)site).getMods();
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst4) {
				switch (inst4) {
				case CLEAR:
					if (((SiteInst4)mod).isClear()){
						return Boolean.TRUE;
					}
					break;
				case LAB:
					if (((SiteInst4)mod).isLab()){
						return Boolean.TRUE;
					}
					break;
				case TEMPLE:
					if (((SiteInst4)mod).isTemple()){
						return Boolean.TRUE;
					}
					break;
				case CLAIM:
					if (((SiteInst4)mod).isClaim()){
						return Boolean.TRUE;
					}
					break;
				case EVIL:
					if (((SiteInst4)mod).isEvil()){
						return Boolean.TRUE;
					}
					break;
				case WILD:
					if (((SiteInst4)mod).isWild()){
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private Object getInst5(Inst inst5, Object site) {
		int homemonCount = 0;
		int homecomCount = 0;
		int monCount = 0;
		int comCount = 0;
		EList<SiteMods> list = ((Site)site).getMods();
		for (SiteMods mod : list) {
			if (mod instanceof SiteInst5) {
				switch (inst5) {
				case HOMEMON1:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 1) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON2:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 2) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON3:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 3) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON4:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 4) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON5:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 5) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON6:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 6) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON7:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 7) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMEMON8:
					if (((SiteInst5)mod).isHomemon()){
						homemonCount++;
						if (homemonCount == 8) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM1:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 1) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM2:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 2) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM3:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 3) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM4:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 4) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM5:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 5) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM6:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 6) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM7:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 7) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case HOMECOM8:
					if (((SiteInst5)mod).isHomecom()){
						homecomCount++;
						if (homecomCount == 8) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON1:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 1) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON2:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 2) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON3:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 3) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON4:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 4) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON5:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 5) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON6:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 6) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON7:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 7) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MON8:
					if (((SiteInst5)mod).isMon()){
						monCount++;
						if (monCount == 8) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM1:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 1) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM2:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 2) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM3:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 3) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM4:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 4) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM5:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 5) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM6:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 6) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM7:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 7) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case COM8:
					if (((SiteInst5)mod).isCom()){
						comCount++;
						if (comCount == 8) {
							String strVal = ((SiteInst5)mod).getValue1();
							Integer intVal = ((SiteInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SUMMON:
					if (((SiteInst5)mod).isSummon()){
						String strVal = ((SiteInst5)mod).getValue1();
						Integer intVal = ((SiteInst5)mod).getValue2();
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
				Site siteToEdit = (Site)input;
				EList<SiteMods> mods = siteToEdit.getMods();				
				for (SiteMods mod : mods) {
					if (mod instanceof SiteInst1) {
						switch (inst2) {
						case NAME:
							if (((SiteInst1)mod).isName()) {
								((SiteInst1)mod).setValue(newName);
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
				Site siteToEdit = (Site)input;
				int inscaleCount = 0;
				int descaleCount = 0;
				EList<SiteMods> mods = siteToEdit.getMods();
				for (SiteMods mod : mods) {
					if (mod instanceof SiteInst2) {
						switch (inst2) {
						case PATH:
							if (((SiteInst2)mod).isPath()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LEVEL:
							if (((SiteInst2)mod).isLevel()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RARITY:
							if (((SiteInst2)mod).isRarity()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LOC:
							if (((SiteInst2)mod).isLoc()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GOLD:
							if (((SiteInst2)mod).isGold()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RES:
							if (((SiteInst2)mod).isRes()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INCSCALE1:
							if (((SiteInst2)mod).isIncscale()){
								inscaleCount++;
								if (inscaleCount == 1) {
									((SiteInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case INCSCALE2:
							if (((SiteInst2)mod).isIncscale()){
								inscaleCount++;
								if (inscaleCount == 2) {
									((SiteInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case DECSCALE1:
							if (((SiteInst2)mod).isDecscale()){
								descaleCount++;
								if (descaleCount == 1) {
									((SiteInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case DECSCALE2:
							if (((SiteInst2)mod).isDecscale()){
								descaleCount++;
								if (descaleCount == 2) {
									((SiteInst2)mod).setValue(Integer.parseInt(newName));
								}
							}
							break;
						case HEAL:
							if (((SiteInst2)mod).isHeal()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CURSE:
							if (((SiteInst2)mod).isCurse()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DISEASE:
							if (((SiteInst2)mod).isDisease()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HORRORMARK:
							if (((SiteInst2)mod).isHorrormark()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HOLYFIRE:
							if (((SiteInst2)mod).isHolyfire()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HOLYPOWER:
							if (((SiteInst2)mod).isHolypower()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CONJCOST:
							if (((SiteInst2)mod).isConjcost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ALTCOST:
							if (((SiteInst2)mod).isAltcost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EVOCOST:
							if (((SiteInst2)mod).isEvocost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CONSTCOST:
							if (((SiteInst2)mod).isConstcost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ENCHCOST:
							if (((SiteInst2)mod).isEnchcost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case THAUCOST:
							if (((SiteInst2)mod).isThaucost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLOODCOST:
							if (((SiteInst2)mod).isBloodcost()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DECUNREST:
							if (((SiteInst2)mod).isDecunrest()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUPPLY:
							if (((SiteInst2)mod).isSupply()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case VOIDGATE:
							if (((SiteInst2)mod).isVoidgate()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORT:
							if (((SiteInst2)mod).isFort()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORTPART:
							if (((SiteInst2)mod).isFortpart()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SCRY:
							if (((SiteInst2)mod).isScry()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRERANGE:
							if (((SiteInst2)mod).isFirerange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AIRRANGE:
							if (((SiteInst2)mod).isAirrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WATERRANGE:
							if (((SiteInst2)mod).isWaterrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EARTHRANGE:
							if (((SiteInst2)mod).isEarthrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ASTRALRANGE:
							if (((SiteInst2)mod).isAstralrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHRANGE:
							if (((SiteInst2)mod).isDeathrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NATURERANGE:
							if (((SiteInst2)mod).isNaturerange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLOODRANGE:
							if (((SiteInst2)mod).isBloodrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ELEMENTRANGE:
							if (((SiteInst2)mod).isElementrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SORCERYRANGE:
							if (((SiteInst2)mod).isSorceryrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ALLRANGE:
							if (((SiteInst2)mod).isAllrange()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case XP:
							if (((SiteInst2)mod).isXp()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ADVENTURERUIN:
							if (((SiteInst2)mod).isAdventureruin()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CLUSTER:
							if (((SiteInst2)mod).isCluster()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DOMINION:
							if (((SiteInst2)mod).isDominion()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMCHAOS:
							if (((SiteInst2)mod).isGoddomchaos()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMLAZY:
							if (((SiteInst2)mod).isGoddomlazy()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMCOLD:
							if (((SiteInst2)mod).isGoddomcold()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMDEATH:
							if (((SiteInst2)mod).isGoddomdeath()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMMISFORTUNE:
							if (((SiteInst2)mod).isGoddommisfortune()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GODDOMDRAIN:
							if (((SiteInst2)mod).isGoddomdrain()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSHP:
							if (((SiteInst2)mod).isBlesshp()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSMR:
							if (((SiteInst2)mod).isBlessmr()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSMOR:
							if (((SiteInst2)mod).isBlessmor()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSSTR:
							if (((SiteInst2)mod).isBlessstr()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSATT:
							if (((SiteInst2)mod).isBlessatt()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSDEF:
							if (((SiteInst2)mod).isBlessdef()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSPREC:
							if (((SiteInst2)mod).isBlessprec()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSFIRERES:
							if (((SiteInst2)mod).isBlessfireres()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSCOLDRES:
							if (((SiteInst2)mod).isBlesscoldres()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSSHOCKRES:
							if (((SiteInst2)mod).isBlessshockres()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSPOISRES:
							if (((SiteInst2)mod).isBlesspoisres()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSAIRSHLD:
							if (((SiteInst2)mod).isBlessairshld()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSREINVIG:
							if (((SiteInst2)mod).isBlessreinvig()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSDTV:
							if (((SiteInst2)mod).isBlessdtv()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSANIMAWE:
							if (((SiteInst2)mod).isBlessanimawe()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSAWE:
							if (((SiteInst2)mod).isBlessawe()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLESSDARKVIS:
							if (((SiteInst2)mod).isBlessdarkvis()){
								((SiteInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						}
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
				Site siteToEdit = (Site)input;
				int gemCount = 0;
				EList<SiteMods> mods = siteToEdit.getMods();
				for (SiteMods mod : mods) {
					if (mod instanceof SiteInst3) {
						switch (inst3) {
						case GEMS1:
							if (((SiteInst3)mod).isGems()) {
								gemCount++;
								if (gemCount == 1) {
									if (value1 != null) {
										((SiteInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SiteInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMS2:
							if (((SiteInst3)mod).isGems()) {
								gemCount++;
								if (gemCount == 2) {
									if (value1 != null) {
										((SiteInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SiteInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMS3:
							if (((SiteInst3)mod).isGems()) {
								gemCount++;
								if (gemCount == 3) {
									if (value1 != null) {
										((SiteInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((SiteInst3)mod).setValue2(Integer.parseInt(value2));
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
	
	private void setInst5(final Inst inst5, final XtextEditor editor, final String newName) {
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				int homemonCount = 0;
				int homecomCount = 0;
				int monCount = 0;
				int comCount = 0;
				Site siteToEdit = (Site)input;
				List<SiteMods> modsToRemove = new ArrayList<SiteMods>();
				List<SiteMods> modsToAdd = new ArrayList<SiteMods>();
				EList<SiteMods> mods = siteToEdit.getMods();
				for (SiteMods mod : mods) {
					if (mod instanceof SiteInst5) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst5) {
						case HOMEMON1:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 1) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON2:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 2) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON3:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 3) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON4:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 4) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON5:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 5) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON6:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 6) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON7:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 7) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMEMON8:
							if (((SiteInst5)mod).isHomemon()){
								homemonCount++;
								if (homemonCount == 8) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomemon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM1:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 1) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM2:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 2) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM3:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 3) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM4:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 4) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM5:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 5) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM6:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 6) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM7:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 7) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case HOMECOM8:
							if (((SiteInst5)mod).isHomecom()){
								homecomCount++;
								if (homecomCount == 8) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setHomecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON1:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 1) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON2:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 2) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON3:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 3) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON4:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 4) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON5:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 5) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON6:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 6) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON7:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 7) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MON8:
							if (((SiteInst5)mod).isMon()){
								monCount++;
								if (monCount == 8) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setMon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM1:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 1) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM2:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 2) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM3:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 3) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM4:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 4) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM5:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 5) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM6:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 6) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM7:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 7) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case COM8:
							if (((SiteInst5)mod).isCom()){
								comCount++;
								if (comCount == 8) {
									modsToRemove.add(mod);
									SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
									newMod.setCom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SUMMON:
							if (((SiteInst5)mod).isSummon()){
								modsToRemove.add(mod);
								SiteInst5 newMod = DmFactory.eINSTANCE.createSiteInst5();
								newMod.setSummon(true);
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
						EList<SiteMods> mods = ((Site)input).getMods();
						SiteInst1 type = DmFactory.eINSTANCE.createSiteInst1();
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
						EList<SiteMods> mods = ((Site)input).getMods();
						SiteInst2 type = DmFactory.eINSTANCE.createSiteInst2();
						switch (inst) {
						case PATH:
							type.setPath(true);
							break;
						case LEVEL:
							type.setLevel(true);
							break;
						case RARITY:
							type.setRarity(true);
							break;
						case LOC:
							type.setLoc(true);
							break;
						case GOLD:
							type.setGold(true);
							break;
						case RES:
							type.setRes(true);
							break;
						case INCSCALE1:
							type.setIncscale(true);
							break;
						case INCSCALE2:
							type.setIncscale(true);
							break;
						case DECSCALE1:
							type.setDecscale(true);
							break;
						case DECSCALE2:
							type.setDecscale(true);
							break;
						case HEAL:
							type.setHeal(true);
							break;
						case CURSE:
							type.setCurse(true);
							break;
						case DISEASE:
							type.setDisease(true);
							break;
						case HORRORMARK:
							type.setHorrormark(true);
							break;
						case HOLYFIRE:
							type.setHolyfire(true);
							break;
						case HOLYPOWER:
							type.setHolypower(true);
							break;
						case CONJCOST:
							type.setConjcost(true);
							break;
						case ALTCOST:
							type.setAltcost(true);
							break;
						case EVOCOST:
							type.setEvocost(true);
							break;
						case CONSTCOST:
							type.setConstcost(true);
							break;
						case ENCHCOST:
							type.setEnchcost(true);
							break;
						case THAUCOST:
							type.setThaucost(true);
							break;
						case BLOODCOST:
							type.setBloodcost(true);
							break;
						case DECUNREST:
							type.setDecunrest(true);
							break;
						case SUPPLY:
							type.setSupply(true);
							break;
						case VOIDGATE:
							type.setVoidgate(true);
							break;
						case FORT:
							type.setFort(true);
							break;
						case FORTPART:
							type.setFortpart(true);
							break;
						case SCRY:
							type.setScry(true);
							break;
						case FIRERANGE:
							type.setFirerange(true);
							break;
						case AIRRANGE:
							type.setAirrange(true);
							break;
						case WATERRANGE:
							type.setWaterrange(true);
							break;
						case EARTHRANGE:
							type.setEarthrange(true);
							break;
						case ASTRALRANGE:
							type.setAstralrange(true);
							break;
						case DEATHRANGE:
							type.setDeathrange(true);
							break;
						case NATURERANGE:
							type.setNaturerange(true);
							break;
						case BLOODRANGE:
							type.setBloodrange(true);
							break;
						case ELEMENTRANGE:
							type.setElementrange(true);
							break;
						case SORCERYRANGE:
							type.setSorceryrange(true);
							break;
						case ALLRANGE:
							type.setAllrange(true);
							break;
						case XP:
							type.setXp(true);
							break;
						case ADVENTURERUIN:
							type.setAdventureruin(true);
							break;
						case CLUSTER:
							type.setCluster(true);
							break;
						case DOMINION:
							type.setDominion(true);
							break;
						case GODDOMCHAOS:
							type.setGoddomchaos(true);
							break;
						case GODDOMLAZY:
							type.setGoddomlazy(true);
							break;
						case GODDOMCOLD:
							type.setGoddomcold(true);
							break;
						case GODDOMDEATH:
							type.setGoddomdeath(true);
							break;
						case GODDOMMISFORTUNE:
							type.setGoddommisfortune(true);
							break;
						case GODDOMDRAIN:
							type.setGoddomdrain(true);
							break;
						case BLESSHP:
							type.setBlesshp(true);
							break;
						case BLESSMR:
							type.setBlessmr(true);
							break;
						case BLESSMOR:
							type.setBlessmor(true);
							break;
						case BLESSSTR:
							type.setBlessstr(true);
							break;
						case BLESSATT:
							type.setBlessatt(true);
							break;
						case BLESSDEF:
							type.setBlessdef(true);
							break;
						case BLESSPREC:
							type.setBlessprec(true);
							break;
						case BLESSFIRERES:
							type.setBlessfireres(true);
							break;
						case BLESSCOLDRES:
							type.setBlesscoldres(true);
							break;
						case BLESSSHOCKRES:
							type.setBlessshockres(true);
							break;
						case BLESSPOISRES:
							type.setBlesspoisres(true);
							break;
						case BLESSAIRSHLD:
							type.setBlessairshld(true);
							break;
						case BLESSREINVIG:
							type.setBlessreinvig(true);
							break;
						case BLESSDTV:
							type.setBlessdtv(true);
							break;
						case BLESSANIMAWE:
							type.setBlessanimawe(true);
							break;
						case BLESSAWE:
							type.setBlessawe(true);
							break;
						case BLESSDARKVIS:
							type.setBlessdarkvis(true);
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
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<SiteMods> mods = ((Site)input).getMods();
						SiteInst3 type = DmFactory.eINSTANCE.createSiteInst3();
						switch (inst) {
						case GEMS1:
							type.setGems(true);
							break;
						case GEMS2:
							type.setGems(true);
							break;
						case GEMS3:
							type.setGems(true);
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
						EList<SiteMods> mods = ((Site)input).getMods();
						SiteInst4 type = DmFactory.eINSTANCE.createSiteInst4();
						switch (inst) {
						case CLEAR:
							type.setClear(true);
							break;
						case LAB:
							type.setLab(true);
							break;
						case TEMPLE:
							type.setTemple(true);
							break;
						case CLAIM:
							type.setClaim(true);
							break;
						case EVIL:
							type.setEvil(true);
							break;
						case WILD:
							type.setWild(true);
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
					public void process(XtextResource state) throws Exception {
						EList<SiteMods> mods = ((Site)input).getMods();
						SiteInst5 type = DmFactory.eINSTANCE.createSiteInst5();
						switch (inst) {
						case HOMEMON1:
							type.setHomemon(true);
							break;
						case HOMEMON2:
							type.setHomemon(true);
							break;
						case HOMEMON3:
							type.setHomemon(true);
							break;
						case HOMEMON4:
							type.setHomemon(true);
							break;
						case HOMEMON5:
							type.setHomemon(true);
							break;
						case HOMEMON6:
							type.setHomemon(true);
							break;
						case HOMEMON7:
							type.setHomemon(true);
							break;
						case HOMEMON8:
							type.setHomemon(true);
							break;
						case HOMECOM1:
							type.setHomecom(true);
							break;
						case HOMECOM2:
							type.setHomecom(true);
							break;
						case HOMECOM3:
							type.setHomecom(true);
							break;
						case HOMECOM4:
							type.setHomecom(true);
							break;
						case HOMECOM5:
							type.setHomecom(true);
							break;
						case HOMECOM6:
							type.setHomecom(true);
							break;
						case HOMECOM7:
							type.setHomecom(true);
							break;
						case HOMECOM8:
							type.setHomecom(true);
							break;
						case MON1:
							type.setMon(true);
							break;
						case MON2:
							type.setMon(true);
							break;
						case MON3:
							type.setMon(true);
							break;
						case MON4:
							type.setMon(true);
							break;
						case MON5:
							type.setMon(true);
							break;
						case MON6:
							type.setMon(true);
							break;
						case MON7:
							type.setMon(true);
							break;
						case MON8:
							type.setMon(true);
							break;
						case COM1:
							type.setCom(true);
							break;
						case COM2:
							type.setCom(true);
							break;
						case COM3:
							type.setCom(true);
							break;
						case COM4:
							type.setCom(true);
							break;
						case COM5:
							type.setCom(true);
							break;
						case COM6:
							type.setCom(true);
							break;
						case COM7:
							type.setCom(true);
							break;
						case COM8:
							type.setCom(true);
							break;
						case SUMMON:
							type.setSummon(true);
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
	
	private void removeInst(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						int homeMonCount = 0;
						int homeComCount = 0;
						int monCount = 0;
						int comCount = 0;
						int inscaleCount = 0;
						int descaleCount = 0;
						int gemCount = 0;
						SiteMods modToRemove = null;
						EList<SiteMods> mods = ((Site)input).getMods();
						for (SiteMods mod : mods) {
							if (mod instanceof SiteInst1) {
								switch (inst) {
								case NAME:
									if (((SiteInst1)mod).isName()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof SiteInst2) {
								switch (inst) {
								case PATH:
									if (((SiteInst2)mod).isPath()){
										modToRemove = mod;
									}
									break;
								case LEVEL:
									if (((SiteInst2)mod).isLevel()){
										modToRemove = mod;
									}
									break;
								case RARITY:
									if (((SiteInst2)mod).isRarity()){
										modToRemove = mod;
									}
									break;
								case LOC:
									if (((SiteInst2)mod).isLoc()){
										modToRemove = mod;
									}
									break;
								case GOLD:
									if (((SiteInst2)mod).isGold()){
										modToRemove = mod;
									}
									break;
								case RES:
									if (((SiteInst2)mod).isRes()){
										modToRemove = mod;
									}
									break;
								case INCSCALE1:
									if (((SiteInst2)mod).isIncscale()){
										inscaleCount++;
										if (inscaleCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case INCSCALE2:
									if (((SiteInst2)mod).isIncscale()){
										inscaleCount++;
										if (inscaleCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case DECSCALE1:
									if (((SiteInst2)mod).isDecscale()){
										descaleCount++;
										if (descaleCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case DECSCALE2:
									if (((SiteInst2)mod).isDecscale()){
										descaleCount++;
										if (descaleCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case HEAL:
									if (((SiteInst2)mod).isHeal()){
										modToRemove = mod;
									}
									break;
								case CURSE:
									if (((SiteInst2)mod).isCurse()){
										modToRemove = mod;
									}
									break;
								case DISEASE:
									if (((SiteInst2)mod).isDisease()){
										modToRemove = mod;
									}
									break;
								case HORRORMARK:
									if (((SiteInst2)mod).isHorrormark()){
										modToRemove = mod;
									}
									break;
								case HOLYFIRE:
									if (((SiteInst2)mod).isHolyfire()){
										modToRemove = mod;
									}
									break;
								case HOLYPOWER:
									if (((SiteInst2)mod).isHolypower()){
										modToRemove = mod;
									}
									break;
								case CONJCOST:
									if (((SiteInst2)mod).isConjcost()){
										modToRemove = mod;
									}
									break;
								case ALTCOST:
									if (((SiteInst2)mod).isAltcost()){
										modToRemove = mod;
									}
									break;
								case EVOCOST:
									if (((SiteInst2)mod).isEvocost()){
										modToRemove = mod;
									}
									break;
								case CONSTCOST:
									if (((SiteInst2)mod).isConstcost()){
										modToRemove = mod;
									}
									break;
								case ENCHCOST:
									if (((SiteInst2)mod).isEnchcost()){
										modToRemove = mod;
									}
									break;
								case THAUCOST:
									if (((SiteInst2)mod).isThaucost()){
										modToRemove = mod;
									}
									break;
								case BLOODCOST:
									if (((SiteInst2)mod).isBloodcost()){
										modToRemove = mod;
									}
									break;
								case DECUNREST:
									if (((SiteInst2)mod).isDecunrest()){
										modToRemove = mod;
									}
									break;
								case SUPPLY:
									if (((SiteInst2)mod).isSupply()){
										modToRemove = mod;
									}
									break;
								case VOIDGATE:
									if (((SiteInst2)mod).isVoidgate()){
										modToRemove = mod;
									}
									break;
								case FORT:
									if (((SiteInst2)mod).isFort()){
										modToRemove = mod;
									}
									break;
								case FORTPART:
									if (((SiteInst2)mod).isFortpart()){
										modToRemove = mod;
									}
									break;
								case SCRY:
									if (((SiteInst2)mod).isScry()){
										modToRemove = mod;
									}
									break;
								case FIRERANGE:
									if (((SiteInst2)mod).isFirerange()){
										modToRemove = mod;
									}
									break;
								case AIRRANGE:
									if (((SiteInst2)mod).isAirrange()){
										modToRemove = mod;
									}
									break;
								case WATERRANGE:
									if (((SiteInst2)mod).isWaterrange()){
										modToRemove = mod;
									}
									break;
								case EARTHRANGE:
									if (((SiteInst2)mod).isEarthrange()){
										modToRemove = mod;
									}
									break;
								case ASTRALRANGE:
									if (((SiteInst2)mod).isAstralrange()){
										modToRemove = mod;
									}
									break;
								case DEATHRANGE:
									if (((SiteInst2)mod).isDeathrange()){
										modToRemove = mod;
									}
									break;
								case NATURERANGE:
									if (((SiteInst2)mod).isNaturerange()){
										modToRemove = mod;
									}
									break;
								case BLOODRANGE:
									if (((SiteInst2)mod).isBloodrange()){
										modToRemove = mod;
									}
									break;
								case ELEMENTRANGE:
									if (((SiteInst2)mod).isElementrange()){
										modToRemove = mod;
									}
									break;
								case SORCERYRANGE:
									if (((SiteInst2)mod).isSorceryrange()){
										modToRemove = mod;
									}
									break;
								case ALLRANGE:
									if (((SiteInst2)mod).isAllrange()){
										modToRemove = mod;
									}
									break;
								case XP:
									if (((SiteInst2)mod).isXp()){
										modToRemove = mod;
									}
									break;
								case ADVENTURERUIN:
									if (((SiteInst2)mod).isAdventureruin()){
										modToRemove = mod;
									}
									break;
								case CLUSTER:
									if (((SiteInst2)mod).isCluster()){
										modToRemove = mod;
									}
									break;
								case DOMINION:
									if (((SiteInst2)mod).isDominion()){
										modToRemove = mod;
									}
									break;
								case GODDOMCHAOS:
									if (((SiteInst2)mod).isGoddomchaos()){
										modToRemove = mod;
									}
									break;
								case GODDOMLAZY:
									if (((SiteInst2)mod).isGoddomlazy()){
										modToRemove = mod;
									}
									break;
								case GODDOMCOLD:
									if (((SiteInst2)mod).isGoddomcold()){
										modToRemove = mod;
									}
									break;
								case GODDOMDEATH:
									if (((SiteInst2)mod).isGoddomdeath()){
										modToRemove = mod;
									}
									break;
								case GODDOMMISFORTUNE:
									if (((SiteInst2)mod).isGoddommisfortune()){
										modToRemove = mod;
									}
									break;
								case GODDOMDRAIN:
									if (((SiteInst2)mod).isGoddomdrain()){
										modToRemove = mod;
									}
									break;
								case BLESSHP:
									if (((SiteInst2)mod).isBlesshp()){
										modToRemove = mod;
									}
									break;
								case BLESSMR:
									if (((SiteInst2)mod).isBlessmr()){
										modToRemove = mod;
									}
									break;
								case BLESSMOR:
									if (((SiteInst2)mod).isBlessmor()){
										modToRemove = mod;
									}
									break;
								case BLESSSTR:
									if (((SiteInst2)mod).isBlessstr()){
										modToRemove = mod;
									}
									break;
								case BLESSATT:
									if (((SiteInst2)mod).isBlessatt()){
										modToRemove = mod;
									}
									break;
								case BLESSDEF:
									if (((SiteInst2)mod).isBlessdef()){
										modToRemove = mod;
									}
									break;
								case BLESSPREC:
									if (((SiteInst2)mod).isBlessprec()){
										modToRemove = mod;
									}
									break;
								case BLESSFIRERES:
									if (((SiteInst2)mod).isBlessfireres()){
										modToRemove = mod;
									}
									break;
								case BLESSCOLDRES:
									if (((SiteInst2)mod).isBlesscoldres()){
										modToRemove = mod;
									}
									break;
								case BLESSSHOCKRES:
									if (((SiteInst2)mod).isBlessshockres()){
										modToRemove = mod;
									}
									break;
								case BLESSPOISRES:
									if (((SiteInst2)mod).isBlesspoisres()){
										modToRemove = mod;
									}
									break;
								case BLESSAIRSHLD:
									if (((SiteInst2)mod).isBlessairshld()){
										modToRemove = mod;
									}
									break;
								case BLESSREINVIG:
									if (((SiteInst2)mod).isBlessreinvig()){
										modToRemove = mod;
									}
									break;
								case BLESSDTV:
									if (((SiteInst2)mod).isBlessdtv()){
										modToRemove = mod;
									}
									break;
								case BLESSANIMAWE:
									if (((SiteInst2)mod).isBlessanimawe()){
										modToRemove = mod;
									}
									break;
								case BLESSAWE:
									if (((SiteInst2)mod).isBlessawe()){
										modToRemove = mod;
									}
									break;
								case BLESSDARKVIS:
									if (((SiteInst2)mod).isBlessdarkvis()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof SiteInst3) {
								switch (inst) {
								case GEMS1:
									if (((SiteInst3)mod).isGems()){
										gemCount++;
										if (gemCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case GEMS2:
									if (((SiteInst3)mod).isGems()){
										gemCount++;
										if (gemCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case GEMS3:
									if (((SiteInst3)mod).isGems()){
										gemCount++;
										if (gemCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								}
							}
							if (mod instanceof SiteInst4) {
								switch (inst) {
								case CLEAR:
									if (((SiteInst4)mod).isClear()){
										modToRemove = mod;
									}
									break;
								case LAB:
									if (((SiteInst4)mod).isLab()){
										modToRemove = mod;
									}
									break;
								case TEMPLE:
									if (((SiteInst4)mod).isTemple()){
										modToRemove = mod;
									}
									break;
								case CLAIM:
									if (((SiteInst4)mod).isClaim()){
										modToRemove = mod;
									}
									break;
								case EVIL:
									if (((SiteInst4)mod).isEvil()){
										modToRemove = mod;
									}
									break;
								case WILD:
									if (((SiteInst4)mod).isWild()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof SiteInst5) {
								switch (inst) {
								case HOMEMON1:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON2:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON3:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON4:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON5:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON6:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON7:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case HOMEMON8:
									if (((SiteInst5)mod).isHomemon()){
										homeMonCount++;
										if (homeMonCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM1:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM2:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM3:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM4:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM5:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM6:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM7:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case HOMECOM8:
									if (((SiteInst5)mod).isHomecom()){
										homeComCount++;
										if (homeComCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case MON1:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 1) {									
											modToRemove = mod;
										}
									}
									break;
								case MON2:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 2) {									
											modToRemove = mod;
										}
									}
									break;
								case MON3:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 3) {									
											modToRemove = mod;
										}
									}
									break;
								case MON4:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 4) {									
											modToRemove = mod;
										}
									}
									break;
								case MON5:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 5) {									
											modToRemove = mod;
										}
									}
									break;
								case MON6:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 6) {									
											modToRemove = mod;
										}
									}
									break;
								case MON7:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 7) {									
											modToRemove = mod;
										}
									}
									break;
								case MON8:
									if (((SiteInst5)mod).isMon()){
										monCount++;
										if (monCount == 8) {									
											modToRemove = mod;
										}
									}
									break;
								case COM1:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case COM2:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case COM3:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case COM4:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case COM5:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case COM6:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case COM7:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case COM8:
									if (((SiteInst5)mod).isCom()){
										comCount++;
										if (comCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case SUMMON:
									if (((SiteInst5)mod).isSummon()){
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
