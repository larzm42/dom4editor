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
import org.larz.dom4.db.NationDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.Nation;
import org.larz.dom4.dm.dm.NationInst1;
import org.larz.dom4.dm.dm.NationInst2;
import org.larz.dom4.dm.dm.NationInst3;
import org.larz.dom4.dm.dm.NationInst4;
import org.larz.dom4.dm.dm.NationInst5;
import org.larz.dom4.dm.dm.NationMods;
import org.larz.dom4.dm.dm.SelectNation;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class NationDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;
	private Text descr;
	private Button descrCheck;
	private Text summary;
	private Button summaryCheck;
	private Text brief;
	private Button briefCheck;
	private Label spriteLabel;

	enum Inst {
		NAME (Messages.getString("NationDetailsSection.mod.name"), ""),
		DESCR (Messages.getString("NationDetailsSection.mod.descr"), ""),
		SUMMARY (Messages.getString("NationDetailsSection.mod.summary"), ""),
		BRIEF (Messages.getString("NationDetailsSection.mod.brief"), ""),
		CLEARNATION (Messages.getString("NationDetailsSection.mod.clearnation")),
		EPITHET (Messages.getString("NationDetailsSection.mod.epithet"), ""),
		ERA (Messages.getString("NationDetailsSection.mod.era"), "0"),
		COLOR (Messages.getString("NationDetailsSection.mod.color"), "0.0", "0.0", "0.0"),
		SECONDARYCOLOR (Messages.getString("NationDetailsSection.mod.secondarycolor"), "0.0", "0.0", "0.0"),
		FLAG (Messages.getString("NationDetailsSection.mod.flag"), ""),
		LIKESTERR (Messages.getString("NationDetailsSection.mod.likesterr"), ""),
		CLEARREC (Messages.getString("NationDetailsSection.mod.clearrec")),
		STARTCOM (Messages.getString("NationDetailsSection.mod.startcom"), "0"),
		STARTSCOUT (Messages.getString("NationDetailsSection.mod.startscout"), "0"),
		STARTUNITTYPE1 (Messages.getString("NationDetailsSection.mod.startunittype1"), "0"),
		STARTUNITNBRS1 (Messages.getString("NationDetailsSection.mod.startunitnbrs1"), "0"),
		STARTUNITTYPE2 (Messages.getString("NationDetailsSection.mod.startunittype2"), "0"),
		STARTUNITNBRS2 (Messages.getString("NationDetailsSection.mod.startunitnbrs2"), "0"),
		ADDRECUNIT1 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM1 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT2 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM2 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT3 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM3 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT4 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM4 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT5 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM5 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT6 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM6 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT7 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM7 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT8 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM8 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT9 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM9 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT10 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM10 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT11 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM11 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT12 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM12 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT13 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM13 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		ADDRECUNIT14 (Messages.getString("NationDetailsSection.mod.addrecunit"), "0"),
		ADDRECCOM14 (Messages.getString("NationDetailsSection.mod.addreccom"), "0"),
		UWUNIT1 (Messages.getString("NationDetailsSection.mod.uwunit1"), "0"),
		UWUNIT2 (Messages.getString("NationDetailsSection.mod.uwunit2"), "0"),
		UWUNIT3 (Messages.getString("NationDetailsSection.mod.uwunit3"), "0"),
		UWUNIT4 (Messages.getString("NationDetailsSection.mod.uwunit4"), "0"),
		UWUNIT5 (Messages.getString("NationDetailsSection.mod.uwunit5"), "0"),
		UWCOM1 (Messages.getString("NationDetailsSection.mod.uwcom1"), "0"),
		UWCOM2 (Messages.getString("NationDetailsSection.mod.uwcom2"), "0"),
		UWCOM3 (Messages.getString("NationDetailsSection.mod.uwcom3"), "0"),
		UWCOM4 (Messages.getString("NationDetailsSection.mod.uwcom4"), "0"),
		UWCOM5 (Messages.getString("NationDetailsSection.mod.uwcom5"), "0"),
		ADDFOREIGNUNIT1 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM1 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT2 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM2 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT3 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM3 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT4 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM4 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT5 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM5 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT6 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM6 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT7 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM7 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT8 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM8 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT9 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM9 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT10 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM10 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT11 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM11 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT12 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM12 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT13 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM13 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		ADDFOREIGNUNIT14 (Messages.getString("NationDetailsSection.mod.addforeignunit"), "0"),
		ADDFOREIGNCOM14 (Messages.getString("NationDetailsSection.mod.addforeigncom"), "0"),
		FORESTREC1 (Messages.getString("NationDetailsSection.mod.forestrec"), "0"),
		FORESTCOM1 (Messages.getString("NationDetailsSection.mod.forestcom"), "0"),
		FORESTREC2 (Messages.getString("NationDetailsSection.mod.forestrec"), "0"),
		FORESTCOM2 (Messages.getString("NationDetailsSection.mod.forestcom"), "0"),
		FORESTREC3 (Messages.getString("NationDetailsSection.mod.forestrec"), "0"),
		FORESTCOM3 (Messages.getString("NationDetailsSection.mod.forestcom"), "0"),
		FORESTREC4 (Messages.getString("NationDetailsSection.mod.forestrec"), "0"),
		FORESTCOM4 (Messages.getString("NationDetailsSection.mod.forestcom"), "0"),
		FORESTREC5 (Messages.getString("NationDetailsSection.mod.forestrec"), "0"),
		FORESTCOM5 (Messages.getString("NationDetailsSection.mod.forestcom"), "0"),
		MOUNTAINREC1 (Messages.getString("NationDetailsSection.mod.mountainrec"), "0"),
		MOUNTAINCOM1 (Messages.getString("NationDetailsSection.mod.mountaincom"), "0"),
		MOUNTAINREC2 (Messages.getString("NationDetailsSection.mod.mountainrec"), "0"),
		MOUNTAINCOM2 (Messages.getString("NationDetailsSection.mod.mountaincom"), "0"),
		MOUNTAINREC3 (Messages.getString("NationDetailsSection.mod.mountainrec"), "0"),
		MOUNTAINCOM3 (Messages.getString("NationDetailsSection.mod.mountaincom"), "0"),
		MOUNTAINREC4 (Messages.getString("NationDetailsSection.mod.mountainrec"), "0"),
		MOUNTAINCOM4 (Messages.getString("NationDetailsSection.mod.mountaincom"), "0"),
		MOUNTAINREC5 (Messages.getString("NationDetailsSection.mod.mountainrec"), "0"),
		MOUNTAINCOM5 (Messages.getString("NationDetailsSection.mod.mountaincom"), "0"),
		SWAMPREC1 (Messages.getString("NationDetailsSection.mod.swamprec"), "0"),
		SWAMPCOM1 (Messages.getString("NationDetailsSection.mod.swampcom"), "0"),
		SWAMPREC2 (Messages.getString("NationDetailsSection.mod.swamprec"), "0"),
		SWAMPCOM2 (Messages.getString("NationDetailsSection.mod.swampcom"), "0"),
		SWAMPREC3 (Messages.getString("NationDetailsSection.mod.swamprec"), "0"),
		SWAMPCOM3 (Messages.getString("NationDetailsSection.mod.swampcom"), "0"),
		SWAMPREC4 (Messages.getString("NationDetailsSection.mod.swamprec"), "0"),
		SWAMPCOM4 (Messages.getString("NationDetailsSection.mod.swampcom"), "0"),
		SWAMPREC5 (Messages.getString("NationDetailsSection.mod.swamprec"), "0"),
		SWAMPCOM5 (Messages.getString("NationDetailsSection.mod.swampcom"), "0"),
		WASTEREC1 (Messages.getString("NationDetailsSection.mod.wasterec"), "0"),
		WASTECOM1 (Messages.getString("NationDetailsSection.mod.wastecom"), "0"),
		WASTEREC2 (Messages.getString("NationDetailsSection.mod.wasterec"), "0"),
		WASTECOM2 (Messages.getString("NationDetailsSection.mod.wastecom"), "0"),
		WASTEREC3 (Messages.getString("NationDetailsSection.mod.wasterec"), "0"),
		WASTECOM3 (Messages.getString("NationDetailsSection.mod.wastecom"), "0"),
		WASTEREC4 (Messages.getString("NationDetailsSection.mod.wasterec"), "0"),
		WASTECOM4 (Messages.getString("NationDetailsSection.mod.wastecom"), "0"),
		WASTEREC5 (Messages.getString("NationDetailsSection.mod.wasterec"), "0"),
		WASTECOM5 (Messages.getString("NationDetailsSection.mod.wastecom"), "0"),
		CAVEREC1 (Messages.getString("NationDetailsSection.mod.caverec"), "0"),
		CAVECOM1 (Messages.getString("NationDetailsSection.mod.cavecom"), "0"),
		CAVEREC2 (Messages.getString("NationDetailsSection.mod.caverec"), "0"),
		CAVECOM2 (Messages.getString("NationDetailsSection.mod.cavecom"), "0"),
		CAVEREC3 (Messages.getString("NationDetailsSection.mod.caverec"), "0"),
		CAVECOM3 (Messages.getString("NationDetailsSection.mod.cavecom"), "0"),
		CAVEREC4 (Messages.getString("NationDetailsSection.mod.caverec"), "0"),
		CAVECOM4 (Messages.getString("NationDetailsSection.mod.cavecom"), "0"),
		CAVEREC5 (Messages.getString("NationDetailsSection.mod.caverec"), "0"),
		CAVECOM5 (Messages.getString("NationDetailsSection.mod.cavecom"), "0"),
		HERO1 (Messages.getString("NationDetailsSection.mod.hero1"), "0"),
		HERO2 (Messages.getString("NationDetailsSection.mod.hero2"), "0"),
		HERO3 (Messages.getString("NationDetailsSection.mod.hero3"), "0"),
		HERO4 (Messages.getString("NationDetailsSection.mod.hero4"), "0"),
		HERO5 (Messages.getString("NationDetailsSection.mod.hero5"), "0"),
		HERO6 (Messages.getString("NationDetailsSection.mod.hero6"), "0"),
		MULTIHERO1 (Messages.getString("NationDetailsSection.mod.multihero1"), "0"),
		MULTIHERO2 (Messages.getString("NationDetailsSection.mod.multihero2"), "0"),
		MERCCOST (Messages.getString("NationDetailsSection.mod.merccost"), "0"),
		CLEARGODS (Messages.getString("NationDetailsSection.mod.cleargods")),
		HOMEREALM (Messages.getString("NationDetailsSection.mod.homerealm"), "10"),
		ADDGOD1 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD1 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD2 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD2 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD3 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD3 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD4 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD4 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD5 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD5 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD6 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD6 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD7 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD7 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD8 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD8 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD9 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD9 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD10 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD10 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD11 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD11 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD12 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD12 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD13 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD13 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		ADDGOD14 (Messages.getString("NationDetailsSection.mod.addgod"), "0"),
		DELGOD14 (Messages.getString("NationDetailsSection.mod.delgod"), "0"),
		DEFCOM1 (Messages.getString("NationDetailsSection.mod.defcom1"), "0"),
		DEFCOM2 (Messages.getString("NationDetailsSection.mod.defcom2"), "0"),
		DEFUNIT1 (Messages.getString("NationDetailsSection.mod.defunit1"), "0"),
		DEFUNIT1B (Messages.getString("NationDetailsSection.mod.defunit1b"), "0"),
		DEFUNIT2 (Messages.getString("NationDetailsSection.mod.defunit2"), "0"),
		DEFUNIT2B (Messages.getString("NationDetailsSection.mod.defunit2b"), "0"),
		DEFMULT1 (Messages.getString("NationDetailsSection.mod.defmult1"), "0"),
		DEFMULT1B (Messages.getString("NationDetailsSection.mod.defmult1b"), "0"),
		DEFMULT2 (Messages.getString("NationDetailsSection.mod.defmult2"), "0"),
		DEFMULT2B (Messages.getString("NationDetailsSection.mod.defmult2b"), "0"),
		LIKESPOP (Messages.getString("NationDetailsSection.mod.likespop"), "0"),
		FORTERA (Messages.getString("NationDetailsSection.mod.fortera"), "0"),
		FORTCOST (Messages.getString("NationDetailsSection.mod.fortcost"), "0"),
		LABCOST (Messages.getString("NationDetailsSection.mod.labcost"), "0"),
		TEMPLECOST (Messages.getString("NationDetailsSection.mod.templecost"), "0"),
		TEMPLEPIC (Messages.getString("NationDetailsSection.mod.templepic"), "0"),
		CLEARSITES (Messages.getString("NationDetailsSection.mod.clearsites")),
		STARTSITE1 (Messages.getString("NationDetailsSection.mod.startsite"), "0"),
		STARTSITE2 (Messages.getString("NationDetailsSection.mod.startsite"), "0"),
		STARTSITE3 (Messages.getString("NationDetailsSection.mod.startsite"), "0"),
		STARTSITE4 (Messages.getString("NationDetailsSection.mod.startsite"), "0"),
		UWNATION (Messages.getString("NationDetailsSection.mod.uwnation")),
		BLOODNATION (Messages.getString("NationDetailsSection.mod.bloodnation")),
		NOPREACH (Messages.getString("NationDetailsSection.mod.nopreach")),
		DYINGDOM (Messages.getString("NationDetailsSection.mod.dyingdom")),
		SACRIFICEDOM (Messages.getString("NationDetailsSection.mod.sacrificedom")),
		SPREADCOLD (Messages.getString("NationDetailsSection.mod.spreadcold"), "0"),
		SPREADHEAT (Messages.getString("NationDetailsSection.mod.spreadheat"), "0"),
		SPREADCHAOS (Messages.getString("NationDetailsSection.mod.spreadchaos"), "0"),
		SPREADLAZY (Messages.getString("NationDetailsSection.mod.spreadlazy"), "0"),
		SPREADDEATH (Messages.getString("NationDetailsSection.mod.spreaddeath"), "0"),
		GOLEMHP (Messages.getString("NationDetailsSection.mod.golemhp"), "0"),
		NODEATHSUPPLY (Messages.getString("NationDetailsSection.mod.nodeathsupply")),
		IDEALCOLD (Messages.getString("NationDetailsSection.mod.idealcold"), "0"),
		CASTLEPROD (Messages.getString("NationDetailsSection.mod.castleprod"), "0"),
		DOMKILL (Messages.getString("NationDetailsSection.mod.domkill"), "0"),
		DOMUNREST (Messages.getString("NationDetailsSection.mod.domunrest"), "0"),
		AUTOUNDEAD (Messages.getString("NationDetailsSection.mod.autoundead")),
		ZOMBIEREANIM (Messages.getString("NationDetailsSection.mod.zombiereanim")),
		HORSEREANIM (Messages.getString("NationDetailsSection.mod.horsereanim")),
		WIGHTREANIM (Messages.getString("NationDetailsSection.mod.wightreanim")),
		MANIKINREANIM (Messages.getString("NationDetailsSection.mod.manikinreanim")),
		TOMBWYRMREANIM (Messages.getString("NationDetailsSection.mod.tombwyrmreanim"));

		private String label;
		private String defaultValue;
		private String defaultValue2;
		private String defaultValue3;
		
		Inst(String label, String defaultValue) {
			this.label = label;
			this.defaultValue = defaultValue;
		}
		
		Inst(String label, String defaultValue, String defaultValue2) {
			this.label = label;
			this.defaultValue = defaultValue;
			this.defaultValue2 = defaultValue2;
		}
		
		Inst(String label, String defaultValue, String defaultValue2, String defaultValue3) {
			this.label = label;
			this.defaultValue = defaultValue;
			this.defaultValue2 = defaultValue2;
			this.defaultValue3 = defaultValue3;
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
		private Label defaultLabel;
	}

	class Inst4Fields implements InstFields {
		private Button check;
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst5Fields implements InstFields {
		private Button check;
		private Text value1;
		private Text value2;
		private Text value3;
	}

	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	private Set<List<Inst>> dynamicFields = new HashSet<List<Inst>>();

	public NationDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.EPITHET, new Inst1Fields());
		instMap.put(Inst.FLAG, new Inst1Fields());
		instMap.put(Inst.LIKESTERR, new Inst2Fields());
		instMap.put(Inst.STARTSITE1, new Inst1Fields());
		instMap.put(Inst.STARTSITE2, new Inst1Fields());
		instMap.put(Inst.STARTSITE3, new Inst1Fields());
		instMap.put(Inst.STARTSITE4, new Inst1Fields());
		instMap.put(Inst.ERA, new Inst2Fields());
		instMap.put(Inst.LABCOST, new Inst2Fields());
		instMap.put(Inst.TEMPLECOST, new Inst2Fields());
		instMap.put(Inst.TEMPLEPIC, new Inst2Fields());
		instMap.put(Inst.STARTUNITNBRS1, new Inst2Fields());
		instMap.put(Inst.STARTUNITNBRS2, new Inst2Fields());
		instMap.put(Inst.HERO1, new Inst2Fields());
		instMap.put(Inst.HERO2, new Inst2Fields());
		instMap.put(Inst.HERO3, new Inst2Fields());
		instMap.put(Inst.HERO4, new Inst2Fields());
		instMap.put(Inst.HERO5, new Inst2Fields());
		instMap.put(Inst.HERO6, new Inst2Fields());
		instMap.put(Inst.MULTIHERO1, new Inst2Fields());
		instMap.put(Inst.MULTIHERO2, new Inst2Fields());
		instMap.put(Inst.DEFMULT1, new Inst2Fields());
		instMap.put(Inst.DEFMULT1B, new Inst2Fields());
		instMap.put(Inst.DEFMULT2, new Inst2Fields());
		instMap.put(Inst.DEFMULT2B, new Inst2Fields());
		instMap.put(Inst.IDEALCOLD, new Inst2Fields());
		instMap.put(Inst.CASTLEPROD, new Inst2Fields());
		instMap.put(Inst.DOMKILL, new Inst2Fields());
		instMap.put(Inst.DOMUNREST, new Inst2Fields());
		instMap.put(Inst.CLEARNATION, new Inst3Fields());
		instMap.put(Inst.CLEARREC, new Inst3Fields());
		instMap.put(Inst.CLEARSITES, new Inst3Fields());
		instMap.put(Inst.UWNATION, new Inst3Fields());
		instMap.put(Inst.BLOODNATION, new Inst3Fields());
		instMap.put(Inst.NOPREACH, new Inst3Fields());
		instMap.put(Inst.DYINGDOM, new Inst3Fields());
		instMap.put(Inst.SACRIFICEDOM, new Inst3Fields());
		instMap.put(Inst.NODEATHSUPPLY, new Inst3Fields());
		instMap.put(Inst.AUTOUNDEAD, new Inst3Fields());
		instMap.put(Inst.ZOMBIEREANIM, new Inst3Fields());
		instMap.put(Inst.HORSEREANIM, new Inst3Fields());
		instMap.put(Inst.WIGHTREANIM, new Inst3Fields());
		instMap.put(Inst.MANIKINREANIM, new Inst3Fields());
		instMap.put(Inst.TOMBWYRMREANIM, new Inst3Fields());
		instMap.put(Inst.STARTCOM, new Inst4Fields());
		instMap.put(Inst.STARTSCOUT, new Inst4Fields());
		instMap.put(Inst.STARTUNITTYPE1, new Inst4Fields());
		instMap.put(Inst.STARTUNITTYPE2, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT1, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT2, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT3, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT4, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT5, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT6, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT7, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT8, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT9, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT10, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT11, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT12, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT13, new Inst4Fields());
		instMap.put(Inst.ADDRECUNIT14, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM1, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM2, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM3, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM4, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM5, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM6, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM7, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM8, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM9, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM10, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM11, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM12, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM13, new Inst4Fields());
		instMap.put(Inst.ADDRECCOM14, new Inst4Fields());
		instMap.put(Inst.UWUNIT1, new Inst4Fields());
		instMap.put(Inst.UWCOM1, new Inst4Fields());
		instMap.put(Inst.UWUNIT2, new Inst4Fields());
		instMap.put(Inst.UWCOM2, new Inst4Fields());
		instMap.put(Inst.UWUNIT3, new Inst4Fields());
		instMap.put(Inst.UWCOM3, new Inst4Fields());
		instMap.put(Inst.UWUNIT4, new Inst4Fields());
		instMap.put(Inst.UWCOM4, new Inst4Fields());
		instMap.put(Inst.UWUNIT5, new Inst4Fields());
		instMap.put(Inst.UWCOM5, new Inst4Fields());
		instMap.put(Inst.DEFCOM1, new Inst4Fields());
		instMap.put(Inst.DEFCOM2, new Inst4Fields());
		instMap.put(Inst.DEFUNIT1, new Inst4Fields());
		instMap.put(Inst.DEFUNIT1B, new Inst4Fields());
		instMap.put(Inst.DEFUNIT2, new Inst4Fields());
		instMap.put(Inst.DEFUNIT2B, new Inst4Fields());
		instMap.put(Inst.COLOR, new Inst5Fields());
		instMap.put(Inst.SECONDARYCOLOR, new Inst5Fields());
		instMap.put(Inst.ADDFOREIGNUNIT1, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT2, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT3, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT4, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT5, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT6, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT7, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT8, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT9, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT10, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT11, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT12, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT13, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNUNIT14, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM1, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM2, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM3, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM4, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM5, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM6, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM7, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM8, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM9, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM10, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM11, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM12, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM13, new Inst4Fields());
		instMap.put(Inst.ADDFOREIGNCOM14, new Inst4Fields());
		instMap.put(Inst.MERCCOST, new Inst2Fields());
		instMap.put(Inst.FORESTREC1, new Inst4Fields());
		instMap.put(Inst.FORESTREC2, new Inst4Fields());
		instMap.put(Inst.FORESTREC3, new Inst4Fields());
		instMap.put(Inst.FORESTREC4, new Inst4Fields());
		instMap.put(Inst.FORESTREC5, new Inst4Fields());
		instMap.put(Inst.FORESTCOM1, new Inst4Fields());
		instMap.put(Inst.FORESTCOM2, new Inst4Fields());
		instMap.put(Inst.FORESTCOM3, new Inst4Fields());
		instMap.put(Inst.FORESTCOM4, new Inst4Fields());
		instMap.put(Inst.FORESTCOM5, new Inst4Fields());
		instMap.put(Inst.MOUNTAINREC1, new Inst4Fields());
		instMap.put(Inst.MOUNTAINREC2, new Inst4Fields());
		instMap.put(Inst.MOUNTAINREC3, new Inst4Fields());
		instMap.put(Inst.MOUNTAINREC4, new Inst4Fields());
		instMap.put(Inst.MOUNTAINREC5, new Inst4Fields());
		instMap.put(Inst.MOUNTAINCOM1, new Inst4Fields());
		instMap.put(Inst.MOUNTAINCOM2, new Inst4Fields());
		instMap.put(Inst.MOUNTAINCOM3, new Inst4Fields());
		instMap.put(Inst.MOUNTAINCOM4, new Inst4Fields());
		instMap.put(Inst.MOUNTAINCOM5, new Inst4Fields());
		instMap.put(Inst.SWAMPREC1, new Inst4Fields());
		instMap.put(Inst.SWAMPREC2, new Inst4Fields());
		instMap.put(Inst.SWAMPREC3, new Inst4Fields());
		instMap.put(Inst.SWAMPREC4, new Inst4Fields());
		instMap.put(Inst.SWAMPREC5, new Inst4Fields());
		instMap.put(Inst.SWAMPCOM1, new Inst4Fields());
		instMap.put(Inst.SWAMPCOM2, new Inst4Fields());
		instMap.put(Inst.SWAMPCOM3, new Inst4Fields());
		instMap.put(Inst.SWAMPCOM4, new Inst4Fields());
		instMap.put(Inst.SWAMPCOM5, new Inst4Fields());
		instMap.put(Inst.WASTEREC1, new Inst4Fields());
		instMap.put(Inst.WASTEREC2, new Inst4Fields());
		instMap.put(Inst.WASTEREC3, new Inst4Fields());
		instMap.put(Inst.WASTEREC4, new Inst4Fields());
		instMap.put(Inst.WASTEREC5, new Inst4Fields());
		instMap.put(Inst.WASTECOM1, new Inst4Fields());
		instMap.put(Inst.WASTECOM2, new Inst4Fields());
		instMap.put(Inst.WASTECOM3, new Inst4Fields());
		instMap.put(Inst.WASTECOM4, new Inst4Fields());
		instMap.put(Inst.WASTECOM5, new Inst4Fields());
		instMap.put(Inst.CAVEREC1, new Inst4Fields());
		instMap.put(Inst.CAVEREC2, new Inst4Fields());
		instMap.put(Inst.CAVEREC3, new Inst4Fields());
		instMap.put(Inst.CAVEREC4, new Inst4Fields());
		instMap.put(Inst.CAVEREC5, new Inst4Fields());
		instMap.put(Inst.CAVECOM1, new Inst4Fields());
		instMap.put(Inst.CAVECOM2, new Inst4Fields());
		instMap.put(Inst.CAVECOM3, new Inst4Fields());
		instMap.put(Inst.CAVECOM4, new Inst4Fields());
		instMap.put(Inst.CAVECOM5, new Inst4Fields());
		instMap.put(Inst.CLEARGODS, new Inst3Fields());
		instMap.put(Inst.ADDGOD1, new Inst4Fields());
		instMap.put(Inst.ADDGOD2, new Inst4Fields());
		instMap.put(Inst.ADDGOD3, new Inst4Fields());
		instMap.put(Inst.ADDGOD4, new Inst4Fields());
		instMap.put(Inst.ADDGOD5, new Inst4Fields());
		instMap.put(Inst.ADDGOD6, new Inst4Fields());
		instMap.put(Inst.ADDGOD7, new Inst4Fields());
		instMap.put(Inst.ADDGOD8, new Inst4Fields());
		instMap.put(Inst.ADDGOD9, new Inst4Fields());
		instMap.put(Inst.ADDGOD10, new Inst4Fields());
		instMap.put(Inst.ADDGOD11, new Inst4Fields());
		instMap.put(Inst.ADDGOD12, new Inst4Fields());
		instMap.put(Inst.ADDGOD13, new Inst4Fields());
		instMap.put(Inst.ADDGOD14, new Inst4Fields());
		instMap.put(Inst.HOMEREALM, new Inst2Fields());
		instMap.put(Inst.DELGOD1, new Inst4Fields());
		instMap.put(Inst.DELGOD2, new Inst4Fields());
		instMap.put(Inst.DELGOD3, new Inst4Fields());
		instMap.put(Inst.DELGOD4, new Inst4Fields());
		instMap.put(Inst.DELGOD5, new Inst4Fields());
		instMap.put(Inst.DELGOD6, new Inst4Fields());
		instMap.put(Inst.DELGOD7, new Inst4Fields());
		instMap.put(Inst.DELGOD8, new Inst4Fields());
		instMap.put(Inst.DELGOD9, new Inst4Fields());
		instMap.put(Inst.DELGOD10, new Inst4Fields());
		instMap.put(Inst.DELGOD11, new Inst4Fields());
		instMap.put(Inst.DELGOD12, new Inst4Fields());
		instMap.put(Inst.DELGOD13, new Inst4Fields());
		instMap.put(Inst.DELGOD14, new Inst4Fields());
		instMap.put(Inst.LIKESPOP, new Inst2Fields());
		instMap.put(Inst.FORTERA, new Inst2Fields());
		instMap.put(Inst.FORTCOST, new Inst2Fields());
		instMap.put(Inst.SPREADCOLD, new Inst2Fields());
		instMap.put(Inst.SPREADHEAT, new Inst2Fields());
		instMap.put(Inst.SPREADCHAOS, new Inst2Fields());
		instMap.put(Inst.SPREADLAZY, new Inst2Fields());
		instMap.put(Inst.SPREADDEATH, new Inst2Fields());
		instMap.put(Inst.GOLEMHP, new Inst2Fields());

		List<Inst> recUnitList = new ArrayList<Inst>();
		recUnitList.add(Inst.ADDRECUNIT1);
		recUnitList.add(Inst.ADDRECUNIT2);
		recUnitList.add(Inst.ADDRECUNIT3);
		recUnitList.add(Inst.ADDRECUNIT4);
		recUnitList.add(Inst.ADDRECUNIT5);
		recUnitList.add(Inst.ADDRECUNIT6);
		recUnitList.add(Inst.ADDRECUNIT7);
		recUnitList.add(Inst.ADDRECUNIT8);
		recUnitList.add(Inst.ADDRECUNIT9);
		recUnitList.add(Inst.ADDRECUNIT10);
		recUnitList.add(Inst.ADDRECUNIT11);
		recUnitList.add(Inst.ADDRECUNIT12);
		recUnitList.add(Inst.ADDRECUNIT13);
		recUnitList.add(Inst.ADDRECUNIT14);
		dynamicFields.add(recUnitList);

		List<Inst> recComList = new ArrayList<Inst>();
		recComList.add(Inst.ADDRECCOM1);
		recComList.add(Inst.ADDRECCOM2);
		recComList.add(Inst.ADDRECCOM3);
		recComList.add(Inst.ADDRECCOM4);
		recComList.add(Inst.ADDRECCOM5);
		recComList.add(Inst.ADDRECCOM6);
		recComList.add(Inst.ADDRECCOM7);
		recComList.add(Inst.ADDRECCOM8);
		recComList.add(Inst.ADDRECCOM9);
		recComList.add(Inst.ADDRECCOM10);
		recComList.add(Inst.ADDRECCOM11);
		recComList.add(Inst.ADDRECCOM12);
		recComList.add(Inst.ADDRECCOM13);
		recComList.add(Inst.ADDRECCOM14);
		dynamicFields.add(recComList);

		List<Inst> forUnitList = new ArrayList<Inst>();
		forUnitList.add(Inst.ADDFOREIGNUNIT1);
		forUnitList.add(Inst.ADDFOREIGNUNIT2);
		forUnitList.add(Inst.ADDFOREIGNUNIT3);
		forUnitList.add(Inst.ADDFOREIGNUNIT4);
		forUnitList.add(Inst.ADDFOREIGNUNIT5);
		forUnitList.add(Inst.ADDFOREIGNUNIT6);
		forUnitList.add(Inst.ADDFOREIGNUNIT7);
		forUnitList.add(Inst.ADDFOREIGNUNIT8);
		forUnitList.add(Inst.ADDFOREIGNUNIT9);
		forUnitList.add(Inst.ADDFOREIGNUNIT10);
		forUnitList.add(Inst.ADDFOREIGNUNIT11);
		forUnitList.add(Inst.ADDFOREIGNUNIT12);
		forUnitList.add(Inst.ADDFOREIGNUNIT13);
		forUnitList.add(Inst.ADDFOREIGNUNIT14);
		dynamicFields.add(forUnitList);
		
		List<Inst> forComList = new ArrayList<Inst>();
		forComList.add(Inst.ADDFOREIGNCOM1);
		forComList.add(Inst.ADDFOREIGNCOM2);
		forComList.add(Inst.ADDFOREIGNCOM3);
		forComList.add(Inst.ADDFOREIGNCOM4);
		forComList.add(Inst.ADDFOREIGNCOM5);
		forComList.add(Inst.ADDFOREIGNCOM6);
		forComList.add(Inst.ADDFOREIGNCOM7);
		forComList.add(Inst.ADDFOREIGNCOM8);
		forComList.add(Inst.ADDFOREIGNCOM9);
		forComList.add(Inst.ADDFOREIGNCOM10);
		forComList.add(Inst.ADDFOREIGNCOM11);
		forComList.add(Inst.ADDFOREIGNCOM12);
		forComList.add(Inst.ADDFOREIGNCOM13);
		forComList.add(Inst.ADDFOREIGNCOM14);
		dynamicFields.add(forComList);
		
		List<Inst> forestRecList = new ArrayList<Inst>();
		forestRecList.add(Inst.FORESTREC1);
		forestRecList.add(Inst.FORESTREC2);
		forestRecList.add(Inst.FORESTREC3);
		forestRecList.add(Inst.FORESTREC4);
		forestRecList.add(Inst.FORESTREC5);
		dynamicFields.add(forestRecList);
		
		List<Inst> forestComList = new ArrayList<Inst>();
		forestComList.add(Inst.FORESTCOM1);
		forestComList.add(Inst.FORESTCOM2);
		forestComList.add(Inst.FORESTCOM3);
		forestComList.add(Inst.FORESTCOM4);
		forestComList.add(Inst.FORESTCOM5);
		dynamicFields.add(forestComList);
		
		List<Inst> mountRecList = new ArrayList<Inst>();
		mountRecList.add(Inst.MOUNTAINREC1);
		mountRecList.add(Inst.MOUNTAINREC2);
		mountRecList.add(Inst.MOUNTAINREC3);
		mountRecList.add(Inst.MOUNTAINREC4);
		mountRecList.add(Inst.MOUNTAINREC5);
		dynamicFields.add(mountRecList);

		List<Inst> mountComList = new ArrayList<Inst>();
		mountComList.add(Inst.MOUNTAINCOM1);
		mountComList.add(Inst.MOUNTAINCOM2);
		mountComList.add(Inst.MOUNTAINCOM3);
		mountComList.add(Inst.MOUNTAINCOM4);
		mountComList.add(Inst.MOUNTAINCOM5);
		dynamicFields.add(mountComList);

		List<Inst> swampRecList = new ArrayList<Inst>();
		swampRecList.add(Inst.SWAMPREC1);
		swampRecList.add(Inst.SWAMPREC2);
		swampRecList.add(Inst.SWAMPREC3);
		swampRecList.add(Inst.SWAMPREC4);
		swampRecList.add(Inst.SWAMPREC5);
		dynamicFields.add(swampRecList);

		List<Inst> swampComList = new ArrayList<Inst>();
		swampComList.add(Inst.SWAMPCOM1);
		swampComList.add(Inst.SWAMPCOM2);
		swampComList.add(Inst.SWAMPCOM3);
		swampComList.add(Inst.SWAMPCOM4);
		swampComList.add(Inst.SWAMPCOM5);
		dynamicFields.add(swampComList);

		List<Inst> wasteRecList = new ArrayList<Inst>();
		wasteRecList.add(Inst.WASTEREC1);
		wasteRecList.add(Inst.WASTEREC2);
		wasteRecList.add(Inst.WASTEREC3);
		wasteRecList.add(Inst.WASTEREC4);
		wasteRecList.add(Inst.WASTEREC5);
		dynamicFields.add(wasteRecList);

		List<Inst> wasteComList = new ArrayList<Inst>();
		wasteComList.add(Inst.WASTECOM1);
		wasteComList.add(Inst.WASTECOM2);
		wasteComList.add(Inst.WASTECOM3);
		wasteComList.add(Inst.WASTECOM4);
		wasteComList.add(Inst.WASTECOM5);
		dynamicFields.add(wasteComList);

		List<Inst> caveRecList = new ArrayList<Inst>();
		caveRecList.add(Inst.CAVEREC1);
		caveRecList.add(Inst.CAVEREC2);
		caveRecList.add(Inst.CAVEREC3);
		caveRecList.add(Inst.CAVEREC4);
		caveRecList.add(Inst.CAVEREC5);
		dynamicFields.add(caveRecList);

		List<Inst> caveComList = new ArrayList<Inst>();
		caveComList.add(Inst.CAVECOM1);
		caveComList.add(Inst.CAVECOM2);
		caveComList.add(Inst.CAVECOM3);
		caveComList.add(Inst.CAVECOM4);
		caveComList.add(Inst.CAVECOM5);
		dynamicFields.add(caveComList);

		List<Inst> addGodList = new ArrayList<Inst>();
		addGodList.add(Inst.ADDGOD1);
		addGodList.add(Inst.ADDGOD2);
		addGodList.add(Inst.ADDGOD3);
		addGodList.add(Inst.ADDGOD4);
		addGodList.add(Inst.ADDGOD5);
		addGodList.add(Inst.ADDGOD6);
		addGodList.add(Inst.ADDGOD7);
		addGodList.add(Inst.ADDGOD8);
		addGodList.add(Inst.ADDGOD9);
		addGodList.add(Inst.ADDGOD10);
		addGodList.add(Inst.ADDGOD11);
		addGodList.add(Inst.ADDGOD12);
		addGodList.add(Inst.ADDGOD13);
		addGodList.add(Inst.ADDGOD14);
		dynamicFields.add(addGodList);

		List<Inst> delGodList = new ArrayList<Inst>();
		delGodList.add(Inst.DELGOD1);
		delGodList.add(Inst.DELGOD2);
		delGodList.add(Inst.DELGOD3);
		delGodList.add(Inst.DELGOD4);
		delGodList.add(Inst.DELGOD5);
		delGodList.add(Inst.DELGOD6);
		delGodList.add(Inst.DELGOD7);
		delGodList.add(Inst.DELGOD8);
		delGodList.add(Inst.DELGOD9);
		delGodList.add(Inst.DELGOD10);
		delGodList.add(Inst.DELGOD11);
		delGodList.add(Inst.DELGOD12);
		delGodList.add(Inst.DELGOD13);
		delGodList.add(Inst.DELGOD14);
		dynamicFields.add(delGodList);

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
		s1.setText(Messages.getString("NationDetailsSection.name")); //$NON-NLS-1$
		TableWrapData td = new TableWrapData(TableWrapData.FILL, TableWrapData.TOP);
		td.grabHorizontal = true;
		s1.setLayoutData(td);
		
		final Composite client = toolkit.createComposite(parent);
		GridLayout glayout = new GridLayout();
		glayout.marginWidth = glayout.marginHeight = 0;
		glayout.numColumns = 2;
		glayout.makeColumnsEqualWidth = true;
		client.setLayout(glayout);
		
		Composite nameComp = toolkit.createComposite(client);
		glayout = new GridLayout(2, false);
		glayout.marginHeight = 0;
		glayout.marginWidth = 0;
		nameComp.setLayout(glayout);
		GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, false);
		gd.horizontalSpan = 2;
		nameComp.setLayoutData(gd);
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("NationDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.NATION_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setNationname(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setNationname(doc, name.getText());
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
					name.setText(Database.getNationName(((SelectNation)input).getValue()));
					nameCheck.setFont(normalFont);
				}
			}
		});

		descrCheck = toolkit.createButton(nameComp, Messages.getString("NationDetailsSection.mod.descr"), SWT.CHECK);
		descrCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.NATION_CATEGORY, "descr"));

		descr = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		descr.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setNationdescr(doc, descr.getText());
			}			
		});
		descr.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setNationdescr(doc, descr.getText());
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
		descrCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (descrCheck.getSelection()) {
					addInst1(Inst.DESCR, doc, "");
					descr.setEnabled(true);
					descr.setBackground(toolkit.getColors().getBackground());
					descr.setText("");
					descrCheck.setFont(boldFont);
				} else {
					removeInst(Inst.DESCR, doc);
					descr.setEnabled(false);
					descr.setBackground(toolkit.getColors().getInactiveBackground());
					descr.setText("");
					descrCheck.setFont(normalFont);
				}
			}
		});

		summaryCheck = toolkit.createButton(nameComp, Messages.getString("NationDetailsSection.mod.summary"), SWT.CHECK);
		summaryCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.NATION_CATEGORY, "summary"));

		summary = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		summary.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setInst1(Inst.SUMMARY, doc, summary.getText());
			}			
		});
		summary.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setInst1(Inst.SUMMARY, doc, summary.getText());
				}
			}
			
		});
		summary.setLayoutData(new GridData(500, SWT.DEFAULT));
		summary.addListener(SWT.Modify, new Listener() {
			
			@Override
			public void handleEvent(Event event) {
				int currentHeight = summary.getSize().y;
				int preferredHeight = summary.computeSize(500, SWT.DEFAULT).y;
				if (currentHeight != preferredHeight) {
					GridData data = (GridData)summary.getLayoutData();
					data.heightHint = preferredHeight;
					client.pack();
				}
			}
		});
		summary.setEnabled(false);
		summary.setBackground(toolkit.getColors().getInactiveBackground());
		summaryCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (summaryCheck.getSelection()) {
					addInst1(Inst.SUMMARY, doc, "");
					summary.setEnabled(true);
					summary.setBackground(toolkit.getColors().getBackground());
					summary.setText("");
					summaryCheck.setFont(boldFont);
				} else {
					removeInst(Inst.SUMMARY, doc);
					summary.setEnabled(false);
					summary.setBackground(toolkit.getColors().getInactiveBackground());
					summary.setText("");
					summaryCheck.setFont(normalFont);
				}
			}
		});

		briefCheck = toolkit.createButton(nameComp, Messages.getString("NationDetailsSection.mod.brief"), SWT.CHECK);
		briefCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.NATION_CATEGORY, "brief"));

		brief = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		brief.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setInst1(Inst.BRIEF, doc, brief.getText());
			}			
		});
		brief.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setInst1(Inst.BRIEF, doc, brief.getText());
				}
			}
			
		});
		brief.setLayoutData(new GridData(500, SWT.DEFAULT));
		brief.addListener(SWT.Modify, new Listener() {
			
			@Override
			public void handleEvent(Event event) {
				int currentHeight = brief.getSize().y;
				int preferredHeight = brief.computeSize(500, SWT.DEFAULT).y;
				if (currentHeight != preferredHeight) {
					GridData data = (GridData)brief.getLayoutData();
					data.heightHint = preferredHeight;
					client.pack();
				}
			}
		});
		brief.setEnabled(false);
		brief.setBackground(toolkit.getColors().getInactiveBackground());
		briefCheck.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (briefCheck.getSelection()) {
					addInst1(Inst.BRIEF, doc, "");
					brief.setEnabled(true);
					brief.setBackground(toolkit.getColors().getBackground());
					brief.setText("");
					briefCheck.setFont(boldFont);
				} else {
					removeInst(Inst.BRIEF, doc);
					brief.setEnabled(false);
					brief.setBackground(toolkit.getColors().getInactiveBackground());
					brief.setText("");
					briefCheck.setFont(normalFont);
				}
			}
		});

		spriteLabel = toolkit.createLabel(nameComp, "", SWT.NONE);

		Composite leftColumn = null;
		Composite rightColumn = null;
		boolean isRight = false;
		boolean inGeneral = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			
			if (key.equals(Inst.CLEARNATION) || 
				key.equals(Inst.CLEARREC) || 
				key.equals(Inst.CLEARGODS) || 
				key.equals(Inst.DEFCOM1) || 
				key.equals(Inst.FORTERA) || 
				key.equals(Inst.CLEARSITES)) {

				final Section expandable = toolkit.createSection(client, ExpandableComposite.TWISTIE | ExpandableComposite.TITLE_BAR);
				switch (key) {
				case CLEARNATION:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.general"));
					inGeneral = true;
					break;
				case CLEARREC:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.units"));
					inGeneral = false;
					break;
				case CLEARGODS:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.gods"));
					break;
				case DEFCOM1:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.pd"));
					break;
				case FORTERA:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.forts"));
					break;
				case CLEARSITES:
					expandable.setText(Messages.getString("NationDetailsSection.mod.section.sites"));
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
				if (inGeneral) {
					header1.setLayout(new GridLayout(1, true));
				} else {
					header1.setLayout(new GridLayout(2, true));
				}
				expandable.setClient(header1);
				if (key.equals(Inst.CLEARNATION)) {
					expandable.setExpanded(true);
				}

				leftColumn = toolkit.createComposite(header1);
				glayout = new GridLayout(5, false);
				glayout.marginHeight = 0;
				glayout.marginWidth = 0;
				glayout.verticalSpacing = 0;
				leftColumn.setLayout(glayout);
				leftColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

				if (inGeneral) {
					rightColumn = leftColumn;
				} else {
					rightColumn = toolkit.createComposite(header1);
					glayout = new GridLayout(5, false);
					glayout.marginHeight = 0;
					glayout.marginWidth = 0;
					glayout.verticalSpacing = 0;
					rightColumn.setLayout(glayout);
					rightColumn.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
				}
				isRight = false;
			}

			final InstFields field = fields.getValue();
			final Button check = new DynamicButton(isRight?rightColumn:leftColumn, SWT.CHECK);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.NATION_CATEGORY, key.label));
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
							addInst3(key, doc);
						} else if (field instanceof Inst4Fields) {
							addInst4(key, doc, key.defaultValue);
						} else if (field instanceof Inst5Fields) {
							addInst5(key, doc, key.defaultValue, key.defaultValue2, key.defaultValue3);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}

			});
			check.setText(key.label);

			Text myValue1 = null;
			Text myValue2 = null;
			Text myValue3 = null;
			if (field instanceof Inst1Fields ||	field instanceof Inst2Fields ||	field instanceof Inst4Fields ||	field instanceof Inst5Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue1 = value;
				
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
											if (Boolean.FALSE.equals(((Inst4Fields)fields.getValue()).value.getData())) {
												((Inst4Fields)fields.getValue()).value.setData(Boolean.TRUE);
												((Inst4Fields)fields.getValue()).check.setData(Boolean.TRUE);
												((Inst4Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
												break;
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
											if (Boolean.TRUE.equals(((Inst4Fields)fields.getValue()).value.getData()) && !((Inst4Fields)fields.getValue()).value.isEnabled()) {
												((Inst4Fields)fields.getValue()).value.setData(Boolean.FALSE);
												((Inst4Fields)fields.getValue()).check.setData(Boolean.FALSE);
												((Inst4Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
												break;
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
						} else if (field instanceof Inst4Fields) {
							setInst4(key, doc, value.getText());
						} else if (field instanceof Inst5Fields) {
							setInst5(key, doc, value.getText(), null, null);
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
								setInst4(key, doc, value.getText());
							} else if (field instanceof Inst5Fields) {
								setInst5(key, doc, value.getText(), null, null);
							}
						}
					}
				});
				value.setEnabled(false);
				
				if (field instanceof Inst1Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = 160;
					gd.horizontalSpan = 4;
				} else if (field instanceof Inst2Fields ||	field instanceof Inst4Fields) {
					gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
					gd.widthHint = DEFAULT_VALUE_WIDTH;
				} else if (field instanceof Inst5Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				}
				value.setLayoutData(gd);
			}
				
			Label defaultLabel1 = null;
			
			if (field instanceof Inst2Fields || field instanceof Inst3Fields || field instanceof Inst4Fields) {
				defaultLabel1 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel1.setEnabled(false);
			}
			if (field instanceof Inst2Fields || field instanceof Inst4Fields) {
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst3Fields) {
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.horizontalSpan = 2;
				gd.heightHint=20;
				check.setLayoutData(gd);
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
			}

			Label defaultLabel2 = null;
			Label defaultLabel3 = null;
			if (field instanceof Inst5Fields) {
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
						setInst5(key, doc, null, value.getText(), null);
					}			
				});
				value.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst5(key, doc, null, value.getText(), null);
						}
					}
				});
				value.setEnabled(false);
				
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value.setLayoutData(gd);
				
				defaultLabel2 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel2.setEnabled(false);

				final Text value3 = toolkit.createText(isRight?rightColumn:leftColumn, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
				myValue3 = value3;
				value3.addVerifyListener(new VerifyListener() {
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
							value3.setEnabled(true);
							value3.setText(key.defaultValue3);
						} else {
							value3.setEnabled(false);
							value3.setText("");
						}
					}

				});
				value3.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(FocusEvent e) {
						setInst5(key, doc, null, null, value3.getText());
					}			
				});
				value3.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst5(key, doc, null, null, value3.getText());
						}
					}
				});
				value3.setEnabled(false);
				
				gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH;
				value3.setLayoutData(gd);
				
				//TODO fix third default label
				//defaultLabel3 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				//defaultLabel3.setEnabled(false);
				
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
				((Inst3Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst4Fields) {
				((Inst4Fields)field).check = check;
				((Inst4Fields)field).value = myValue1;
				((Inst4Fields)field).defaultLabel = defaultLabel1;
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
			} else if (field instanceof Inst5Fields) {
				((Inst5Fields)field).check = check;
				((Inst5Fields)field).value1 = myValue1;
				((Inst5Fields)field).value2 = myValue2;
				((Inst5Fields)field).value3 = myValue3;
			}

			if (key.equals(Inst.CLEARREC)) {
				createSpacer(toolkit, rightColumn, 5);
				isRight = !isRight;
			}
			if (key.equals(Inst.CLEARGODS)) {
				createSpacer(toolkit, rightColumn, 5);
				isRight = !isRight;
			}
			if (key.equals(Inst.CLEARSITES)) {
				createSpacer(toolkit, rightColumn, 5);
				isRight = !isRight;
			}
			isRight = !isRight;
		}

		createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
	}
	
	public void update() {
		if (input != null) {
			String sprite = null;
			boolean fromZip = false;
			String nameString = getInst1(Inst.NAME, input);
			if (nameString != null) {
				name.setText(nameString);
				name.setEnabled(true);
				nameCheck.setSelection(true);
				nameCheck.setFont(boldFont);
			} else {
				if (input instanceof SelectNation) {
					String nameStr = Database.getNationName(((SelectNation)input).getValue());
					name.setText(nameStr != null ? nameStr : "");
					name.setEnabled(false);
					nameCheck.setSelection(false);
					nameCheck.setFont(normalFont);
				}
			}

			if (getInst1(Inst.FLAG, input) != null) {
				sprite = getInst1(Inst.FLAG, input);
			} else {
				if (input instanceof SelectNation) {
					sprite = ((SelectNation)input).getValue() + ".png";
				}
				fromZip = true;
			}

			if (sprite != null) {
				if (fromZip) {
					spriteLabel.setImage(getSpriteFromZip(sprite, "flags"));
				} else {
					spriteLabel.setImage(getSprite(sprite));
				}
			} else {
				spriteLabel.setImage(null);
			}

			final FormToolkit toolkit = mform.getToolkit();
			String description = getInst1(Inst.DESCR, input);
			if (description != null) {
				descr.setText(description);
				descr.setEnabled(true);
				descr.setBackground(toolkit.getColors().getBackground());
				descrCheck.setSelection(true);
				descrCheck.setFont(boldFont);
			} else {
				descr.setText("");
				descr.setEnabled(false);
				descr.setBackground(toolkit.getColors().getInactiveBackground());
				descrCheck.setSelection(false);
				descrCheck.setFont(normalFont);
			}

			String summaryStr = getInst1(Inst.SUMMARY, input);
			if (summaryStr != null) {
				summary.setText(summaryStr);
				summary.setEnabled(true);
				summary.setBackground(toolkit.getColors().getBackground());
				summaryCheck.setSelection(true);
				summaryCheck.setFont(boldFont);
			} else {
				summary.setText("");
				summary.setEnabled(false);
				summary.setBackground(toolkit.getColors().getInactiveBackground());
				summaryCheck.setSelection(false);
				summaryCheck.setFont(normalFont);
			}

			String briefStr = getInst1(Inst.BRIEF, input);
			if (briefStr != null) {
				brief.setText(briefStr);
				brief.setEnabled(true);
				brief.setBackground(toolkit.getColors().getBackground());
				briefCheck.setSelection(true);
				briefCheck.setFont(boldFont);
			} else {
				brief.setText("");
				brief.setEnabled(false);
				brief.setBackground(toolkit.getColors().getInactiveBackground());
				briefCheck.setSelection(false);
				briefCheck.setFont(normalFont);
			}
		}
		NationDB nationDB = new NationDB();
		if (input instanceof SelectNation) {
			nationDB = Database.getNation(((SelectNation)input).getValue());
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
				}
			} else {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText("");
					((Inst2Fields)fields.getValue()).value.setEnabled(false);
					((Inst2Fields)fields.getValue()).check.setSelection(false);
					((Inst2Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Boolean isVal = getInst3(fields.getKey(), input);
			if (isVal != null) {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).check.setSelection(isVal);
				}
			}
			Object val4 = getInst4(fields.getKey(), input);
			if (val4 != null) {
				if (fields.getValue() instanceof Inst4Fields) {
					((Inst4Fields)fields.getValue()).value.setText(val4.toString());
					((Inst4Fields)fields.getValue()).value.setEnabled(true);
					((Inst4Fields)fields.getValue()).check.setSelection(true);
					((Inst4Fields)fields.getValue()).check.setFont(boldFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst4Fields)fields.getValue()).value.getData())) {
								((Inst4Fields)fields.getValue()).value.setData(Boolean.TRUE);
								((Inst4Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst4Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
								break;
							}
						}
					}
				}
			} else {
				if (fields.getValue() instanceof Inst4Fields) {
					((Inst4Fields)fields.getValue()).value.setText("");
					((Inst4Fields)fields.getValue()).value.setEnabled(false);
					((Inst4Fields)fields.getValue()).check.setSelection(false);
					((Inst4Fields)fields.getValue()).check.setFont(normalFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic)) {
								if (Boolean.TRUE.equals(((Inst4Fields)fields.getValue()).value.getData())) {
									((Inst4Fields)fields.getValue()).value.setData(Boolean.FALSE);
									((Inst4Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst4Fields)fields.getValue()).defaultLabel.setData(Boolean.FALSE);
									break;
								}
							} else {
								dynamicFirstEmpty.add(dynamic);
								if (Boolean.FALSE.equals(((Inst4Fields)fields.getValue()).value.getData())) {
									((Inst4Fields)fields.getValue()).value.setData(Boolean.TRUE);
									((Inst4Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst4Fields)fields.getValue()).defaultLabel.setData(Boolean.TRUE);
									break;
								}
							}
						}
					}
				}
			}
			Double[] val5 = getInst5(fields.getKey(), input);
			if (val5 != null) {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value1.setText(val5[0] != null ? val5[0].toString() : "");
					((Inst5Fields)fields.getValue()).value1.setEnabled(true);
					((Inst5Fields)fields.getValue()).value2.setText(val5[1] != null ? val5[1].toString() : "");
					((Inst5Fields)fields.getValue()).value2.setEnabled(true);
					((Inst5Fields)fields.getValue()).value3.setText(val5[2] != null ? val5[2].toString() : "");
					((Inst5Fields)fields.getValue()).value3.setEnabled(true);
					((Inst5Fields)fields.getValue()).check.setSelection(true);
					((Inst5Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value1.setText("");
					((Inst5Fields)fields.getValue()).value1.setEnabled(false);
					((Inst5Fields)fields.getValue()).value2.setText("");
					((Inst5Fields)fields.getValue()).value2.setEnabled(false);
					((Inst5Fields)fields.getValue()).value3.setText("");
					((Inst5Fields)fields.getValue()).value3.setEnabled(false);
					((Inst5Fields)fields.getValue()).check.setSelection(false);
					((Inst5Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			if (input instanceof Nation) {
				switch (fields.getKey()) {
				case EPITHET:
					if (nationDB.epithet != null) {
						Inst.EPITHET.defaultValue = nationDB.epithet;
					}
					break;
				case DESCR:
					if (nationDB.descr != null) {
						Inst.DESCR.defaultValue = nationDB.descr;
					}
					break;
				case STARTSITE1:
					if (nationDB.startsite1 != null) {
						Inst.STARTSITE1.defaultValue = nationDB.startsite1;
					}
					break;
				case STARTSITE2:
					if (nationDB.startsite2 != null) {
						Inst.STARTSITE2.defaultValue = nationDB.startsite2;
					}
					break;
				case STARTSITE3:
					if (nationDB.startsite3 != null) {
						Inst.STARTSITE3.defaultValue = nationDB.startsite3;
					}
					break;
				case STARTSITE4:
					if (nationDB.startsite4 != null) {
						Inst.STARTSITE4.defaultValue = nationDB.startsite4;
					}
					break;
				case ERA:
					if (nationDB.era != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", nationDB.era));
						Inst.ERA.defaultValue = nationDB.era.toString();
					}
					break;
				case LIKESTERR:
					if (nationDB.likesterr != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", nationDB.likesterr));
						Inst.LIKESTERR.defaultValue = nationDB.likesterr.toString();
					}
					break;
				}
			}
		}
		name.getParent().getParent().getParent().layout(true, true);
	}
	
	private void setNationname(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Nation nationToEdit = (Nation)input;
				EList<NationMods> mods = nationToEdit.getMods();
				boolean nameSet = false;
				for (NationMods mod : mods) {
					if (mod instanceof NationInst1) {
						if (((NationInst1)mod).isName()) {
							((NationInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					NationInst1 nameInst = DmFactory.eINSTANCE.createNationInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}
	
	private void setNationdescr(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Nation nationToEdit = (Nation)input;
				EList<NationMods> mods = nationToEdit.getMods();
				boolean nameSet = false;
				for (NationMods mod : mods) {
					if (mod instanceof NationInst1) {
						if (((NationInst1)mod).isDescr()) {
							((NationInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					NationInst1 nameInst = DmFactory.eINSTANCE.createNationInst1();
					nameInst.setDescr(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private String getInst1(Inst inst2, Object nation) {
		EList<NationMods> list = ((Nation)nation).getMods();
		int siteCount = 0;
		for (NationMods mod : list) {
			if (mod instanceof NationInst1) {
				switch (inst2) {
				case NAME:
					if (((NationInst1)mod).isName()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case DESCR:
					if (((NationInst1)mod).isDescr()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case EPITHET:
					if (((NationInst1)mod).isEpithet()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case SUMMARY:
					if (((NationInst1)mod).isSummary()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case BRIEF:
					if (((NationInst1)mod).isBrief()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case FLAG:
					if (((NationInst1)mod).isFlag()){
						return ((NationInst1)mod).getValue();
					}
					break;
				case STARTSITE1:
					if (((NationInst1)mod).isStartsite()){
						siteCount++;
						if (siteCount == 1) {
							return ((NationInst1)mod).getValue();
						}
					}
					break;
				case STARTSITE2:
					if (((NationInst1)mod).isStartsite()){
						siteCount++;
						if (siteCount == 2) {
							return ((NationInst1)mod).getValue();
						}
					}
					break;
				case STARTSITE3:
					if (((NationInst1)mod).isStartsite()){
						siteCount++;
						if (siteCount == 3) {
							return ((NationInst1)mod).getValue();
						}
					}
					break;
				case STARTSITE4:
					if (((NationInst1)mod).isStartsite()){
						siteCount++;
						if (siteCount == 4) {
							return ((NationInst1)mod).getValue();
						}
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer getInst2(Inst inst2, Object nation) {
		EList<NationMods> list = ((Nation)nation).getMods();
		for (NationMods mod : list) {
			if (mod instanceof NationInst2) {
				switch (inst2) {
				case ERA:
					if (((NationInst2)mod).isEra()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case LIKESTERR:
					if (((NationInst2)mod).isLikesterr()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case LABCOST:
					if (((NationInst2)mod).isLabcost()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case TEMPLECOST:
					if (((NationInst2)mod).isTemplecost()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case TEMPLEPIC:
					if (((NationInst2)mod).isTemplepic()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case STARTUNITNBRS1:
					if (((NationInst2)mod).isStartunitnbrs1()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case STARTUNITNBRS2:
					if (((NationInst2)mod).isStartunitnbrs2()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO1:
					if (((NationInst2)mod).isHero1()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO2:
					if (((NationInst2)mod).isHero2()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO3:
					if (((NationInst2)mod).isHero3()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO4:
					if (((NationInst2)mod).isHero4()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO5:
					if (((NationInst2)mod).isHero5()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HERO6:
					if (((NationInst2)mod).isHero6()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case MULTIHERO1:
					if (((NationInst2)mod).isMultihero1()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case MULTIHERO2:
					if (((NationInst2)mod).isMultihero2()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DEFMULT1:
					if (((NationInst2)mod).isDefmult1()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DEFMULT1B:
					if (((NationInst2)mod).isDefmult1b()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DEFMULT2:
					if (((NationInst2)mod).isDefmult2()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DEFMULT2B:
					if (((NationInst2)mod).isDefmult2b()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case IDEALCOLD:
					if (((NationInst2)mod).isIdealcold()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case CASTLEPROD:
					if (((NationInst2)mod).isCastleprod()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DOMKILL:
					if (((NationInst2)mod).isDomkill()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case DOMUNREST:
					if (((NationInst2)mod).isDomunrest()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case MERCCOST:
					if (((NationInst2)mod).isMerccost()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case HOMEREALM:
					if (((NationInst2)mod).isHomerealm()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case LIKESPOP:
					if (((NationInst2)mod).isLikespop()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case FORTERA:
					if (((NationInst2)mod).isFortera()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case FORTCOST:
					if (((NationInst2)mod).isFortcost()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case SPREADCOLD:
					if (((NationInst2)mod).isSpreadcold()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case SPREADHEAT:
					if (((NationInst2)mod).isSpreadheat()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case SPREADCHAOS:
					if (((NationInst2)mod).isSpreadchaos()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case SPREADLAZY:
					if (((NationInst2)mod).isSpreadlazy()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case SPREADDEATH:
					if (((NationInst2)mod).isSpreaddeath()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				case GOLEMHP:
					if (((NationInst2)mod).isGolemhp()){
						return Integer.valueOf(((NationInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Boolean getInst3(Inst inst3, Object nation) {
		EList<NationMods> list = ((Nation)nation).getMods();
		for (NationMods mod : list) {
			if (mod instanceof NationInst3) {
				switch (inst3) {
				case CLEARNATION:
					if (((NationInst3)mod).isClearnation()){
						return Boolean.TRUE;
					}
					break;
				case CLEARREC:
					if (((NationInst3)mod).isClearrec()){
						return Boolean.TRUE;
					}
					break;
				case CLEARSITES:
					if (((NationInst3)mod).isClearsites()){
						return Boolean.TRUE;
					}
					break;
				case CLEARGODS:
					if (((NationInst3)mod).isCleargods()){
						return Boolean.TRUE;
					}
					break;
				case UWNATION:
					if (((NationInst3)mod).isUwnation()){
						return Boolean.TRUE;
					}
					break;
				case BLOODNATION:
					if (((NationInst3)mod).isBloodnation()){
						return Boolean.TRUE;
					}
					break;
				case NOPREACH:
					if (((NationInst3)mod).isNopreach()){
						return Boolean.TRUE;
					}
					break;
				case DYINGDOM:
					if (((NationInst3)mod).isDyingdom()){
						return Boolean.TRUE;
					}
					break;
				case SACRIFICEDOM:
					if (((NationInst3)mod).isSacrificedom()){
						return Boolean.TRUE;
					}
					break;
				case NODEATHSUPPLY:
					if (((NationInst3)mod).isNodeathsupply()){
						return Boolean.TRUE;
					}
					break;
				case AUTOUNDEAD:
					if (((NationInst3)mod).isAutoundead()){
						return Boolean.TRUE;
					}
					break;
				case ZOMBIEREANIM:
					if (((NationInst3)mod).isZombiereanim()){
						return Boolean.TRUE;
					}
					break;
				case HORSEREANIM:
					if (((NationInst3)mod).isHorsereanim()){
						return Boolean.TRUE;
					}
					break;
				case WIGHTREANIM:
					if (((NationInst3)mod).isWightreanim()){
						return Boolean.TRUE;
					}
					break;
				case MANIKINREANIM:
					if (((NationInst3)mod).isManikinreanim()){
						return Boolean.TRUE;
					}
					break;
				case TOMBWYRMREANIM:
					if (((NationInst3)mod).isTombwyrmreanim()){
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private Object getInst4(Inst inst4, Object nation) {
		int addreccom = 0;
		int addrecunit = 0;
		int addforeignunit = 0;
		int addforeigncom = 0;
		int forestrec = 0;
		int forestcom = 0;
		int mountainrec = 0;
		int mountaincom = 0;
		int swamprec = 0;
		int swampcom = 0;
		int wasterec = 0;
		int wastecom = 0;
		int caverec = 0;
		int cavecom = 0;
		int addgod = 0;
		int delgod = 0;
		EList<NationMods> list = ((Nation)nation).getMods();
		for (NationMods mod : list) {
			if (mod instanceof NationInst4) {
				switch (inst4) {
				case STARTCOM:
					if (((NationInst4)mod).isStartcom()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case STARTSCOUT:
					if (((NationInst4)mod).isStartscout()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case STARTUNITTYPE1:
					if (((NationInst4)mod).isStartunittype1()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case STARTUNITTYPE2:
					if (((NationInst4)mod).isStartunittype2()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case ADDRECUNIT1:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT2:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT3:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT4:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT5:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT6:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT7:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT8:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT9:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT10:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT11:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT12:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT13:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECUNIT14:
					if (((NationInst4)mod).isAddrecunit()){
						addrecunit++;
						if (addrecunit == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM1:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM2:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM3:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM4:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM5:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM6:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM7:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM8:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM9:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM10:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM11:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM12:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM13:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDRECCOM14:
					if (((NationInst4)mod).isAddreccom()){
						addreccom++;
						if (addreccom == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case UWUNIT1:
					if (((NationInst4)mod).isUwunit1()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWUNIT2:
					if (((NationInst4)mod).isUwunit2()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWUNIT3:
					if (((NationInst4)mod).isUwunit3()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWUNIT4:
					if (((NationInst4)mod).isUwunit4()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWUNIT5:
					if (((NationInst4)mod).isUwunit5()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWCOM1:
					if (((NationInst4)mod).isUwcom1()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWCOM2:
					if (((NationInst4)mod).isUwcom2()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWCOM3:
					if (((NationInst4)mod).isUwcom3()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWCOM4:
					if (((NationInst4)mod).isUwcom4()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case UWCOM5:
					if (((NationInst4)mod).isUwcom5()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFCOM1:
					if (((NationInst4)mod).isDefcom1()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFCOM2:
					if (((NationInst4)mod).isDefcom2()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFUNIT1:
					if (((NationInst4)mod).isDefunit1()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFUNIT1B:
					if (((NationInst4)mod).isDefunit1b()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFUNIT2:
					if (((NationInst4)mod).isDefunit2()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DEFUNIT2B:
					if (((NationInst4)mod).isDefunit2b()){
						String strVal = ((NationInst4)mod).getValue1();
						Integer intVal = ((NationInst4)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case ADDFOREIGNUNIT1:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT2:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT3:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT4:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT5:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT6:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT7:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT8:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT9:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT10:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT11:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT12:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT13:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNUNIT14:
					if (((NationInst4)mod).isAddforeignunit()){
						addforeignunit++;
						if (addforeignunit == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM1:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM2:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM3:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM4:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM5:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM6:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM7:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM8:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM9:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM10:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM11:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM12:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM13:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDFOREIGNCOM14:
					if (((NationInst4)mod).isAddforeigncom()){
						addforeigncom++;
						if (addforeigncom == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTREC1:
					if (((NationInst4)mod).isForestrec()){
						forestrec++;
						if (forestrec == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTREC2:
					if (((NationInst4)mod).isForestrec()){
						forestrec++;
						if (forestrec == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTREC3:
					if (((NationInst4)mod).isForestrec()){
						forestrec++;
						if (forestrec == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTREC4:
					if (((NationInst4)mod).isForestrec()){
						forestrec++;
						if (forestrec == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTREC5:
					if (((NationInst4)mod).isForestrec()){
						forestrec++;
						if (forestrec == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTCOM1:
					if (((NationInst4)mod).isForestcom()){
						forestcom++;
						if (forestcom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTCOM2:
					if (((NationInst4)mod).isForestcom()){
						forestcom++;
						if (forestcom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTCOM3:
					if (((NationInst4)mod).isForestcom()){
						forestcom++;
						if (forestcom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTCOM4:
					if (((NationInst4)mod).isForestcom()){
						forestcom++;
						if (forestcom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case FORESTCOM5:
					if (((NationInst4)mod).isForestcom()){
						forestcom++;
						if (forestcom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINREC1:
					if (((NationInst4)mod).isMountainrec()){
						mountainrec++;
						if (mountainrec == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINREC2:
					if (((NationInst4)mod).isMountainrec()){
						mountainrec++;
						if (mountainrec == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINREC3:
					if (((NationInst4)mod).isMountainrec()){
						mountainrec++;
						if (mountainrec == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINREC4:
					if (((NationInst4)mod).isMountainrec()){
						mountainrec++;
						if (mountainrec == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINREC5:
					if (((NationInst4)mod).isMountainrec()){
						mountainrec++;
						if (mountainrec == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINCOM1:
					if (((NationInst4)mod).isMountaincom()){
						mountaincom++;
						if (mountaincom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINCOM2:
					if (((NationInst4)mod).isMountaincom()){
						mountaincom++;
						if (mountaincom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINCOM3:
					if (((NationInst4)mod).isMountaincom()){
						mountaincom++;
						if (mountaincom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINCOM4:
					if (((NationInst4)mod).isMountaincom()){
						mountaincom++;
						if (mountaincom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case MOUNTAINCOM5:
					if (((NationInst4)mod).isMountaincom()){
						mountaincom++;
						if (mountaincom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPREC1:
					if (((NationInst4)mod).isSwamprec()){
						swamprec++;
						if (swamprec == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPREC2:
					if (((NationInst4)mod).isSwamprec()){
						swamprec++;
						if (swamprec == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPREC3:
					if (((NationInst4)mod).isSwamprec()){
						swamprec++;
						if (swamprec == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPREC4:
					if (((NationInst4)mod).isSwamprec()){
						swamprec++;
						if (swamprec == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPREC5:
					if (((NationInst4)mod).isSwamprec()){
						swamprec++;
						if (swamprec == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPCOM1:
					if (((NationInst4)mod).isSwampcom()){
						swampcom++;
						if (swampcom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPCOM2:
					if (((NationInst4)mod).isSwampcom()){
						swampcom++;
						if (swampcom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPCOM3:
					if (((NationInst4)mod).isSwampcom()){
						swampcom++;
						if (swampcom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPCOM4:
					if (((NationInst4)mod).isSwampcom()){
						swampcom++;
						if (swampcom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case SWAMPCOM5:
					if (((NationInst4)mod).isSwampcom()){
						swampcom++;
						if (swampcom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTEREC1:
					if (((NationInst4)mod).isWasterec()){
						wasterec++;
						if (wasterec == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTEREC2:
					if (((NationInst4)mod).isWasterec()){
						wasterec++;
						if (wasterec == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTEREC3:
					if (((NationInst4)mod).isWasterec()){
						wasterec++;
						if (wasterec == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTEREC4:
					if (((NationInst4)mod).isWasterec()){
						wasterec++;
						if (wasterec == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTEREC5:
					if (((NationInst4)mod).isWasterec()){
						wasterec++;
						if (wasterec == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTECOM1:
					if (((NationInst4)mod).isWastecom()){
						wastecom++;
						if (wastecom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTECOM2:
					if (((NationInst4)mod).isWastecom()){
						wastecom++;
						if (wastecom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTECOM3:
					if (((NationInst4)mod).isWastecom()){
						wastecom++;
						if (wastecom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTECOM4:
					if (((NationInst4)mod).isWastecom()){
						wastecom++;
						if (wastecom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WASTECOM5:
					if (((NationInst4)mod).isWastecom()){
						wastecom++;
						if (wastecom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVEREC1:
					if (((NationInst4)mod).isCaverec()){
						caverec++;
						if (caverec == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVEREC2:
					if (((NationInst4)mod).isCaverec()){
						caverec++;
						if (caverec == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVEREC3:
					if (((NationInst4)mod).isCaverec()){
						caverec++;
						if (caverec == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVEREC4:
					if (((NationInst4)mod).isCaverec()){
						caverec++;
						if (caverec == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVEREC5:
					if (((NationInst4)mod).isCaverec()){
						caverec++;
						if (caverec == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVECOM1:
					if (((NationInst4)mod).isCavecom()){
						cavecom++;
						if (cavecom == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVECOM2:
					if (((NationInst4)mod).isCavecom()){
						cavecom++;
						if (cavecom == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVECOM3:
					if (((NationInst4)mod).isCavecom()){
						cavecom++;
						if (cavecom == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVECOM4:
					if (((NationInst4)mod).isCavecom()){
						cavecom++;
						if (cavecom == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case CAVECOM5:
					if (((NationInst4)mod).isCavecom()){
						cavecom++;
						if (cavecom == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD1:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD2:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD3:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD4:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD5:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD6:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD7:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD8:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD9:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD10:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD11:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD12:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD13:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ADDGOD14:
					if (((NationInst4)mod).isAddgod()){
						addgod++;
						if (addgod == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD1:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 1) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD2:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 2) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD3:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 3) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD4:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 4) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD5:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 5) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD6:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 6) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD7:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 7) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD8:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 8) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD9:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 9) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD10:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 10) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD11:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 11) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD12:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 12) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD13:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 13) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case DELGOD14:
					if (((NationInst4)mod).isDelgod()){
						delgod++;
						if (delgod == 14) {
							String strVal = ((NationInst4)mod).getValue1();
							Integer intVal = ((NationInst4)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Double[] getInst5(Inst inst5, Object nation) {
		EList<NationMods> list = ((Nation)nation).getMods();
		for (NationMods mod : list) {
			if (mod instanceof NationInst5) {
				switch (inst5) {
				case COLOR:
					if (((NationInst5)mod).isColor()){
						Double value1 = ((NationInst5)mod).getValue1() != null ? Double.valueOf(((NationInst5)mod).getValue1()) : null; 
						Double value2 = ((NationInst5)mod).getValue2() != null ? Double.valueOf(((NationInst5)mod).getValue2()) : null;
						Double value3 = ((NationInst5)mod).getValue3() != null ? Double.valueOf(((NationInst5)mod).getValue3()) : null;
						return new Double[]{value1, value2, value3};
					}
					break;
				case SECONDARYCOLOR:
					if (((NationInst5)mod).isSecondarycolor()){
						Double value1 = ((NationInst5)mod).getValue1() != null ? Double.valueOf(((NationInst5)mod).getValue1()) : null; 
						Double value2 = ((NationInst5)mod).getValue2() != null ? Double.valueOf(((NationInst5)mod).getValue2()) : null;
						Double value3 = ((NationInst5)mod).getValue3() != null ? Double.valueOf(((NationInst5)mod).getValue3()) : null;
						return new Double[]{value1, value2, value3};
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
				Nation nationToEdit = (Nation)input;
				int siteCount = 0;
				EList<NationMods> mods = nationToEdit.getMods();				
				for (NationMods mod : mods) {
					if (mod instanceof NationInst1) {
						switch (inst2) {
						case EPITHET:
							if (((NationInst1)mod).isEpithet()) {
								((NationInst1)mod).setValue(newName);
							}
							break;
						case SUMMARY:
							if (((NationInst1)mod).isSummary()) {
								((NationInst1)mod).setValue(newName);
							}
							break;
						case BRIEF:
							if (((NationInst1)mod).isBrief()) {
								((NationInst1)mod).setValue(newName);
							}
							break;
						case FLAG:
							if (((NationInst1)mod).isFlag()) {
								((NationInst1)mod).setValue(newName);
								spriteLabel.setImage(getSprite(newName));
								spriteLabel.getParent().layout(true, true);
							}
							break;
						case STARTSITE1:
							if (((NationInst1)mod).isStartsite()) {
								siteCount++;
								if (siteCount == 1) {
									((NationInst1)mod).setValue(newName);
								}
							}
							break;
						case STARTSITE2:
							if (((NationInst1)mod).isStartsite()) {
								siteCount++;
								if (siteCount == 2) {
									((NationInst1)mod).setValue(newName);
								}
							}
							break;
						case STARTSITE3:
							if (((NationInst1)mod).isStartsite()) {
								siteCount++;
								if (siteCount == 3) {
									((NationInst1)mod).setValue(newName);
								}
							}
							break;
						case STARTSITE4:
							if (((NationInst1)mod).isStartsite()) {
								siteCount++;
								if (siteCount == 4) {
									((NationInst1)mod).setValue(newName);
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

	private void setInst2(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Nation nationToEdit = (Nation)input;
				EList<NationMods> mods = nationToEdit.getMods();
				for (NationMods mod : mods) {
					if (mod instanceof NationInst2) {
						switch (inst2) {
						case ERA:
							if (((NationInst2)mod).isEra()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LIKESTERR:
							if (((NationInst2)mod).isLikesterr()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LABCOST:
							if (((NationInst2)mod).isLabcost()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TEMPLECOST:
							if (((NationInst2)mod).isTemplecost()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TEMPLEPIC:
							if (((NationInst2)mod).isTemplepic()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STARTUNITNBRS1:
							if (((NationInst2)mod).isStartunitnbrs1()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STARTUNITNBRS2:
							if (((NationInst2)mod).isStartunitnbrs2()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO1:
							if (((NationInst2)mod).isHero1()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO2:
							if (((NationInst2)mod).isHero2()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO3:
							if (((NationInst2)mod).isHero3()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO4:
							if (((NationInst2)mod).isHero4()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO5:
							if (((NationInst2)mod).isHero5()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERO6:
							if (((NationInst2)mod).isHero6()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MULTIHERO1:
							if (((NationInst2)mod).isMultihero1()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MULTIHERO2:
							if (((NationInst2)mod).isMultihero2()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEFMULT1:
							if (((NationInst2)mod).isDefmult1()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEFMULT1B:
							if (((NationInst2)mod).isDefmult1b()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEFMULT2:
							if (((NationInst2)mod).isDefmult2()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEFMULT2B:
							if (((NationInst2)mod).isDefmult2b()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case IDEALCOLD:
							if (((NationInst2)mod).isIdealcold()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CASTLEPROD:
							if (((NationInst2)mod).isCastleprod()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DOMKILL:
							if (((NationInst2)mod).isDomkill()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DOMUNREST:
							if (((NationInst2)mod).isDomunrest()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MERCCOST:
							if (((NationInst2)mod).isMerccost()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HOMEREALM:
							if (((NationInst2)mod).isHomerealm()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LIKESPOP:
							if (((NationInst2)mod).isLikespop()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORTERA:
							if (((NationInst2)mod).isFortera()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORTCOST:
							if (((NationInst2)mod).isFortcost()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADCOLD:
							if (((NationInst2)mod).isSpreadcold()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADHEAT:
							if (((NationInst2)mod).isSpreadheat()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADCHAOS:
							if (((NationInst2)mod).isSpreadchaos()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADLAZY:
							if (((NationInst2)mod).isSpreadlazy()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADDEATH:
							if (((NationInst2)mod).isSpreaddeath()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GOLEMHP:
							if (((NationInst2)mod).isGolemhp()){
								((NationInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						}
					}
				}

			}  
		});

		updateSelection();
	}

	private void setInst4(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				int addreccom = 0;
				int addrecunit = 0;
				int addforeignunit = 0;
				int addforeigncom = 0;
				int forestrec = 0;
				int forestcom = 0;
				int mountainrec = 0;
				int mountaincom = 0;
				int swamprec = 0;
				int swampcom = 0;
				int wasterec = 0;
				int wastecom = 0;
				int caverec = 0;
				int cavecom = 0;
				int addgod = 0;
				int delgod = 0;
				Nation nationToEdit = (Nation)input;
				List<NationMods> modsToRemove = new ArrayList<NationMods>();
				List<NationMods> modsToAdd = new ArrayList<NationMods>();
				EList<NationMods> mods = nationToEdit.getMods();
				for (NationMods mod : mods) {
					if (mod instanceof NationInst4) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst2) {
						case STARTCOM:
							if (((NationInst4)mod).isStartcom()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setStartcom(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case STARTSCOUT:
							if (((NationInst4)mod).isStartscout()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setStartscout(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case STARTUNITTYPE1:
							if (((NationInst4)mod).isStartunittype1()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setStartunittype1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case STARTUNITTYPE2:
							if (((NationInst4)mod).isStartunittype2()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setStartunittype2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case ADDRECUNIT1:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT2:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT3:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT4:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT5:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT6:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT7:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT8:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT9:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT10:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT11:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT12:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT13:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECUNIT14:
							if (((NationInst4)mod).isAddrecunit()){
								addrecunit++;
								if (addrecunit == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddrecunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM1:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM2:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM3:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM4:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM5:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM6:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM7:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM8:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM9:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM10:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM11:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM12:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM13:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDRECCOM14:
							if (((NationInst4)mod).isAddreccom()){
								addreccom++;
								if (addreccom == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddreccom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case UWUNIT1:
							if (((NationInst4)mod).isUwunit1()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwunit1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWUNIT2:
							if (((NationInst4)mod).isUwunit2()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwunit2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWUNIT3:
							if (((NationInst4)mod).isUwunit3()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwunit3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWUNIT4:
							if (((NationInst4)mod).isUwunit4()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwunit4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWUNIT5:
							if (((NationInst4)mod).isUwunit5()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwunit5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWCOM1:
							if (((NationInst4)mod).isUwcom1()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwcom1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWCOM2:
							if (((NationInst4)mod).isUwcom2()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwcom2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWCOM3:
							if (((NationInst4)mod).isUwcom3()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwcom3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWCOM4:
							if (((NationInst4)mod).isUwcom4()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwcom4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case UWCOM5:
							if (((NationInst4)mod).isUwcom5()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setUwcom5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFCOM1:
							if (((NationInst4)mod).isDefcom1()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefcom1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFCOM2:
							if (((NationInst4)mod).isDefcom2()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefcom2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFUNIT1:
							if (((NationInst4)mod).isDefunit1()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefunit1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFUNIT1B:
							if (((NationInst4)mod).isDefunit1b()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefunit1b(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFUNIT2:
							if (((NationInst4)mod).isDefunit2()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefunit2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DEFUNIT2B:
							if (((NationInst4)mod).isDefunit2b()){
								modsToRemove.add(mod);
								NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
								newMod.setDefunit2b(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case ADDFOREIGNUNIT1:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT2:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT3:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT4:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT5:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT6:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT7:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT8:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT9:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT10:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT11:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT12:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT13:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNUNIT14:
							if (((NationInst4)mod).isAddforeignunit()){
								addforeignunit++;
								if (addforeignunit == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeignunit(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM1:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM2:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM3:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM4:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM5:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM6:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM7:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM8:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM9:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM10:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM11:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM12:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM13:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDFOREIGNCOM14:
							if (((NationInst4)mod).isAddforeigncom()){
								addforeigncom++;
								if (addforeigncom == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddforeigncom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTREC1:
							if (((NationInst4)mod).isForestrec()){
								forestrec++;
								if (forestrec == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTREC2:
							if (((NationInst4)mod).isForestrec()){
								forestrec++;
								if (forestrec == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTREC3:
							if (((NationInst4)mod).isForestrec()){
								forestrec++;
								if (forestrec == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTREC4:
							if (((NationInst4)mod).isForestrec()){
								forestrec++;
								if (forestrec == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTREC5:
							if (((NationInst4)mod).isForestrec()){
								forestrec++;
								if (forestrec == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTCOM1:
							if (((NationInst4)mod).isForestcom()){
								forestcom++;
								if (forestcom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTCOM2:
							if (((NationInst4)mod).isForestcom()){
								forestcom++;
								if (forestcom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTCOM3:
							if (((NationInst4)mod).isForestcom()){
								forestcom++;
								if (forestcom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTCOM4:
							if (((NationInst4)mod).isForestcom()){
								forestcom++;
								if (forestcom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case FORESTCOM5:
							if (((NationInst4)mod).isForestcom()){
								forestcom++;
								if (forestcom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setForestcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINREC1:
							if (((NationInst4)mod).isMountainrec()){
								mountainrec++;
								if (mountainrec == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountainrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINREC2:
							if (((NationInst4)mod).isMountainrec()){
								mountainrec++;
								if (mountainrec == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountainrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINREC3:
							if (((NationInst4)mod).isMountainrec()){
								mountainrec++;
								if (mountainrec == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountainrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINREC4:
							if (((NationInst4)mod).isMountainrec()){
								mountainrec++;
								if (mountainrec == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountainrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINREC5:
							if (((NationInst4)mod).isMountainrec()){
								mountainrec++;
								if (mountainrec == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountainrec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINCOM1:
							if (((NationInst4)mod).isMountaincom()){
								mountaincom++;
								if (mountaincom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountaincom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINCOM2:
							if (((NationInst4)mod).isMountaincom()){
								mountaincom++;
								if (mountaincom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountaincom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINCOM3:
							if (((NationInst4)mod).isMountaincom()){
								mountaincom++;
								if (mountaincom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountaincom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINCOM4:
							if (((NationInst4)mod).isMountaincom()){
								mountaincom++;
								if (mountaincom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountaincom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case MOUNTAINCOM5:
							if (((NationInst4)mod).isMountaincom()){
								mountaincom++;
								if (mountaincom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setMountaincom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPREC1:
							if (((NationInst4)mod).isSwamprec()){
								swamprec++;
								if (swamprec == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwamprec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPREC2:
							if (((NationInst4)mod).isSwamprec()){
								swamprec++;
								if (swamprec == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwamprec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPREC3:
							if (((NationInst4)mod).isSwamprec()){
								swamprec++;
								if (swamprec == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwamprec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPREC4:
							if (((NationInst4)mod).isSwamprec()){
								swamprec++;
								if (swamprec == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwamprec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPREC5:
							if (((NationInst4)mod).isSwamprec()){
								swamprec++;
								if (swamprec == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwamprec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPCOM1:
							if (((NationInst4)mod).isSwampcom()){
								swampcom++;
								if (swampcom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwampcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPCOM2:
							if (((NationInst4)mod).isSwampcom()){
								swampcom++;
								if (swampcom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwampcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPCOM3:
							if (((NationInst4)mod).isSwampcom()){
								swampcom++;
								if (swampcom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwampcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPCOM4:
							if (((NationInst4)mod).isSwampcom()){
								swampcom++;
								if (swampcom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwampcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case SWAMPCOM5:
							if (((NationInst4)mod).isSwampcom()){
								swampcom++;
								if (swampcom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setSwampcom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTEREC1:
							if (((NationInst4)mod).isWasterec()){
								wasterec++;
								if (wasterec == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWasterec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTEREC2:
							if (((NationInst4)mod).isWasterec()){
								wasterec++;
								if (wasterec == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWasterec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTEREC3:
							if (((NationInst4)mod).isWasterec()){
								wasterec++;
								if (wasterec == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWasterec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTEREC4:
							if (((NationInst4)mod).isWasterec()){
								wasterec++;
								if (wasterec == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWasterec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTEREC5:
							if (((NationInst4)mod).isWasterec()){
								wasterec++;
								if (wasterec == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWasterec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTECOM1:
							if (((NationInst4)mod).isWastecom()){
								wastecom++;
								if (wastecom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWastecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTECOM2:
							if (((NationInst4)mod).isWastecom()){
								wastecom++;
								if (wastecom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWastecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTECOM3:
							if (((NationInst4)mod).isWastecom()){
								wastecom++;
								if (wastecom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWastecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTECOM4:
							if (((NationInst4)mod).isWastecom()){
								wastecom++;
								if (wastecom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWastecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WASTECOM5:
							if (((NationInst4)mod).isWastecom()){
								wastecom++;
								if (wastecom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setWastecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVEREC1:
							if (((NationInst4)mod).isCaverec()){
								caverec++;
								if (caverec == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCaverec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVEREC2:
							if (((NationInst4)mod).isCaverec()){
								caverec++;
								if (caverec == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCaverec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVEREC3:
							if (((NationInst4)mod).isCaverec()){
								caverec++;
								if (caverec == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCaverec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVEREC4:
							if (((NationInst4)mod).isCaverec()){
								caverec++;
								if (caverec == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCaverec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVEREC5:
							if (((NationInst4)mod).isCaverec()){
								caverec++;
								if (caverec == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCaverec(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVECOM1:
							if (((NationInst4)mod).isCavecom()){
								cavecom++;
								if (cavecom == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCavecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVECOM2:
							if (((NationInst4)mod).isCavecom()){
								cavecom++;
								if (cavecom == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCavecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVECOM3:
							if (((NationInst4)mod).isCavecom()){
								cavecom++;
								if (cavecom == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCavecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVECOM4:
							if (((NationInst4)mod).isCavecom()){
								cavecom++;
								if (cavecom == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCavecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case CAVECOM5:
							if (((NationInst4)mod).isCavecom()){
								cavecom++;
								if (cavecom == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setCavecom(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD1:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD2:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD3:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD4:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD5:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD6:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD7:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD8:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD9:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD10:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD11:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD12:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD13:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ADDGOD14:
							if (((NationInst4)mod).isAddgod()){
								addgod++;
								if (addgod == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setAddgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD1:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 1) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD2:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 2) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD3:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 3) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD4:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 4) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD5:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 5) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD6:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 6) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD7:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 7) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD8:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 8) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD9:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 9) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD10:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 10) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD11:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 11) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD12:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 12) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD13:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 13) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case DELGOD14:
							if (((NationInst4)mod).isDelgod()){
								delgod++;
								if (delgod == 14) {
									modsToRemove.add(mod);
									NationInst4 newMod = DmFactory.eINSTANCE.createNationInst4();
									newMod.setDelgod(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
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
	
	private void setInst5(final Inst inst5, final XtextEditor editor, final String value1, final String value2, final String value3) { 
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Nation nationToEdit = (Nation)input;
				EList<NationMods> mods = nationToEdit.getMods();
				for (NationMods mod : mods) {
					if (mod instanceof NationInst5) {
						switch (inst5) {
						case COLOR:
							if (((NationInst5)mod).isColor()) {
								if (value1 != null) {
									try {
										Double.parseDouble(value1);
										((NationInst5)mod).setValue1(value1);
									} catch(NumberFormatException e) {
										// Skip if not a double
									}
								}
								if (value2 != null) {
									try {
										Double.parseDouble(value2);
										((NationInst5)mod).setValue2(value2);
									} catch(NumberFormatException e) {
										// Skip if not a double
									}
								}
								if (value3 != null) {
									try {
										Double.parseDouble(value3);
										((NationInst5)mod).setValue3(value3);
									} catch(NumberFormatException e) {
										// Skip if not a double
									}
								}
							}
							break;
						case SECONDARYCOLOR:
							if (((NationInst5)mod).isSecondarycolor()) {
								if (value1 != null) {
									try {
										Double.parseDouble(value1);
										((NationInst5)mod).setValue1(value1);
									} catch(NumberFormatException e) {
										// Skip if not a double
									}
								}
								if (value2 != null) {
									try {
										Double.parseDouble(value2);
										((NationInst5)mod).setValue2(value2);
									} catch(NumberFormatException e) {
										// Skip if not a double
									}
								}
								if (value3 != null) {
									try {
										Double.parseDouble(value3);
										((NationInst5)mod).setValue3(value3);
									} catch(NumberFormatException e) {
										// Skip if not a double
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
	
	private void addInst1(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<NationMods> mods = ((Nation)input).getMods();
						NationInst1 type = DmFactory.eINSTANCE.createNationInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
							break;
						case EPITHET:
							type.setEpithet(true);
							break;
						case DESCR:
							type.setDescr(true);
							break;
						case SUMMARY:
							type.setSummary(true);
							break;
						case BRIEF:
							type.setBrief(true);
							break;
						case FLAG:
							type.setFlag(true);
							break;
						case STARTSITE1:
							type.setStartsite(true);
							break;
						case STARTSITE2:
							type.setStartsite(true);
							break;
						case STARTSITE3:
							type.setStartsite(true);
							break;
						case STARTSITE4:
							type.setStartsite(true);
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
						EList<NationMods> mods = ((Nation)input).getMods();
						NationInst2 type = DmFactory.eINSTANCE.createNationInst2();
						switch (inst) {
						case ERA:
							type.setEra(true);
							break;
						case LIKESTERR:
							type.setLikesterr(true);
							break;
						case LABCOST:
							type.setLabcost(true);
							break;
						case TEMPLECOST:
							type.setTemplecost(true);
							break;
						case TEMPLEPIC:
							type.setTemplepic(true);
							break;
						case STARTUNITNBRS1:
							type.setStartunitnbrs1(true);
							break;
						case STARTUNITNBRS2:
							type.setStartunitnbrs2(true);
							break;
						case HERO1:
							type.setHero1(true);
							break;
						case HERO2:
							type.setHero2(true);
							break;
						case HERO3:
							type.setHero3(true);
							break;
						case HERO4:
							type.setHero4(true);
							break;
						case HERO5:
							type.setHero5(true);
							break;
						case HERO6:
							type.setHero6(true);
							break;
						case MULTIHERO1:
							type.setMultihero1(true);
							break;
						case MULTIHERO2:
							type.setMultihero2(true);
							break;
						case DEFMULT1:
							type.setDefmult1(true);
							break;
						case DEFMULT1B:
							type.setDefmult1b(true);
							break;
						case DEFMULT2:
							type.setDefmult2(true);
							break;
						case DEFMULT2B:
							type.setDefmult2b(true);
							break;
						case IDEALCOLD:
							type.setIdealcold(true);
							break;
						case CASTLEPROD:
							type.setCastleprod(true);
							break;
						case DOMKILL:
							type.setDomkill(true);
							break;
						case DOMUNREST:
							type.setDomunrest(true);
							break;
						case MERCCOST:
							type.setMerccost(true);
							break;
						case HOMEREALM:
							type.setHomerealm(true);
							break;
						case LIKESPOP:
							type.setLikespop(true);
							break;
						case FORTERA:
							type.setFortera(true);
							break;
						case FORTCOST:
							type.setFortcost(true);
							break;
						case SPREADCOLD:
							type.setSpreadcold(true);
							break;
						case SPREADHEAT:
							type.setSpreadheat(true);
							break;
						case SPREADCHAOS:
							type.setSpreadchaos(true);
							break;
						case SPREADLAZY:
							type.setSpreadlazy(true);
							break;
						case SPREADDEATH:
							type.setSpreaddeath(true);
							break;
						case GOLEMHP:
							type.setGolemhp(true);
							break;
						}
						try {
							type.setValue(Integer.valueOf(newName));
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void addInst3(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<NationMods> mods = ((Nation)input).getMods();
						NationInst3 type = DmFactory.eINSTANCE.createNationInst3();
						switch (inst) {
						case CLEARNATION:
							type.setClearnation(true);
							break;
						case CLEARREC:
							type.setClearrec(true);
							break;
						case CLEARSITES:
							type.setClearsites(true);
							break;
						case UWNATION:
							type.setUwnation(true);
							break;
						case BLOODNATION:
							type.setBloodnation(true);
							break;
						case NOPREACH:
							type.setNopreach(true);
							break;
						case DYINGDOM:
							type.setDyingdom(true);
							break;
						case SACRIFICEDOM:
							type.setSacrificedom(true);
							break;
						case NODEATHSUPPLY:
							type.setNodeathsupply(true);
							break;
						case AUTOUNDEAD:
							type.setAutoundead(true);
							break;
						case ZOMBIEREANIM:
							type.setZombiereanim(true);
							break;
						case HORSEREANIM:
							type.setHorsereanim(true);
							break;
						case WIGHTREANIM:
							type.setWightreanim(true);
							break;
						case MANIKINREANIM:
							type.setManikinreanim(true);
							break;
						case TOMBWYRMREANIM:
							type.setTombwyrmreanim(true);
							break;
						case CLEARGODS:
							type.setCleargods(true);
							break;
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}
	
	private void addInst4(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<NationMods> mods = ((Nation)input).getMods();
						NationInst4 type = DmFactory.eINSTANCE.createNationInst4();
						switch (inst) {
						case STARTCOM:
							type.setStartcom(true);
							break;
						case STARTSCOUT:
							type.setStartscout(true);
							break;
						case STARTUNITTYPE1:
							type.setStartunittype1(true);
							break;
						case STARTUNITTYPE2:
							type.setStartunittype2(true);
							break;
						case ADDRECUNIT1:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT2:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT3:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT4:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT5:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT6:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT7:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT8:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT9:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT10:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT11:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT12:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT13:
							type.setAddrecunit(true);
							break;
						case ADDRECUNIT14:
							type.setAddrecunit(true);
							break;
						case ADDRECCOM1:
							type.setAddreccom(true);
							break;
						case ADDRECCOM2:
							type.setAddreccom(true);
							break;
						case ADDRECCOM3:
							type.setAddreccom(true);
							break;
						case ADDRECCOM4:
							type.setAddreccom(true);
							break;
						case ADDRECCOM5:
							type.setAddreccom(true);
							break;
						case ADDRECCOM6:
							type.setAddreccom(true);
							break;
						case ADDRECCOM7:
							type.setAddreccom(true);
							break;
						case ADDRECCOM8:
							type.setAddreccom(true);
							break;
						case ADDRECCOM9:
							type.setAddreccom(true);
							break;
						case ADDRECCOM10:
							type.setAddreccom(true);
							break;
						case ADDRECCOM11:
							type.setAddreccom(true);
							break;
						case ADDRECCOM12:
							type.setAddreccom(true);
							break;
						case ADDRECCOM13:
							type.setAddreccom(true);
							break;
						case ADDRECCOM14:
							type.setAddreccom(true);
							break;
						case UWUNIT1:
							type.setUwunit1(true);
							break;
						case UWUNIT2:
							type.setUwunit2(true);
							break;
						case UWUNIT3:
							type.setUwunit3(true);
							break;
						case UWUNIT4:
							type.setUwunit4(true);
							break;
						case UWUNIT5:
							type.setUwunit5(true);
							break;
						case UWCOM1:
							type.setUwcom1(true);
							break;
						case UWCOM2:
							type.setUwcom2(true);
							break;
						case UWCOM3:
							type.setUwcom3(true);
							break;
						case UWCOM4:
							type.setUwcom4(true);
							break;
						case UWCOM5:
							type.setUwcom5(true);
							break;
						case DEFCOM1:
							type.setDefcom1(true);
							break;
						case DEFCOM2:
							type.setDefcom2(true);
							break;
						case DEFUNIT1:
							type.setDefunit1(true);
							break;
						case DEFUNIT1B:
							type.setDefunit1b(true);
							break;
						case DEFUNIT2:
							type.setDefunit2(true);
							break;
						case DEFUNIT2B:
							type.setDefunit2b(true);
							break;
						case ADDFOREIGNUNIT1:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT2:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT3:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT4:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT5:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT6:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT7:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT8:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT9:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT10:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT11:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT12:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT13:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNUNIT14:
							type.setAddforeignunit(true);
							break;
						case ADDFOREIGNCOM1:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM2:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM3:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM4:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM5:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM6:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM7:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM8:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM9:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM10:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM11:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM12:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM13:
							type.setAddforeigncom(true);
							break;
						case ADDFOREIGNCOM14:
							type.setAddforeigncom(true);
							break;
						case FORESTREC1:
							type.setForestrec(true);
							break;
						case FORESTREC2:
							type.setForestrec(true);
							break;
						case FORESTREC3:
							type.setForestrec(true);
							break;
						case FORESTREC4:
							type.setForestrec(true);
							break;
						case FORESTREC5:
							type.setForestrec(true);
							break;
						case FORESTCOM1:
							type.setForestcom(true);
							break;
						case FORESTCOM2:
							type.setForestcom(true);
							break;
						case FORESTCOM3:
							type.setForestcom(true);
							break;
						case FORESTCOM4:
							type.setForestcom(true);
							break;
						case FORESTCOM5:
							type.setForestcom(true);
							break;
						case MOUNTAINREC1:
							type.setMountainrec(true);
							break;
						case MOUNTAINREC2:
							type.setMountainrec(true);
							break;
						case MOUNTAINREC3:
							type.setMountainrec(true);
							break;
						case MOUNTAINREC4:
							type.setMountainrec(true);
							break;
						case MOUNTAINREC5:
							type.setMountainrec(true);
							break;
						case MOUNTAINCOM1:
							type.setMountaincom(true);
							break;
						case MOUNTAINCOM2:
							type.setMountaincom(true);
							break;
						case MOUNTAINCOM3:
							type.setMountaincom(true);
							break;
						case MOUNTAINCOM4:
							type.setMountaincom(true);
							break;
						case MOUNTAINCOM5:
							type.setMountaincom(true);
							break;
						case SWAMPREC1:
							type.setSwamprec(true);
							break;
						case SWAMPREC2:
							type.setSwamprec(true);
							break;
						case SWAMPREC3:
							type.setSwamprec(true);
							break;
						case SWAMPREC4:
							type.setSwamprec(true);
							break;
						case SWAMPREC5:
							type.setSwamprec(true);
							break;
						case SWAMPCOM1:
							type.setSwampcom(true);
							break;
						case SWAMPCOM2:
							type.setSwampcom(true);
							break;
						case SWAMPCOM3:
							type.setSwampcom(true);
							break;
						case SWAMPCOM4:
							type.setSwampcom(true);
							break;
						case SWAMPCOM5:
							type.setSwampcom(true);
							break;
						case WASTEREC1:
							type.setWasterec(true);
							break;
						case WASTEREC2:
							type.setWasterec(true);
							break;
						case WASTEREC3:
							type.setWasterec(true);
							break;
						case WASTEREC4:
							type.setWasterec(true);
							break;
						case WASTEREC5:
							type.setWasterec(true);
							break;
						case WASTECOM1:
							type.setWastecom(true);
							break;
						case WASTECOM2:
							type.setWastecom(true);
							break;
						case WASTECOM3:
							type.setWastecom(true);
							break;
						case WASTECOM4:
							type.setWastecom(true);
							break;
						case WASTECOM5:
							type.setWastecom(true);
							break;
						case CAVEREC1:
							type.setCaverec(true);
							break;
						case CAVEREC2:
							type.setCaverec(true);
							break;
						case CAVEREC3:
							type.setCaverec(true);
							break;
						case CAVEREC4:
							type.setCaverec(true);
							break;
						case CAVEREC5:
							type.setCaverec(true);
							break;
						case CAVECOM1:
							type.setCavecom(true);
							break;
						case CAVECOM2:
							type.setCavecom(true);
							break;
						case CAVECOM3:
							type.setCavecom(true);
							break;
						case CAVECOM4:
							type.setCavecom(true);
							break;
						case CAVECOM5:
							type.setCavecom(true);
							break;
						case ADDGOD1:
							type.setAddgod(true);
							break;
						case ADDGOD2:
							type.setAddgod(true);
							break;
						case ADDGOD3:
							type.setAddgod(true);
							break;
						case ADDGOD4:
							type.setAddgod(true);
							break;
						case ADDGOD5:
							type.setAddgod(true);
							break;
						case ADDGOD6:
							type.setAddgod(true);
							break;
						case ADDGOD7:
							type.setAddgod(true);
							break;
						case ADDGOD8:
							type.setAddgod(true);
							break;
						case ADDGOD9:
							type.setAddgod(true);
							break;
						case ADDGOD10:
							type.setAddgod(true);
							break;
						case ADDGOD11:
							type.setAddgod(true);
							break;
						case ADDGOD12:
							type.setAddgod(true);
							break;
						case ADDGOD13:
							type.setAddgod(true);
							break;
						case ADDGOD14:
							type.setAddgod(true);
							break;
						case DELGOD1:
							type.setDelgod(true);
							break;
						case DELGOD2:
							type.setDelgod(true);
							break;
						case DELGOD3:
							type.setDelgod(true);
							break;
						case DELGOD4:
							type.setDelgod(true);
							break;
						case DELGOD5:
							type.setDelgod(true);
							break;
						case DELGOD6:
							type.setDelgod(true);
							break;
						case DELGOD7:
							type.setDelgod(true);
							break;
						case DELGOD8:
							type.setDelgod(true);
							break;
						case DELGOD9:
							type.setDelgod(true);
							break;
						case DELGOD10:
							type.setDelgod(true);
							break;
						case DELGOD11:
							type.setDelgod(true);
							break;
						case DELGOD12:
							type.setDelgod(true);
							break;
						case DELGOD13:
							type.setDelgod(true);
							break;
						case DELGOD14:
							type.setDelgod(true);
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
	
	private void addInst5(final Inst inst, final XtextEditor editor, final String newName1, final String newName2, final String newName3) { 
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<NationMods> mods = ((Nation)input).getMods();
						NationInst5 type = DmFactory.eINSTANCE.createNationInst5();
						switch (inst) {
						case COLOR:
							type.setColor(true);
							break;
						case SECONDARYCOLOR:
							type.setSecondarycolor(true);
							break;
						}
						type.setValue1(newName1);
						type.setValue2(newName2);
						type.setValue3(newName3);
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
						NationMods modToRemove = null;
						int siteCount = 0;
						int addreccom = 0;
						int addrecunit = 0;
						int addforeignunit = 0;
						int addforeigncom = 0;
						int forestrec = 0;
						int forestcom = 0;
						int mountainrec = 0;
						int mountaincom = 0;
						int swamprec = 0;
						int swampcom = 0;
						int wasterec = 0;
						int wastecom = 0;
						int caverec = 0;
						int cavecom = 0;
						int addgod = 0;
						int delgod = 0;
						EList<NationMods> mods = ((Nation)input).getMods();
						for (NationMods mod : mods) {
							if (mod instanceof NationInst1) {
								switch (inst) {
								case NAME:
									if (((NationInst1)mod).isName()){
										modToRemove = mod;
									}
									break;
								case EPITHET:
									if (((NationInst1)mod).isEpithet()){
										modToRemove = mod;
									}
									break;
								case DESCR:
									if (((NationInst1)mod).isDescr()){
										modToRemove = mod;
									}
									break;
								case SUMMARY:
									if (((NationInst1)mod).isSummary()){
										modToRemove = mod;
									}
									break;
								case BRIEF:
									if (((NationInst1)mod).isBrief()){
										modToRemove = mod;
									}
									break;
								case FLAG:
									if (((NationInst1)mod).isFlag()){
										modToRemove = mod;
									}
									break;
								case STARTSITE1:
									if (((NationInst1)mod).isStartsite()){
										siteCount++;
										if (siteCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case STARTSITE2:
									if (((NationInst1)mod).isStartsite()){
										siteCount++;
										if (siteCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case STARTSITE3:
									if (((NationInst1)mod).isStartsite()){
										siteCount++;
										if (siteCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case STARTSITE4:
									if (((NationInst1)mod).isStartsite()){
										siteCount++;
										if (siteCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								}
							}
							if (mod instanceof NationInst2) {
								switch (inst) {
								case ERA:
									if (((NationInst2)mod).isEra()){
										modToRemove = mod;
									}
									break;
								case LIKESTERR:
									if (((NationInst2)mod).isLikesterr()){
										modToRemove = mod;
									}
									break;
								case LABCOST:
									if (((NationInst2)mod).isLabcost()){
										modToRemove = mod;
									}
									break;
								case TEMPLECOST:
									if (((NationInst2)mod).isTemplecost()){
										modToRemove = mod;
									}
									break;
								case TEMPLEPIC:
									if (((NationInst2)mod).isTemplepic()){
										modToRemove = mod;
									}
									break;
								case STARTUNITNBRS1:
									if (((NationInst2)mod).isStartunitnbrs1()){
										modToRemove = mod;
									}
									break;
								case STARTUNITNBRS2:
									if (((NationInst2)mod).isStartunitnbrs2()){
										modToRemove = mod;
									}
									break;
								case HERO1:
									if (((NationInst2)mod).isHero1()){
										modToRemove = mod;
									}
									break;
								case HERO2:
									if (((NationInst2)mod).isHero2()){
										modToRemove = mod;
									}
									break;
								case HERO3:
									if (((NationInst2)mod).isHero3()){
										modToRemove = mod;
									}
									break;
								case HERO4:
									if (((NationInst2)mod).isHero4()){
										modToRemove = mod;
									}
									break;
								case HERO5:
									if (((NationInst2)mod).isHero5()){
										modToRemove = mod;
									}
									break;
								case HERO6:
									if (((NationInst2)mod).isHero6()){
										modToRemove = mod;
									}
									break;
								case MULTIHERO1:
									if (((NationInst2)mod).isMultihero1()){
										modToRemove = mod;
									}
									break;
								case MULTIHERO2:
									if (((NationInst2)mod).isMultihero2()){
										modToRemove = mod;
									}
									break;
								case DEFMULT1:
									if (((NationInst2)mod).isDefmult1()){
										modToRemove = mod;
									}
									break;
								case DEFMULT1B:
									if (((NationInst2)mod).isDefmult1b()){
										modToRemove = mod;
									}
									break;
								case DEFMULT2:
									if (((NationInst2)mod).isDefmult2()){
										modToRemove = mod;
									}
									break;
								case DEFMULT2B:
									if (((NationInst2)mod).isDefmult2b()){
										modToRemove = mod;
									}
									break;
								case IDEALCOLD:
									if (((NationInst2)mod).isIdealcold()){
										modToRemove = mod;
									}
									break;
								case CASTLEPROD:
									if (((NationInst2)mod).isCastleprod()){
										modToRemove = mod;
									}
									break;
								case DOMKILL:
									if (((NationInst2)mod).isDomkill()){
										modToRemove = mod;
									}
									break;
								case DOMUNREST:
									if (((NationInst2)mod).isDomunrest()){
										modToRemove = mod;
									}
									break;
								case MERCCOST:
									if (((NationInst2)mod).isMerccost()){
										modToRemove = mod;
									}
									break;
								case HOMEREALM:
									if (((NationInst2)mod).isHomerealm()){
										modToRemove = mod;
									}
									break;
								case LIKESPOP:
									if (((NationInst2)mod).isLikespop()){
										modToRemove = mod;
									}
									break;
								case FORTERA:
									if (((NationInst2)mod).isFortera()){
										modToRemove = mod;
									}
									break;
								case FORTCOST:
									if (((NationInst2)mod).isFortcost()){
										modToRemove = mod;
									}
									break;
								case SPREADCOLD:
									if (((NationInst2)mod).isSpreadcold()){
										modToRemove = mod;
									}
									break;
								case SPREADHEAT:
									if (((NationInst2)mod).isSpreadheat()){
										modToRemove = mod;
									}
									break;
								case SPREADCHAOS:
									if (((NationInst2)mod).isSpreadchaos()){
										modToRemove = mod;
									}
									break;
								case SPREADLAZY:
									if (((NationInst2)mod).isSpreadlazy()){
										modToRemove = mod;
									}
									break;
								case SPREADDEATH:
									if (((NationInst2)mod).isSpreaddeath()){
										modToRemove = mod;
									}
									break;
								case GOLEMHP:
									if (((NationInst2)mod).isGolemhp()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof NationInst3) {
								switch (inst) {
								case CLEARNATION:
									if (((NationInst3)mod).isClearnation()){
										modToRemove = mod;
									}
									break;
								case CLEARREC:
									if (((NationInst3)mod).isClearrec()){
										modToRemove = mod;
									}
									break;
								case CLEARSITES:
									if (((NationInst3)mod).isClearsites()){
										modToRemove = mod;
									}
									break;
								case CLEARGODS:
									if (((NationInst3)mod).isCleargods()){
										modToRemove = mod;
									}
									break;
								case UWNATION:
									if (((NationInst3)mod).isUwnation()){
										modToRemove = mod;
									}
									break;
								case BLOODNATION:
									if (((NationInst3)mod).isBloodnation()){
										modToRemove = mod;
									}
									break;
								case NOPREACH:
									if (((NationInst3)mod).isNopreach()){
										modToRemove = mod;
									}
									break;
								case DYINGDOM:
									if (((NationInst3)mod).isDyingdom()){
										modToRemove = mod;
									}
									break;
								case SACRIFICEDOM:
									if (((NationInst3)mod).isSacrificedom()){
										modToRemove = mod;
									}
									break;
								case NODEATHSUPPLY:
									if (((NationInst3)mod).isNodeathsupply()){
										modToRemove = mod;
									}
									break;
								case AUTOUNDEAD:
									if (((NationInst3)mod).isAutoundead()){
										modToRemove = mod;
									}
									break;
								case ZOMBIEREANIM:
									if (((NationInst3)mod).isZombiereanim()){
										modToRemove = mod;
									}
									break;
								case HORSEREANIM:
									if (((NationInst3)mod).isHorsereanim()){
										modToRemove = mod;
									}
									break;
								case WIGHTREANIM:
									if (((NationInst3)mod).isWightreanim()){
										modToRemove = mod;
									}
									break;
								case MANIKINREANIM:
									if (((NationInst3)mod).isManikinreanim()){
										modToRemove = mod;
									}
									break;
								case TOMBWYRMREANIM:
									if (((NationInst3)mod).isTombwyrmreanim()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof NationInst4) {
								switch (inst) {
								case STARTCOM:
									if (((NationInst4)mod).isStartcom()){
										modToRemove = mod;
									}
									break;
								case STARTSCOUT:
									if (((NationInst4)mod).isStartscout()){
										modToRemove = mod;
									}
									break;
								case STARTUNITTYPE1:
									if (((NationInst4)mod).isStartunittype1()){
										modToRemove = mod;
									}
									break;
								case STARTUNITTYPE2:
									if (((NationInst4)mod).isStartunittype2()){
										modToRemove = mod;
									}
									break;
								case ADDRECUNIT1:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 1) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT2:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 2) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT3:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 3) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT4:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT5:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT6:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 6) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT7:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 7) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT8:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 8) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT9:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 9) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT10:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 10) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT11:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 11) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT12:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 12) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT13:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 13) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECUNIT14:
									if (((NationInst4)mod).isAddrecunit()){
										addrecunit++;
										if (addrecunit == 14) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM1:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM2:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM3:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM4:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM5:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM6:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 6) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM7:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 7) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM8:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 8) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM9:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 9) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM10:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 10) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM11:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 11) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM12:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 12) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM13:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 13) {
											modToRemove = mod;
										}
									}
									break;
								case ADDRECCOM14:
									if (((NationInst4)mod).isAddreccom()){
										addreccom++;
										if (addreccom == 14) {
											modToRemove = mod;
										}
									}
									break;
								case UWUNIT1:
									if (((NationInst4)mod).isUwunit1()){
										modToRemove = mod;
									}
									break;
								case UWUNIT2:
									if (((NationInst4)mod).isUwunit2()){
										modToRemove = mod;
									}
									break;
								case UWUNIT3:
									if (((NationInst4)mod).isUwunit3()){
										modToRemove = mod;
									}
									break;
								case UWUNIT4:
									if (((NationInst4)mod).isUwunit4()){
										modToRemove = mod;
									}
									break;
								case UWUNIT5:
									if (((NationInst4)mod).isUwunit5()){
										modToRemove = mod;
									}
									break;
								case UWCOM1:
									if (((NationInst4)mod).isUwcom1()){
										modToRemove = mod;
									}
									break;
								case UWCOM2:
									if (((NationInst4)mod).isUwcom2()){
										modToRemove = mod;
									}
									break;
								case UWCOM3:
									if (((NationInst4)mod).isUwcom3()){
										modToRemove = mod;
									}
									break;
								case UWCOM4:
									if (((NationInst4)mod).isUwcom4()){
										modToRemove = mod;
									}
									break;
								case UWCOM5:
									if (((NationInst4)mod).isUwcom5()){
										modToRemove = mod;
									}
									break;
								case DEFCOM1:
									if (((NationInst4)mod).isDefcom1()){
										modToRemove = mod;
									}
									break;
								case DEFCOM2:
									if (((NationInst4)mod).isDefcom2()){
										modToRemove = mod;
									}
									break;
								case DEFUNIT1:
									if (((NationInst4)mod).isDefunit1()){
										modToRemove = mod;
									}
									break;
								case DEFUNIT1B:
									if (((NationInst4)mod).isDefunit1b()){
										modToRemove = mod;
									}
									break;
								case DEFUNIT2:
									if (((NationInst4)mod).isDefunit2()){
										modToRemove = mod;
									}
									break;
								case DEFUNIT2B:
									if (((NationInst4)mod).isDefunit2b()){
										modToRemove = mod;
									}
									break;
								case ADDFOREIGNUNIT1:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 1) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT2:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 2) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT3:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 3) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT4:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT5:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT6:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 6) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT7:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 7) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT8:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 8) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT9:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 9) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT10:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 10) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT11:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 11) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT12:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 12) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT13:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 13) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNUNIT14:
									if (((NationInst4)mod).isAddforeignunit()){
										addforeignunit++;
										if (addforeignunit == 14) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM1:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM2:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM3:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM4:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM5:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM6:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 6) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM7:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 7) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM8:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 8) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM9:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 9) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM10:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 10) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM11:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 11) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM12:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 12) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM13:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 13) {
											modToRemove = mod;
										}
									}
									break;
								case ADDFOREIGNCOM14:
									if (((NationInst4)mod).isAddforeigncom()){
										addforeigncom++;
										if (addforeigncom == 14) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTREC1:
									if (((NationInst4)mod).isForestrec()){
										forestrec++;
										if (forestrec == 1) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTREC2:
									if (((NationInst4)mod).isForestrec()){
										forestrec++;
										if (forestrec == 2) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTREC3:
									if (((NationInst4)mod).isForestrec()){
										forestrec++;
										if (forestrec == 3) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTREC4:
									if (((NationInst4)mod).isForestrec()){
										forestrec++;
										if (forestrec == 4) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTREC5:
									if (((NationInst4)mod).isForestrec()){
										forestrec++;
										if (forestrec == 5) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTCOM1:
									if (((NationInst4)mod).isForestcom()){
										forestcom++;
										if (forestcom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTCOM2:
									if (((NationInst4)mod).isForestcom()){
										forestcom++;
										if (forestcom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTCOM3:
									if (((NationInst4)mod).isForestcom()){
										forestcom++;
										if (forestcom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTCOM4:
									if (((NationInst4)mod).isForestcom()){
										forestcom++;
										if (forestcom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case FORESTCOM5:
									if (((NationInst4)mod).isForestcom()){
										forestcom++;
										if (forestcom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINREC1:
									if (((NationInst4)mod).isMountainrec()){
										mountainrec++;
										if (mountainrec == 1) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINREC2:
									if (((NationInst4)mod).isMountainrec()){
										mountainrec++;
										if (mountainrec == 2) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINREC3:
									if (((NationInst4)mod).isMountainrec()){
										mountainrec++;
										if (mountainrec == 3) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINREC4:
									if (((NationInst4)mod).isMountainrec()){
										mountainrec++;
										if (mountainrec == 4) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINREC5:
									if (((NationInst4)mod).isMountainrec()){
										mountainrec++;
										if (mountainrec == 5) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINCOM1:
									if (((NationInst4)mod).isMountaincom()){
										mountaincom++;
										if (mountaincom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINCOM2:
									if (((NationInst4)mod).isMountaincom()){
										mountaincom++;
										if (mountaincom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINCOM3:
									if (((NationInst4)mod).isMountaincom()){
										mountaincom++;
										if (mountaincom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINCOM4:
									if (((NationInst4)mod).isMountaincom()){
										mountaincom++;
										if (mountaincom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case MOUNTAINCOM5:
									if (((NationInst4)mod).isMountaincom()){
										mountaincom++;
										if (mountaincom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPREC1:
									if (((NationInst4)mod).isSwamprec()){
										swamprec++;
										if (swamprec == 1) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPREC2:
									if (((NationInst4)mod).isSwamprec()){
										swamprec++;
										if (swamprec == 2) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPREC3:
									if (((NationInst4)mod).isSwamprec()){
										swamprec++;
										if (swamprec == 3) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPREC4:
									if (((NationInst4)mod).isSwamprec()){
										swamprec++;
										if (swamprec == 4) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPREC5:
									if (((NationInst4)mod).isSwamprec()){
										swamprec++;
										if (swamprec == 5) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPCOM1:
									if (((NationInst4)mod).isSwampcom()){
										swampcom++;
										if (swampcom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPCOM2:
									if (((NationInst4)mod).isSwampcom()){
										swampcom++;
										if (swampcom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPCOM3:
									if (((NationInst4)mod).isSwampcom()){
										swampcom++;
										if (swampcom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPCOM4:
									if (((NationInst4)mod).isSwampcom()){
										swampcom++;
										if (swampcom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case SWAMPCOM5:
									if (((NationInst4)mod).isSwampcom()){
										swampcom++;
										if (swampcom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case WASTEREC1:
									if (((NationInst4)mod).isWasterec()){
										wasterec++;
										if (wasterec == 1) {
											modToRemove = mod;
										}
									}
									break;
								case WASTEREC2:
									if (((NationInst4)mod).isWasterec()){
										wasterec++;
										if (wasterec == 2) {
											modToRemove = mod;
										}
									}
									break;
								case WASTEREC3:
									if (((NationInst4)mod).isWasterec()){
										wasterec++;
										if (wasterec == 3) {
											modToRemove = mod;
										}
									}
									break;
								case WASTEREC4:
									if (((NationInst4)mod).isWasterec()){
										wasterec++;
										if (wasterec == 4) {
											modToRemove = mod;
										}
									}
									break;
								case WASTEREC5:
									if (((NationInst4)mod).isWasterec()){
										wasterec++;
										if (wasterec == 5) {
											modToRemove = mod;
										}
									}
									break;
								case WASTECOM1:
									if (((NationInst4)mod).isWastecom()){
										wastecom++;
										if (wastecom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case WASTECOM2:
									if (((NationInst4)mod).isWastecom()){
										wastecom++;
										if (wastecom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case WASTECOM3:
									if (((NationInst4)mod).isWastecom()){
										wastecom++;
										if (wastecom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case WASTECOM4:
									if (((NationInst4)mod).isWastecom()){
										wastecom++;
										if (wastecom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case WASTECOM5:
									if (((NationInst4)mod).isWastecom()){
										wastecom++;
										if (wastecom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case CAVEREC1:
									if (((NationInst4)mod).isCaverec()){
										caverec++;
										if (caverec == 1) {
											modToRemove = mod;
										}
									}
									break;
								case CAVEREC2:
									if (((NationInst4)mod).isCaverec()){
										caverec++;
										if (caverec == 2) {
											modToRemove = mod;
										}
									}
									break;
								case CAVEREC3:
									if (((NationInst4)mod).isCaverec()){
										caverec++;
										if (caverec == 3) {
											modToRemove = mod;
										}
									}
									break;
								case CAVEREC4:
									if (((NationInst4)mod).isCaverec()){
										caverec++;
										if (caverec == 4) {
											modToRemove = mod;
										}
									}
									break;
								case CAVEREC5:
									if (((NationInst4)mod).isCaverec()){
										caverec++;
										if (caverec == 5) {
											modToRemove = mod;
										}
									}
									break;
								case CAVECOM1:
									if (((NationInst4)mod).isCavecom()){
										cavecom++;
										if (cavecom == 1) {
											modToRemove = mod;
										}
									}
									break;
								case CAVECOM2:
									if (((NationInst4)mod).isCavecom()){
										cavecom++;
										if (cavecom == 2) {
											modToRemove = mod;
										}
									}
									break;
								case CAVECOM3:
									if (((NationInst4)mod).isCavecom()){
										cavecom++;
										if (cavecom == 3) {
											modToRemove = mod;
										}
									}
									break;
								case CAVECOM4:
									if (((NationInst4)mod).isCavecom()){
										cavecom++;
										if (cavecom == 4) {
											modToRemove = mod;
										}
									}
									break;
								case CAVECOM5:
									if (((NationInst4)mod).isCavecom()){
										cavecom++;
										if (cavecom == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD1:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 1) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD2:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 2) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD3:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 3) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD4:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD5:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 5) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD6:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 6) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD7:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 7) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD8:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 8) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD9:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 9) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD10:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 10) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD11:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 11) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD12:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 12) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD13:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 13) {
											modToRemove = mod;
										}
									}
									break;
								case ADDGOD14:
									if (((NationInst4)mod).isAddgod()){
										addgod++;
										if (addgod == 14) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD1:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 1) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD2:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 2) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD3:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 3) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD4:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 4) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD5:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 5) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD6:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 6) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD7:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 7) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD8:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 8) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD9:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 9) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD10:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 10) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD11:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 11) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD12:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 12) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD13:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 13) {
											modToRemove = mod;
										}
									}
									break;
								case DELGOD14:
									if (((NationInst4)mod).isDelgod()){
										delgod++;
										if (delgod == 14) {
											modToRemove = mod;
										}
									}
									break;
								}
							}
							if (mod instanceof NationInst5) {
								switch (inst) {
								case COLOR:
									if (((NationInst5)mod).isColor()){
										modToRemove = mod;
									}
									break;
								case SECONDARYCOLOR:
									if (((NationInst5)mod).isSecondarycolor()){
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
