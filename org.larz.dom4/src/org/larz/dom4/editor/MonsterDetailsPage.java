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
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.text.DecimalFormat;
import java.text.Format;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.compress.archivers.zip.ZipFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
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
import org.larz.dom4.Activator;
import org.larz.dom4.db.Database;
import org.larz.dom4.db.MonsterDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.Monster;
import org.larz.dom4.dm.dm.MonsterInst1;
import org.larz.dom4.dm.dm.MonsterInst2;
import org.larz.dom4.dm.dm.MonsterInst3;
import org.larz.dom4.dm.dm.MonsterInst4;
import org.larz.dom4.dm.dm.MonsterInst5;
import org.larz.dom4.dm.dm.MonsterInst6;
import org.larz.dom4.dm.dm.MonsterMods;
import org.larz.dom4.dm.dm.SelectMonsterById;
import org.larz.dom4.dm.dm.SelectMonsterByName;
import org.larz.dom4.dm.ui.editor.DmXtextEditor;
import org.larz.dom4.dm.ui.help.HelpTextHelper;
import org.larz.dom4.image.ImageConverter;
import org.larz.dom4.image.ImageLoader;

public class MonsterDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;
	private Text descr;
	private Button descCheck;
	private Text spr1;
	private Button spr1Check;
	private Button spr1Browse;
	private Text spr2;
	private Button spr2Check;
	private Button spr2Browse;
	private Label sprite1Label;
	private Label sprite2Label;

	enum Inst {
		NAME (Messages.getString("MonsterDetailsSection.mod.name"), ""),
		SPR1 (Messages.getString("MonsterDetailsSection.mod.spr1"), ""),
		SPR2 (Messages.getString("MonsterDetailsSection.mod.spr2"), ""),
		SPECIALLOOK (Messages.getString("MonsterDetailsSection.mod.speciallook"), "1"),
		DESCR (Messages.getString("MonsterDetailsSection.mod.descr"), ""),
		AP (Messages.getString("MonsterDetailsSection.mod.ap"), "12"),
		MAPMOVE (Messages.getString("MonsterDetailsSection.mod.mapmove"), "1"),
		HP (Messages.getString("MonsterDetailsSection.mod.hp"), "10"),
		PROT (Messages.getString("MonsterDetailsSection.mod.prot"), "0"),
		SIZE (Messages.getString("MonsterDetailsSection.mod.size"), "2"),
		RESSIZE (Messages.getString("MonsterDetailsSection.mod.ressize"), "1"),
		STR (Messages.getString("MonsterDetailsSection.mod.str"), "10"),
		ENC (Messages.getString("MonsterDetailsSection.mod.enc"), "3"),
		ATT (Messages.getString("MonsterDetailsSection.mod.att"), "10"),
		DEF (Messages.getString("MonsterDetailsSection.mod.def"), "10"),
		PREC (Messages.getString("MonsterDetailsSection.mod.prec"), "10"),
		MR (Messages.getString("MonsterDetailsSection.mod.mr"), "10"),
		MOR (Messages.getString("MonsterDetailsSection.mod.mor"), "10"),
		GCOST (Messages.getString("MonsterDetailsSection.mod.gcost"), "10"),
		RCOST (Messages.getString("MonsterDetailsSection.mod.rcost"), "1"),
		PATHCOST (Messages.getString("MonsterDetailsSection.mod.pathcost"), "10"),
		STARTDOM (Messages.getString("MonsterDetailsSection.mod.startdom"), "1"),
		WEAPON1 (Messages.getString("MonsterDetailsSection.mod.weapon"), "1"),
		WEAPON2 (Messages.getString("MonsterDetailsSection.mod.weapon"), "2"),
		WEAPON3 (Messages.getString("MonsterDetailsSection.mod.weapon"), "3"),
		WEAPON4 (Messages.getString("MonsterDetailsSection.mod.weapon"), "4"),
		ARMOR1 (Messages.getString("MonsterDetailsSection.mod.armor"), ""),
		ARMOR2 (Messages.getString("MonsterDetailsSection.mod.armor"), ""),
		ARMOR3 (Messages.getString("MonsterDetailsSection.mod.armor"), ""),
		EYES (Messages.getString("MonsterDetailsSection.mod.eyes"), "2"),
		CLEAR (Messages.getString("MonsterDetailsSection.mod.clear")),
		CLEARWEAPONS (Messages.getString("MonsterDetailsSection.mod.clearweapons")),
		CLEARARMOR (Messages.getString("MonsterDetailsSection.mod.cleararmor")),
		CLEARMAGIC (Messages.getString("MonsterDetailsSection.mod.clearmagic")),
		CLEARSPEC (Messages.getString("MonsterDetailsSection.mod.clearspec")),
		COPYSTATS (Messages.getString("MonsterDetailsSection.mod.copystats"), "0"),
		COPYSPR (Messages.getString("MonsterDetailsSection.mod.copyspr"), "0"),
		RESTRICTEDGOD (Messages.getString("MonsterDetailsSection.mod.restrictedgod"), "0"),
		FEMALE (Messages.getString("MonsterDetailsSection.mod.female")),
		MOUNTED (Messages.getString("MonsterDetailsSection.mod.mounted")),
		HOLY (Messages.getString("MonsterDetailsSection.mod.holy")),
		ANIMAL (Messages.getString("MonsterDetailsSection.mod.animal")),
		UNDEAD (Messages.getString("MonsterDetailsSection.mod.undead")),
		DEMON (Messages.getString("MonsterDetailsSection.mod.demon")),
		MAGICBEING (Messages.getString("MonsterDetailsSection.mod.magicbeing")),
		STONEBEING (Messages.getString("MonsterDetailsSection.mod.stonebeing")),
		INANIMATE (Messages.getString("MonsterDetailsSection.mod.inanimate")),
		COLDBLOOD (Messages.getString("MonsterDetailsSection.mod.coldblood")),
		IMMORTAL (Messages.getString("MonsterDetailsSection.mod.immortal")),
		BLIND (Messages.getString("MonsterDetailsSection.mod.blind")),
		UNIQUE (Messages.getString("MonsterDetailsSection.mod.unique")),
		SHATTEREDSOUL (Messages.getString("MonsterDetailsSection.mod.shatteredsoul"), "10"),
		IMMOBILE (Messages.getString("MonsterDetailsSection.mod.immobile")),
		AQUATIC (Messages.getString("MonsterDetailsSection.mod.aquatic")),
		AMPHIBIAN (Messages.getString("MonsterDetailsSection.mod.amphibian")),
		POORAMPHIBIAN (Messages.getString("MonsterDetailsSection.mod.pooramphibian")),
		FLYING (Messages.getString("MonsterDetailsSection.mod.flying")),
		STORMIMMUNE (Messages.getString("MonsterDetailsSection.mod.stormimmune")),
		SAILING (Messages.getString("MonsterDetailsSection.mod.sailing")),
		FORESTSURVIVAL (Messages.getString("MonsterDetailsSection.mod.forestsurvival")),
		MOUNTAINSURVIVAL (Messages.getString("MonsterDetailsSection.mod.mountainsurvival")),
		SWAMPSURVIVAL (Messages.getString("MonsterDetailsSection.mod.swampsurvival")),
		WASTESURVIVAL (Messages.getString("MonsterDetailsSection.mod.wastesurvival")),
		COLDRES (Messages.getString("MonsterDetailsSection.mod.coldres"), "100"),
		FIRERES (Messages.getString("MonsterDetailsSection.mod.fireres"), "100"),
		POISONRES (Messages.getString("MonsterDetailsSection.mod.poisonres"), "100"),
		SHOCKRES (Messages.getString("MonsterDetailsSection.mod.shockres"), "100"),
		DARKVISION (Messages.getString("MonsterDetailsSection.mod.darkvision"), "100"),
		STEALTHY (Messages.getString("MonsterDetailsSection.mod.stealthy"), "0"),
		ILLUSION (Messages.getString("MonsterDetailsSection.mod.illusion")),
		SPY (Messages.getString("MonsterDetailsSection.mod.spy")),
		ASSASSIN (Messages.getString("MonsterDetailsSection.mod.assassin")),
		SEDUCE (Messages.getString("MonsterDetailsSection.mod.seduce"), "10"),
		SUCCUBUS (Messages.getString("MonsterDetailsSection.mod.succubus"), "10"),
		BECKON (Messages.getString("MonsterDetailsSection.mod.beckon"), "10"),
		STARTAGE (Messages.getString("MonsterDetailsSection.mod.startage"), "20"),
		MAXAGE (Messages.getString("MonsterDetailsSection.mod.maxage"), "50"),
		OLDER (Messages.getString("MonsterDetailsSection.mod.older"), "10"),
		HEALER (Messages.getString("MonsterDetailsSection.mod.healer"), "10"),
		HEAL (Messages.getString("MonsterDetailsSection.mod.heal")),
		NOHEAL (Messages.getString("MonsterDetailsSection.mod.noheal")),
		STARTAFF (Messages.getString("MonsterDetailsSection.mod.startaff"), "10"),
		SUPPLYBONUS (Messages.getString("MonsterDetailsSection.mod.supplybonus"), "10"),
		NEEDNOTEAT (Messages.getString("MonsterDetailsSection.mod.neednoteat")),
		UWDAMAGE (Messages.getString("MonsterDetailsSection.mod.uwdamage"), "10"),
		HOMESICK (Messages.getString("MonsterDetailsSection.mod.homesick"), "10"),
		COLDPOWER (Messages.getString("MonsterDetailsSection.mod.coldpower"), "10"),
		FIREPOWER (Messages.getString("MonsterDetailsSection.mod.firepower"), "10"),
		STORMPOWER (Messages.getString("MonsterDetailsSection.mod.stormpower"), "10"),
		DARKPOWER (Messages.getString("MonsterDetailsSection.mod.darkpower"), "10"),
		SPRINGPOWER (Messages.getString("MonsterDetailsSection.mod.springpower"), "10"),
		SUMMERPOWER (Messages.getString("MonsterDetailsSection.mod.summerpower"), "10"),
		FALLPOWER (Messages.getString("MonsterDetailsSection.mod.fallpower"), "10"),
		WINTERPOWER (Messages.getString("MonsterDetailsSection.mod.winterpower"), "10"),
		AMBIDEXTROUS (Messages.getString("MonsterDetailsSection.mod.ambidextrous"), "2"),
		BANEFIRESHIELD (Messages.getString("MonsterDetailsSection.mod.banefireshield"), "8"),
		BERSERK (Messages.getString("MonsterDetailsSection.mod.berserk"), "3"),
		ETHEREAL (Messages.getString("MonsterDetailsSection.mod.ethereal")),
		STANDARD (Messages.getString("MonsterDetailsSection.mod.standard"), "3"),
		ANIMALAWE (Messages.getString("MonsterDetailsSection.mod.animalawe"), "1"),
		AWE (Messages.getString("MonsterDetailsSection.mod.awe"), "1"),
		FEAR (Messages.getString("MonsterDetailsSection.mod.fear"), "0"),
		REGENERATION (Messages.getString("MonsterDetailsSection.mod.regeneration"), "10"),
		REINVIGORATION (Messages.getString("MonsterDetailsSection.mod.reinvigoration"), "10"),
		FIRESHIELD (Messages.getString("MonsterDetailsSection.mod.fireshield"), "8"),
		HEAT (Messages.getString("MonsterDetailsSection.mod.heat"), "3"),
		COLD (Messages.getString("MonsterDetailsSection.mod.cold"), "3"),
		ICEPROT (Messages.getString("MonsterDetailsSection.mod.iceprot"), "2"),
		TRAMPLE (Messages.getString("MonsterDetailsSection.mod.trample")),
		ENTANGLE (Messages.getString("MonsterDetailsSection.mod.entangle")),
		EYELOSS (Messages.getString("MonsterDetailsSection.mod.eyeloss")),
		HORRORMARK (Messages.getString("MonsterDetailsSection.mod.horrormark")),
		POISONARMOR (Messages.getString("MonsterDetailsSection.mod.poisonarmor")),
		POISONCLOUD (Messages.getString("MonsterDetailsSection.mod.poisoncloud"), "6"),
		DISEASECLOUD (Messages.getString("MonsterDetailsSection.mod.diseasecloud"), "6"),
		BLOODVENGEANCE (Messages.getString("MonsterDetailsSection.mod.bloodvengeance"), "1"),
		CASTLEDEF (Messages.getString("MonsterDetailsSection.mod.castledef"), "10"),
		SIEGEBONUS (Messages.getString("MonsterDetailsSection.mod.siegebonus"), "10"),
		PATROLBONUS (Messages.getString("MonsterDetailsSection.mod.patrolbonus"), "10"),
		PILLAGEBONUS (Messages.getString("MonsterDetailsSection.mod.pillagebonus"), "10"),
		RESEARCHBONUS (Messages.getString("MonsterDetailsSection.mod.researchbonus"), "4"),
		FORGEBONUS (Messages.getString("MonsterDetailsSection.mod.forgebonus"), "10"),
		DOUSE (Messages.getString("MonsterDetailsSection.mod.douse"), "1"),
		NOBADEVENTS (Messages.getString("MonsterDetailsSection.mod.nobadevents"), "10"),
		INCUNREST (Messages.getString("MonsterDetailsSection.mod.incunrest"), "10"),
		SPREADDOM (Messages.getString("MonsterDetailsSection.mod.spreaddom"), "10"),
		LEPER (Messages.getString("MonsterDetailsSection.mod.leper"), "10"),
		POPKILL (Messages.getString("MonsterDetailsSection.mod.popkill"), "10"),
		INQUISITOR (Messages.getString("MonsterDetailsSection.mod.inquisitor")),
		HERETIC (Messages.getString("MonsterDetailsSection.mod.heretic"), "1"),
		ITEMSLOTS (Messages.getString("MonsterDetailsSection.mod.itemslots"), "15494"),
		NOITEM (Messages.getString("MonsterDetailsSection.mod.noitem")),
		MAGICSKILL1 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC1 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL2 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC2 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL3 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC3 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL4 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC4 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL5 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC5 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL6 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC6 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL7 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC7 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICSKILL8 (Messages.getString("MonsterDetailsSection.mod.magicskill"), "0", "1"),
		CUSTOMMAGIC8 (Messages.getString("MonsterDetailsSection.mod.custommagic"), "128", "100"),
		MAGICBOOST1 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD1 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST2 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD2 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST3 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD3 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST4 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD4 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST5 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD5 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST6 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD6 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST7 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD7 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		MAGICBOOST8 (Messages.getString("MonsterDetailsSection.mod.magicboost"), "0", "1"),
		GEMPROD8 (Messages.getString("MonsterDetailsSection.mod.gemprod"), "0", "1"),
		ONEBATTLESPELL (Messages.getString("MonsterDetailsSection.mod.onebattlespell"), ""),
		DRAINIMMUNE (Messages.getString("MonsterDetailsSection.mod.drainimmune"), ""),
		FIRSTSHAPE (Messages.getString("MonsterDetailsSection.mod.firstshape"), ""),
		SECONDSHAPE (Messages.getString("MonsterDetailsSection.mod.secondshape"), ""),
		SECONDTMPSHAPE (Messages.getString("MonsterDetailsSection.mod.secondtmpshape"), ""),
		SHAPECHANGE (Messages.getString("MonsterDetailsSection.mod.shapechange"), ""),
		LANDSHAPE (Messages.getString("MonsterDetailsSection.mod.landshape"), ""),
		WATERSHAPE (Messages.getString("MonsterDetailsSection.mod.watershape"), ""),
		FORESTSHAPE (Messages.getString("MonsterDetailsSection.mod.forestshape"), ""),
		PLAINSHAPE (Messages.getString("MonsterDetailsSection.mod.plainshape"), ""),
		DOMSUMMON (Messages.getString("MonsterDetailsSection.mod.domsummon"), ""),
		DOMSUMMON2 (Messages.getString("MonsterDetailsSection.mod.domsummon2"), ""),
		DOMSUMMON20 (Messages.getString("MonsterDetailsSection.mod.domsummon20"), ""),
		MAKEMONSTER1 (Messages.getString("MonsterDetailsSection.mod.makemonster1"), ""),
		MAKEMONSTER2 (Messages.getString("MonsterDetailsSection.mod.makemonster2"), ""),
		MAKEMONSTER3 (Messages.getString("MonsterDetailsSection.mod.makemonster3"), ""),
		MAKEMONSTER4 (Messages.getString("MonsterDetailsSection.mod.makemonster4"), ""),
		MAKEMONSTER5 (Messages.getString("MonsterDetailsSection.mod.makemonster5"), ""),
		SUMMON1 (Messages.getString("MonsterDetailsSection.mod.summon1"), ""),
		SUMMON5 (Messages.getString("MonsterDetailsSection.mod.summon5"), ""),
		NAMETYPE (Messages.getString("MonsterDetailsSection.mod.nametype"), "100"),
		NOLEADER (Messages.getString("MonsterDetailsSection.mod.noleader")),
		POORLEADER (Messages.getString("MonsterDetailsSection.mod.poorleader")),
		OKLEADER (Messages.getString("MonsterDetailsSection.mod.okleader")),
		GOODLEADER (Messages.getString("MonsterDetailsSection.mod.goodleader")),
		EXPERTLEADER (Messages.getString("MonsterDetailsSection.mod.expertleader")),
		SUPERIORLEADER (Messages.getString("MonsterDetailsSection.mod.superiorleader")),
		NOMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.nomagicleader")),
		POORMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.poormagicleader")),
		OKMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.okmagicleader")),
		GOODMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.goodmagicleader")),
		EXPERTMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.expertmagicleader")),
		SUPERIORMAGICLEADER (Messages.getString("MonsterDetailsSection.mod.superiormagicleader")),
		NOUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.noundeadleader")),
		POORUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.poorundeadleader")),
		OKUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.okundeadleader")),
		GOODUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.goodundeadleader")),
		EXPERTUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.expertundeadleader")),
		SUPERIORUNDEADLEADER (Messages.getString("MonsterDetailsSection.mod.superiorundeadleader"));
		
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
		private Label defaultLabel;
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
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst7Fields implements InstFields {
		private Button check;
		private Text value1;
		private Text value2;
		private Label defaultLabel1;
		private Label defaultLabel2;
	}
	
	class Inst8Fields implements InstFields {
		private Button check;
		private MappedDynamicCombo value1;
		private MappedDynamicCombo value2;
		private MappedDynamicCombo value3;
		private MappedDynamicCombo value4;
		private MappedDynamicCombo value5;
		private Label defaultLabel;
	}
	
	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	private Set<List<Inst>> dynamicFields = new HashSet<List<Inst>>();
	
	public MonsterDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.SPECIALLOOK, new Inst2Fields());
		instMap.put(Inst.AP, new Inst2Fields());
		instMap.put(Inst.MAPMOVE, new Inst2Fields());
		instMap.put(Inst.HP, new Inst2Fields());
		instMap.put(Inst.PROT, new Inst2Fields());
		instMap.put(Inst.SIZE, new Inst2Fields());
		instMap.put(Inst.RESSIZE, new Inst2Fields());
		instMap.put(Inst.STR, new Inst2Fields());
		instMap.put(Inst.ENC, new Inst2Fields());
		instMap.put(Inst.ATT, new Inst2Fields());
		instMap.put(Inst.DEF, new Inst2Fields());
		instMap.put(Inst.PREC, new Inst2Fields());
		instMap.put(Inst.MR, new Inst2Fields());
		instMap.put(Inst.MOR, new Inst2Fields());
		instMap.put(Inst.GCOST, new Inst2Fields());
		instMap.put(Inst.RCOST, new Inst2Fields());
		instMap.put(Inst.PATHCOST, new Inst2Fields());
		instMap.put(Inst.STARTDOM, new Inst2Fields());
		instMap.put(Inst.EYES, new Inst2Fields());
		instMap.put(Inst.COPYSTATS, new Inst2Fields());
		instMap.put(Inst.COPYSPR, new Inst2Fields());
		instMap.put(Inst.RESTRICTEDGOD, new Inst2Fields());
		instMap.put(Inst.SHATTEREDSOUL, new Inst2Fields());
		instMap.put(Inst.COLDRES, new Inst2Fields());
		instMap.put(Inst.FIRERES, new Inst2Fields());
		instMap.put(Inst.POISONRES, new Inst2Fields());
		instMap.put(Inst.SHOCKRES, new Inst2Fields());
		instMap.put(Inst.DARKVISION, new Inst2Fields());
		instMap.put(Inst.STEALTHY, new Inst6Fields());
		instMap.put(Inst.SEDUCE, new Inst2Fields());
		instMap.put(Inst.SUCCUBUS, new Inst2Fields());
		instMap.put(Inst.BECKON, new Inst2Fields());
		instMap.put(Inst.STARTAGE, new Inst2Fields());
		instMap.put(Inst.MAXAGE, new Inst2Fields());
		instMap.put(Inst.OLDER, new Inst2Fields());
		instMap.put(Inst.HEALER, new Inst2Fields());
		instMap.put(Inst.STARTAFF, new Inst2Fields());
		instMap.put(Inst.SUPPLYBONUS, new Inst2Fields());
		instMap.put(Inst.UWDAMAGE, new Inst2Fields());
		instMap.put(Inst.HOMESICK, new Inst2Fields());
		instMap.put(Inst.COLDPOWER, new Inst2Fields());
		instMap.put(Inst.FIREPOWER, new Inst2Fields());
		instMap.put(Inst.STORMPOWER, new Inst2Fields());
		instMap.put(Inst.DARKPOWER, new Inst2Fields());
		instMap.put(Inst.SPRINGPOWER, new Inst2Fields());
		instMap.put(Inst.SUMMERPOWER, new Inst2Fields());
		instMap.put(Inst.FALLPOWER, new Inst2Fields());
		instMap.put(Inst.WINTERPOWER, new Inst2Fields());
		instMap.put(Inst.AMBIDEXTROUS, new Inst2Fields());
		instMap.put(Inst.BANEFIRESHIELD, new Inst2Fields());
		instMap.put(Inst.BERSERK, new Inst2Fields());
		instMap.put(Inst.STANDARD, new Inst2Fields());
		instMap.put(Inst.ANIMALAWE, new Inst2Fields());
		instMap.put(Inst.AWE, new Inst2Fields());
		instMap.put(Inst.FEAR, new Inst2Fields());
		instMap.put(Inst.REGENERATION, new Inst2Fields());
		instMap.put(Inst.REINVIGORATION, new Inst2Fields());
		instMap.put(Inst.FIRESHIELD, new Inst2Fields());
		instMap.put(Inst.HEAT, new Inst6Fields());
		instMap.put(Inst.COLD, new Inst6Fields());
		instMap.put(Inst.ICEPROT, new Inst2Fields());
		instMap.put(Inst.POISONCLOUD, new Inst2Fields());
		instMap.put(Inst.DISEASECLOUD, new Inst2Fields());
		instMap.put(Inst.BLOODVENGEANCE, new Inst2Fields());
		instMap.put(Inst.CASTLEDEF, new Inst2Fields());
		instMap.put(Inst.SIEGEBONUS, new Inst2Fields());
		instMap.put(Inst.PATROLBONUS, new Inst2Fields());
		instMap.put(Inst.PILLAGEBONUS, new Inst2Fields());
		instMap.put(Inst.RESEARCHBONUS, new Inst2Fields());
		instMap.put(Inst.FORGEBONUS, new Inst2Fields());
		instMap.put(Inst.DOUSE, new Inst2Fields());
		instMap.put(Inst.NOBADEVENTS, new Inst2Fields());
		instMap.put(Inst.INCUNREST, new Inst2Fields());
		instMap.put(Inst.SPREADDOM, new Inst2Fields());
		instMap.put(Inst.LEPER, new Inst2Fields());
		instMap.put(Inst.POPKILL, new Inst2Fields());
		instMap.put(Inst.HERETIC, new Inst2Fields());
		instMap.put(Inst.ITEMSLOTS, new Inst8Fields());
		instMap.put(Inst.NAMETYPE, new Inst2Fields());
		instMap.put(Inst.MAGICSKILL1, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL2, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL3, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL4, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL5, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL6, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL7, new Inst3Fields());
		instMap.put(Inst.MAGICSKILL8, new Inst3Fields());
		instMap.put(Inst.CUSTOMMAGIC1, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC2, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC3, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC4, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC5, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC6, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC7, new Inst7Fields());
		instMap.put(Inst.CUSTOMMAGIC8, new Inst7Fields());
		instMap.put(Inst.MAGICBOOST1, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST2, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST3, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST4, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST5, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST6, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST7, new Inst3Fields());
		instMap.put(Inst.MAGICBOOST8, new Inst3Fields());
		instMap.put(Inst.GEMPROD1, new Inst3Fields());
		instMap.put(Inst.GEMPROD2, new Inst3Fields());
		instMap.put(Inst.GEMPROD3, new Inst3Fields());
		instMap.put(Inst.GEMPROD4, new Inst3Fields());
		instMap.put(Inst.GEMPROD5, new Inst3Fields());
		instMap.put(Inst.GEMPROD6, new Inst3Fields());
		instMap.put(Inst.GEMPROD7, new Inst3Fields());
		instMap.put(Inst.GEMPROD8, new Inst3Fields());
		instMap.put(Inst.CLEAR, new Inst4Fields());
		instMap.put(Inst.CLEARWEAPONS, new Inst4Fields());
		instMap.put(Inst.CLEARARMOR, new Inst4Fields());
		instMap.put(Inst.CLEARMAGIC, new Inst4Fields());
		instMap.put(Inst.CLEARSPEC, new Inst4Fields());
		instMap.put(Inst.FEMALE, new Inst4Fields());
		instMap.put(Inst.MOUNTED, new Inst4Fields());
		instMap.put(Inst.HOLY, new Inst4Fields());
		instMap.put(Inst.ANIMAL, new Inst4Fields());
		instMap.put(Inst.UNDEAD, new Inst4Fields());
		instMap.put(Inst.DEMON, new Inst4Fields());
		instMap.put(Inst.MAGICBEING, new Inst4Fields());
		instMap.put(Inst.STONEBEING, new Inst4Fields());
		instMap.put(Inst.INANIMATE, new Inst4Fields());
		instMap.put(Inst.COLDBLOOD, new Inst4Fields());
		instMap.put(Inst.IMMORTAL, new Inst4Fields());
		instMap.put(Inst.BLIND, new Inst4Fields());
		instMap.put(Inst.UNIQUE, new Inst4Fields());
		instMap.put(Inst.IMMOBILE, new Inst4Fields());
		instMap.put(Inst.AQUATIC, new Inst4Fields());
		instMap.put(Inst.AMPHIBIAN, new Inst4Fields());
		instMap.put(Inst.POORAMPHIBIAN, new Inst4Fields());
		instMap.put(Inst.FLYING, new Inst4Fields());
		instMap.put(Inst.STORMIMMUNE, new Inst4Fields());
		instMap.put(Inst.SAILING, new Inst4Fields());
		instMap.put(Inst.FORESTSURVIVAL, new Inst4Fields());
		instMap.put(Inst.MOUNTAINSURVIVAL, new Inst4Fields());
		instMap.put(Inst.SWAMPSURVIVAL, new Inst4Fields());
		instMap.put(Inst.WASTESURVIVAL, new Inst4Fields());
		instMap.put(Inst.ILLUSION, new Inst4Fields());
		instMap.put(Inst.SPY, new Inst4Fields());
		instMap.put(Inst.ASSASSIN, new Inst4Fields());
		instMap.put(Inst.HEAL, new Inst4Fields());
		instMap.put(Inst.NOHEAL, new Inst4Fields());
		instMap.put(Inst.NEEDNOTEAT, new Inst4Fields());
		instMap.put(Inst.ETHEREAL, new Inst4Fields());
		instMap.put(Inst.TRAMPLE, new Inst4Fields());
		instMap.put(Inst.ENTANGLE, new Inst4Fields());
		instMap.put(Inst.EYELOSS, new Inst4Fields());
		instMap.put(Inst.HORRORMARK, new Inst4Fields());
		instMap.put(Inst.POISONARMOR, new Inst4Fields());
		instMap.put(Inst.INQUISITOR, new Inst4Fields());
		instMap.put(Inst.NOITEM, new Inst4Fields());
		instMap.put(Inst.DRAINIMMUNE, new Inst4Fields());
		instMap.put(Inst.NOLEADER, new Inst4Fields());
		instMap.put(Inst.POORLEADER, new Inst4Fields());
		instMap.put(Inst.OKLEADER, new Inst4Fields());
		instMap.put(Inst.GOODLEADER, new Inst4Fields());
		instMap.put(Inst.EXPERTLEADER, new Inst4Fields());
		instMap.put(Inst.SUPERIORLEADER, new Inst4Fields());
		instMap.put(Inst.NOMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.POORMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.OKMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.GOODMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.EXPERTMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.SUPERIORMAGICLEADER, new Inst4Fields());
		instMap.put(Inst.NOUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.POORUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.OKUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.GOODUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.EXPERTUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.SUPERIORUNDEADLEADER, new Inst4Fields());
		instMap.put(Inst.WEAPON1, new Inst5Fields());
		instMap.put(Inst.WEAPON2, new Inst5Fields());
		instMap.put(Inst.WEAPON3, new Inst5Fields());
		instMap.put(Inst.WEAPON4, new Inst5Fields());
		instMap.put(Inst.ARMOR1, new Inst5Fields());
		instMap.put(Inst.ARMOR2, new Inst5Fields());
		instMap.put(Inst.ARMOR3, new Inst5Fields());
		instMap.put(Inst.ONEBATTLESPELL, new Inst5Fields());
		instMap.put(Inst.FIRSTSHAPE, new Inst5Fields());
		instMap.put(Inst.SECONDSHAPE, new Inst5Fields());
		instMap.put(Inst.SECONDTMPSHAPE, new Inst5Fields());
		instMap.put(Inst.SHAPECHANGE, new Inst5Fields());
		instMap.put(Inst.LANDSHAPE, new Inst5Fields());
		instMap.put(Inst.WATERSHAPE, new Inst5Fields());
		instMap.put(Inst.FORESTSHAPE, new Inst5Fields());
		instMap.put(Inst.PLAINSHAPE, new Inst5Fields());
		instMap.put(Inst.DOMSUMMON, new Inst5Fields());
		instMap.put(Inst.DOMSUMMON2, new Inst5Fields());
		instMap.put(Inst.DOMSUMMON20, new Inst5Fields());
		instMap.put(Inst.MAKEMONSTER1, new Inst5Fields());
		instMap.put(Inst.MAKEMONSTER2, new Inst5Fields());
		instMap.put(Inst.MAKEMONSTER3, new Inst5Fields());
		instMap.put(Inst.MAKEMONSTER4, new Inst5Fields());
		instMap.put(Inst.MAKEMONSTER5, new Inst5Fields());
		instMap.put(Inst.SUMMON1, new Inst5Fields());
		instMap.put(Inst.SUMMON5, new Inst5Fields());
		
		List<Inst> magicList = new ArrayList<Inst>();
		magicList.add(Inst.MAGICSKILL1);
		magicList.add(Inst.MAGICSKILL2);
		magicList.add(Inst.MAGICSKILL3);
		magicList.add(Inst.MAGICSKILL4);
		magicList.add(Inst.MAGICSKILL5);
		magicList.add(Inst.MAGICSKILL6);
		magicList.add(Inst.MAGICSKILL7);
		magicList.add(Inst.MAGICSKILL8);
		dynamicFields.add(magicList);
		List<Inst> customList = new ArrayList<Inst>();
		customList.add(Inst.CUSTOMMAGIC1);
		customList.add(Inst.CUSTOMMAGIC2);
		customList.add(Inst.CUSTOMMAGIC3);
		customList.add(Inst.CUSTOMMAGIC4);
		customList.add(Inst.CUSTOMMAGIC5);
		customList.add(Inst.CUSTOMMAGIC6);
		customList.add(Inst.CUSTOMMAGIC7);
		customList.add(Inst.CUSTOMMAGIC8);
		dynamicFields.add(customList);
		List<Inst> boostList = new ArrayList<Inst>();
		boostList.add(Inst.MAGICBOOST1);
		boostList.add(Inst.MAGICBOOST2);
		boostList.add(Inst.MAGICBOOST3);
		boostList.add(Inst.MAGICBOOST4);
		boostList.add(Inst.MAGICBOOST5);
		boostList.add(Inst.MAGICBOOST6);
		boostList.add(Inst.MAGICBOOST7);
		boostList.add(Inst.MAGICBOOST8);
		dynamicFields.add(boostList);
		List<Inst> gemList = new ArrayList<Inst>();
		gemList.add(Inst.GEMPROD1);
		gemList.add(Inst.GEMPROD2);
		gemList.add(Inst.GEMPROD3);
		gemList.add(Inst.GEMPROD4);
		gemList.add(Inst.GEMPROD5);
		gemList.add(Inst.GEMPROD6);
		gemList.add(Inst.GEMPROD7);
		gemList.add(Inst.GEMPROD8);
		dynamicFields.add(gemList);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.forms.IDetailsPage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	public void createContents(final Composite parent) {
		TableWrapLayout layout = new TableWrapLayout();
		layout.topMargin = 5;
		layout.leftMargin = 5;
		layout.rightMargin = 2;
		layout.bottomMargin = 2;
		parent.setLayout(layout);

		final FormToolkit toolkit = mform.getToolkit();
		Section s1 = toolkit.createSection(parent, Section.DESCRIPTION|Section.TITLE_BAR);
		s1.marginWidth = 10;
		s1.setText(Messages.getString("MonsterDetailsSection.name"));
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
		glayout = new GridLayout(3, false);
		glayout.marginHeight = 0;
		glayout.marginWidth = 0;
		nameComp.setLayout(glayout);
		GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, false);
		gd.horizontalSpan = 2;
		nameComp.setLayoutData(gd);
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("MonsterDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MONSTER_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setMonstername(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setMonstername(doc, name.getText());
				}
			}
			
		});
		
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 500;
		gd.horizontalSpan = 2;
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
					if (input instanceof SelectMonsterById || input instanceof SelectMonsterByName) {
						String monsterName = getSelectMonstername((Monster)input);
						name.setText(monsterName != null ? monsterName : "");
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		descCheck = toolkit.createButton(nameComp, Messages.getString("MonsterDetailsSection.mod.descr"), SWT.CHECK);
		descCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MONSTER_CATEGORY, "descr"));

		descr = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		descr.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setMonsterdescr(doc, descr.getText());
			}			
		});
		descr.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setMonsterdescr(doc, descr.getText());
				}
			}
			
		});
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		gd.widthHint = 500;
		gd.horizontalSpan = 2;
		descr.setLayoutData(gd);
		descr.addListener(SWT.Modify, new Listener() {
			
			@Override
			public void handleEvent(Event event) {
				int currentHeight = descr.getSize().y;
				int preferredHeight = descr.computeSize(500, SWT.DEFAULT).y;
				if (currentHeight < preferredHeight || currentHeight > 1.5*preferredHeight) {
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
					descr.setText(getSelectMonsterdescr((Monster)input));
					descCheck.setFont(boldFont);
				} else {
					removeInst(Inst.DESCR, doc);
					descr.setEnabled(false);
					descr.setBackground(toolkit.getColors().getInactiveBackground());
					descr.setText(getSelectMonsterdescr((Monster)input));
					descCheck.setFont(normalFont);
				}
			}
		});

		Composite spriteComp = toolkit.createComposite(nameComp);
		spriteComp.setLayout(new GridLayout(2, false));
		gd = new GridData();
		gd.horizontalSpan = 3;
		spriteComp.setLayoutData(gd);
		sprite1Label = new Label(spriteComp, SWT.NONE);
		sprite2Label = new Label(spriteComp, SWT.NONE);
		
		spr1Check = toolkit.createButton(nameComp, Messages.getString("MonsterDetailsSection.mod.spr1"), SWT.CHECK);
		spr1Check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MONSTER_CATEGORY, "spr1"));

		spr1 = toolkit.createText(nameComp, null, SWT.BORDER); //$NON-NLS-1$
		spr1.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setInst1(Inst.SPR1, doc, spr1.getText());
				sprite1Label.setImage(getSprite(spr1.getText()));
				update();
			}			
		});
		spr1.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setInst1(Inst.SPR1, doc, spr1.getText());
					sprite1Label.setImage(getSprite(spr1.getText()));
					update();
				}
			}
			
		});
		spr1.setLayoutData(new GridData(450, SWT.DEFAULT));
		spr1Browse = toolkit.createButton(nameComp, "Browse...", SWT.PUSH);
		spr1Browse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog dialog = new FileDialog(parent.getShell(), SWT.OPEN);
				dialog.setFilterExtensions(new String[]{"*.tga", "*.rgb", "*.sgi"});
				if (dialog.open() != null) {
					String targetpath = new File(dialog.getFilterPath() + File.separator + dialog.getFileName()).getAbsolutePath();
					String basepath = ((DmXtextEditor)doc).getPath();
					String relativepath = ResourceUtils.getRelativePath(targetpath, basepath, "/");
					spr1.setText("./"+relativepath);
					setInst1(Inst.SPR1, doc, spr1.getText());
					sprite1Label.setImage(getSprite(spr1.getText()));
					update();
				}
			}
		}); 
		spr1Browse.setEnabled(false);
		spr1Check.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (spr1Check.getSelection()) {
					addInst1(Inst.SPR1, doc, "");
					spr1.setEnabled(true);
					spr1.setText("");
					spr1Check.setFont(boldFont);
					spr1Browse.setEnabled(true);
				} else {
					removeInst(Inst.SPR1, doc);
					spr1.setEnabled(false);
					spr1.setText("");
					spr1Check.setFont(normalFont);
					spr1Browse.setEnabled(false);
				}
			}
		});

		spr2Check = toolkit.createButton(nameComp, Messages.getString("MonsterDetailsSection.mod.spr2"), SWT.CHECK);
		spr2Check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MONSTER_CATEGORY, "spr2"));

		spr2 = toolkit.createText(nameComp, null, SWT.BORDER); //$NON-NLS-1$
		spr2.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setInst1(Inst.SPR2, doc, spr2.getText());
				sprite2Label.setImage(getSprite(spr2.getText()));
				update();
			}			
		});
		spr2.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setInst1(Inst.SPR2, doc, spr2.getText());
					sprite2Label.setImage(getSprite(spr2.getText()));
					update();
				}
			}
			
		});
		spr2.setLayoutData(new GridData(450, SWT.DEFAULT));
		spr2Browse = toolkit.createButton(nameComp, "Browse...", SWT.PUSH);
		spr2Browse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				FileDialog dialog = new FileDialog(parent.getShell(), SWT.OPEN);
				dialog.setFilterExtensions(new String[]{"*.tga", "*.rgb", "*.sgi"});
				if (dialog.open() != null) {
					String targetpath = new File(dialog.getFilterPath() + File.separator + dialog.getFileName()).getAbsolutePath();
					String basepath = ((DmXtextEditor)doc).getPath();
					String relativepath = ResourceUtils.getRelativePath(targetpath, basepath, "/");
					spr2.setText("./"+relativepath);
					setInst1(Inst.SPR2, doc, spr2.getText());
					sprite2Label.setImage(getSprite(spr2.getText()));
					update();
				}
			}
		}); 
		spr2Browse.setEnabled(false);
		spr2Check.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (spr2Check.getSelection()) {
					addInst1(Inst.SPR2, doc, "");
					spr2.setEnabled(true);
					spr2.setText("");
					spr2Browse.setEnabled(true);
					spr2Check.setFont(boldFont);
				} else {
					removeInst(Inst.SPR2, doc);
					spr2.setEnabled(false);
					spr2.setText("");
					spr2Browse.setEnabled(false);
					spr2Check.setFont(normalFont);
				}
			}
		});

		Composite leftColumn = null;
		Composite rightColumn = null;
		boolean isRight = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			
			if (key.equals(Inst.SPECIALLOOK) || 
				key.equals(Inst.CLEAR) || 
				key.equals(Inst.RESTRICTEDGOD) || 
				key.equals(Inst.IMMOBILE) || 
				key.equals(Inst.COLDRES) || 
				key.equals(Inst.STEALTHY) || 
				key.equals(Inst.STARTAGE) || 
				key.equals(Inst.HEALER) || 
				key.equals(Inst.COLDPOWER) || 
				key.equals(Inst.AMBIDEXTROUS) || 
				key.equals(Inst.CASTLEDEF) || 
				key.equals(Inst.ITEMSLOTS) || 
				key.equals(Inst.MAGICSKILL1) || 
				key.equals(Inst.FIRSTSHAPE) || 
				key.equals(Inst.DOMSUMMON) || 
				key.equals(Inst.NAMETYPE) || 
				key.equals(Inst.NOLEADER)) {
				
				final Section expandable = toolkit.createSection(client, ExpandableComposite.TWISTIE|ExpandableComposite.EXPANDED);
				switch (key) {
				case SPECIALLOOK:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.basic"));
					break;
				case CLEAR:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.clearing"));
					break;
				case RESTRICTEDGOD:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.type"));
					break;
				case IMMOBILE:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.movement"));
					break;
				case COLDRES:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.resistance"));
					break;
				case STEALTHY:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.stealth"));
					break;
				case STARTAGE:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.age"));
					break;
				case HEALER:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.healing"));
					break;
				case COLDPOWER:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.element"));
					break;
				case AMBIDEXTROUS:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.combat"));
					break;
				case CASTLEDEF:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.noncombat"));
					break;
				case ITEMSLOTS:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.items"));
					break;
				case MAGICSKILL1:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.magic"));
					break;
				case FIRSTSHAPE:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.shapes"));
					break;
				case DOMSUMMON:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.summons"));
					break;
				case NAMETYPE:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.names"));
					break;
				case NOLEADER:
					expandable.setText(Messages.getString("MonsterDetailsSection.mod.section.leadership"));
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
				if (key.equals(Inst.SPECIALLOOK)) {
					expandable.setExpanded(true);
				} else {
					expandable.setExpanded(false);					
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
			Composite checkParent;
			if (field instanceof Inst4Fields) {
				checkParent = toolkit.createComposite(isRight?rightColumn:leftColumn);
				glayout = new GridLayout(2, false);
				glayout.marginHeight = 0;
				glayout.marginWidth = 0;
				checkParent.setLayout(glayout);
				gd = new GridData(SWT.BEGINNING, SWT.DEFAULT, false, false);
				gd.horizontalSpan = 3;
				gd.heightHint=20;
				checkParent.setLayoutData(gd);
			} else {
				checkParent = isRight?rightColumn:leftColumn;
			}
			final Button check = new DynamicButton(checkParent, SWT.CHECK);
			check.setText(key.label);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.MONSTER_CATEGORY, key.label));
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
							addInst6(key, doc, key.defaultValue);
						} else if (field instanceof Inst7Fields) {
							addInst3(key, doc, key.defaultValue, key.defaultValue2);
						} else if (field instanceof Inst8Fields) {
							addInst2(key, doc, key.defaultValue);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}

			});

			Text myValue1 = null;
			Text myValue2 = null;
			if (field instanceof Inst1Fields ||	field instanceof Inst2Fields ||	field instanceof Inst7Fields ||	field instanceof Inst5Fields || field instanceof Inst6Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER);
				myValue1 = value;
				
				if (field instanceof Inst2Fields ||	field instanceof Inst7Fields || field instanceof Inst6Fields) {
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
											if (Boolean.FALSE.equals(((Inst7Fields)fields.getValue()).value1.getData())) {
												((Inst7Fields)fields.getValue()).value1.setData(Boolean.TRUE);
												((Inst7Fields)fields.getValue()).value2.setData(Boolean.TRUE);
												((Inst7Fields)fields.getValue()).check.setData(Boolean.TRUE);
												((Inst7Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
												((Inst7Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
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
											if (Boolean.TRUE.equals(((Inst7Fields)fields.getValue()).value1.getData()) && !((Inst7Fields)fields.getValue()).value1.isEnabled()) {
												((Inst7Fields)fields.getValue()).value1.setData(Boolean.FALSE);
												((Inst7Fields)fields.getValue()).value2.setData(Boolean.FALSE);
												((Inst7Fields)fields.getValue()).check.setData(Boolean.FALSE);
												((Inst7Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
												((Inst7Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
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
						} else if (field instanceof Inst7Fields) {
							setInst3(key, doc, value.getText(), null);
						} else if (field instanceof Inst5Fields) {
							setInst5(key, doc, value.getText());
						} else if (field instanceof Inst6Fields) {
							setInst6(key, doc, value.getText());
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
							} else if (field instanceof Inst7Fields) {
								setInst3(key, doc, value.getText(), null);
							} else if (field instanceof Inst5Fields) {
								setInst5(key, doc, value.getText());
							} else if (field instanceof Inst6Fields) {
								setInst6(key, doc, value.getText());
							}
						}
					}
				});
				value.setEnabled(false);
				if (field instanceof Inst1Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = 140;
					gd.horizontalSpan = 3;
				} else if (field instanceof Inst2Fields || field instanceof Inst6Fields) {
					gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
					gd.widthHint = DEFAULT_VALUE_WIDTH;
				} else if (field instanceof Inst7Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = DEFAULT_VALUE_WIDTH-12;
				} else if (field instanceof Inst5Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					if (fields.getKey() == Inst.ONEBATTLESPELL) {
						gd.widthHint = DEFAULT_VALUE_WIDTH-12;
					} else {
						gd.widthHint = DEFAULT_VALUE_WIDTH;
					}
				}
				value.setLayoutData(gd);
				
			}
			
			MappedDynamicCombo myInst3Value1 = null;
			Text myInst3Value2 = null;
			if (field instanceof Inst3Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst3Value1 = value;
				
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							setComboItems(fields.getKey(), value);
							int selection = Integer.parseInt(key.defaultValue);
							value.select(selection);
							for (List<Inst> dynamic : dynamicFields) {
								if (dynamic.contains(key)) {
									for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
										if (dynamic.contains(fields.getKey())) {
											if (Boolean.FALSE.equals(((Inst3Fields)fields.getValue()).value1.getData())) {
												((Inst3Fields)fields.getValue()).value1.setData(Boolean.TRUE);
												((Inst3Fields)fields.getValue()).value2.setData(Boolean.TRUE);
												((Inst3Fields)fields.getValue()).check.setData(Boolean.TRUE);
												((Inst3Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
												((Inst3Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
												break;
											}
										}
									}
									update();
									mform.fireSelectionChanged(mform.getParts()[0], viewer.getSelection());
								}
							}
						} else {
							value.setEnabled(true);
							value.removeAll();
							value.setEnabled(false);
							for (List<Inst> dynamic : dynamicFields) {
								if (dynamic.contains(key)) {
									@SuppressWarnings("rawtypes")
									List<Map.Entry> entries = Arrays.asList(instMap.entrySet().toArray(new Map.Entry[instMap.entrySet().size()]));
									Collections.reverse(entries);
									for (final Map.Entry<Inst, InstFields> fields : entries) {
										if (!key.equals(fields.getKey()) && dynamic.contains(fields.getKey())) {
											if (Boolean.TRUE.equals(((Inst3Fields)fields.getValue()).value1.getData()) && !((Inst3Fields)fields.getValue()).value1.isEnabled()) {
												((Inst3Fields)fields.getValue()).value1.setData(Boolean.FALSE);
												((Inst3Fields)fields.getValue()).value2.setData(Boolean.FALSE);
												((Inst3Fields)fields.getValue()).check.setData(Boolean.FALSE);
												((Inst3Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
												((Inst3Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
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
				value.addSelectionListener(new SelectionListener() {
					
					@Override
					public void widgetSelected(SelectionEvent e) {
						int val = value.getSelectedValue();
						setInst3(key, doc, Integer.toString(val), null);
					}
					
					@Override
					public void widgetDefaultSelected(SelectionEvent e) {
					}
				});
				value.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH+16;
				value.setLayoutData(gd);
				
			}

			MappedDynamicCombo myInst8Value1 = null;
			MappedDynamicCombo myInst8Value2 = null;
			MappedDynamicCombo myInst8Value3 = null;
			MappedDynamicCombo myInst8Value4 = null;
			MappedDynamicCombo myInst8Value5 = null;
			if (field instanceof Inst8Fields) {
				final MappedDynamicCombo value1 = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				final MappedDynamicCombo value2 = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				new Label(isRight?rightColumn:leftColumn, SWT.NONE);
				new Label(isRight?rightColumn:leftColumn, SWT.NONE);
				final MappedDynamicCombo value3 = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				final MappedDynamicCombo value4 = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				final MappedDynamicCombo value5 = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				new Label(isRight?rightColumn:leftColumn, SWT.NONE);
				new Label(isRight?rightColumn:leftColumn, SWT.NONE);
				myInst8Value1 = value1;
				myInst8Value2 = value2;
				myInst8Value3 = value3;
				myInst8Value4 = value4;
				myInst8Value5 = value5;
				
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value1.setEnabled(true);
							value1.setItems(new String[]{
									"0 hands",	"1 hand", "2 hands", "3 hands", "4 hands"},
									new int[]{0, 2, 6, 14, 30});
							value1.select(getHands(Integer.parseInt(key.defaultValue)));

							value2.setEnabled(true);
							value2.setItems(new String[]{
									"0 heads",	"1 head", "2 heads"},
									new int[]{0, 128, 384});
							value2.select(getHeads(Integer.parseInt(key.defaultValue)));

							value3.setEnabled(true);
							value3.setItems(new String[]{
									"0 bodies",	"1 body"},
									new int[]{0, 1024});
							value3.select(getBodies(Integer.parseInt(key.defaultValue)));

							value4.setEnabled(true);
							value4.setItems(new String[]{
									"0 feet", "1 foot"},
									new int[]{0, 2048});
							value4.select(getFeet(Integer.parseInt(key.defaultValue)));

							value5.setEnabled(true);
							value5.setItems(new String[]{
									"0 misc", "1 misc", "2 misc", "3 misc", "4 misc"},
									new int[]{0, 4096, 12288, 28672, 61440});
							value5.select(getMisc(Integer.parseInt(key.defaultValue)));
						} else {
							value1.setEnabled(true);
							value1.removeAll();
							value1.setEnabled(false);
							value2.setEnabled(true);
							value2.removeAll();
							value2.setEnabled(false);
							value3.setEnabled(true);
							value3.removeAll();
							value3.setEnabled(false);
							value4.setEnabled(true);
							value4.removeAll();
							value4.setEnabled(false);
							value5.setEnabled(true);
							value5.removeAll();
							value5.setEnabled(false);
						}
					}

				});
				SelectionListener selectionListener = new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						int val = getItemMask(value1.getSelectedValue(), value2.getSelectedValue(), value3.getSelectedValue(), value4.getSelectedValue(), value5.getSelectedValue());
						setInst2(key, doc, Integer.toString(val));
					}
				};
				value1.addSelectionListener(selectionListener);
				value2.addSelectionListener(selectionListener);
				value3.addSelectionListener(selectionListener);
				value4.addSelectionListener(selectionListener);
				value5.addSelectionListener(selectionListener);
				value1.setEnabled(false);
				value2.setEnabled(false);
				value3.setEnabled(false);
				value4.setEnabled(false);
				value5.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.FILL, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH+16;
				value1.setLayoutData(gd);
				
			}

			Label defaultLabel1 = null;
			
			if (field instanceof Inst1Fields || field instanceof Inst2Fields || field instanceof Inst3Fields || field instanceof Inst5Fields || field instanceof Inst6Fields || field instanceof Inst7Fields || field instanceof Inst8Fields) {
				defaultLabel1 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel1.setEnabled(false);
			}
			if (field instanceof Inst4Fields) {
				defaultLabel1 = toolkit.createLabel(checkParent, "");
				defaultLabel1.setEnabled(false);
			}
			if (field instanceof Inst2Fields || field instanceof Inst5Fields || field instanceof Inst6Fields) {
				gd = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
			} else if (field instanceof Inst1Fields || field instanceof Inst3Fields || field instanceof Inst7Fields) {
				gd = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst4Fields) {
				gd = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
			} else if (field instanceof Inst8Fields) {
				gd = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			}

			Label defaultLabel2 = null;
			if (field instanceof Inst3Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER);
				myInst3Value2 = value;
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
							update();
						} else {
							value.setEnabled(false);
							value.setText("");
							update();
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
				gd = new GridData(SWT.BEGINNING, SWT.DEFAULT, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH-24;
				value.setLayoutData(gd);
				
				defaultLabel2 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel2.setEnabled(false);

				for (List<Inst> list : dynamicFields) {
					boolean firstElement = true;
					for (Inst inst : list) {
						if (key.equals(inst)) {
							if (firstElement) {
								myInst3Value1.setData(Boolean.TRUE);
								myInst3Value2.setData(Boolean.TRUE);
								check.setData(Boolean.TRUE);
								defaultLabel1.setData(Boolean.TRUE);
								defaultLabel2.setData(Boolean.TRUE);
							} else {
								myInst3Value1.setData(Boolean.FALSE);
								myInst3Value2.setData(Boolean.FALSE);
								check.setData(Boolean.FALSE);
								defaultLabel1.setData(Boolean.FALSE);
								defaultLabel2.setData(Boolean.FALSE);
							}
						}
						firstElement = false;
					}
				}
			}
			if (field instanceof Inst7Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER);
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
							update();
						} else {
							value.setEnabled(false);
							value.setText("");
							update();
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
				gd = new GridData(SWT.BEGINNING, SWT.DEFAULT, false, false);
				gd.widthHint = DEFAULT_VALUE_WIDTH-24;
				value.setLayoutData(gd);
				
				defaultLabel2 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel2.setEnabled(false);

				for (List<Inst> list : dynamicFields) {
					boolean firstElement = true;
					for (Inst inst : list) {
						if (key.equals(inst)) {
							if (firstElement) {
								myValue1.setData(Boolean.TRUE);
								myValue2.setData(Boolean.TRUE);
								check.setData(Boolean.TRUE);
								defaultLabel1.setData(Boolean.TRUE);
								defaultLabel2.setData(Boolean.TRUE);
							} else {
								myValue1.setData(Boolean.FALSE);
								myValue2.setData(Boolean.FALSE);
								check.setData(Boolean.FALSE);
								defaultLabel1.setData(Boolean.FALSE);
								defaultLabel2.setData(Boolean.FALSE);
							}
						}
						firstElement = false;
					}
				}
			}
			
			if (field instanceof Inst1Fields) {
				((Inst1Fields)field).check = check;
				((Inst1Fields)field).value = myValue1;
				((Inst1Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst2Fields) {
				((Inst2Fields)field).check = check;
				((Inst2Fields)field).value = myValue1;
				((Inst2Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst3Fields) {
				((Inst3Fields)field).check = check;
				((Inst3Fields)field).value1 = myInst3Value1;
				((Inst3Fields)field).defaultLabel1 = defaultLabel1;
				((Inst3Fields)field).value2 = myInst3Value2;
				((Inst3Fields)field).defaultLabel2 = defaultLabel2;
			} else if (field instanceof Inst4Fields) {
				((Inst4Fields)field).check = check;
				((Inst4Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst5Fields) {
				((Inst5Fields)field).check = check;
				((Inst5Fields)field).value = myValue1;
				((Inst5Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst6Fields) {
				((Inst6Fields)field).check = check;
				((Inst6Fields)field).value = myValue1;
				((Inst6Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst7Fields) {
				((Inst7Fields)field).check = check;
				((Inst7Fields)field).value1 = myValue1;
				((Inst7Fields)field).defaultLabel1 = defaultLabel1;
				((Inst7Fields)field).value2 = myValue2;
				((Inst7Fields)field).defaultLabel2 = defaultLabel2;
			} else if (field instanceof Inst8Fields) {
				((Inst8Fields)field).check = check;
				((Inst8Fields)field).value1 = myInst8Value1;
				((Inst8Fields)field).value2 = myInst8Value2;
				((Inst8Fields)field).value3 = myInst8Value3;
				((Inst8Fields)field).value4 = myInst8Value4;
				((Inst8Fields)field).value5 = myInst8Value5;
				((Inst8Fields)field).defaultLabel = defaultLabel1;
			}

			isRight = !isRight;
		}
	}
	
	private Image getSpriteFromZip(final String sprite) {
		ImageLoader loader1 = new ImageLoader() {
			@Override
			public InputStream getStream() throws IOException {
				Path path = new Path("$nl$/lib/sprites.zip");
				URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
				String dbPath = FileLocator.toFileURL(url).getPath();
				ZipFile zipFile = new ZipFile(new File(dbPath));
				return zipFile.getInputStream(zipFile.getEntry(sprite));
			}
		};
		Image image = null;
		try {
			if (spriteMap.get(sprite) != null) {
				image = spriteMap.get(sprite);
			} else {
				image = new Image(null, ImageConverter.convertToSWT(ImageConverter.cropImage(loader1.loadImage())));
				spriteMap.put(sprite, image);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return image;
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
			return "Priest";
		case 50:
			return "Random";
		case 51:
			return "Elemental";
		case 52:
			return "Sorcery";			
		case 53:
			return "All";			
		}
		return "Unknown";
	}
	
	private void setComboItems(Inst key, MappedDynamicCombo combo) {
		if (key == Inst.MAGICSKILL1 ||
			key == Inst.MAGICSKILL2 ||
			key == Inst.MAGICSKILL3 ||
			key == Inst.MAGICSKILL4 ||
			key == Inst.MAGICSKILL5 ||
			key == Inst.MAGICSKILL6 ||
			key == Inst.MAGICSKILL7 ||
			key == Inst.MAGICSKILL8) {
			combo.setItems(new String[]{
					"Fire",	"Air", "Water", "Earth", "Astral", "Death", "Nature", "Blood", "Priest", "Random", "Elemental", "Sorcery"},
					new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 50, 51, 52});
		}
		if (key == Inst.MAGICBOOST1 ||
			key == Inst.MAGICBOOST2 ||
			key == Inst.MAGICBOOST3 ||
			key == Inst.MAGICBOOST4 ||
			key == Inst.MAGICBOOST5 ||
			key == Inst.MAGICBOOST6 ||
			key == Inst.MAGICBOOST7 ||
			key == Inst.MAGICBOOST8) {
			combo.setItems(new String[]{
					"Fire",	"Air", "Water", "Earth", "Astral", "Death",	"Nature", "Blood", "Priest", "All"},
					new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 53});
		}
		if (key == Inst.GEMPROD1 ||
			key == Inst.GEMPROD2 ||
			key == Inst.GEMPROD3 ||
			key == Inst.GEMPROD4 ||
			key == Inst.GEMPROD5 ||
			key == Inst.GEMPROD6 ||
			key == Inst.GEMPROD7 ||
			key == Inst.GEMPROD8) {
			combo.setItems(new String[]{
					"Fire",	"Air", "Water", "Earth", "Astral", "Death",	"Nature", "Blood"},
					new int[]{0, 1, 2, 3, 4, 5, 6, 7});
		}
	}
	
	public void update() {
		if (input != null) {
			String nameString = getInst1(Inst.NAME, (Monster)input);

			String sprite1 = null;
			String sprite2 = null;
			boolean fromZip1 = false;
			boolean fromZip2 = false;
			final Format format = new DecimalFormat("0000");
			if (input instanceof SelectMonsterByName || input instanceof SelectMonsterById) {
				if (nameString != null) {
					name.setText(nameString);
					name.setEnabled(true);
					nameCheck.setSelection(true);
					nameCheck.setFont(boldFont);
				} else {
					String monsterName = getSelectMonstername((Monster)input);
					name.setText(monsterName != null ? monsterName : "");
					name.setEnabled(false);
					nameCheck.setSelection(false);
					nameCheck.setFont(normalFont);
				}

				int id = getSelectMonsterid((Monster)input);
				if (getInst1(Inst.SPR1, input) != null) {
					sprite1 = getInst1(Inst.SPR1, input);
				} else if (getInst2(Inst.COPYSPR, input) != null) {
					Integer copyId = getInst2(Inst.COPYSPR, input);
					sprite1 = format.format(copyId) + "_1.tga";
					fromZip1 = true;
				} else {
					sprite1 = format.format(id) + "_1.tga";
					fromZip1 = true;
				}

				if (getInst1(Inst.SPR2, input) != null) {
					sprite2 = getInst1(Inst.SPR2, input);
				} else if (getInst2(Inst.COPYSPR, input) != null) {
					Integer copyId = getInst2(Inst.COPYSPR, input);
					sprite2 = format.format(copyId) + "_2.tga";
					fromZip2 = true;
				} else {
					sprite2 = format.format(id) + "_2.tga";
					fromZip2 = true;
				}
				
			} else {
				if (nameString != null) {
					name.setText(nameString);
					name.setEnabled(true);
					nameCheck.setSelection(true);
					nameCheck.setFont(boldFont);
				} else {
					String str = getMonstername((Monster)input);
					name.setText(str!=null?str:"");
					name.setEnabled(false);
					nameCheck.setSelection(false);
					nameCheck.setFont(normalFont);
				}
				nameCheck.setEnabled(false);
				
				if (getInst1(Inst.SPR1, input) != null) {
					sprite1 = getInst1(Inst.SPR1, input);
				} else if (getInst1(Inst.COPYSPR, input) != null) {
					String strId = getInst1(Inst.COPYSPR, input);
					sprite1 = format.format(Integer.parseInt(strId)) + "_1.tga";
					fromZip1 = true;
				}

				if (getInst1(Inst.SPR2, input) != null) {
					sprite2 = getInst1(Inst.SPR2, input);
				} else if (getInst1(Inst.COPYSPR, input) != null) {
					String strId = getInst1(Inst.COPYSPR, input);
					sprite2 = format.format(Integer.parseInt(strId)) + "_2.tga";
					fromZip2 = true;
				}
			}
			if (sprite1 != null) {
				if (fromZip1) {
					sprite1Label.setImage(getSpriteFromZip(sprite1));
				} else {
					sprite1Label.setImage(getSprite(sprite1));
				}
			} else {
				sprite1Label.setImage(null);
			}
			if (sprite2 != null) {
				if (fromZip2) {
					sprite2Label.setImage(getSpriteFromZip(sprite2));
				} else {
					sprite2Label.setImage(getSprite(sprite2));
				}
			} else {
				sprite2Label.setImage(null);
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
				descr.setText(getSelectMonsterdescr((Monster)input));
				descr.setEnabled(false);
				descr.setBackground(toolkit.getColors().getInactiveBackground());
				descCheck.setSelection(false);
				descCheck.setFont(normalFont);
			}

			String spr1Text = getInst1(Inst.SPR1, input);
			if (spr1Text != null) {
				spr1.setText(spr1Text);
				spr1.setEnabled(true);
				spr1Browse.setEnabled(true);
				spr1Check.setSelection(true);
				spr1Check.setFont(boldFont);
			} else {
				spr1.setText("");
				spr1.setEnabled(false);
				spr1Browse.setEnabled(false);
				spr1Check.setSelection(false);
				spr1Check.setFont(normalFont);
			}
			String spr2Text = getInst1(Inst.SPR2, input);
			if (spr2Text != null) {
				spr2.setText(spr2Text);
				spr2.setEnabled(true);
				spr2Browse.setEnabled(true);
				spr2Check.setSelection(true);
				spr2Check.setFont(boldFont);
			} else {
				spr2.setText("");
				spr2.setEnabled(false);
				spr2Browse.setEnabled(false);
				spr2Check.setSelection(false);
				spr2Check.setFont(normalFont);
			}
		}
		MonsterDB monsterDB = new MonsterDB();
		if (input instanceof SelectMonsterById) {
			monsterDB = Database.getMonster(((SelectMonsterById)input).getValue());
		} else if (input instanceof SelectMonsterByName) {
			monsterDB = Database.getMonster(((SelectMonsterByName)input).getValue());
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
				if (fields.getValue() instanceof Inst8Fields) {
					((Inst8Fields)fields.getValue()).value1.setEnabled(true);
					((Inst8Fields)fields.getValue()).value1.setItems(new String[]{
							"0 hands",	"1 hand", "2 hands", "3 hands", "4 hands"},
							new int[]{0, 2, 6, 14, 30});
					((Inst8Fields)fields.getValue()).value1.select(getHands(Integer.parseInt(val.toString())));

					((Inst8Fields)fields.getValue()).value2.setEnabled(true);
					((Inst8Fields)fields.getValue()).value2.setItems(new String[]{
							"0 heads",	"1 head", "2 heads"},
							new int[]{0, 128, 384});
					((Inst8Fields)fields.getValue()).value2.select(getHeads(Integer.parseInt(val.toString())));

					((Inst8Fields)fields.getValue()).value3.setEnabled(true);
					((Inst8Fields)fields.getValue()).value3.setItems(new String[]{
							"0 bodies",	"1 body"},
							new int[]{0, 1024});
					((Inst8Fields)fields.getValue()).value3.select(getBodies(Integer.parseInt(val.toString())));

					((Inst8Fields)fields.getValue()).value4.setEnabled(true);
					((Inst8Fields)fields.getValue()).value4.setItems(new String[]{
							"0 feet", "1 foot"},
							new int[]{0, 2048});
					((Inst8Fields)fields.getValue()).value4.select(getFeet(Integer.parseInt(val.toString())));

					((Inst8Fields)fields.getValue()).value5.setEnabled(true);
					((Inst8Fields)fields.getValue()).value5.setItems(new String[]{
							"0 misc", "1 misc", "2 misc", "3 misc", "4 misc"},
							new int[]{0, 4096, 12288, 28672, 61440});
					((Inst8Fields)fields.getValue()).value5.select(getMisc(Integer.parseInt(val.toString())));

					((Inst8Fields)fields.getValue()).check.setSelection(true);
					((Inst8Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText("");
					((Inst2Fields)fields.getValue()).value.setEnabled(false);
					((Inst2Fields)fields.getValue()).check.setSelection(false);
					((Inst2Fields)fields.getValue()).check.setFont(normalFont);
				}
				if (fields.getValue() instanceof Inst8Fields) {
					((Inst8Fields)fields.getValue()).value1.removeAll();
					((Inst8Fields)fields.getValue()).value1.setEnabled(false);
					((Inst8Fields)fields.getValue()).value2.removeAll();
					((Inst8Fields)fields.getValue()).value2.setEnabled(false);
					((Inst8Fields)fields.getValue()).value3.removeAll();
					((Inst8Fields)fields.getValue()).value3.setEnabled(false);
					((Inst8Fields)fields.getValue()).value4.removeAll();
					((Inst8Fields)fields.getValue()).value4.setEnabled(false);
					((Inst8Fields)fields.getValue()).value5.removeAll();
					((Inst8Fields)fields.getValue()).value5.setEnabled(false);
					((Inst8Fields)fields.getValue()).check.setSelection(false);
					((Inst8Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Integer[] vals = getInst3(fields.getKey(), input);
			if (vals != null) {
				if (fields.getValue() instanceof Inst3Fields) {
					int selection = vals[0];
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					setComboItems(fields.getKey(), ((Inst3Fields)fields.getValue()).value1);
					((Inst3Fields)fields.getValue()).value1.select(selection);
					((Inst3Fields)fields.getValue()).value2.setText(vals[1].toString());
					((Inst3Fields)fields.getValue()).value2.setEnabled(true);
					((Inst3Fields)fields.getValue()).check.setSelection(true);
					((Inst3Fields)fields.getValue()).check.setFont(boldFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst3Fields)fields.getValue()).value1.getData())) {
								((Inst3Fields)fields.getValue()).value1.setData(Boolean.TRUE);
								((Inst3Fields)fields.getValue()).value2.setData(Boolean.TRUE);
								((Inst3Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst3Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
								((Inst3Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
							}
						}
					}
				} else if (fields.getValue() instanceof Inst7Fields) {
					((Inst7Fields)fields.getValue()).value1.setEnabled(true);
					((Inst7Fields)fields.getValue()).value1.setText(vals[0].toString());
					((Inst7Fields)fields.getValue()).value2.setText(vals[1].toString());
					((Inst7Fields)fields.getValue()).value2.setEnabled(true);
					((Inst7Fields)fields.getValue()).check.setSelection(true);
					((Inst7Fields)fields.getValue()).check.setFont(boldFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst7Fields)fields.getValue()).value1.getData())) {
								((Inst7Fields)fields.getValue()).value1.setData(Boolean.TRUE);
								((Inst7Fields)fields.getValue()).value2.setData(Boolean.TRUE);
								((Inst7Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst7Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
								((Inst7Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
							}
						}
					}
				}
			} else {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value1.setEnabled(true);
					((Inst3Fields)fields.getValue()).value1.removeAll();
					((Inst3Fields)fields.getValue()).value1.setEnabled(false);
					((Inst3Fields)fields.getValue()).value2.setText("");
					((Inst3Fields)fields.getValue()).value2.setEnabled(false);
					((Inst3Fields)fields.getValue()).check.setSelection(false);
					((Inst3Fields)fields.getValue()).check.setFont(normalFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic) && !isDefaultValue(fields.getKey(), monsterDB)) {
								if (Boolean.TRUE.equals(((Inst3Fields)fields.getValue()).value1.getData())) {
									((Inst3Fields)fields.getValue()).value1.setData(Boolean.FALSE);
									((Inst3Fields)fields.getValue()).value2.setData(Boolean.FALSE);
									((Inst3Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst3Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
									((Inst3Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
								}
							} else {
								if (!isDefaultValue(fields.getKey(), monsterDB)) {
									dynamicFirstEmpty.add(dynamic);
								}
								if (Boolean.FALSE.equals(((Inst3Fields)fields.getValue()).value1.getData())) {
									((Inst3Fields)fields.getValue()).value1.setData(Boolean.TRUE);
									((Inst3Fields)fields.getValue()).value2.setData(Boolean.TRUE);
									((Inst3Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst3Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
									((Inst3Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
								}
							}
						}
					}
				} else if (fields.getValue() instanceof Inst7Fields) {
					((Inst7Fields)fields.getValue()).value1.setEnabled(true);
					((Inst7Fields)fields.getValue()).value1.setText("");
					((Inst7Fields)fields.getValue()).value1.setEnabled(false);
					((Inst7Fields)fields.getValue()).value2.setText("");
					((Inst7Fields)fields.getValue()).value2.setEnabled(false);
					((Inst7Fields)fields.getValue()).check.setSelection(false);
					((Inst7Fields)fields.getValue()).check.setFont(normalFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic) && !isDefaultValue(fields.getKey(), monsterDB)) {
								if (Boolean.TRUE.equals(((Inst7Fields)fields.getValue()).value1.getData())) {
									((Inst7Fields)fields.getValue()).value1.setData(Boolean.FALSE);
									((Inst7Fields)fields.getValue()).value2.setData(Boolean.FALSE);
									((Inst7Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst7Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
									((Inst7Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
								}
							} else {
								if (!isDefaultValue(fields.getKey(), monsterDB)) {
									dynamicFirstEmpty.add(dynamic);
								}
								if (Boolean.FALSE.equals(((Inst7Fields)fields.getValue()).value1.getData())) {
									((Inst7Fields)fields.getValue()).value1.setData(Boolean.TRUE);
									((Inst7Fields)fields.getValue()).value2.setData(Boolean.TRUE);
									((Inst7Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst7Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
									((Inst7Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
								}
							}
						}
					}
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
			Integer val6 = getInst6(fields.getKey(), input);
			if (val6 != null) {
				if (fields.getValue() instanceof Inst6Fields) {
					((Inst6Fields)fields.getValue()).value.setText(val6.equals(Integer.valueOf(0)) ? "" : val6.toString());
					((Inst6Fields)fields.getValue()).value.setEnabled(true);
					((Inst6Fields)fields.getValue()).check.setSelection(true);
					((Inst6Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst6Fields) {
					((Inst6Fields)fields.getValue()).value.setText("");
					((Inst6Fields)fields.getValue()).value.setEnabled(false);
					((Inst6Fields)fields.getValue()).check.setSelection(false);
					((Inst6Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			if (input instanceof SelectMonsterByName || input instanceof SelectMonsterById) {
				switch (fields.getKey()) {
				case ARMOR1:
					if (monsterDB.armor1 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.armor1));
						Inst.ARMOR1.defaultValue = monsterDB.armor1;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ARMOR1.defaultValue = "";
					}
					break;
				case ARMOR2:
					if (monsterDB.armor2 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.armor2));
						Inst.ARMOR2.defaultValue = monsterDB.armor2;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ARMOR2.defaultValue = "";
					}
					break;
				case ARMOR3:
					if (monsterDB.armor3 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.armor3));
						Inst.ARMOR3.defaultValue = monsterDB.armor3;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ARMOR3.defaultValue = "";
					}
					break;
				case SPECIALLOOK:
					if (monsterDB.speciallook != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.speciallook));
						Inst.SPECIALLOOK.defaultValue = monsterDB.speciallook.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPECIALLOOK.defaultValue = "1";
					}
					break;
				case AP:
					if (monsterDB.ap != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.ap));
						Inst.AP.defaultValue = monsterDB.ap.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AP.defaultValue = "";
					}
					break;
				case MAPMOVE:
					if (monsterDB.mapmove != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.mapmove));
						Inst.MAPMOVE.defaultValue = monsterDB.mapmove.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAPMOVE.defaultValue = "";
					}
					break;
				case HP:
					if (monsterDB.hp != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.hp));
						Inst.HP.defaultValue = monsterDB.hp.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HP.defaultValue = "";
					}
					break;
				case PROT:
					if (monsterDB.prot != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.prot));
						Inst.PROT.defaultValue = monsterDB.prot.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PROT.defaultValue = "";
					}
					break;
				case SIZE:
					if (monsterDB.size != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.size));
						Inst.SIZE.defaultValue = monsterDB.size.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SIZE.defaultValue = "";
					}
					break;
				case RESSIZE:
					if (monsterDB.ressize != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.ressize));
						Inst.RESSIZE.defaultValue = monsterDB.ressize.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RESSIZE.defaultValue = "1";
					}
					break;
				case STR:
					if (monsterDB.str != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.str));
						Inst.STR.defaultValue = monsterDB.str.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STR.defaultValue = "";
					}
					break;
				case ENC:
					if (monsterDB.enc != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.enc));
						Inst.ENC.defaultValue = monsterDB.enc.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ENC.defaultValue = "";
					}
					break;
				case ATT:
					if (monsterDB.att != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.att));
						Inst.ATT.defaultValue = monsterDB.att.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ATT.defaultValue = "";
					}
					break;
				case DEF:
					if (monsterDB.def != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.def));
						Inst.DEF.defaultValue = monsterDB.def.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEF.defaultValue = "";
					}
					break;
				case PREC:
					if (monsterDB.prec != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.prec));
						Inst.PREC.defaultValue = monsterDB.prec.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PREC.defaultValue = "";
					}
					break;
				case MR:
					if (monsterDB.mr != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.mr));
						Inst.MR.defaultValue = monsterDB.mr.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MR.defaultValue = "";
					}
					break;
				case MOR:
					if (monsterDB.mor != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.mor));
						Inst.MOR.defaultValue = monsterDB.mor.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MOR.defaultValue = "";
					}
					break;
				case GCOST:
					if (monsterDB.gcost != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.gcost));
						Inst.GCOST.defaultValue = monsterDB.gcost.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GCOST.defaultValue = "";
					}
					break;
				case RCOST:
					if (monsterDB.rcost != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.rcost));
						Inst.RCOST.defaultValue = monsterDB.rcost.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RCOST.defaultValue = "";
					}
					break;
				case PATHCOST:
					if (monsterDB.pathcost != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.pathcost));
						Inst.PATHCOST.defaultValue = monsterDB.pathcost.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PATHCOST.defaultValue = "";
					}
					break;
				case STARTDOM:
					if (monsterDB.startdom != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.startdom));
						Inst.STARTDOM.defaultValue = monsterDB.startdom.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STARTDOM.defaultValue = "";
					}
					break;
				case EYES:
					if (monsterDB.eyes != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.eyes));
						Inst.EYES.defaultValue = monsterDB.eyes.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EYES.defaultValue = "";
					}
					break;
				case COPYSTATS:
					if (monsterDB.copystats != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.copystats));
						Inst.COPYSTATS.defaultValue = monsterDB.copystats.toString();
					}
					break;
				case COPYSPR:
					if (monsterDB.copyspr != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.copyspr));
						Inst.COPYSPR.defaultValue = monsterDB.copyspr.toString();
					}
					break;
				case RESTRICTEDGOD:
					if (monsterDB.restrictedgod != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.restrictedgod));
						Inst.RESTRICTEDGOD.defaultValue = monsterDB.restrictedgod.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RESTRICTEDGOD.defaultValue = "";
					}
					break;
				case SHATTEREDSOUL:
					if (monsterDB.shatteredsoul != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.shatteredsoul));
						Inst.SHATTEREDSOUL.defaultValue = monsterDB.shatteredsoul.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SHATTEREDSOUL.defaultValue = "";
					}
					break;
				case COLDRES:
					if (monsterDB.coldres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.coldres));
						Inst.COLDRES.defaultValue = monsterDB.coldres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLDRES.defaultValue = "";
					}
					break;
				case FIRERES:
					if (monsterDB.fireres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.fireres));
						Inst.FIRERES.defaultValue = monsterDB.fireres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRERES.defaultValue = "";
					}
					break;
				case POISONRES:
					if (monsterDB.poisonres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poisonres));
						Inst.POISONRES.defaultValue = monsterDB.poisonres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POISONRES.defaultValue = "";
					}
					break;
				case SHOCKRES:
					if (monsterDB.shockres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.shockres));
						Inst.SHOCKRES.defaultValue = monsterDB.shockres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SHOCKRES.defaultValue = "";
					}
					break;
				case DARKVISION:
					if (monsterDB.darkvision != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.darkvision));
						Inst.DARKVISION.defaultValue = monsterDB.darkvision.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DARKVISION.defaultValue = "";
					}
					break;
				case STEALTHY:
					if (monsterDB.stealthy != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.stealthy));
						Inst.STEALTHY.defaultValue = monsterDB.stealthy.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STEALTHY.defaultValue = "";
					}
					break;
				case SEDUCE:
					if (monsterDB.seduce != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.seduce));
						Inst.SEDUCE.defaultValue = monsterDB.seduce.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SEDUCE.defaultValue = "";
					}
					break;
				case SUCCUBUS:
					if (monsterDB.succubus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.succubus));
						Inst.SUCCUBUS.defaultValue = monsterDB.succubus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUCCUBUS.defaultValue = "";
					}
					break;
				case BECKON:
					if (monsterDB.beckon != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.beckon));
						Inst.BECKON.defaultValue = monsterDB.beckon.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BECKON.defaultValue = "";
					}
					break;
				case STARTAGE:
					if (monsterDB.startage != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.startage));
						Inst.STARTAGE.defaultValue = monsterDB.startage.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STARTAGE.defaultValue = "";
					}
					break;
				case MAXAGE:
					if (monsterDB.maxage != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.maxage));
						Inst.MAXAGE.defaultValue = monsterDB.maxage.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAXAGE.defaultValue = "";
					}
					break;
				case OLDER:
					if (monsterDB.older != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.older));
						Inst.OLDER.defaultValue = monsterDB.older.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.OLDER.defaultValue = "";
					}
					break;
				case HEALER:
					if (monsterDB.healer != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.healer));
						Inst.HEALER.defaultValue = monsterDB.healer.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HEALER.defaultValue = "";
					}
					break;
				case STARTAFF:
					if (monsterDB.startaff != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.startaff));
						Inst.STARTAFF.defaultValue = monsterDB.startaff.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STARTAFF.defaultValue = "";
					}
					break;
				case SUPPLYBONUS:
					if (monsterDB.supplybonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.supplybonus));
						Inst.SUPPLYBONUS.defaultValue = monsterDB.supplybonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUPPLYBONUS.defaultValue = "";
					}
					break;
				case UWDAMAGE:
					if (monsterDB.uwdamage != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.uwdamage));
						Inst.UWDAMAGE.defaultValue = monsterDB.uwdamage.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UWDAMAGE.defaultValue = "";
					}
					break;
				case COLDPOWER:
					if (monsterDB.coldpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.coldpower));
						Inst.COLDPOWER.defaultValue = monsterDB.coldpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLDPOWER.defaultValue = "";
					}
					break;
				case FIREPOWER:
					if (monsterDB.firepower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.firepower));
						Inst.FIREPOWER.defaultValue = monsterDB.firepower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIREPOWER.defaultValue = "";
					}
					break;
				case STORMPOWER:
					if (monsterDB.stormpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.stormpower));
						Inst.STORMPOWER.defaultValue = monsterDB.stormpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STORMPOWER.defaultValue = "";
					}
					break;
				case DARKPOWER:
					if (monsterDB.darkpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.darkpower));
						Inst.DARKPOWER.defaultValue = monsterDB.darkpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DARKPOWER.defaultValue = "";
					}
					break;
				case SPRINGPOWER:
					if (monsterDB.springpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.springpower));
						Inst.SPRINGPOWER.defaultValue = monsterDB.springpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPRINGPOWER.defaultValue = "";
					}
					break;
				case SUMMERPOWER:
					if (monsterDB.summerpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.summerpower));
						Inst.SUMMERPOWER.defaultValue = monsterDB.summerpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMERPOWER.defaultValue = "";
					}
					break;
				case FALLPOWER:
					if (monsterDB.fallpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.fallpower));
						Inst.FALLPOWER.defaultValue = monsterDB.fallpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FALLPOWER.defaultValue = "";
					}
					break;
				case WINTERPOWER:
					if (monsterDB.winterpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.winterpower));
						Inst.WINTERPOWER.defaultValue = monsterDB.winterpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WINTERPOWER.defaultValue = "";
					}
					break;
				case AMBIDEXTROUS:
					if (monsterDB.ambidextrous != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.ambidextrous));
						Inst.AMBIDEXTROUS.defaultValue = monsterDB.ambidextrous.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AMBIDEXTROUS.defaultValue = "";
					}
					break;
				case BANEFIRESHIELD:
					if (monsterDB.banefireshield != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.banefireshield));
						Inst.BANEFIRESHIELD.defaultValue = monsterDB.banefireshield.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BANEFIRESHIELD.defaultValue = "";
					}
					break;
				case BERSERK:
					if (monsterDB.berserk != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.berserk));
						Inst.BERSERK.defaultValue = monsterDB.berserk.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BERSERK.defaultValue = "";
					}
					break;
				case STANDARD:
					if (monsterDB.standard != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.standard));
						Inst.STANDARD.defaultValue = monsterDB.standard.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STANDARD.defaultValue = "";
					}
					break;
				case ANIMALAWE:
					if (monsterDB.animalawe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.animalawe));
						Inst.ANIMALAWE.defaultValue = monsterDB.animalawe.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ANIMALAWE.defaultValue = "";
					}
					break;
				case AWE:
					if (monsterDB.awe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.awe));
						Inst.AWE.defaultValue = monsterDB.awe.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AWE.defaultValue = "";
					}
					break;
				case FEAR:
					if (monsterDB.fear != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.fear));
						Inst.FEAR.defaultValue = monsterDB.fear.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FEAR.defaultValue = "";
					}
					break;
				case REGENERATION:
					if (monsterDB.regeneration != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.regeneration));
						Inst.REGENERATION.defaultValue = monsterDB.regeneration.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.REGENERATION.defaultValue = "";
					}
					break;
				case REINVIGORATION:
					if (monsterDB.reinvigoration != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.reinvigoration));
						Inst.REINVIGORATION.defaultValue = monsterDB.reinvigoration.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.REINVIGORATION.defaultValue = "";
					}
					break;
				case FIRESHIELD:
					if (monsterDB.fireshield != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.fireshield));
						Inst.FIRESHIELD.defaultValue = monsterDB.fireshield.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRESHIELD.defaultValue = "";
					}
					break;
				case HEAT:
					if (monsterDB.heat != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.heat));
						Inst.HEAT.defaultValue = monsterDB.heat.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HEAT.defaultValue = "";
					}
					break;
				case COLD:
					if (monsterDB.cold != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.cold));
						Inst.COLD.defaultValue = monsterDB.cold.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLD.defaultValue = "";
					}
					break;
				case ICEPROT:
					if (monsterDB.iceprot != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.iceprot));
						Inst.ICEPROT.defaultValue = monsterDB.iceprot.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ICEPROT.defaultValue = "";
					}
					break;
				case POISONCLOUD:
					if (monsterDB.poisoncloud != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poisoncloud));
						Inst.POISONCLOUD.defaultValue = monsterDB.poisoncloud.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POISONCLOUD.defaultValue = "";
					}
					break;
				case DISEASECLOUD:
					if (monsterDB.diseasecloud != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.diseasecloud));
						Inst.DISEASECLOUD.defaultValue = monsterDB.diseasecloud.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DISEASECLOUD.defaultValue = "";
					}
					break;
				case BLOODVENGEANCE:
					if (monsterDB.bloodvengeance != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.bloodvengeance));
						Inst.BLOODVENGEANCE.defaultValue = monsterDB.bloodvengeance.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLOODVENGEANCE.defaultValue = "";
					}
					break;
				case CASTLEDEF:
					if (monsterDB.castledef != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.castledef));
						Inst.CASTLEDEF.defaultValue = monsterDB.castledef.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CASTLEDEF.defaultValue = "";
					}
					break;
				case SIEGEBONUS:
					if (monsterDB.siegebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.siegebonus));
						Inst.SIEGEBONUS.defaultValue = monsterDB.siegebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SIEGEBONUS.defaultValue = "";
					}
					break;
				case PATROLBONUS:
					if (monsterDB.patrolbonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.patrolbonus));
						Inst.PATROLBONUS.defaultValue = monsterDB.patrolbonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PATROLBONUS.defaultValue = "";
					}
					break;
				case PILLAGEBONUS:
					if (monsterDB.pillagebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.pillagebonus));
						Inst.PILLAGEBONUS.defaultValue = monsterDB.pillagebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PILLAGEBONUS.defaultValue = "";
					}
					break;
				case RESEARCHBONUS:
					if (monsterDB.researchbonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.researchbonus));
						Inst.RESEARCHBONUS.defaultValue = monsterDB.researchbonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RESEARCHBONUS.defaultValue = "";
					}
					break;
				case FORGEBONUS:
					if (monsterDB.forgebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.forgebonus));
						Inst.FORGEBONUS.defaultValue = monsterDB.forgebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FORGEBONUS.defaultValue = "";
					}
					break;
				case DOUSE:
					if (monsterDB.douse != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.douse));
						Inst.DOUSE.defaultValue = monsterDB.douse.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOUSE.defaultValue = "";
					}
					break;
				case NOBADEVENTS:
					if (monsterDB.nobadevents != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.nobadevents));
						Inst.NOBADEVENTS.defaultValue = monsterDB.nobadevents.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOBADEVENTS.defaultValue = "";
					}
					break;
				case INCUNREST:
					if (monsterDB.incunrest != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.incunrest));
						Inst.INCUNREST.defaultValue = monsterDB.incunrest.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCUNREST.defaultValue = "";
					}
					break;
				case SPREADDOM:
					if (monsterDB.spreaddom != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.spreaddom));
						Inst.SPREADDOM.defaultValue = monsterDB.spreaddom.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPREADDOM.defaultValue = "";
					}
					break;
				case LEPER:
					if (monsterDB.leper != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.leper));
						Inst.LEPER.defaultValue = monsterDB.leper.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.LEPER.defaultValue = "";
					}
					break;
				case POPKILL:
					if (monsterDB.popkill != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.popkill));
						Inst.POPKILL.defaultValue = monsterDB.popkill.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POPKILL.defaultValue = "";
					}
					break;
				case HERETIC:
					if (monsterDB.heretic != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.heretic));
						Inst.HERETIC.defaultValue = monsterDB.heretic.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HERETIC.defaultValue = "";
					}
					break;
				case ITEMSLOTS:
					if (monsterDB.itemslots != null) {
						((Inst8Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getItemMaskString(monsterDB.itemslots)));
						Inst.ITEMSLOTS.defaultValue = monsterDB.itemslots.toString();
					} else {
						((Inst8Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ITEMSLOTS.defaultValue = "";
					}
					break;
				case NAMETYPE:
					if (monsterDB.nametype != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.nametype));
						Inst.NAMETYPE.defaultValue = monsterDB.nametype.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ITEMSLOTS.defaultValue = "";
					}
					break;
				case MAGICSKILL1:
					if (monsterDB.magicskillpath1 != null && monsterDB.magicskilllevel1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(monsterDB.magicskillpath1)));
						Inst.MAGICSKILL1.defaultValue = monsterDB.magicskillpath1.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicskilllevel1));
						Inst.MAGICSKILL1.defaultValue2 = monsterDB.magicskilllevel1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.MAGICSKILL1.defaultValue = "0";
						Inst.MAGICSKILL1.defaultValue2 = "1";
					}
					break;
				case MAGICSKILL2:
					if (monsterDB.magicskillpath2 != null && monsterDB.magicskilllevel2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(monsterDB.magicskillpath2)));
						Inst.MAGICSKILL2.defaultValue = monsterDB.magicskillpath2.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicskilllevel2));
						Inst.MAGICSKILL2.defaultValue2 = monsterDB.magicskilllevel2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.MAGICSKILL2.defaultValue = "0";
						Inst.MAGICSKILL2.defaultValue2 = "1";
					}
					break;
				case MAGICSKILL3:
					if (monsterDB.magicskillpath3 != null && monsterDB.magicskilllevel3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(monsterDB.magicskillpath3)));
						Inst.MAGICSKILL3.defaultValue = monsterDB.magicskillpath3.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicskilllevel3));
						Inst.MAGICSKILL3.defaultValue2 = monsterDB.magicskilllevel3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.MAGICSKILL3.defaultValue = "0";
						Inst.MAGICSKILL3.defaultValue2 = "1";
					}
					break;
				case MAGICSKILL4:
					if (monsterDB.magicskillpath4 != null && monsterDB.magicskilllevel4 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(monsterDB.magicskillpath4)));
						Inst.MAGICSKILL4.defaultValue = monsterDB.magicskillpath4.toString();
						((Inst3Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicskilllevel4));
						Inst.MAGICSKILL4.defaultValue2 = monsterDB.magicskilllevel4.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst3Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.MAGICSKILL4.defaultValue = "0";
						Inst.MAGICSKILL4.defaultValue2 = "1";
					}
					break;
				case CUSTOMMAGIC1:
					if (monsterDB.custommagicpath1 != null && monsterDB.custommagicchance1 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath1));
						Inst.CUSTOMMAGIC1.defaultValue = monsterDB.custommagicpath1.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance1));
						Inst.CUSTOMMAGIC1.defaultValue2 = monsterDB.custommagicchance1.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC1.defaultValue = "128";
						Inst.CUSTOMMAGIC1.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC2:
					if (monsterDB.custommagicpath2 != null && monsterDB.custommagicchance2 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath2));
						Inst.CUSTOMMAGIC2.defaultValue = monsterDB.custommagicpath2.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance2));
						Inst.CUSTOMMAGIC2.defaultValue2 = monsterDB.custommagicchance2.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC2.defaultValue = "128";
						Inst.CUSTOMMAGIC2.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC3:
					if (monsterDB.custommagicpath3 != null && monsterDB.custommagicchance3 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath3));
						Inst.CUSTOMMAGIC3.defaultValue = monsterDB.custommagicpath3.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance3));
						Inst.CUSTOMMAGIC3.defaultValue2 = monsterDB.custommagicchance3.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC3.defaultValue = "128";
						Inst.CUSTOMMAGIC3.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC4:
					if (monsterDB.custommagicpath4 != null && monsterDB.custommagicchance4 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath4));
						Inst.CUSTOMMAGIC4.defaultValue = monsterDB.custommagicpath4.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance4));
						Inst.CUSTOMMAGIC4.defaultValue2 = monsterDB.custommagicchance4.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC4.defaultValue = "128";
						Inst.CUSTOMMAGIC4.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC5:
					if (monsterDB.custommagicpath5 != null && monsterDB.custommagicchance5 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath5));
						Inst.CUSTOMMAGIC5.defaultValue = monsterDB.custommagicpath5.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance5));
						Inst.CUSTOMMAGIC5.defaultValue2 = monsterDB.custommagicchance5.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC5.defaultValue = "128";
						Inst.CUSTOMMAGIC5.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC6:
					if (monsterDB.custommagicpath6 != null && monsterDB.custommagicchance6 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath6));
						Inst.CUSTOMMAGIC6.defaultValue = monsterDB.custommagicpath6.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance6));
						Inst.CUSTOMMAGIC6.defaultValue2 = monsterDB.custommagicchance6.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC6.defaultValue = "128";
						Inst.CUSTOMMAGIC6.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC7:
					if (monsterDB.custommagicpath7 != null && monsterDB.custommagicchance7 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath7));
						Inst.CUSTOMMAGIC7.defaultValue = monsterDB.custommagicpath7.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance7));
						Inst.CUSTOMMAGIC7.defaultValue2 = monsterDB.custommagicchance7.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC7.defaultValue = "128";
						Inst.CUSTOMMAGIC7.defaultValue2 = "100";
					}
					break;
				case CUSTOMMAGIC8:
					if (monsterDB.custommagicpath8 != null && monsterDB.custommagicchance8 != null) {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicpath8));
						Inst.CUSTOMMAGIC8.defaultValue = monsterDB.custommagicpath8.toString();
						((Inst7Fields)fields.getValue()).defaultLabel2.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.custommagicchance8));
						Inst.CUSTOMMAGIC8.defaultValue2 = monsterDB.custommagicchance8.toString();
					} else {
						((Inst7Fields)fields.getValue()).defaultLabel1.setText("");
						((Inst7Fields)fields.getValue()).defaultLabel2.setText("");
						Inst.CUSTOMMAGIC8.defaultValue = "128";
						Inst.CUSTOMMAGIC8.defaultValue2 = "100";
					}
					break;
				case MAGICBOOST1:
					((Inst3Fields)fields.getValue()).defaultLabel1.setText(monsterDB.magicboost1 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicboost1) : "");
					((Inst3Fields)fields.getValue()).defaultLabel2.setText(monsterDB.magicboost2 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicboost2) : "");
					break;
				case GEMPROD1:
					((Inst3Fields)fields.getValue()).defaultLabel1.setText(monsterDB.gemprod1 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.gemprod1) : "");
					((Inst3Fields)fields.getValue()).defaultLabel2.setText(monsterDB.gemprod2 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.gemprod2) : "");
					break;
				case ONEBATTLESPELL:
					if (monsterDB.onebattlespell != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.onebattlespell));
						Inst.ONEBATTLESPELL.defaultValue = monsterDB.onebattlespell.toString();
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONEBATTLESPELL.defaultValue = "";
					}
					break;
				case CLEAR:
					if (monsterDB.clear != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.clear));
						Inst.CLEAR.defaultValue = monsterDB.clear.toString();
					}
					break;
				case CLEARMAGIC:
					if (monsterDB.clearmagic != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.clearmagic));
						Inst.CLEARMAGIC.defaultValue = monsterDB.clearmagic.toString();
					}
					break;
				case CLEARSPEC:
					if (monsterDB.clearspec != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.clearspec));
						Inst.CLEARSPEC.defaultValue = monsterDB.clearspec.toString();
					}
					break;
				case FEMALE:
					if (monsterDB.female != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.female));
						Inst.FEMALE.defaultValue = monsterDB.female.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FEMALE.defaultValue = "";
					}
					break;
				case MOUNTED:
					if (monsterDB.mounted != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.mounted));
						Inst.MOUNTED.defaultValue = monsterDB.mounted.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MOUNTED.defaultValue = "";
					}
					break;
				case HOLY:
					if (monsterDB.holy != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.holy));
						Inst.HOLY.defaultValue = monsterDB.holy.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HOLY.defaultValue = "";
					}
					break;
				case ANIMAL:
					if (monsterDB.animal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.animal));
						Inst.ANIMAL.defaultValue = monsterDB.animal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ANIMAL.defaultValue = "";
					}
					break;
				case UNDEAD:
					if (monsterDB.undead != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.undead));
						Inst.UNDEAD.defaultValue = monsterDB.undead.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UNDEAD.defaultValue = "";
					}
					break;
				case DEMON:
					if (monsterDB.demon != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.demon));
						Inst.DEMON.defaultValue = monsterDB.demon.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEMON.defaultValue = "";
					}
					break;
				case MAGICBEING:
					if (monsterDB.magicbeing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.magicbeing));
						Inst.MAGICBEING.defaultValue = monsterDB.magicbeing.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAGICBEING.defaultValue = "";
					}
					break;
				case STONEBEING:
					if (monsterDB.stonebeing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.stonebeing));
						Inst.STONEBEING.defaultValue = monsterDB.stonebeing.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STONEBEING.defaultValue = "";
					}
					break;
				case INANIMATE:
					if (monsterDB.inanimate != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.inanimate));
						Inst.INANIMATE.defaultValue = monsterDB.inanimate.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INANIMATE.defaultValue = "";
					}
					break;
				case COLDBLOOD:
					if (monsterDB.coldblood != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.coldblood));
						Inst.COLDBLOOD.defaultValue = monsterDB.coldblood.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLDBLOOD.defaultValue = "";
					}
					break;
				case IMMORTAL:
					if (monsterDB.immortal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.immortal));
						Inst.IMMORTAL.defaultValue = monsterDB.immortal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.IMMORTAL.defaultValue = "";
					}
					break;
				case BLIND:
					if (monsterDB.blind != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.blind));
						Inst.BLIND.defaultValue = monsterDB.blind.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLIND.defaultValue = "";
					}
					break;
				case UNIQUE:
					if (monsterDB.unique != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.unique));
						Inst.UNIQUE.defaultValue = monsterDB.unique.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UNIQUE.defaultValue = "";
					}
					break;
				case IMMOBILE:
					if (monsterDB.immobile != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.immobile));
						Inst.IMMOBILE.defaultValue = monsterDB.immobile.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.IMMOBILE.defaultValue = "";
					}
					break;
				case AQUATIC:
					if (monsterDB.aquatic != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.aquatic));
						Inst.AQUATIC.defaultValue = monsterDB.aquatic.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AQUATIC.defaultValue = "";
					}
					break;
				case AMPHIBIAN:
					if (monsterDB.amphibian != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.amphibian));
						Inst.AMPHIBIAN.defaultValue = monsterDB.amphibian.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AMPHIBIAN.defaultValue = "";
					}
					break;
				case POORAMPHIBIAN:
					if (monsterDB.pooramphibian != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.pooramphibian));
						Inst.POORAMPHIBIAN.defaultValue = monsterDB.pooramphibian.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POORAMPHIBIAN.defaultValue = "";
					}
					break;
				case FLYING:
					if (monsterDB.flying != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.flying));
						Inst.FLYING.defaultValue = monsterDB.flying.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FLYING.defaultValue = "";
					}
					break;
				case STORMIMMUNE:
					if (monsterDB.stormimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.stormimmune));
						Inst.STORMIMMUNE.defaultValue = monsterDB.stormimmune.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STORMIMMUNE.defaultValue = "";
					}
					break;
				case SAILING:
					if (monsterDB.sailing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.sailing));
						Inst.SAILING.defaultValue = monsterDB.sailing.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SAILING.defaultValue = "";
					}
					break;
				case FORESTSURVIVAL:
					if (monsterDB.forestsurvival != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.forestsurvival));
						Inst.FORESTSURVIVAL.defaultValue = monsterDB.forestsurvival.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FORESTSURVIVAL.defaultValue = "";
					}
					break;
				case MOUNTAINSURVIVAL:
					if (monsterDB.mountainsurvival != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.mountainsurvival));
						Inst.MOUNTAINSURVIVAL.defaultValue = monsterDB.mountainsurvival.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MOUNTAINSURVIVAL.defaultValue = "";
					}
					break;
				case SWAMPSURVIVAL:
					if (monsterDB.swampsurvival != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.swampsurvival));
						Inst.SWAMPSURVIVAL.defaultValue = monsterDB.swampsurvival.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SWAMPSURVIVAL.defaultValue = "";
					}
					break;
				case WASTESURVIVAL:
					if (monsterDB.wastesurvival != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.wastesurvival));
						Inst.WASTESURVIVAL.defaultValue = monsterDB.wastesurvival.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WASTESURVIVAL.defaultValue = "";
					}
					break;
				case ILLUSION:
					if (monsterDB.illusion != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.illusion));
						Inst.ILLUSION.defaultValue = monsterDB.illusion.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ILLUSION.defaultValue = "";
					}
					break;
				case SPY:
					if (monsterDB.spy != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.spy));
						Inst.SPY.defaultValue = monsterDB.spy.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPY.defaultValue = "";
					}
					break;
				case ASSASSIN:
					if (monsterDB.assassin != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.assassin));
						Inst.ASSASSIN.defaultValue = monsterDB.assassin.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ASSASSIN.defaultValue = "";
					}
					break;
				case HEAL:
					if (monsterDB.heal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.heal));
						Inst.HEAL.defaultValue = monsterDB.heal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HEAL.defaultValue = "";
					}
					break;
				case NOHEAL:
					if (monsterDB.noheal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.noheal));
						Inst.NOHEAL.defaultValue = monsterDB.noheal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOHEAL.defaultValue = "";
					}
					break;
				case NEEDNOTEAT:
					if (monsterDB.neednoteat != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.neednoteat));
						Inst.NEEDNOTEAT.defaultValue = monsterDB.neednoteat.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NEEDNOTEAT.defaultValue = "";
					}
					break;
				case ETHEREAL:
					if (monsterDB.ethereal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.ethereal));
						Inst.ETHEREAL.defaultValue = monsterDB.ethereal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ETHEREAL.defaultValue = "";
					}
					break;
				case TRAMPLE:
					if (monsterDB.trample != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.trample));
						Inst.TRAMPLE.defaultValue = monsterDB.trample.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TRAMPLE.defaultValue = "";
					}
					break;
				case ENTANGLE:
					if (monsterDB.entangle != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.entangle));
						Inst.ENTANGLE.defaultValue = monsterDB.entangle.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ENTANGLE.defaultValue = "";
					}
					break;
				case EYELOSS:
					if (monsterDB.eyeloss != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.eyeloss));
						Inst.EYELOSS.defaultValue = monsterDB.eyeloss.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EYELOSS.defaultValue = "";
					}
					break;
				case HORRORMARK:
					if (monsterDB.horrormark != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.horrormark));
						Inst.HORRORMARK.defaultValue = monsterDB.horrormark.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HORRORMARK.defaultValue = "";
					}
					break;
				case POISONARMOR:
					if (monsterDB.poisonarmor != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poisonarmor));
						Inst.POISONARMOR.defaultValue = monsterDB.poisonarmor.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POISONARMOR.defaultValue = "";
					}
					break;
				case INQUISITOR:
					if (monsterDB.inquisitor != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.inquisitor));
						Inst.INQUISITOR.defaultValue = monsterDB.inquisitor.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INQUISITOR.defaultValue = "";
					}
					break;
				case NOITEM:
					if (monsterDB.noitem != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.noitem));
						Inst.NOITEM.defaultValue = monsterDB.noitem.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOITEM.defaultValue = "";
					}
					break;
				case NOLEADER:
					if (monsterDB.noleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.noleader));
						Inst.NOLEADER.defaultValue = monsterDB.noleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOLEADER.defaultValue = "";
					}
					break;
				case POORLEADER:
					if (monsterDB.poorleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poorleader));
						Inst.POORLEADER.defaultValue = monsterDB.poorleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POORLEADER.defaultValue = "";
					}
					break;
				case OKLEADER:
					if (monsterDB.okleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.okleader));
						Inst.OKLEADER.defaultValue = monsterDB.okleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.OKLEADER.defaultValue = "";
					}
					break;
				case GOODLEADER:
					if (monsterDB.goodleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.goodleader));
						Inst.GOODLEADER.defaultValue = monsterDB.goodleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GOODLEADER.defaultValue = "";
					}
					break;
				case EXPERTLEADER:
					if (monsterDB.expertleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.expertleader));
						Inst.EXPERTLEADER.defaultValue = monsterDB.expertleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EXPERTLEADER.defaultValue = "";
					}
					break;
				case SUPERIORLEADER:
					if (monsterDB.superiorleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.superiorleader));
						Inst.SUPERIORLEADER.defaultValue = monsterDB.superiorleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUPERIORLEADER.defaultValue = "";
					}
					break;
				case NOMAGICLEADER:
					if (monsterDB.nomagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.nomagicleader));
						Inst.NOMAGICLEADER.defaultValue = monsterDB.nomagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOMAGICLEADER.defaultValue = "";
					}
					break;
				case POORMAGICLEADER:
					if (monsterDB.poormagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poormagicleader));
						Inst.POORMAGICLEADER.defaultValue = monsterDB.poormagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POORMAGICLEADER.defaultValue = "";
					}
					break;
				case OKMAGICLEADER:
					if (monsterDB.okmagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.okmagicleader));
						Inst.OKMAGICLEADER.defaultValue = monsterDB.okmagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.OKMAGICLEADER.defaultValue = "";
					}
					break;
				case GOODMAGICLEADER:
					if (monsterDB.goodmagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.goodmagicleader));
						Inst.GOODMAGICLEADER.defaultValue = monsterDB.goodmagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GOODMAGICLEADER.defaultValue = "";
					}
					break;
				case EXPERTMAGICLEADER:
					if (monsterDB.expertmagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.expertmagicleader));
						Inst.EXPERTMAGICLEADER.defaultValue = monsterDB.expertmagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EXPERTMAGICLEADER.defaultValue = "";
					}
					break;
				case SUPERIORMAGICLEADER:
					if (monsterDB.superiormagicleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.superiormagicleader));
						Inst.SUPERIORMAGICLEADER.defaultValue = monsterDB.superiormagicleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUPERIORMAGICLEADER.defaultValue = "";
					}
					break;
				case NOUNDEADLEADER:
					if (monsterDB.noundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.noundeadleader));
						Inst.NOUNDEADLEADER.defaultValue = monsterDB.noundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOUNDEADLEADER.defaultValue = "";
					}
					break;
				case POORUNDEADLEADER:
					if (monsterDB.poorundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.poorundeadleader));
						Inst.POORUNDEADLEADER.defaultValue = monsterDB.poorundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POORUNDEADLEADER.defaultValue = "";
					}
					break;
				case OKUNDEADLEADER:
					if (monsterDB.okundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.okundeadleader));
						Inst.OKUNDEADLEADER.defaultValue = monsterDB.okundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.OKUNDEADLEADER.defaultValue = "";
					}
					break;
				case GOODUNDEADLEADER:
					if (monsterDB.goodundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.goodundeadleader));
						Inst.GOODUNDEADLEADER.defaultValue = monsterDB.goodundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GOODUNDEADLEADER.defaultValue = "";
					}
					break;
				case EXPERTUNDEADLEADER:
					if (monsterDB.expertundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.expertundeadleader));
						Inst.EXPERTUNDEADLEADER.defaultValue = monsterDB.expertundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EXPERTUNDEADLEADER.defaultValue = "";
					}
					break;
				case SUPERIORUNDEADLEADER:
					if (monsterDB.superiorundeadleader != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.superiorundeadleader));
						Inst.SUPERIORUNDEADLEADER.defaultValue = monsterDB.superiorundeadleader.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUPERIORUNDEADLEADER.defaultValue = "";
					}
					break;
				case WEAPON1:
					if (monsterDB.weapon1 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.weapon1));
						Inst.WEAPON1.defaultValue = monsterDB.weapon1;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WEAPON1.defaultValue = "";
					}
					break;
				case WEAPON2:
					if (monsterDB.weapon2 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.weapon2));
						Inst.WEAPON2.defaultValue = monsterDB.weapon2;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WEAPON2.defaultValue = "";
					}
					break;
				case WEAPON3:
					if (monsterDB.weapon3 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.weapon3));
						Inst.WEAPON3.defaultValue = monsterDB.weapon3;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WEAPON3.defaultValue = "";
					}
					break;
				case WEAPON4:
					if (monsterDB.weapon4 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.weapon4));
						Inst.WEAPON4.defaultValue = monsterDB.weapon4;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WEAPON4.defaultValue = "";
					}
					break;
				case FIRSTSHAPE:
					if (monsterDB.firstshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.firstshape));
						Inst.FIRSTSHAPE.defaultValue = monsterDB.firstshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRSTSHAPE.defaultValue = "";
					}
					break;
				case SECONDSHAPE:
					if (monsterDB.secondshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.secondshape));
						Inst.SECONDSHAPE.defaultValue = monsterDB.secondshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SECONDSHAPE.defaultValue = "";
					}
					break;
				case SECONDTMPSHAPE:
					if (monsterDB.secondtmpshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.secondtmpshape));
						Inst.SECONDTMPSHAPE.defaultValue = monsterDB.secondtmpshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SECONDTMPSHAPE.defaultValue = "";
					}
					break;
				case SHAPECHANGE:
					if (monsterDB.shapechange != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.shapechange));
						Inst.SHAPECHANGE.defaultValue = monsterDB.shapechange;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SHAPECHANGE.defaultValue = "";
					}
					break;
				case LANDSHAPE:
					if (monsterDB.landshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.landshape));
						Inst.LANDSHAPE.defaultValue = monsterDB.landshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.LANDSHAPE.defaultValue = "";
					}
					break;
				case WATERSHAPE:
					if (monsterDB.watershape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.watershape));
						Inst.WATERSHAPE.defaultValue = monsterDB.watershape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WATERSHAPE.defaultValue = "";
					}
					break;
				case FORESTSHAPE:
					if (monsterDB.forestshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.forestshape));
						Inst.FORESTSHAPE.defaultValue = monsterDB.forestshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FORESTSHAPE.defaultValue = "";
					}
					break;
				case PLAINSHAPE:
					if (monsterDB.plainshape != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.plainshape));
						Inst.PLAINSHAPE.defaultValue = monsterDB.plainshape;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PLAINSHAPE.defaultValue = "";
					}
					break;
				case DOMSUMMON:
					if (monsterDB.domsummon != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.domsummon));
						Inst.DOMSUMMON.defaultValue = monsterDB.domsummon;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON.defaultValue = "";
					}
					break;
				case DOMSUMMON2:
					if (monsterDB.domsummon2 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.domsummon2));
						Inst.DOMSUMMON2.defaultValue = monsterDB.domsummon2;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON2.defaultValue = "";
					}
					break;
				case DOMSUMMON20:
					if (monsterDB.domsummon20 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.domsummon20));
						Inst.DOMSUMMON20.defaultValue = monsterDB.domsummon20;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON20.defaultValue = "";
					}
					break;
				case MAKEMONSTER1:
					if (monsterDB.makemonster1 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.makemonster1));
						Inst.MAKEMONSTER1.defaultValue = monsterDB.makemonster1;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTER1.defaultValue = "";
					}
					break;
				case MAKEMONSTER2:
					if (monsterDB.makemonster2 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.makemonster2));
						Inst.MAKEMONSTER2.defaultValue = monsterDB.makemonster2;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTER2.defaultValue = "";
					}
					break;
				case MAKEMONSTER3:
					if (monsterDB.makemonster3 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.makemonster3));
						Inst.MAKEMONSTER3.defaultValue = monsterDB.makemonster3;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTER3.defaultValue = "";
					}
					break;
				case MAKEMONSTER4:
					if (monsterDB.makemonster4 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.makemonster4));
						Inst.MAKEMONSTER4.defaultValue = monsterDB.makemonster4;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTER4.defaultValue = "";
					}
					break;
				case MAKEMONSTER5:
					if (monsterDB.makemonster5 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.makemonster5));
						Inst.MAKEMONSTER5.defaultValue = monsterDB.makemonster5;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTER5.defaultValue = "";
					}
					break;
				case SUMMON1:
					if (monsterDB.summon1 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.summon1));
						Inst.SUMMON1.defaultValue = monsterDB.summon1;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON1.defaultValue = "";
					}
					break;
				case SUMMON5:
					if (monsterDB.summon5 != null) {
						((Inst5Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", monsterDB.summon5));
						Inst.SUMMON5.defaultValue = monsterDB.summon5;
					} else {
						((Inst5Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON5.defaultValue = "";
					}
					break;
				}
			}
		}
		name.getParent().getParent().layout(true, true);
	}
	
	private boolean isDefaultValue(Inst value, MonsterDB monsterDB) {
		switch (value) {
		case MAGICSKILL1:
			if (monsterDB.magicskillpath1 != null && monsterDB.magicskilllevel1 != null) {
				return true;
			}
			break;
		case MAGICSKILL2:
			if (monsterDB.magicskillpath2 != null && monsterDB.magicskilllevel2 != null) {
				return true;
			}
			break;
		case MAGICSKILL3:
			if (monsterDB.magicskillpath3 != null && monsterDB.magicskilllevel3 != null) {
				return true;
			}
			break;
		case MAGICSKILL4:
			if (monsterDB.magicskillpath4 != null && monsterDB.magicskilllevel4 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC1:
			if (monsterDB.custommagicpath1 != null && monsterDB.custommagicchance1 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC2:
			if (monsterDB.custommagicpath2 != null && monsterDB.custommagicchance2 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC3:
			if (monsterDB.custommagicpath3 != null && monsterDB.custommagicchance3 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC4:
			if (monsterDB.custommagicpath4 != null && monsterDB.custommagicchance4 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC5:
			if (monsterDB.custommagicpath5 != null && monsterDB.custommagicchance5 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC6:
			if (monsterDB.custommagicpath6 != null && monsterDB.custommagicchance6 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC7:
			if (monsterDB.custommagicpath7 != null && monsterDB.custommagicchance7 != null) {
				return true;
			}
			break;
		case CUSTOMMAGIC8:
			if (monsterDB.custommagicpath8 != null && monsterDB.custommagicchance8 != null) {
				return true;
			}
			break;
		}
		return false;
	}
	
	private String getSelectMonstername(Monster monster) {
		if (monster instanceof SelectMonsterByName) {
			return ((SelectMonsterByName)monster).getValue();
		} else {
			int id = ((SelectMonsterById)monster).getValue();
			return Database.getMonsterName(id);
		}
	}
	
	private String getSelectMonsterdescr(Monster monster) {
		if (monster instanceof SelectMonsterByName) {
			String name = ((SelectMonsterByName)monster).getValue();
			return Database.getMonsterDescr(Database.getMonster(name).id);
		} else if (monster instanceof SelectMonsterById) {
			return Database.getMonsterDescr(((SelectMonsterById)monster).getValue());
		}
		return "";
	}
	
	private int getSelectMonsterid(Monster monster) {
		if (monster instanceof SelectMonsterByName) {
			MonsterDB monsterDB = Database.getMonster(((SelectMonsterByName) monster).getValue());
			return monsterDB != null && monsterDB.id != null ? monsterDB.id.intValue() : 0;
		} else {
			return ((SelectMonsterById)monster).getValue();
		}
	}
	
	private String getMonstername(Monster item) {
		EList<MonsterMods> list = item.getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst1) {
				if (((MonsterInst1)mod).isName()) {
					return ((MonsterInst1)mod).getValue();
				}
			}
		}
		return null;
	}
	
	private void setMonstername(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Monster monsterToEdit = (Monster)input;
				EList<MonsterMods> mods = monsterToEdit.getMods();
				boolean nameSet = false;
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst1) {
						if (((MonsterInst1)mod).isName()) {
							((MonsterInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					MonsterInst1 nameInst = DmFactory.eINSTANCE.createMonsterInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}

	private void setMonsterdescr(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Monster monsterToEdit = (Monster)input;
				EList<MonsterMods> mods = monsterToEdit.getMods();
				boolean nameSet = false;
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst1) {
						if (((MonsterInst1)mod).isDescr()) {
							((MonsterInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					MonsterInst1 nameInst = DmFactory.eINSTANCE.createMonsterInst1();
					nameInst.setDescr(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}
	
	private int getItemMask(int hands, int heads, int bodies, int feet, int misc) {
		return hands | heads | bodies | feet | misc;
	}

	private int getHands(int mask) {
		return mask & 30;
	}

	private int getHeads(int mask) {
		return mask & 384;
	}

	private int getBodies(int mask) {
		return mask & 1024;
	}

	private int getFeet(int mask) {
		return mask & 2048;
	}

	private int getMisc(int mask) {
		return mask & 61440;
	}
	
	private String getItemMaskString(int mask) {
		StringBuffer string = new StringBuffer();
		if ((mask & 30) == 2) {
			string.append("1 hand");
		} else if ((mask & 30) == 6) {
			string.append("2 hands");
		} else if ((mask & 30) == 14) {
			string.append("3 hands");
		} else if ((mask & 30) == 30) {
			string.append("4 hands");
		}
		if ((mask & 384) == 128) {
			if (string.length() > 0) string.append(",");
			string.append("1 head");
		} else if ((mask & 384) == 384) {
			if (string.length() > 0) string.append(",");
			string.append("2 heads");
		}
		if ((mask & 1024) == 1024) {
			if (string.length() > 0) string.append(",");
			string.append("1 body");
		}
		if ((mask & 2048) == 2048) {
			if (string.length() > 0) string.append(",");
			string.append("1 feet");
		}
		if ((mask & 61440) == 4096) {
			if (string.length() > 0) string.append(",");
			string.append("1 misc");
		} else if ((mask & 61440) == 12288) {
			if (string.length() > 0) string.append(",");
			string.append("2 misc");
		} else if ((mask & 61440) == 28672) {
			if (string.length() > 0) string.append(",");
			string.append("3 misc");
		} else if ((mask & 61440) == 61440) {
			if (string.length() > 0) string.append(",");
			string.append("4 misc");
		}
		return string.toString();
	}

	private String getInst1(Inst inst2, Object monster) {
		EList<MonsterMods> list = ((Monster)monster).getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst1) {
				switch (inst2) {
				case NAME:
					if (((MonsterInst1)mod).isName()){
						return ((MonsterInst1)mod).getValue();
					}
					break;
				case SPR1:
					if (((MonsterInst1)mod).isSpr1()){
						return ((MonsterInst1)mod).getValue();
					}
					break;
				case SPR2:
					if (((MonsterInst1)mod).isSpr2()){
						return ((MonsterInst1)mod).getValue();
					}
					break;
				case DESCR:
					if (((MonsterInst1)mod).isDescr()){
						return ((MonsterInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer getInst2(Inst inst2, Object monster) {
		EList<MonsterMods> list = ((Monster)monster).getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst2) {
				switch (inst2) {
				case SPECIALLOOK:
					if (((MonsterInst2)mod).isSpeciallook()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case AP:
					if (((MonsterInst2)mod).isAp()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case MAPMOVE:
					if (((MonsterInst2)mod).isMapmove()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case HP:
					if (((MonsterInst2)mod).isHp()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case PROT:
					if (((MonsterInst2)mod).isProt()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SIZE:
					if (((MonsterInst2)mod).isSize()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case RESSIZE:
					if (((MonsterInst2)mod).isRessize()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STR:
					if (((MonsterInst2)mod).isStr()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case ENC:
					if (((MonsterInst2)mod).isEnc()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case ATT:
					if (((MonsterInst2)mod).isAtt()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case DEF:
					if (((MonsterInst2)mod).isDef()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case PREC:
					if (((MonsterInst2)mod).isPrec()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case MR:
					if (((MonsterInst2)mod).isMr()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case MOR:
					if (((MonsterInst2)mod).isMor()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case GCOST:
					if (((MonsterInst2)mod).isGcost()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case RCOST:
					if (((MonsterInst2)mod).isRcost()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case PATHCOST:
					if (((MonsterInst2)mod).isPathcost()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STARTDOM:
					if (((MonsterInst2)mod).isStartdom()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case EYES:
					if (((MonsterInst2)mod).isEyes()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case COPYSTATS:
					if (((MonsterInst2)mod).isCopystats()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case COPYSPR:
					if (((MonsterInst2)mod).isCopyspr()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case RESTRICTEDGOD:
					if (((MonsterInst2)mod).isRestrictedgod()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SHATTEREDSOUL:
					if (((MonsterInst2)mod).isShatteredsoul()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case COLDRES:
					if (((MonsterInst2)mod).isColdres()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FIRERES:
					if (((MonsterInst2)mod).isFireres()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case POISONRES:
					if (((MonsterInst2)mod).isPoisonres()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SHOCKRES:
					if (((MonsterInst2)mod).isShockres()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case DARKVISION:
					if (((MonsterInst2)mod).isDarkvision()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SEDUCE:
					if (((MonsterInst2)mod).isSeduce()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SUCCUBUS:
					if (((MonsterInst2)mod).isSuccubus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case BECKON:
					if (((MonsterInst2)mod).isBeckon()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STARTAGE:
					if (((MonsterInst2)mod).isStartage()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case MAXAGE:
					if (((MonsterInst2)mod).isMaxage()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case OLDER:
					if (((MonsterInst2)mod).isOlder()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case HEALER:
					if (((MonsterInst2)mod).isHealer()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STARTAFF:
					if (((MonsterInst2)mod).isStartaff()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SUPPLYBONUS:
					if (((MonsterInst2)mod).isSupplybonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case UWDAMAGE:
					if (((MonsterInst2)mod).isUwdamage()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case HOMESICK:
					if (((MonsterInst2)mod).isHomesick()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case COLDPOWER:
					if (((MonsterInst2)mod).isColdpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FIREPOWER:
					if (((MonsterInst2)mod).isFirepower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STORMPOWER:
					if (((MonsterInst2)mod).isStormpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case DARKPOWER:
					if (((MonsterInst2)mod).isDarkpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SPRINGPOWER:
					if (((MonsterInst2)mod).isSpringpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SUMMERPOWER:
					if (((MonsterInst2)mod).isSummerpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FALLPOWER:
					if (((MonsterInst2)mod).isFallpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case WINTERPOWER:
					if (((MonsterInst2)mod).isWinterpower()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case AMBIDEXTROUS:
					if (((MonsterInst2)mod).isAmbidextrous()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case BANEFIRESHIELD:
					if (((MonsterInst2)mod).isBanefireshield()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case BERSERK:
					if (((MonsterInst2)mod).isBerserk()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case STANDARD:
					if (((MonsterInst2)mod).isStandard()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case ANIMALAWE:
					if (((MonsterInst2)mod).isAnimalawe()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case AWE:
					if (((MonsterInst2)mod).isAwe()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FEAR:
					if (((MonsterInst2)mod).isFear()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case REGENERATION:
					if (((MonsterInst2)mod).isRegeneration()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case REINVIGORATION:
					if (((MonsterInst2)mod).isReinvigoration()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FIRESHIELD:
					if (((MonsterInst2)mod).isFireshield()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
//				case HEAT:
//					if (((MonsterInst6)mod).isHeat()){
//						return Integer.valueOf(((MonsterInst2)mod).getValue());
//					}
//					break;
//				case COLD:
//					if (((MonsterInst6)mod).isCold()){
//						return Integer.valueOf(((MonsterInst2)mod).getValue());
//					}
//					break;
				case ICEPROT:
					if (((MonsterInst2)mod).isIceprot()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case POISONCLOUD:
					if (((MonsterInst2)mod).isPoisoncloud()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case DISEASECLOUD:
					if (((MonsterInst2)mod).isDiseasecloud()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case BLOODVENGEANCE:
					if (((MonsterInst2)mod).isBloodvengeance()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case CASTLEDEF:
					if (((MonsterInst2)mod).isCastledef()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SIEGEBONUS:
					if (((MonsterInst2)mod).isSiegebonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case PATROLBONUS:
					if (((MonsterInst2)mod).isPatrolbonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case PILLAGEBONUS:
					if (((MonsterInst2)mod).isPillagebonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case RESEARCHBONUS:
					if (((MonsterInst2)mod).isResearchbonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case FORGEBONUS:
					if (((MonsterInst2)mod).isForgebonus()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case DOUSE:
					if (((MonsterInst2)mod).isDouse()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case NOBADEVENTS:
					if (((MonsterInst2)mod).isNobadevents()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case INCUNREST:
					if (((MonsterInst2)mod).isIncunrest()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case SPREADDOM:
					if (((MonsterInst2)mod).isSpreaddom()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case LEPER:
					if (((MonsterInst2)mod).isLeper()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case POPKILL:
					if (((MonsterInst2)mod).isPopkill()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case HERETIC:
					if (((MonsterInst2)mod).isHeretic()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case ITEMSLOTS:
					if (((MonsterInst2)mod).isItemslots()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				case NAMETYPE:
					if (((MonsterInst2)mod).isNametype()){
						return Integer.valueOf(((MonsterInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer[] getInst3(Inst inst3, Object monster) {
		int magicSkillCount = 0;
		int customMagicCount = 0;
		int boostCount = 0;
		int gemProdCount = 0;
		EList<MonsterMods> list = ((Monster)monster).getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst3) {
				switch (inst3) {
				case MAGICSKILL1:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 1) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL2:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 2) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL3:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 3) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL4:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 4) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL5:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 5) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL6:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 6) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL7:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 7) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICSKILL8:
					if (((MonsterInst3)mod).isMagicskill()) {
						magicSkillCount++;
						if (magicSkillCount == 8) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC1:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 1) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC2:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 2) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC3:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 3) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC4:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 4) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC5:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 5) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC6:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 6) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC7:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 7) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case CUSTOMMAGIC8:
					if (((MonsterInst3)mod).isCustommagic()) {
						customMagicCount++;
						if (customMagicCount == 8) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST1:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 1) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST2:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 2) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST3:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 3) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST4:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 4) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST5:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 5) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST6:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 6) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST7:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 7) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST8:
					if (((MonsterInst3)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 8) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD1:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 1) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD2:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 2) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD3:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 3) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD4:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 4) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD5:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 5) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD6:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 6) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD7:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 7) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				case GEMPROD8:
					if (((MonsterInst3)mod).isGemprod()) {
						gemProdCount++;
						if (gemProdCount == 8) {
							return new Integer[]{Integer.valueOf(((MonsterInst3)mod).getValue1()), Integer.valueOf(((MonsterInst3)mod).getValue2())};
						}
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Boolean getInst4(Inst inst4, Object monster) {
		EList<MonsterMods> list = ((Monster)monster).getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst4) {
				switch (inst4) {
				case CLEAR:
					if (((MonsterInst4)mod).isClear()){
						return Boolean.TRUE;
					}
					break;
				case CLEARMAGIC:
					if (((MonsterInst4)mod).isClearmagic()){
						return Boolean.TRUE;
					}
					break;
				case CLEARWEAPONS:
					if (((MonsterInst4)mod).isClearweapons()){
						return Boolean.TRUE;
					}
					break;
				case CLEARARMOR:
					if (((MonsterInst4)mod).isCleararmor()){
						return Boolean.TRUE;
					}
					break;
				case CLEARSPEC:
					if (((MonsterInst4)mod).isClearspec()){
						return Boolean.TRUE;
					}
					break;
				case FEMALE:
					if (((MonsterInst4)mod).isFemale()){
						return Boolean.TRUE;
					}
					break;
				case MOUNTED:
					if (((MonsterInst4)mod).isMounted()){
						return Boolean.TRUE;
					}
					break;
				case HOLY:
					if (((MonsterInst4)mod).isHoly()){
						return Boolean.TRUE;
					}
					break;
				case ANIMAL:
					if (((MonsterInst4)mod).isAnimal()){
						return Boolean.TRUE;
					}
					break;
				case UNDEAD:
					if (((MonsterInst4)mod).isUndead()){
						return Boolean.TRUE;
					}
					break;
				case DEMON:
					if (((MonsterInst4)mod).isDemon()){
						return Boolean.TRUE;
					}
					break;
				case MAGICBEING:
					if (((MonsterInst4)mod).isMagicbeing()){
						return Boolean.TRUE;
					}
					break;
				case STONEBEING:
					if (((MonsterInst4)mod).isStonebeing()){
						return Boolean.TRUE;
					}
					break;
				case INANIMATE:
					if (((MonsterInst4)mod).isInanimate()){
						return Boolean.TRUE;
					}
					break;
				case COLDBLOOD:
					if (((MonsterInst4)mod).isColdblood()){
						return Boolean.TRUE;
					}
					break;
				case IMMORTAL:
					if (((MonsterInst4)mod).isImmortal()){
						return Boolean.TRUE;
					}
					break;
				case BLIND:
					if (((MonsterInst4)mod).isBlind()){
						return Boolean.TRUE;
					}
					break;
				case UNIQUE:
					if (((MonsterInst4)mod).isUnique()){
						return Boolean.TRUE;
					}
					break;
				case IMMOBILE:
					if (((MonsterInst4)mod).isImmobile()){
						return Boolean.TRUE;
					}
					break;
				case AQUATIC:
					if (((MonsterInst4)mod).isAquatic()){
						return Boolean.TRUE;
					}
					break;
				case AMPHIBIAN:
					if (((MonsterInst4)mod).isAmphibian()){
						return Boolean.TRUE;
					}
					break;
				case POORAMPHIBIAN:
					if (((MonsterInst4)mod).isPooramphibian()){
						return Boolean.TRUE;
					}
					break;
				case FLYING:
					if (((MonsterInst4)mod).isFlying()){
						return Boolean.TRUE;
					}
					break;
				case STORMIMMUNE:
					if (((MonsterInst4)mod).isStormimmune()){
						return Boolean.TRUE;
					}
					break;
				case SAILING:
					if (((MonsterInst4)mod).isSailing()){
						return Boolean.TRUE;
					}
					break;
				case FORESTSURVIVAL:
					if (((MonsterInst4)mod).isForestsurvival()){
						return Boolean.TRUE;
					}
					break;
				case MOUNTAINSURVIVAL:
					if (((MonsterInst4)mod).isMountainsurvival()){
						return Boolean.TRUE;
					}
					break;
				case SWAMPSURVIVAL:
					if (((MonsterInst4)mod).isSwampsurvival()){
						return Boolean.TRUE;
					}
					break;
				case WASTESURVIVAL:
					if (((MonsterInst4)mod).isWastesurvival()){
						return Boolean.TRUE;
					}
					break;
				case ILLUSION:
					if (((MonsterInst4)mod).isIllusion()){
						return Boolean.TRUE;
					}
					break;
				case SPY:
					if (((MonsterInst4)mod).isSpy()){
						return Boolean.TRUE;
					}
					break;
				case ASSASSIN:
					if (((MonsterInst4)mod).isAssassin()){
						return Boolean.TRUE;
					}
					break;
				case HEAL:
					if (((MonsterInst4)mod).isHeal()){
						return Boolean.TRUE;
					}
					break;
				case NOHEAL:
					if (((MonsterInst4)mod).isNoheal()){
						return Boolean.TRUE;
					}
					break;
				case NEEDNOTEAT:
					if (((MonsterInst4)mod).isNeednoteat()){
						return Boolean.TRUE;
					}
					break;
				case ETHEREAL:
					if (((MonsterInst4)mod).isEthereal()){
						return Boolean.TRUE;
					}
					break;
				case TRAMPLE:
					if (((MonsterInst4)mod).isTrample()){
						return Boolean.TRUE;
					}
					break;
				case ENTANGLE:
					if (((MonsterInst4)mod).isEntangle()){
						return Boolean.TRUE;
					}
					break;
				case EYELOSS:
					if (((MonsterInst4)mod).isEyeloss()){
						return Boolean.TRUE;
					}
					break;
				case HORRORMARK:
					if (((MonsterInst4)mod).isHorrormark()){
						return Boolean.TRUE;
					}
					break;
				case POISONARMOR:
					if (((MonsterInst4)mod).isPoisonarmor()){
						return Boolean.TRUE;
					}
					break;
				case INQUISITOR:
					if (((MonsterInst4)mod).isInquisitor()){
						return Boolean.TRUE;
					}
					break;
				case NOITEM:
					if (((MonsterInst4)mod).isNoitem()){
						return Boolean.TRUE;
					}
					break;
				case DRAINIMMUNE:
					if (((MonsterInst4)mod).isDrainimmune()){
						return Boolean.TRUE;
					}
					break;
				case NOLEADER:
					if (((MonsterInst4)mod).isNoleader()){
						return Boolean.TRUE;
					}
					break;
				case POORLEADER:
					if (((MonsterInst4)mod).isPoorleader()){
						return Boolean.TRUE;
					}
					break;
				case OKLEADER:
					if (((MonsterInst4)mod).isOkleader()){
						return Boolean.TRUE;
					}
					break;
				case GOODLEADER:
					if (((MonsterInst4)mod).isGoodleader()){
						return Boolean.TRUE;
					}
					break;
				case EXPERTLEADER:
					if (((MonsterInst4)mod).isExpertleader()){
						return Boolean.TRUE;
					}
					break;
				case SUPERIORLEADER:
					if (((MonsterInst4)mod).isSuperiorleader()){
						return Boolean.TRUE;
					}
					break;
				case NOMAGICLEADER:
					if (((MonsterInst4)mod).isNomagicleader()){
						return Boolean.TRUE;
					}
					break;
				case POORMAGICLEADER:
					if (((MonsterInst4)mod).isPoormagicleader()){
						return Boolean.TRUE;
					}
					break;
				case OKMAGICLEADER:
					if (((MonsterInst4)mod).isOkmagicleader()){
						return Boolean.TRUE;
					}
					break;
				case GOODMAGICLEADER:
					if (((MonsterInst4)mod).isGoodmagicleader()){
						return Boolean.TRUE;
					}
					break;
				case EXPERTMAGICLEADER:
					if (((MonsterInst4)mod).isExpertmagicleader()){
						return Boolean.TRUE;
					}
					break;
				case SUPERIORMAGICLEADER:
					if (((MonsterInst4)mod).isSuperiormagicleader()){
						return Boolean.TRUE;
					}
					break;
				case NOUNDEADLEADER:
					if (((MonsterInst4)mod).isNoundeadleader()){
						return Boolean.TRUE;
					}
					break;
				case POORUNDEADLEADER:
					if (((MonsterInst4)mod).isPoorundeadleader()){
						return Boolean.TRUE;
					}
					break;
				case OKUNDEADLEADER:
					if (((MonsterInst4)mod).isOkundeadleader()){
						return Boolean.TRUE;
					}
					break;
				case GOODUNDEADLEADER:
					if (((MonsterInst4)mod).isGoodundeadleader()){
						return Boolean.TRUE;
					}
					break;
				case EXPERTUNDEADLEADER:
					if (((MonsterInst4)mod).isExpertundeadleader()){
						return Boolean.TRUE;
					}
					break;
				case SUPERIORUNDEADLEADER:
					if (((MonsterInst4)mod).isSuperiorundeadleader()){
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private Object getInst5(Inst inst2, Object monster) {
		EList<MonsterMods> list = ((Monster)monster).getMods();
		int weaponCount = 0;
		int armorCount = 0;
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst5) {
				switch (inst2) {
				case WEAPON1:
					if (((MonsterInst5)mod).isWeapon()){
						weaponCount ++;
						if (weaponCount == 1) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WEAPON2:
					if (((MonsterInst5)mod).isWeapon()){
						weaponCount ++;
						if (weaponCount == 2) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WEAPON3:
					if (((MonsterInst5)mod).isWeapon()){
						weaponCount ++;
						if (weaponCount == 3) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case WEAPON4:
					if (((MonsterInst5)mod).isWeapon()){
						weaponCount ++;
						if (weaponCount == 4) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ARMOR1:
					if (((MonsterInst5)mod).isArmor()){
						armorCount ++;
						if (armorCount == 1) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ARMOR2:
					if (((MonsterInst5)mod).isArmor()){
						armorCount ++;
						if (armorCount == 2) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ARMOR3:
					if (((MonsterInst5)mod).isArmor()){
						armorCount ++;
						if (armorCount == 3) {
							String strVal = ((MonsterInst5)mod).getValue1();
							Integer intVal = ((MonsterInst5)mod).getValue2();
							if (strVal != null) {
								return strVal;
							}
							return intVal;
						}
					}
					break;
				case ONEBATTLESPELL:
					if (((MonsterInst5)mod).isOnebattlespell()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case FIRSTSHAPE:
					if (((MonsterInst5)mod).isFirstshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SECONDSHAPE:
					if (((MonsterInst5)mod).isSecondshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SECONDTMPSHAPE:
					if (((MonsterInst5)mod).isSecondtmpshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SHAPECHANGE:
					if (((MonsterInst5)mod).isShapechange()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case LANDSHAPE:
					if (((MonsterInst5)mod).isLandshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case WATERSHAPE:
					if (((MonsterInst5)mod).isWatershape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case FORESTSHAPE:
					if (((MonsterInst5)mod).isForestshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case PLAINSHAPE:
					if (((MonsterInst5)mod).isPlainshape()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON:
					if (((MonsterInst5)mod).isDomsummon()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON2:
					if (((MonsterInst5)mod).isDomsummon2()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON20:
					if (((MonsterInst5)mod).isDomsummon20()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTER1:
					if (((MonsterInst5)mod).isMakemonster1()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTER2:
					if (((MonsterInst5)mod).isMakemonster2()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTER3:
					if (((MonsterInst5)mod).isMakemonster3()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTER4:
					if (((MonsterInst5)mod).isMakemonster4()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTER5:
					if (((MonsterInst5)mod).isMakemonster5()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON1:
					if (((MonsterInst5)mod).isSummon1()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON5:
					if (((MonsterInst5)mod).isSummon5()){
						String strVal = ((MonsterInst5)mod).getValue1();
						Integer intVal = ((MonsterInst5)mod).getValue2();
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
	
	private Integer getInst6(Inst inst2, Object monster) {
		EList<MonsterMods> list = ((Monster)monster).getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst6) {
				switch (inst2) {
				case HEAT:
					if (((MonsterInst6)mod).isHeat()){
						return Integer.valueOf(((MonsterInst6)mod).getValue());
					}
					break;
				case COLD:
					if (((MonsterInst6)mod).isCold()){
						return Integer.valueOf(((MonsterInst6)mod).getValue());
					}
					break;
				case STEALTHY:
					if (((MonsterInst6)mod).isStealthy()){
						return Integer.valueOf(((MonsterInst6)mod).getValue());
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
				Monster monsterToEdit = (Monster)input;
				EList<MonsterMods> mods = monsterToEdit.getMods();				
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst1) {
						switch (inst2) {
						case SPR1:
							if (((MonsterInst1)mod).isSpr1()) {
								((MonsterInst1)mod).setValue(newName);
							}
							break;
						case SPR2:
							if (((MonsterInst1)mod).isSpr2()) {
								((MonsterInst1)mod).setValue(newName);
							}
							break;
						case DESCR:
							if (((MonsterInst1)mod).isDescr()) {
								((MonsterInst1)mod).setValue(newName);
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
		try {
			// If this is not an int, return
			Integer.parseInt(newName);
		} catch (NumberFormatException e) {
			return;
		}
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Monster monsterToEdit = (Monster)input;
				EList<MonsterMods> mods = monsterToEdit.getMods();
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst2) {
						switch (inst2) {
						case SPECIALLOOK:
							if (((MonsterInst2)mod).isSpeciallook()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AP:
							if (((MonsterInst2)mod).isAp()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAPMOVE:
							if (((MonsterInst2)mod).isMapmove()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HP:
							if (((MonsterInst2)mod).isHp()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PROT:
							if (((MonsterInst2)mod).isProt()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SIZE:
							if (((MonsterInst2)mod).isSize()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESSIZE:
							if (((MonsterInst2)mod).isRessize()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STR:
							if (((MonsterInst2)mod).isStr()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ENC:
							if (((MonsterInst2)mod).isEnc()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ATT:
							if (((MonsterInst2)mod).isAtt()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEF:
							if (((MonsterInst2)mod).isDef()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PREC:
							if (((MonsterInst2)mod).isPrec()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MR:
							if (((MonsterInst2)mod).isMr()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MOR:
							if (((MonsterInst2)mod).isMor()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GCOST:
							if (((MonsterInst2)mod).isGcost()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RCOST:
							if (((MonsterInst2)mod).isRcost()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PATHCOST:
							if (((MonsterInst2)mod).isPathcost()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STARTDOM:
							if (((MonsterInst2)mod).isStartdom()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EYES:
							if (((MonsterInst2)mod).isEyes()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COPYSTATS:
							if (((MonsterInst2)mod).isCopystats()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COPYSPR:
							if (((MonsterInst2)mod).isCopyspr()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESTRICTEDGOD:
							if (((MonsterInst2)mod).isRestrictedgod()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SHATTEREDSOUL:
							if (((MonsterInst2)mod).isShatteredsoul()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COLDRES:
							if (((MonsterInst2)mod).isColdres()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRERES:
							if (((MonsterInst2)mod).isFireres()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POISONRES:
							if (((MonsterInst2)mod).isPoisonres()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SHOCKRES:
							if (((MonsterInst2)mod).isShockres()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DARKVISION:
							if (((MonsterInst2)mod).isDarkvision()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SEDUCE:
							if (((MonsterInst2)mod).isSeduce()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUCCUBUS:
							if (((MonsterInst2)mod).isSuccubus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BECKON:
							if (((MonsterInst2)mod).isBeckon()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STARTAGE:
							if (((MonsterInst2)mod).isStartage()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAXAGE:
							if (((MonsterInst2)mod).isMaxage()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case OLDER:
							if (((MonsterInst2)mod).isOlder()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HEALER:
							if (((MonsterInst2)mod).isHealer()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STARTAFF:
							if (((MonsterInst2)mod).isStartaff()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUPPLYBONUS:
							if (((MonsterInst2)mod).isSupplybonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case UWDAMAGE:
							if (((MonsterInst2)mod).isUwdamage()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HOMESICK:
							if (((MonsterInst2)mod).isHomesick()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COLDPOWER:
							if (((MonsterInst2)mod).isColdpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIREPOWER:
							if (((MonsterInst2)mod).isFirepower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STORMPOWER:
							if (((MonsterInst2)mod).isStormpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DARKPOWER:
							if (((MonsterInst2)mod).isDarkpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPRINGPOWER:
							if (((MonsterInst2)mod).isSpringpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUMMERPOWER:
							if (((MonsterInst2)mod).isSummerpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FALLPOWER:
							if (((MonsterInst2)mod).isFallpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WINTERPOWER:
							if (((MonsterInst2)mod).isWinterpower()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AMBIDEXTROUS:
							if (((MonsterInst2)mod).isAmbidextrous()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BANEFIRESHIELD:
							if (((MonsterInst2)mod).isBanefireshield()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BERSERK:
							if (((MonsterInst2)mod).isBerserk()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STANDARD:
							if (((MonsterInst2)mod).isStandard()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ANIMALAWE:
							if (((MonsterInst2)mod).isAnimalawe()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AWE:
							if (((MonsterInst2)mod).isAwe()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FEAR:
							if (((MonsterInst2)mod).isFear()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case REGENERATION:
							if (((MonsterInst2)mod).isRegeneration()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case REINVIGORATION:
							if (((MonsterInst2)mod).isReinvigoration()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRESHIELD:
							if (((MonsterInst2)mod).isFireshield()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ICEPROT:
							if (((MonsterInst2)mod).isIceprot()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POISONCLOUD:
							if (((MonsterInst2)mod).isPoisoncloud()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DISEASECLOUD:
							if (((MonsterInst2)mod).isDiseasecloud()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLOODVENGEANCE:
							if (((MonsterInst2)mod).isBloodvengeance()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CASTLEDEF:
							if (((MonsterInst2)mod).isCastledef()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SIEGEBONUS:
							if (((MonsterInst2)mod).isSiegebonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PATROLBONUS:
							if (((MonsterInst2)mod).isPatrolbonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PILLAGEBONUS:
							if (((MonsterInst2)mod).isPillagebonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESEARCHBONUS:
							if (((MonsterInst2)mod).isResearchbonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORGEBONUS:
							if (((MonsterInst2)mod).isForgebonus()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DOUSE:
							if (((MonsterInst2)mod).isDouse()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NOBADEVENTS:
							if (((MonsterInst2)mod).isNobadevents()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INCUNREST:
							if (((MonsterInst2)mod).isIncunrest()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADDOM:
							if (((MonsterInst2)mod).isSpreaddom()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LEPER:
							if (((MonsterInst2)mod).isLeper()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POPKILL:
							if (((MonsterInst2)mod).isPopkill()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERETIC:
							if (((MonsterInst2)mod).isHeretic()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ITEMSLOTS:
							if (((MonsterInst2)mod).isItemslots()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NAMETYPE:
							if (((MonsterInst2)mod).isNametype()){
								((MonsterInst2)mod).setValue(Integer.parseInt(newName));
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
		try {
			// If this is not an int, return
			if (value1 != null) {
				Integer.parseInt(value1);
			}
			if (value2 != null) {
				Integer.parseInt(value2);
			}
		} catch (NumberFormatException e) {
			return;
		}
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				int magicSkillCount = 0;
				int customMagicCount = 0;
				int boostCount = 0;
				int gemProdCount = 0;
				Monster monsterToEdit = (Monster)input;
				EList<MonsterMods> mods = monsterToEdit.getMods();
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst3) {
						switch (inst3) {
						case MAGICSKILL1:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 1) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL2:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 2) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL3:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 3) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL4:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 4) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL5:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 5) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL6:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 6) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL7:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 7) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICSKILL8:
							if (((MonsterInst3)mod).isMagicskill()) {
								magicSkillCount++;
								if (magicSkillCount == 8) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC1:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 1) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC2:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 2) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC3:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 3) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC4:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 4) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC5:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 5) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC6:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 6) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC7:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 7) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case CUSTOMMAGIC8:
							if (((MonsterInst3)mod).isCustommagic()) {
								customMagicCount++;
								if (customMagicCount == 8) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST1:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 1) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST2:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 2) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST3:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 3) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST4:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 4) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST5:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 5) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST6:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 6) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST7:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 7) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST8:
							if (((MonsterInst3)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 8) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD1:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 1) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD2:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 2) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD3:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 3) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD4:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 4) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD5:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 5) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD6:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 6) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD7:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 7) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case GEMPROD8:
							if (((MonsterInst3)mod).isGemprod()) {
								gemProdCount++;
								if (gemProdCount == 8) {
									if (value1 != null) {
										((MonsterInst3)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((MonsterInst3)mod).setValue2(Integer.parseInt(value2));
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
				Monster monsterToEdit = (Monster)input;
				int weaponCount = 0;
				int armorCount = 0;
				List<MonsterMods> modsToRemove = new ArrayList<MonsterMods>();
				List<MonsterMods> modsToAdd = new ArrayList<MonsterMods>();
				EList<MonsterMods> mods = monsterToEdit.getMods();
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst5) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst2) {
						case WEAPON1:
							if (((MonsterInst5)mod).isWeapon()){
								weaponCount++;
								if (weaponCount == 1) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setWeapon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WEAPON2:
							if (((MonsterInst5)mod).isWeapon()){
								weaponCount++;
								if (weaponCount == 2) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setWeapon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WEAPON3:
							if (((MonsterInst5)mod).isWeapon()){
								weaponCount++;
								if (weaponCount == 3) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setWeapon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case WEAPON4:
							if (((MonsterInst5)mod).isWeapon()){
								weaponCount++;
								if (weaponCount == 4) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setWeapon(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ARMOR1:
							if (((MonsterInst5)mod).isArmor()){
								armorCount++;
								if (armorCount == 1) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setArmor(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ARMOR2:
							if (((MonsterInst5)mod).isArmor()){
								armorCount++;
								if (armorCount == 2) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setArmor(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ARMOR3:
							if (((MonsterInst5)mod).isArmor()){
								armorCount++;
								if (armorCount == 3) {
									modsToRemove.add(mod);
									MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
									newMod.setArmor(true);
									if (newValue != null) {
										newMod.setValue2(Integer.parseInt(newName));
									} else {
										newMod.setValue1(newName);
									}
									modsToAdd.add(newMod);
								}
							}
							break;
						case ONEBATTLESPELL:
							if (((MonsterInst5)mod).isOnebattlespell()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setOnebattlespell(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case FIRSTSHAPE:
							if (((MonsterInst5)mod).isFirstshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setFirstshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SECONDSHAPE:
							if (((MonsterInst5)mod).isSecondshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setSecondshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SECONDTMPSHAPE:
							if (((MonsterInst5)mod).isSecondtmpshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setSecondtmpshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SHAPECHANGE:
							if (((MonsterInst5)mod).isShapechange()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setShapechange(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case LANDSHAPE:
							if (((MonsterInst5)mod).isLandshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setLandshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case WATERSHAPE:
							if (((MonsterInst5)mod).isWatershape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setWatershape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case FORESTSHAPE:
							if (((MonsterInst5)mod).isForestshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setForestshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case PLAINSHAPE:
							if (((MonsterInst5)mod).isPlainshape()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setPlainshape(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DOMSUMMON:
							if (((MonsterInst5)mod).isDomsummon()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setDomsummon(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DOMSUMMON2:
							if (((MonsterInst5)mod).isDomsummon2()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setDomsummon2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DOMSUMMON20:
							if (((MonsterInst5)mod).isDomsummon20()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setDomsummon20(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTER1:
							if (((MonsterInst5)mod).isMakemonster1()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setMakemonster1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTER2:
							if (((MonsterInst5)mod).isMakemonster2()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setMakemonster2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTER3:
							if (((MonsterInst5)mod).isMakemonster3()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setMakemonster3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTER4:
							if (((MonsterInst5)mod).isMakemonster4()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setMakemonster4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTER5:
							if (((MonsterInst5)mod).isMakemonster5()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setMakemonster5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON1:
							if (((MonsterInst5)mod).isSummon1()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setSummon1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON5:
							if (((MonsterInst5)mod).isSummon5()){
								modsToRemove.add(mod);
								MonsterInst5 newMod = DmFactory.eINSTANCE.createMonsterInst5();
								newMod.setSummon5(true);
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

	private void setInst6(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Monster monsterToEdit = (Monster)input;
				List<MonsterMods> modsToRemove = new ArrayList<MonsterMods>();
				List<MonsterMods> modsToAdd = new ArrayList<MonsterMods>();
				EList<MonsterMods> mods = monsterToEdit.getMods();
				for (MonsterMods mod : mods) {
					if (mod instanceof MonsterInst6) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst2) {
						case HEAT:
							if (((MonsterInst6)mod).isHeat()){
								modsToRemove.add(mod);
								MonsterInst6 newMod = DmFactory.eINSTANCE.createMonsterInst6();
								newMod.setHeat(true);
								if (newValue != null) {
									newMod.setValue(Integer.parseInt(newName));
								}
								modsToAdd.add(newMod);
							}
							break;
						case COLD:
							if (((MonsterInst6)mod).isCold()){
								modsToRemove.add(mod);
								MonsterInst6 newMod = DmFactory.eINSTANCE.createMonsterInst6();
								newMod.setCold(true);
								if (newValue != null) {
									newMod.setValue(Integer.parseInt(newName));
								}
								modsToAdd.add(newMod);
							}
							break;
						case STEALTHY:
							if (((MonsterInst6)mod).isStealthy()){
								modsToRemove.add(mod);
								MonsterInst6 newMod = DmFactory.eINSTANCE.createMonsterInst6();
								newMod.setStealthy(true);
								if (newValue != null) {
									newMod.setValue(Integer.parseInt(newName));
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
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst1 type = DmFactory.eINSTANCE.createMonsterInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
							break;
						case SPR1:
							type.setSpr1(true);
							break;
						case SPR2:
							type.setSpr2(true);
							break;
						case DESCR:
							type.setDescr(true);
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
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst2 type = DmFactory.eINSTANCE.createMonsterInst2();
						switch (inst) {
						case SPECIALLOOK:
							type.setSpeciallook(true);
							break;
						case AP:
							type.setAp(true);
							break;
						case MAPMOVE:
							type.setMapmove(true);
							break;
						case HP:
							type.setHp(true);
							break;
						case PROT:
							type.setProt(true);
							break;
						case SIZE:
							type.setSize(true);
							break;
						case RESSIZE:
							type.setRessize(true);
							break;
						case STR:
							type.setStr(true);
							break;
						case ENC:
							type.setEnc(true);
							break;
						case ATT:
							type.setAtt(true);
							break;
						case DEF:
							type.setDef(true);
							break;
						case PREC:
							type.setPrec(true);
							break;
						case MR:
							type.setMr(true);
							break;
						case MOR:
							type.setMor(true);
							break;
						case GCOST:
							type.setGcost(true);
							break;
						case RCOST:
							type.setRcost(true);
							break;
						case PATHCOST:
							type.setPathcost(true);
							break;
						case STARTDOM:
							type.setStartdom(true);
							break;
						case EYES:
							type.setEyes(true);
							break;
						case COPYSTATS:
							type.setCopystats(true);
							break;
						case COPYSPR:
							type.setCopyspr(true);
							break;
						case RESTRICTEDGOD:
							type.setRestrictedgod(true);
							break;
						case SHATTEREDSOUL:
							type.setShatteredsoul(true);
							break;
						case COLDRES:
							type.setColdres(true);
							break;
						case FIRERES:
							type.setFireres(true);
							break;
						case POISONRES:
							type.setPoisonres(true);
							break;
						case SHOCKRES:
							type.setShockres(true);
							break;
						case DARKVISION:
							type.setDarkvision(true);
							break;
						case SEDUCE:
							type.setSeduce(true);
							break;
						case SUCCUBUS:
							type.setSuccubus(true);
							break;
						case BECKON:
							type.setBeckon(true);
							break;
						case STARTAGE:
							type.setStartage(true);
							break;
						case MAXAGE:
							type.setMaxage(true);
							break;
						case OLDER:
							type.setOlder(true);
							break;
						case HEALER:
							type.setHealer(true);
							break;
						case STARTAFF:
							type.setStartaff(true);
							break;
						case SUPPLYBONUS:
							type.setSupplybonus(true);
							break;
						case UWDAMAGE:
							type.setUwdamage(true);
							break;
						case HOMESICK:
							type.setHomesick(true);
							break;
						case COLDPOWER:
							type.setColdpower(true);
							break;
						case FIREPOWER:
							type.setFirepower(true);
							break;
						case STORMPOWER:
							type.setStormpower(true);
							break;
						case DARKPOWER:
							type.setDarkpower(true);
							break;
						case SPRINGPOWER:
							type.setSpringpower(true);
							break;
						case SUMMERPOWER:
							type.setSummerpower(true);
							break;
						case FALLPOWER:
							type.setFallpower(true);
							break;
						case WINTERPOWER:
							type.setWinterpower(true);
							break;
						case AMBIDEXTROUS:
							type.setAmbidextrous(true);
							break;
						case BANEFIRESHIELD:
							type.setBanefireshield(true);
							break;
						case BERSERK:
							type.setBerserk(true);
							break;
						case STANDARD:
							type.setStandard(true);
							break;
						case ANIMALAWE:
							type.setAnimalawe(true);
							break;
						case AWE:
							type.setAwe(true);
							break;
						case FEAR:
							type.setFear(true);
							break;
						case REGENERATION:
							type.setRegeneration(true);
							break;
						case REINVIGORATION:
							type.setReinvigoration(true);
							break;
						case FIRESHIELD:
							type.setFireshield(true);
							break;
						case ICEPROT:
							type.setIceprot(true);
							break;
						case POISONCLOUD:
							type.setPoisoncloud(true);
							break;
						case DISEASECLOUD:
							type.setDiseasecloud(true);
							break;
						case BLOODVENGEANCE:
							type.setBloodvengeance(true);
							break;
						case CASTLEDEF:
							type.setCastledef(true);
							break;
						case SIEGEBONUS:
							type.setSiegebonus(true);
							break;
						case PATROLBONUS:
							type.setPatrolbonus(true);
							break;
						case PILLAGEBONUS:
							type.setPillagebonus(true);
							break;
						case RESEARCHBONUS:
							type.setResearchbonus(true);
							break;
						case FORGEBONUS:
							type.setForgebonus(true);
							break;
						case DOUSE:
							type.setDouse(true);
							break;
						case NOBADEVENTS:
							type.setNobadevents(true);
							break;
						case INCUNREST:
							type.setIncunrest(true);
							break;
						case SPREADDOM:
							type.setSpreaddom(true);
							break;
						case LEPER:
							type.setLeper(true);
							break;
						case POPKILL:
							type.setPopkill(true);
							break;
						case HERETIC:
							type.setHeretic(true);
							break;
						case ITEMSLOTS:
							type.setItemslots(true);
							break;
						case NAMETYPE:
							type.setNametype(true);
							break;				
						}
						type.setValue(Integer.valueOf(newName));
						// copystats should be the first command
						if (inst == Inst.COPYSTATS) {
							mods.add(0, type);
						} else {
							mods.add(type);	
						}
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
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst3 type = DmFactory.eINSTANCE.createMonsterInst3();
						switch (inst) {
						case MAGICSKILL1:
							type.setMagicskill(true);
							break;
						case MAGICSKILL2:
							type.setMagicskill(true);
							break;
						case MAGICSKILL3:
							type.setMagicskill(true);
							break;
						case MAGICSKILL4:
							type.setMagicskill(true);
							break;
						case MAGICSKILL5:
							type.setMagicskill(true);
							break;
						case MAGICSKILL6:
							type.setMagicskill(true);
							break;
						case MAGICSKILL7:
							type.setMagicskill(true);
							break;
						case MAGICSKILL8:
							type.setMagicskill(true);
							break;
						case CUSTOMMAGIC1:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC2:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC3:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC4:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC5:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC6:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC7:
							type.setCustommagic(true);
							break;
						case CUSTOMMAGIC8:
							type.setCustommagic(true);
							break;
						case MAGICBOOST1:
							type.setMagicboost(true);
							break;
						case MAGICBOOST2:
							type.setMagicboost(true);
							break;
						case MAGICBOOST3:
							type.setMagicboost(true);
							break;
						case MAGICBOOST4:
							type.setMagicboost(true);
							break;
						case MAGICBOOST5:
							type.setMagicboost(true);
							break;
						case MAGICBOOST6:
							type.setMagicboost(true);
							break;
						case MAGICBOOST7:
							type.setMagicboost(true);
							break;
						case MAGICBOOST8:
							type.setMagicboost(true);
							break;
						case GEMPROD1:
							type.setGemprod(true);
							break;
						case GEMPROD2:
							type.setGemprod(true);
							break;
						case GEMPROD3:
							type.setGemprod(true);
							break;
						case GEMPROD4:
							type.setGemprod(true);
							break;
						case GEMPROD5:
							type.setGemprod(true);
							break;
						case GEMPROD6:
							type.setGemprod(true);
							break;
						case GEMPROD7:
							type.setGemprod(true);
							break;
						case GEMPROD8:
							type.setGemprod(true);
							break;
						}
						type.setValue1(Integer.valueOf(newName1));
						type.setValue2(Integer.valueOf(newName2));
						mods.add(type);
					}  
				});

			}
		});
		updateSelection();
	}
	
	private void addInst4(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst4 type = DmFactory.eINSTANCE.createMonsterInst4();
						switch (inst) {
						case CLEAR:
							type.setClear(true);
							break;
						case CLEARMAGIC:
							type.setClearmagic(true);
							break;
						case CLEARWEAPONS:
							type.setClearweapons(true);
							break;
						case CLEARARMOR:
							type.setCleararmor(true);
							break;
						case CLEARSPEC:
							type.setClearspec(true);
							break;
						case FEMALE:
							type.setFemale(true);
							break;
						case MOUNTED:
							type.setMounted(true);
							break;
						case HOLY:
							type.setHoly(true);
							break;
						case ANIMAL:
							type.setAnimal(true);
							break;
						case UNDEAD:
							type.setUndead(true);
							break;
						case DEMON:
							type.setDemon(true);
							break;
						case MAGICBEING:
							type.setMagicbeing(true);
							break;
						case STONEBEING:
							type.setStonebeing(true);
							break;
						case INANIMATE:
							type.setInanimate(true);
							break;
						case COLDBLOOD:
							type.setColdblood(true);
							break;
						case IMMORTAL:
							type.setImmortal(true);
							break;
						case BLIND:
							type.setBlind(true);
							break;
						case UNIQUE:
							type.setUnique(true);
							break;
						case IMMOBILE:
							type.setImmobile(true);
							break;
						case AQUATIC:
							type.setAquatic(true);
							break;
						case AMPHIBIAN:
							type.setAmphibian(true);
							break;
						case POORAMPHIBIAN:
							type.setPooramphibian(true);
							break;
						case FLYING:
							type.setFlying(true);
							break;
						case STORMIMMUNE:
							type.setStormimmune(true);
							break;
						case SAILING:
							type.setSailing(true);
							break;
						case FORESTSURVIVAL:
							type.setForestsurvival(true);
							break;
						case MOUNTAINSURVIVAL:
							type.setMountainsurvival(true);
							break;
						case SWAMPSURVIVAL:
							type.setSwampsurvival(true);
							break;
						case WASTESURVIVAL:
							type.setWastesurvival(true);
							break;
						case ILLUSION:
							type.setIllusion(true);
							break;
						case SPY:
							type.setSpy(true);
							break;
						case ASSASSIN:
							type.setAssassin(true);
							break;
						case HEAL:
							type.setHeal(true);
							break;
						case NOHEAL:
							type.setNoheal(true);
							break;
						case NEEDNOTEAT:
							type.setNeednoteat(true);
							break;
						case ETHEREAL:
							type.setEthereal(true);
							break;
						case TRAMPLE:
							type.setTrample(true);
							break;
						case ENTANGLE:
							type.setEntangle(true);
							break;
						case EYELOSS:
							type.setEyeloss(true);
							break;
						case HORRORMARK:
							type.setHorrormark(true);
							break;
						case POISONARMOR:
							type.setPoisonarmor(true);
							break;
						case INQUISITOR:
							type.setInquisitor(true);
							break;
						case NOITEM:
							type.setNoitem(true);
							break;
						case DRAINIMMUNE:
							type.setDrainimmune(true);
							break;
						case NOLEADER:
							type.setNoleader(true);
							break;
						case POORLEADER:
							type.setPoorleader(true);
							break;
						case OKLEADER:
							type.setOkleader(true);
							break;
						case GOODLEADER:
							type.setGoodleader(true);
							break;
						case EXPERTLEADER:
							type.setExpertleader(true);
							break;
						case SUPERIORLEADER:
							type.setSuperiorleader(true);
							break;
						case NOMAGICLEADER:
							type.setNomagicleader(true);
							break;
						case POORMAGICLEADER:
							type.setPoormagicleader(true);
							break;
						case OKMAGICLEADER:
							type.setOkmagicleader(true);
							break;
						case GOODMAGICLEADER:
							type.setGoodmagicleader(true);
							break;
						case EXPERTMAGICLEADER:
							type.setExpertmagicleader(true);
							break;
						case SUPERIORMAGICLEADER:
							type.setSuperiormagicleader(true);
							break;
						case NOUNDEADLEADER:
							type.setNoundeadleader(true);
							break;
						case POORUNDEADLEADER:
							type.setPoorundeadleader(true);
							break;
						case OKUNDEADLEADER:
							type.setOkundeadleader(true);
							break;
						case GOODUNDEADLEADER:
							type.setGoodundeadleader(true);
							break;
						case EXPERTUNDEADLEADER:
							type.setExpertundeadleader(true);
							break;
						case SUPERIORUNDEADLEADER:
							type.setSuperiorundeadleader(true);
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
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst5 type = DmFactory.eINSTANCE.createMonsterInst5();
						switch (inst) {
						case WEAPON1:
							type.setWeapon(true);
							break;
						case WEAPON2:
							type.setWeapon(true);
							break;
						case WEAPON3:
							type.setWeapon(true);
							break;
						case WEAPON4:
							type.setWeapon(true);
							break;
						case ARMOR1:
							type.setArmor(true);
							break;
						case ARMOR2:
							type.setArmor(true);
							break;
						case ARMOR3:
							type.setArmor(true);
							break;
						case ONEBATTLESPELL:
							type.setOnebattlespell(true);
							break;
						case FIRSTSHAPE:
							type.setFirstshape(true);
							break;
						case SECONDSHAPE:
							type.setSecondshape(true);
							break;
						case SECONDTMPSHAPE:
							type.setSecondtmpshape(true);
							break;
						case SHAPECHANGE:
							type.setShapechange(true);
							break;
						case LANDSHAPE:
							type.setLandshape(true);
							break;
						case WATERSHAPE:
							type.setWatershape(true);
							break;
						case FORESTSHAPE:
							type.setForestshape(true);
							break;
						case PLAINSHAPE:
							type.setPlainshape(true);
							break;
						case DOMSUMMON:
							type.setDomsummon(true);
							break;
						case DOMSUMMON2:
							type.setDomsummon2(true);
							break;
						case DOMSUMMON20:
							type.setDomsummon20(true);
							break;
						case MAKEMONSTER1:
							type.setMakemonster1(true);
							break;
						case MAKEMONSTER2:
							type.setMakemonster2(true);
							break;
						case MAKEMONSTER3:
							type.setMakemonster3(true);
							break;
						case MAKEMONSTER4:
							type.setMakemonster4(true);
							break;
						case MAKEMONSTER5:
							type.setMakemonster5(true);
							break;
						case SUMMON1:
							type.setSummon1(true);
							break;
						case SUMMON5:
							type.setSummon5(true);
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
	
	private void addInst6(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<MonsterMods> mods = ((Monster)input).getMods();
						MonsterInst6 type = DmFactory.eINSTANCE.createMonsterInst6();
						switch (inst) {
						case HEAT:
							type.setHeat(true);
							break;
						case COLD:
							type.setCold(true);
							break;
						case STEALTHY:
							type.setStealthy(true);
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
	
	private void removeInst(final Inst inst2, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						int magicSkillCount = 0;
						int customMagicCount = 0;
						int boostCount = 0;
						int gemProdCount = 0;
						MonsterMods modToRemove = null;
						int armorCount = 0;
						int weaponCount = 0;
						EList<MonsterMods> mods = ((Monster)input).getMods();
						for (MonsterMods mod : mods) {
							if (mod instanceof MonsterInst1) {
								switch (inst2) {
								case NAME:
									if (((MonsterInst1)mod).isName()){
										modToRemove = mod;
									}
									break;
								case SPR1:
									if (((MonsterInst1)mod).isSpr1()){
										modToRemove = mod;
									}
									break;
								case SPR2:
									if (((MonsterInst1)mod).isSpr2()){
										modToRemove = mod;
									}
									break;
								case DESCR:
									if (((MonsterInst1)mod).isDescr()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof MonsterInst2) {
								switch (inst2) {
								case SPECIALLOOK:
									if (((MonsterInst2)mod).isSpeciallook()){
										modToRemove = mod;
									}
									break;
								case AP:
									if (((MonsterInst2)mod).isAp()){
										modToRemove = mod;
									}
									break;
								case MAPMOVE:
									if (((MonsterInst2)mod).isMapmove()){
										modToRemove = mod;
									}
									break;
								case HP:
									if (((MonsterInst2)mod).isHp()){
										modToRemove = mod;
									}
									break;
								case PROT:
									if (((MonsterInst2)mod).isProt()){
										modToRemove = mod;
									}
									break;
								case SIZE:
									if (((MonsterInst2)mod).isSize()){
										modToRemove = mod;
									}
									break;
								case RESSIZE:
									if (((MonsterInst2)mod).isRessize()){
										modToRemove = mod;
									}
									break;
								case STR:
									if (((MonsterInst2)mod).isStr()){
										modToRemove = mod;
									}
									break;
								case ENC:
									if (((MonsterInst2)mod).isEnc()){
										modToRemove = mod;
									}
									break;
								case ATT:
									if (((MonsterInst2)mod).isAtt()){
										modToRemove = mod;
									}
									break;
								case DEF:
									if (((MonsterInst2)mod).isDef()){
										modToRemove = mod;
									}
									break;
								case PREC:
									if (((MonsterInst2)mod).isPrec()){
										modToRemove = mod;
									}
									break;
								case MR:
									if (((MonsterInst2)mod).isMr()){
										modToRemove = mod;
									}
									break;
								case MOR:
									if (((MonsterInst2)mod).isMor()){
										modToRemove = mod;
									}
									break;
								case GCOST:
									if (((MonsterInst2)mod).isGcost()){
										modToRemove = mod;
									}
									break;
								case RCOST:
									if (((MonsterInst2)mod).isRcost()){
										modToRemove = mod;
									}
									break;
								case PATHCOST:
									if (((MonsterInst2)mod).isPathcost()){
										modToRemove = mod;
									}
									break;
								case STARTDOM:
									if (((MonsterInst2)mod).isStartdom()){
										modToRemove = mod;
									}
									break;
								case EYES:
									if (((MonsterInst2)mod).isEyes()){
										modToRemove = mod;
									}
									break;
								case COPYSTATS:
									if (((MonsterInst2)mod).isCopystats()){
										modToRemove = mod;
									}
									break;
								case COPYSPR:
									if (((MonsterInst2)mod).isCopyspr()){
										modToRemove = mod;
									}
									break;
								case RESTRICTEDGOD:
									if (((MonsterInst2)mod).isRestrictedgod()){
										modToRemove = mod;
									}
									break;
								case SHATTEREDSOUL:
									if (((MonsterInst2)mod).isShatteredsoul()){
										modToRemove = mod;
									}
									break;
								case COLDRES:
									if (((MonsterInst2)mod).isColdres()){
										modToRemove = mod;
									}
									break;
								case FIRERES:
									if (((MonsterInst2)mod).isFireres()){
										modToRemove = mod;
									}
									break;
								case POISONRES:
									if (((MonsterInst2)mod).isPoisonres()){
										modToRemove = mod;
									}
									break;
								case SHOCKRES:
									if (((MonsterInst2)mod).isShockres()){
										modToRemove = mod;
									}
									break;
								case DARKVISION:
									if (((MonsterInst2)mod).isDarkvision()){
										modToRemove = mod;
									}
									break;
								case SEDUCE:
									if (((MonsterInst2)mod).isSeduce()){
										modToRemove = mod;
									}
									break;
								case SUCCUBUS:
									if (((MonsterInst2)mod).isSuccubus()){
										modToRemove = mod;
									}
									break;
								case BECKON:
									if (((MonsterInst2)mod).isBeckon()){
										modToRemove = mod;
									}
									break;
								case STARTAGE:
									if (((MonsterInst2)mod).isStartage()){
										modToRemove = mod;
									}
									break;
								case MAXAGE:
									if (((MonsterInst2)mod).isMaxage()){
										modToRemove = mod;
									}
									break;
								case OLDER:
									if (((MonsterInst2)mod).isOlder()){
										modToRemove = mod;
									}
									break;
								case HEALER:
									if (((MonsterInst2)mod).isHealer()){
										modToRemove = mod;
									}
									break;
								case STARTAFF:
									if (((MonsterInst2)mod).isStartaff()){
										modToRemove = mod;
									}
									break;
								case SUPPLYBONUS:
									if (((MonsterInst2)mod).isSupplybonus()){
										modToRemove = mod;
									}
									break;
								case UWDAMAGE:
									if (((MonsterInst2)mod).isUwdamage()){
										modToRemove = mod;
									}
									break;
								case HOMESICK:
									if (((MonsterInst2)mod).isHomesick()){
										modToRemove = mod;
									}
									break;
								case COLDPOWER:
									if (((MonsterInst2)mod).isColdpower()){
										modToRemove = mod;
									}
									break;
								case FIREPOWER:
									if (((MonsterInst2)mod).isFirepower()){
										modToRemove = mod;
									}
									break;
								case STORMPOWER:
									if (((MonsterInst2)mod).isStormpower()){
										modToRemove = mod;
									}
									break;
								case DARKPOWER:
									if (((MonsterInst2)mod).isDarkpower()){
										modToRemove = mod;
									}
									break;
								case SPRINGPOWER:
									if (((MonsterInst2)mod).isSpringpower()){
										modToRemove = mod;
									}
									break;
								case SUMMERPOWER:
									if (((MonsterInst2)mod).isSummerpower()){
										modToRemove = mod;
									}
									break;
								case FALLPOWER:
									if (((MonsterInst2)mod).isFallpower()){
										modToRemove = mod;
									}
									break;
								case WINTERPOWER:
									if (((MonsterInst2)mod).isWinterpower()){
										modToRemove = mod;
									}
									break;
								case AMBIDEXTROUS:
									if (((MonsterInst2)mod).isAmbidextrous()){
										modToRemove = mod;
									}
									break;
								case BANEFIRESHIELD:
									if (((MonsterInst2)mod).isBanefireshield()){
										modToRemove = mod;
									}
									break;
								case BERSERK:
									if (((MonsterInst2)mod).isBerserk()){
										modToRemove = mod;
									}
									break;
								case STANDARD:
									if (((MonsterInst2)mod).isStandard()){
										modToRemove = mod;
									}
									break;
								case ANIMALAWE:
									if (((MonsterInst2)mod).isAnimalawe()){
										modToRemove = mod;
									}
									break;
								case AWE:
									if (((MonsterInst2)mod).isAwe()){
										modToRemove = mod;
									}
									break;
								case FEAR:
									if (((MonsterInst2)mod).isFear()){
										modToRemove = mod;
									}
									break;
								case REGENERATION:
									if (((MonsterInst2)mod).isRegeneration()){
										modToRemove = mod;
									}
									break;
								case REINVIGORATION:
									if (((MonsterInst2)mod).isReinvigoration()){
										modToRemove = mod;
									}
									break;
								case FIRESHIELD:
									if (((MonsterInst2)mod).isFireshield()){
										modToRemove = mod;
									}
									break;
								case ICEPROT:
									if (((MonsterInst2)mod).isIceprot()){
										modToRemove = mod;
									}
									break;
								case POISONCLOUD:
									if (((MonsterInst2)mod).isPoisoncloud()){
										modToRemove = mod;
									}
									break;
								case DISEASECLOUD:
									if (((MonsterInst2)mod).isDiseasecloud()){
										modToRemove = mod;
									}
									break;
								case BLOODVENGEANCE:
									if (((MonsterInst2)mod).isBloodvengeance()){
										modToRemove = mod;
									}
									break;
								case CASTLEDEF:
									if (((MonsterInst2)mod).isCastledef()){
										modToRemove = mod;
									}
									break;
								case SIEGEBONUS:
									if (((MonsterInst2)mod).isSiegebonus()){
										modToRemove = mod;
									}
									break;
								case PATROLBONUS:
									if (((MonsterInst2)mod).isPatrolbonus()){
										modToRemove = mod;
									}
									break;
								case PILLAGEBONUS:
									if (((MonsterInst2)mod).isPillagebonus()){
										modToRemove = mod;
									}
									break;
								case RESEARCHBONUS:
									if (((MonsterInst2)mod).isResearchbonus()){
										modToRemove = mod;
									}
									break;
								case FORGEBONUS:
									if (((MonsterInst2)mod).isForgebonus()){
										modToRemove = mod;
									}
									break;
								case DOUSE:
									if (((MonsterInst2)mod).isDouse()){
										modToRemove = mod;
									}
									break;
								case NOBADEVENTS:
									if (((MonsterInst2)mod).isNobadevents()){
										modToRemove = mod;
									}
									break;
								case INCUNREST:
									if (((MonsterInst2)mod).isIncunrest()){
										modToRemove = mod;
									}
									break;
								case SPREADDOM:
									if (((MonsterInst2)mod).isSpreaddom()){
										modToRemove = mod;
									}
									break;
								case LEPER:
									if (((MonsterInst2)mod).isLeper()){
										modToRemove = mod;
									}
									break;
								case POPKILL:
									if (((MonsterInst2)mod).isPopkill()){
										modToRemove = mod;
									}
									break;
								case HERETIC:
									if (((MonsterInst2)mod).isHeretic()){
										modToRemove = mod;
									}
									break;
								case ITEMSLOTS:
									if (((MonsterInst2)mod).isItemslots()){
										modToRemove = mod;
									}
									break;
								case NAMETYPE:
									if (((MonsterInst2)mod).isNametype()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof MonsterInst3) {
								switch (inst2) {
								case MAGICSKILL1:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL2:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL3:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL4:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL5:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL6:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL7:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICSKILL8:
									if (((MonsterInst3)mod).isMagicskill()){
										magicSkillCount++;
										if (magicSkillCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC1:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC2:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC3:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC4:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC5:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC6:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC7:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case CUSTOMMAGIC8:
									if (((MonsterInst3)mod).isCustommagic()){
										customMagicCount++;
										if (customMagicCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST1:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST2:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST3:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST4:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST5:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST6:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST7:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST8:
									if (((MonsterInst3)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD1:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD2:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD3:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD4:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD5:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD6:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD7:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case GEMPROD8:
									if (((MonsterInst3)mod).isGemprod()){
										gemProdCount++;
										if (gemProdCount == 8) {
											modToRemove = mod;
										}
									}
									break;
								}
							}
							if (mod instanceof MonsterInst4) {
								switch (inst2) {
								case CLEAR:
									if (((MonsterInst4)mod).isClear()){
										modToRemove = mod;
									}
									break;
								case CLEARMAGIC:
									if (((MonsterInst4)mod).isClearmagic()){
										modToRemove = mod;
									}
									break;
								case CLEARWEAPONS:
									if (((MonsterInst4)mod).isClearweapons()){
										modToRemove = mod;
									}
									break;
								case CLEARARMOR:
									if (((MonsterInst4)mod).isCleararmor()){
										modToRemove = mod;
									}
									break;
								case CLEARSPEC:
									if (((MonsterInst4)mod).isClearspec()){
										modToRemove = mod;
									}
									break;
								case FEMALE:
									if (((MonsterInst4)mod).isFemale()){
										modToRemove = mod;
									}
									break;
								case MOUNTED:
									if (((MonsterInst4)mod).isMounted()){
										modToRemove = mod;
									}
									break;
								case HOLY:
									if (((MonsterInst4)mod).isHoly()){
										modToRemove = mod;
									}
									break;
								case ANIMAL:
									if (((MonsterInst4)mod).isAnimal()){
										modToRemove = mod;
									}
									break;
								case UNDEAD:
									if (((MonsterInst4)mod).isUndead()){
										modToRemove = mod;
									}
									break;
								case DEMON:
									if (((MonsterInst4)mod).isDemon()){
										modToRemove = mod;
									}
									break;
								case MAGICBEING:
									if (((MonsterInst4)mod).isMagicbeing()){
										modToRemove = mod;
									}
									break;
								case STONEBEING:
									if (((MonsterInst4)mod).isStonebeing()){
										modToRemove = mod;
									}
									break;
								case INANIMATE:
									if (((MonsterInst4)mod).isInanimate()){
										modToRemove = mod;
									}
									break;
								case COLDBLOOD:
									if (((MonsterInst4)mod).isColdblood()){
										modToRemove = mod;
									}
									break;
								case IMMORTAL:
									if (((MonsterInst4)mod).isImmortal()){
										modToRemove = mod;
									}
									break;
								case BLIND:
									if (((MonsterInst4)mod).isBlind()){
										modToRemove = mod;
									}
									break;
								case UNIQUE:
									if (((MonsterInst4)mod).isUnique()){
										modToRemove = mod;
									}
									break;
								case IMMOBILE:
									if (((MonsterInst4)mod).isImmobile()){
										modToRemove = mod;
									}
									break;
								case AQUATIC:
									if (((MonsterInst4)mod).isAquatic()){
										modToRemove = mod;
									}
									break;
								case AMPHIBIAN:
									if (((MonsterInst4)mod).isAmphibian()){
										modToRemove = mod;
									}
									break;
								case POORAMPHIBIAN:
									if (((MonsterInst4)mod).isPooramphibian()){
										modToRemove = mod;
									}
									break;
								case FLYING:
									if (((MonsterInst4)mod).isFlying()){
										modToRemove = mod;
									}
									break;
								case STORMIMMUNE:
									if (((MonsterInst4)mod).isStormimmune()){
										modToRemove = mod;
									}
									break;
								case SAILING:
									if (((MonsterInst4)mod).isSailing()){
										modToRemove = mod;
									}
									break;
								case FORESTSURVIVAL:
									if (((MonsterInst4)mod).isForestsurvival()){
										modToRemove = mod;
									}
									break;
								case MOUNTAINSURVIVAL:
									if (((MonsterInst4)mod).isMountainsurvival()){
										modToRemove = mod;
									}
									break;
								case SWAMPSURVIVAL:
									if (((MonsterInst4)mod).isSwampsurvival()){
										modToRemove = mod;
									}
									break;
								case WASTESURVIVAL:
									if (((MonsterInst4)mod).isWastesurvival()){
										modToRemove = mod;
									}
									break;
								case ILLUSION:
									if (((MonsterInst4)mod).isIllusion()){
										modToRemove = mod;
									}
									break;
								case SPY:
									if (((MonsterInst4)mod).isSpy()){
										modToRemove = mod;
									}
									break;
								case ASSASSIN:
									if (((MonsterInst4)mod).isAssassin()){
										modToRemove = mod;
									}
									break;
								case HEAL:
									if (((MonsterInst4)mod).isHeal()){
										modToRemove = mod;
									}
									break;
								case NOHEAL:
									if (((MonsterInst4)mod).isNoheal()){
										modToRemove = mod;
									}
									break;
								case NEEDNOTEAT:
									if (((MonsterInst4)mod).isNeednoteat()){
										modToRemove = mod;
									}
									break;
								case ETHEREAL:
									if (((MonsterInst4)mod).isEthereal()){
										modToRemove = mod;
									}
									break;
								case TRAMPLE:
									if (((MonsterInst4)mod).isTrample()){
										modToRemove = mod;
									}
									break;
								case ENTANGLE:
									if (((MonsterInst4)mod).isEntangle()){
										modToRemove = mod;
									}
									break;
								case EYELOSS:
									if (((MonsterInst4)mod).isEyeloss()){
										modToRemove = mod;
									}
									break;
								case HORRORMARK:
									if (((MonsterInst4)mod).isHorrormark()){
										modToRemove = mod;
									}
									break;
								case POISONARMOR:
									if (((MonsterInst4)mod).isPoisonarmor()){
										modToRemove = mod;
									}
									break;
								case INQUISITOR:
									if (((MonsterInst4)mod).isInquisitor()){
										modToRemove = mod;
									}
									break;
								case NOITEM:
									if (((MonsterInst4)mod).isNoitem()){
										modToRemove = mod;
									}
									break;
								case DRAINIMMUNE:
									if (((MonsterInst4)mod).isDrainimmune()){
										modToRemove = mod;
									}
									break;
								case NOLEADER:
									if (((MonsterInst4)mod).isNoleader()){
										modToRemove = mod;
									}
									break;
								case POORLEADER:
									if (((MonsterInst4)mod).isPoorleader()){
										modToRemove = mod;
									}
									break;
								case OKLEADER:
									if (((MonsterInst4)mod).isOkleader()){
										modToRemove = mod;
									}
									break;
								case GOODLEADER:
									if (((MonsterInst4)mod).isGoodleader()){
										modToRemove = mod;
									}
									break;
								case EXPERTLEADER:
									if (((MonsterInst4)mod).isExpertleader()){
										modToRemove = mod;
									}
									break;
								case SUPERIORLEADER:
									if (((MonsterInst4)mod).isSuperiorleader()){
										modToRemove = mod;
									}
									break;
								case NOMAGICLEADER:
									if (((MonsterInst4)mod).isNomagicleader()){
										modToRemove = mod;
									}
									break;
								case POORMAGICLEADER:
									if (((MonsterInst4)mod).isPoormagicleader()){
										modToRemove = mod;
									}
									break;
								case OKMAGICLEADER:
									if (((MonsterInst4)mod).isOkmagicleader()){
										modToRemove = mod;
									}
									break;
								case GOODMAGICLEADER:
									if (((MonsterInst4)mod).isGoodmagicleader()){
										modToRemove = mod;
									}
									break;
								case EXPERTMAGICLEADER:
									if (((MonsterInst4)mod).isExpertmagicleader()){
										modToRemove = mod;
									}
									break;
								case SUPERIORMAGICLEADER:
									if (((MonsterInst4)mod).isSuperiormagicleader()){
										modToRemove = mod;
									}
									break;
								case NOUNDEADLEADER:
									if (((MonsterInst4)mod).isNoundeadleader()){
										modToRemove = mod;
									}
									break;
								case POORUNDEADLEADER:
									if (((MonsterInst4)mod).isPoorundeadleader()){
										modToRemove = mod;
									}
									break;
								case OKUNDEADLEADER:
									if (((MonsterInst4)mod).isOkundeadleader()){
										modToRemove = mod;
									}
									break;
								case GOODUNDEADLEADER:
									if (((MonsterInst4)mod).isGoodundeadleader()){
										modToRemove = mod;
									}
									break;
								case EXPERTUNDEADLEADER:
									if (((MonsterInst4)mod).isExpertundeadleader()){
										modToRemove = mod;
									}
									break;
								case SUPERIORUNDEADLEADER:
									if (((MonsterInst4)mod).isSuperiorundeadleader()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof MonsterInst5) {
								switch (inst2) {
								case WEAPON1:
									if (((MonsterInst5)mod).isWeapon()){
										weaponCount++;
										if (weaponCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case WEAPON2:
									if (((MonsterInst5)mod).isWeapon()){
										weaponCount++;
										if (weaponCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case WEAPON3:
									if (((MonsterInst5)mod).isWeapon()){
										weaponCount++;
										if (weaponCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case WEAPON4:
									if (((MonsterInst5)mod).isWeapon()){
										weaponCount++;
										if (weaponCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case ARMOR1:
									if (((MonsterInst5)mod).isArmor()){
										armorCount++;
										if (armorCount == 1) {
											modToRemove = mod;
										}
									}
									break;						
								case ARMOR2:
									if (((MonsterInst5)mod).isArmor()){
										armorCount++;
										if (armorCount == 2) {
											modToRemove = mod;
										}
									}
									break;						
								case ARMOR3:
									if (((MonsterInst5)mod).isArmor()){
										armorCount++;
										if (armorCount == 3) {
											modToRemove = mod;
										}
									}
									break;						
								case ONEBATTLESPELL:
									if (((MonsterInst5)mod).isOnebattlespell()){
										modToRemove = mod;
									}
									break;
								case FIRSTSHAPE:
									if (((MonsterInst5)mod).isFirstshape()){
										modToRemove = mod;
									}
									break;
								case SECONDSHAPE:
									if (((MonsterInst5)mod).isSecondshape()){
										modToRemove = mod;
									}
									break;
								case SECONDTMPSHAPE:
									if (((MonsterInst5)mod).isSecondtmpshape()){
										modToRemove = mod;
									}
									break;
								case SHAPECHANGE:
									if (((MonsterInst5)mod).isShapechange()){
										modToRemove = mod;
									}
									break;
								case LANDSHAPE:
									if (((MonsterInst5)mod).isLandshape()){
										modToRemove = mod;
									}
									break;
								case WATERSHAPE:
									if (((MonsterInst5)mod).isWatershape()){
										modToRemove = mod;
									}
									break;
								case FORESTSHAPE:
									if (((MonsterInst5)mod).isForestshape()){
										modToRemove = mod;
									}
									break;
								case PLAINSHAPE:
									if (((MonsterInst5)mod).isPlainshape()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON:
									if (((MonsterInst5)mod).isDomsummon()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON2:
									if (((MonsterInst5)mod).isDomsummon2()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON20:
									if (((MonsterInst5)mod).isDomsummon20()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTER1:
									if (((MonsterInst5)mod).isMakemonster1()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTER2:
									if (((MonsterInst5)mod).isMakemonster2()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTER3:
									if (((MonsterInst5)mod).isMakemonster3()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTER4:
									if (((MonsterInst5)mod).isMakemonster4()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTER5:
									if (((MonsterInst5)mod).isMakemonster5()){
										modToRemove = mod;
									}
									break;
								case SUMMON1:
									if (((MonsterInst5)mod).isSummon1()){
										modToRemove = mod;
									}
									break;
								case SUMMON5:
									if (((MonsterInst5)mod).isSummon5()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof MonsterInst6) {
								switch (inst2) {
								case HEAT:
									if (((MonsterInst6)mod).isHeat()){
										modToRemove = mod;
									}
									break;
								case COLD:
									if (((MonsterInst6)mod).isCold()){
										modToRemove = mod;
									}
									break;
								case STEALTHY:
									if (((MonsterInst6)mod).isStealthy()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (modToRemove != null) {
								break;
							}
						}
						if (modToRemove != null) {
							mods.remove(modToRemove);
						}
					}  
				});
			}
		});
		updateSelection();
	}
}
