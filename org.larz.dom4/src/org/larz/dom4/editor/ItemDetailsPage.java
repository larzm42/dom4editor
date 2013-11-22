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
import org.larz.dom4.db.ItemDB;
import org.larz.dom4.dm.dm.DmFactory;
import org.larz.dom4.dm.dm.Item;
import org.larz.dom4.dm.dm.ItemInst1;
import org.larz.dom4.dm.dm.ItemInst2;
import org.larz.dom4.dm.dm.ItemInst3;
import org.larz.dom4.dm.dm.ItemInst4;
import org.larz.dom4.dm.dm.ItemInst5;
import org.larz.dom4.dm.dm.ItemMods;
import org.larz.dom4.dm.dm.SelectItemById;
import org.larz.dom4.dm.dm.SelectItemByName;
import org.larz.dom4.dm.ui.help.HelpTextHelper;

@SuppressWarnings("incomplete-switch")
public class ItemDetailsPage extends AbstractDetailsPage {
	private Text name;
	private Button nameCheck;
	private Text descr;
	private Button descrCheck;
	private Label spriteLabel;

	enum Inst {
		NAME (Messages.getString("ItemDetailsSection.mod.name"), ""),
		DESCR (Messages.getString("ItemDetailsSection.mod.descr"), ""),
		COPYITEM (Messages.getString("ItemDetailsSection.mod.copyitem"), "0"),
		COPYSPR (Messages.getString("ItemDetailsSection.mod.copyspr"), "0"),
		SPR (Messages.getString("ItemDetailsSection.mod.spr"), "0"),
		CONSTLEVEL (Messages.getString("ItemDetailsSection.mod.constlevel"), "0"),
		MAINPATH (Messages.getString("ItemDetailsSection.mod.mainpath"), "0"),
		MAINLEVEL (Messages.getString("ItemDetailsSection.mod.mainlevel"), "1"),
		SECONDARYPATH (Messages.getString("ItemDetailsSection.mod.secondarypath"), "0"),
		SECONDARYLEVEL (Messages.getString("ItemDetailsSection.mod.secondarylevel"), "1"),		
		TYPE (Messages.getString("ItemDetailsSection.mod.type"), "1"),
		WEAPON (Messages.getString("ItemDetailsSection.mod.weapon"), "0"),
		ARMOR (Messages.getString("ItemDetailsSection.mod.armor"), ""),
		STR (Messages.getString("ItemDetailsSection.mod.str"), "0"),
		ATT (Messages.getString("ItemDetailsSection.mod.att"), "0"),
		DEF (Messages.getString("ItemDetailsSection.mod.def"), "0"),
		PREC (Messages.getString("ItemDetailsSection.mod.prec"), "0"),
		MR (Messages.getString("ItemDetailsSection.mod.mr"), "0"),
		MORALE (Messages.getString("ItemDetailsSection.mod.morale"), "0"),
		VOIDSANITY (Messages.getString("ItemDetailsSection.mod.voidsanity"), "0"),
		FIRERES (Messages.getString("ItemDetailsSection.mod.fireres"), "0"),
		COLDRES (Messages.getString("ItemDetailsSection.mod.coldres"), "0"),
		SHOCKRES (Messages.getString("ItemDetailsSection.mod.shockres"), "0"),
		POISONRES (Messages.getString("ItemDetailsSection.mod.poisonres"), "0"),
		ETHEREAL (Messages.getString("ItemDetailsSection.mod.ethereal")),
		RESTRICTED (Messages.getString("ItemDetailsSection.mod.restricted"), "0"),
		NOMOUNTED (Messages.getString("ItemDetailsSection.mod.nomounted")),
		NOCOLDBLOOD (Messages.getString("ItemDetailsSection.mod.nocoldblood")),
		NODEMON (Messages.getString("ItemDetailsSection.mod.nodemon")),
		NOUNDEAD (Messages.getString("ItemDetailsSection.mod.noundead")),
		NOINANIM (Messages.getString("ItemDetailsSection.mod.noinanim")),
		NOFEMALE (Messages.getString("ItemDetailsSection.mod.nofemale")),
		ONLYMOUNTED (Messages.getString("ItemDetailsSection.mod.onlymounted")),
		ONLYCOLDBLOOD (Messages.getString("ItemDetailsSection.mod.onlycoldblood")),
		ONLYDEMON (Messages.getString("ItemDetailsSection.mod.onlydemon")),
		ONLYUNDEAD (Messages.getString("ItemDetailsSection.mod.onlyundead")),
		ONLYINANIM (Messages.getString("ItemDetailsSection.mod.onlyinanim")),
		ONLYFEMALE (Messages.getString("ItemDetailsSection.mod.onlyfemale")),
		REQEYES (Messages.getString("ItemDetailsSection.mod.reqeyes")),
		NOFIND (Messages.getString("ItemDetailsSection.mod.nofind")),
		PEN (Messages.getString("ItemDetailsSection.mod.pen"), "0"),
		MAGICBOOST1 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		SPELL (Messages.getString("ItemDetailsSection.mod.spell"), "0"),
		MAGICBOOST2 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		AUTOSPELL (Messages.getString("ItemDetailsSection.mod.autospell"), "0"),
		MAGICBOOST3 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		AUTOSPELLREPEAT (Messages.getString("ItemDetailsSection.mod.autospellrepeat"), "0"),
		MAGICBOOST4 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		RANDOMSPELL (Messages.getString("ItemDetailsSection.mod.randomspell"), "0"),
		MAGICBOOST5 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		LUCK (Messages.getString("ItemDetailsSection.mod.luck")),
		MAGICBOOST6 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		QUICKNESS (Messages.getString("ItemDetailsSection.mod.quickness")),
		MAGICBOOST7 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		BLESS (Messages.getString("ItemDetailsSection.mod.bless")),
		MAGICBOOST8 (Messages.getString("ItemDetailsSection.mod.magicboost"), "0", "0"),
		BARKSKIN (Messages.getString("ItemDetailsSection.mod.barkskin")),
		STONESKIN (Messages.getString("ItemDetailsSection.mod.stoneskin")),
		IRONSKIN (Messages.getString("ItemDetailsSection.mod.ironskin")),
		MAPSPEED (Messages.getString("ItemDetailsSection.mod.mapspeed"), "0"),
		WATERBREATHING (Messages.getString("ItemDetailsSection.mod.waterbreathing")),
		FLOAT (Messages.getString("ItemDetailsSection.mod.float")),
		FLY (Messages.getString("ItemDetailsSection.mod.fly")),
		STORMIMMUNE (Messages.getString("ItemDetailsSection.mod.stormimmune")),
		RUN (Messages.getString("ItemDetailsSection.mod.run")),
		TRAMPLE (Messages.getString("ItemDetailsSection.mod.trample")),
		BERS (Messages.getString("ItemDetailsSection.mod.bers")),
		EXTRALIFE (Messages.getString("ItemDetailsSection.mod.extralife")),
		CHAMPPRIZE (Messages.getString("ItemDetailsSection.mod.champprize")),
		AUTOCOMPETE (Messages.getString("ItemDetailsSection.mod.autocompete")),
		TAINTED (Messages.getString("ItemDetailsSection.mod.tainted"), "0"),
		CURSED (Messages.getString("ItemDetailsSection.mod.cursed")),
		CURSE (Messages.getString("ItemDetailsSection.mod.curse")),
		DISEASE (Messages.getString("ItemDetailsSection.mod.disease")),
		CHESTWOUND (Messages.getString("ItemDetailsSection.mod.chestwound")),
		FEEBLEMIND (Messages.getString("ItemDetailsSection.mod.feeblemind")),
		MUTE (Messages.getString("ItemDetailsSection.mod.mute")),
		NHWOUND (Messages.getString("ItemDetailsSection.mod.nhwound")),
		CRIPPLED (Messages.getString("ItemDetailsSection.mod.crippled")),
		LOSEEYE (Messages.getString("ItemDetailsSection.mod.loseeye")),

		SPECIALLOOK (Messages.getString("ItemDetailsSection.mod.speciallook"), "0"),
		SINGLEBATTLE (Messages.getString("ItemDetailsSection.mod.singlebattle")),
		CHAOSREC (Messages.getString("ItemDetailsSection.mod.chaosrec")),
		STONEBEING (Messages.getString("ItemDetailsSection.mod.stonebeing")),

		NORIVERPASS (Messages.getString("ItemDetailsSection.mod.noriverpass")),
		UNTELEPORTABLE (Messages.getString("ItemDetailsSection.mod.unteleportable")),
		GIFTOFWATER (Messages.getString("ItemDetailsSection.mod.giftofwater")),
		
		SEDUCE (Messages.getString("ItemDetailsSection.mod.seduce"), "0"),
		SUCCUBUS (Messages.getString("ItemDetailsSection.mod.succubus"), "0"),
		BECKON (Messages.getString("ItemDetailsSection.mod.beckon"), "0"),
		FALSEARMY (Messages.getString("ItemDetailsSection.mod.falsearmy"), "0"),
		FOOLSCOUTS (Messages.getString("ItemDetailsSection.mod.foolscouts"), "0"),
		
		SLASHRES (Messages.getString("ItemDetailsSection.mod.slashres")),
		PIERCERES (Messages.getString("ItemDetailsSection.mod.pierceres")),
		BLUNTRES (Messages.getString("ItemDetailsSection.mod.bluntres")),
		ICEPROT (Messages.getString("ItemDetailsSection.mod.iceprot"), "0"),
		INVULNERABLE (Messages.getString("ItemDetailsSection.mod.invulnerable"), "0"),

		HEALER (Messages.getString("ItemDetailsSection.mod.healer"), "0"),
		AUTOHEALER (Messages.getString("ItemDetailsSection.mod.autohealer"), "0"),
		AUTODISHEALER (Messages.getString("ItemDetailsSection.mod.autodishealer"), "0"),
		AUTODISGRINDER (Messages.getString("ItemDetailsSection.mod.autodisgrinder"), "0"),
		HOMESICK (Messages.getString("ItemDetailsSection.mod.homesick"), "0"),
		UWDAMAGE (Messages.getString("ItemDetailsSection.mod.uwdamage"), "0"),
		REGENERATION (Messages.getString("ItemDetailsSection.mod.regeneration"), "0"),
		REINVIGORATION (Messages.getString("ItemDetailsSection.mod.reinvigoration"), "0"),
		WOUNDFEND (Messages.getString("ItemDetailsSection.mod.woundfend"), "0"),
		HPOVERFLOW (Messages.getString("ItemDetailsSection.mod.hpoverflow")),

		POISONCLOUD (Messages.getString("ItemDetailsSection.mod.poisoncloud"), "0"),
		DISEASECLOUD (Messages.getString("ItemDetailsSection.mod.diseasecloud"), "0"),
		ANIMALAWE (Messages.getString("ItemDetailsSection.mod.animalawe"), "0"),
		AWE (Messages.getString("ItemDetailsSection.mod.awe"), "0"),
		FEAR (Messages.getString("ItemDetailsSection.mod.fear"), "0"),
		FIRESHIELD (Messages.getString("ItemDetailsSection.mod.fireshield"), "0"),
		BANEFIRESHIELD (Messages.getString("ItemDetailsSection.mod.banefireshield"), "0"),
		DAMAGEREV (Messages.getString("ItemDetailsSection.mod.damagerev"), "0"),
		BLOODVENGEANCE (Messages.getString("ItemDetailsSection.mod.bloodvengeance"), "0"),
		SLIMER (Messages.getString("ItemDetailsSection.mod.slimer"), "0"),
		DEATHCURSE (Messages.getString("ItemDetailsSection.mod.deathcurse")),
		DEATHDISEASE (Messages.getString("ItemDetailsSection.mod.deathdisease"), "0"),
		DEATHPARALYZE (Messages.getString("ItemDetailsSection.mod.deathparalyze"), "0"),
		DEATHFIRE (Messages.getString("ItemDetailsSection.mod.deathfire"), "0"),
		
		CHAOSPOWER (Messages.getString("ItemDetailsSection.mod.chaospower"), "0"),
		FIREPOWER (Messages.getString("ItemDetailsSection.mod.firepower"), "0"),
		COLDPOWER (Messages.getString("ItemDetailsSection.mod.coldpower"), "0"),
		MAGICPOWER (Messages.getString("ItemDetailsSection.mod.magicpower"), "0"),
		STORMPOWER (Messages.getString("ItemDetailsSection.mod.stormpower"), "0"),
		DARKPOWER (Messages.getString("ItemDetailsSection.mod.darkpower"), "0"),
		
		SPRINGPOWER (Messages.getString("ItemDetailsSection.mod.springpower"), "0"),
		SUMMERPOWER (Messages.getString("ItemDetailsSection.mod.summerpower"), "0"),
		FALLPOWER (Messages.getString("ItemDetailsSection.mod.fallpower"), "0"),
		WINTERPOWER (Messages.getString("ItemDetailsSection.mod.winterpower"), "0"),

		AMBIDEXTROUS (Messages.getString("ItemDetailsSection.mod.ambidextrous"), "0"),
		BERSERK (Messages.getString("ItemDetailsSection.mod.berserk"), "0"),
		DARKVISION (Messages.getString("ItemDetailsSection.mod.darkvision"), "0"),
		TRAMPSWALLOW (Messages.getString("ItemDetailsSection.mod.trampswallow")),
		DIGEST (Messages.getString("ItemDetailsSection.mod.digest"), "0"),
		INCORPORATE (Messages.getString("ItemDetailsSection.mod.incorporate"), "0"),
		
		CASTLEDEF (Messages.getString("ItemDetailsSection.mod.castledef"), "0"),
		SIEGEBONUS (Messages.getString("ItemDetailsSection.mod.siegebonus"), "0"),
		PATROLBONUS (Messages.getString("ItemDetailsSection.mod.patrolbonus"), "0"),
		PILLAGEBONUS (Messages.getString("ItemDetailsSection.mod.pillagebonus"), "0"),
		SUPPLYBONUS (Messages.getString("ItemDetailsSection.mod.supplybonus"), "0"),
		NOBADEVENTS (Messages.getString("ItemDetailsSection.mod.nobadevents"), "0"),
		INCPROVDEF (Messages.getString("ItemDetailsSection.mod.incprovdef"), "0"),
		INCUNREST (Messages.getString("ItemDetailsSection.mod.incunrest"), "0"),
		LEPER (Messages.getString("ItemDetailsSection.mod.leper"), "0"),
		POPKILL (Messages.getString("ItemDetailsSection.mod.popkill"), "0"),
		INQUISITOR (Messages.getString("ItemDetailsSection.mod.inquisitor")),
		HERETIC (Messages.getString("ItemDetailsSection.mod.heretic"), "0"),
		ELEGIST (Messages.getString("ItemDetailsSection.mod.elegist"), "0"),
		SPREADDOM (Messages.getString("ItemDetailsSection.mod.spreaddom"), "0"),
		SHATTEREDSOUL (Messages.getString("ItemDetailsSection.mod.shatteredsoul"), "0"),
		TAXCOLLECTOR (Messages.getString("ItemDetailsSection.mod.taxcollector")),
		GOLD (Messages.getString("ItemDetailsSection.mod.gold"), "0"),
		
		INSPIRATIONAL (Messages.getString("ItemDetailsSection.mod.inspirational"), "0"),
		BEASTMASTER (Messages.getString("ItemDetailsSection.mod.beastmaster"), "0"),
		TASKMASTER (Messages.getString("ItemDetailsSection.mod.taskmaster"), "0"),
		UNDISCIPLINED (Messages.getString("ItemDetailsSection.mod.undisciplined")),
		FORMATIONFIGHTER (Messages.getString("ItemDetailsSection.mod.formationfighter"), "0"),
		BODYGUARD (Messages.getString("ItemDetailsSection.mod.bodyguard"), "0"),
		STANDARD (Messages.getString("ItemDetailsSection.mod.standard"), "0"),
		
		DOUSE (Messages.getString("ItemDetailsSection.mod.douse"), "0"),
		RESEARCHBONUS (Messages.getString("ItemDetailsSection.mod.researchbonus"), "0"),
		INSPIRINGRES (Messages.getString("ItemDetailsSection.mod.inspiringres"), "0"),
		DIVINEINS (Messages.getString("ItemDetailsSection.mod.divineins"), "0"),
		DRAINIMMUNE (Messages.getString("ItemDetailsSection.mod.drainimmune")),
		MAGICIMMUNE (Messages.getString("ItemDetailsSection.mod.magicimmune")),
		FORGEBONUS (Messages.getString("ItemDetailsSection.mod.forgebonus"), "0"),
		FIXFORGEBONUS (Messages.getString("ItemDetailsSection.mod.fixforgebonus"), "0"),
		CROSSBREEDER (Messages.getString("ItemDetailsSection.mod.crossbreeder"), "0"),
		BONUSSPELLS (Messages.getString("ItemDetailsSection.mod.bonusspells"), "0"),
		COMSLAVE (Messages.getString("ItemDetailsSection.mod.comslave")),
		DEATHBANISH (Messages.getString("ItemDetailsSection.mod.deathbanish"), "0"),
		KOKYTOSRET (Messages.getString("ItemDetailsSection.mod.kokytosret"), "0"),
		INFERNORET (Messages.getString("ItemDetailsSection.mod.infernoret"), "0"),
		VOIDRET (Messages.getString("ItemDetailsSection.mod.voidret"), "0"),
		ALLRET (Messages.getString("ItemDetailsSection.mod.allret"), "0"),
		
		FIRERANGE (Messages.getString("ItemDetailsSection.mod.firerange"), "0"),
		AIRRANGE (Messages.getString("ItemDetailsSection.mod.airrange"), "0"),
		WATERRANGE (Messages.getString("ItemDetailsSection.mod.waterrange"), "0"),
		EARTHRANGE (Messages.getString("ItemDetailsSection.mod.earthrange"), "0"),
		ASTRALRANGE (Messages.getString("ItemDetailsSection.mod.astralrange"), "0"),
		DEATHRANGE (Messages.getString("ItemDetailsSection.mod.deathrange"), "0"),
		NATURERANGE (Messages.getString("ItemDetailsSection.mod.naturerange"), "0"),
		BLOODRANGE (Messages.getString("ItemDetailsSection.mod.bloodrange"), "0"),
		ELEMENTRANGE (Messages.getString("ItemDetailsSection.mod.elementrange"), "0"),
		SORCERYRANGE (Messages.getString("ItemDetailsSection.mod.sorceryrange"), "0"),
		ALLRANGE (Messages.getString("ItemDetailsSection.mod.allrange"), "0"),
		
		MAKEPEARLS (Messages.getString("ItemDetailsSection.mod.makepearls"), "0"),
		TMPFIREGEMS (Messages.getString("ItemDetailsSection.mod.tmpfiregems"), "0"),
		TMPAIRGEMS (Messages.getString("ItemDetailsSection.mod.tmpairgems"), "0"),
		TMPWATERGEMS (Messages.getString("ItemDetailsSection.mod.tmpwatergems"), "0"),
		TMPEARTHGEMS (Messages.getString("ItemDetailsSection.mod.tmpearthgems"), "0"),
		TMPASTRALGEMS (Messages.getString("ItemDetailsSection.mod.tmpastralgems"), "0"),
		TMPDEATHGEMS (Messages.getString("ItemDetailsSection.mod.tmpdeathgems"), "0"),
		TMPNATUREGEMS (Messages.getString("ItemDetailsSection.mod.tmpnaturegems"), "0"),
		TMPBLOODSLAVES (Messages.getString("ItemDetailsSection.mod.tmpbloodslaves"), "0"),
		
		DOMSUMMON (Messages.getString("ItemDetailsSection.mod.domsummon"), "0"),
		DOMSUMMON2 (Messages.getString("ItemDetailsSection.mod.domsummon2"), "0"),
		DOMSUMMON20 (Messages.getString("ItemDetailsSection.mod.domsummon20"), "0"),
		RAREDOMSUMMON (Messages.getString("ItemDetailsSection.mod.raredomsummon"), "0"),
		SUMMON1 (Messages.getString("ItemDetailsSection.mod.summon1"), "0"),
		SUMMON2 (Messages.getString("ItemDetailsSection.mod.summon2"), "0"),
		SUMMON3 (Messages.getString("ItemDetailsSection.mod.summon3"), "0"),
		SUMMON4 (Messages.getString("ItemDetailsSection.mod.summon4"), "0"),
		SUMMON5 (Messages.getString("ItemDetailsSection.mod.summon5"), "0"),
		MAKEMONSTERS1 (Messages.getString("ItemDetailsSection.mod.makemonsters1"), "0"),
		MAKEMONSTERS2 (Messages.getString("ItemDetailsSection.mod.makemonsters2"), "0"),
		MAKEMONSTERS3 (Messages.getString("ItemDetailsSection.mod.makemonsters3"), "0"),
		MAKEMONSTERS4 (Messages.getString("ItemDetailsSection.mod.makemonsters4"), "0"),
		MAKEMONSTERS5 (Messages.getString("ItemDetailsSection.mod.makemonsters5"), "0"),
		BATTLESUM1 (Messages.getString("ItemDetailsSection.mod.battlesum1"), "0"),
		BATTLESUM2 (Messages.getString("ItemDetailsSection.mod.battlesum2"), "0"),
		BATTLESUM3 (Messages.getString("ItemDetailsSection.mod.battlesum3"), "0"),
		BATTLESUM4 (Messages.getString("ItemDetailsSection.mod.battlesum4"), "0"),
		BATTLESUM5 (Messages.getString("ItemDetailsSection.mod.battlesum5"), "0"),
		BATSTARTSUM1 (Messages.getString("ItemDetailsSection.mod.batstartsum1"), "0"),
		BATSTARTSUM2 (Messages.getString("ItemDetailsSection.mod.batstartsum2"), "0"),
		BATSTARTSUM3 (Messages.getString("ItemDetailsSection.mod.batstartsum3"), "0"),
		BATSTARTSUM4 (Messages.getString("ItemDetailsSection.mod.batstartsum4"), "0"),
		BATSTARTSUM5 (Messages.getString("ItemDetailsSection.mod.batstartsum5"), "0"),
		BATSTARTSUM1D6 (Messages.getString("ItemDetailsSection.mod.batstartsum1d6"), "0"),
		BATSTARTSUM2D6 (Messages.getString("ItemDetailsSection.mod.batstartsum2d6"), "0"),
		BATSTARTSUM3D6 (Messages.getString("ItemDetailsSection.mod.batstartsum3d6"), "0"),
		BATSTARTSUM4D6 (Messages.getString("ItemDetailsSection.mod.batstartsum4d6"), "0"),
		BATSTARTSUM5D6 (Messages.getString("ItemDetailsSection.mod.batstartsum5d6"), "0");
		
		private String label;
		private String defaultValue;
		private String defaultValue2;
		
		Inst(String label) {
			this.label = label;
		}

		Inst(String label, String defaultValue) {
			this.label = label;
			this.defaultValue = defaultValue;
		}
		
		Inst(String label, String defaultValue, String defaultValue2) {
			this.label = label;
			this.defaultValue = defaultValue;
			this.defaultValue2 = defaultValue2;
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
		private Text value;
		private Label defaultLabel;
	}
	
	class Inst4Fields implements InstFields {
		private Button check;
		private Label defaultLabel;
	}

	class Inst5Fields implements InstFields {
		private Button check;
		private MappedDynamicCombo value1;
		private Text value2;
		private Label defaultLabel1;
		private Label defaultLabel2;
	}

	class Inst6Fields implements InstFields {
		private Button check;
		private MappedDynamicCombo value;
		private Label defaultLabel;
	}
	
	private EnumMap<Inst, InstFields> instMap = new EnumMap<Inst, InstFields>(Inst.class);
	private Set<List<Inst>> dynamicFields = new HashSet<List<Inst>>();

	public ItemDetailsPage(XtextEditor doc, TableViewer viewer) {
		super(doc, viewer);
		instMap.put(Inst.ARMOR, new Inst1Fields());
		instMap.put(Inst.CONSTLEVEL, new Inst2Fields());
		instMap.put(Inst.MAINPATH, new Inst6Fields());
		instMap.put(Inst.MAINLEVEL, new Inst2Fields());
		instMap.put(Inst.SECONDARYPATH, new Inst6Fields());
		instMap.put(Inst.SECONDARYLEVEL, new Inst2Fields());
		instMap.put(Inst.COPYSPR, new Inst3Fields());
		instMap.put(Inst.TYPE, new Inst6Fields());
		instMap.put(Inst.WEAPON, new Inst3Fields());
		instMap.put(Inst.COPYITEM, new Inst3Fields());
		instMap.put(Inst.SPR, new Inst1Fields());
		instMap.put(Inst.STR, new Inst2Fields());
		instMap.put(Inst.ATT, new Inst2Fields());
		instMap.put(Inst.DEF, new Inst2Fields());
		instMap.put(Inst.PREC, new Inst2Fields());
		instMap.put(Inst.MR, new Inst2Fields());
		instMap.put(Inst.MORALE, new Inst2Fields());
		instMap.put(Inst.VOIDSANITY, new Inst2Fields());
		instMap.put(Inst.GIFTOFWATER, new Inst2Fields());
		instMap.put(Inst.INVULNERABLE, new Inst2Fields());
		instMap.put(Inst.INSPIRINGRES, new Inst2Fields());
		instMap.put(Inst.FIRERES, new Inst2Fields());
		instMap.put(Inst.COLDRES, new Inst2Fields());
		instMap.put(Inst.SHOCKRES, new Inst2Fields());
		instMap.put(Inst.POISONRES, new Inst2Fields());
		instMap.put(Inst.ETHEREAL, new Inst4Fields());
		instMap.put(Inst.RESTRICTED, new Inst2Fields());
		instMap.put(Inst.NOMOUNTED, new Inst4Fields());
		instMap.put(Inst.NOCOLDBLOOD, new Inst4Fields());
		instMap.put(Inst.NODEMON, new Inst4Fields());
		instMap.put(Inst.NOUNDEAD, new Inst4Fields());
		instMap.put(Inst.NOINANIM, new Inst4Fields());
		instMap.put(Inst.NOFEMALE, new Inst4Fields());
		instMap.put(Inst.ONLYMOUNTED, new Inst4Fields());
		instMap.put(Inst.ONLYCOLDBLOOD, new Inst4Fields());
		instMap.put(Inst.ONLYDEMON, new Inst4Fields());
		instMap.put(Inst.ONLYUNDEAD, new Inst4Fields());
		instMap.put(Inst.ONLYINANIM, new Inst4Fields());
		instMap.put(Inst.ONLYFEMALE, new Inst4Fields());
		instMap.put(Inst.REQEYES, new Inst4Fields());
		instMap.put(Inst.NOFIND, new Inst4Fields());
		instMap.put(Inst.PEN, new Inst2Fields());
		instMap.put(Inst.MAGICBOOST1, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST2, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST3, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST4, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST5, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST6, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST7, new Inst5Fields());
		instMap.put(Inst.MAGICBOOST8, new Inst5Fields());
		instMap.put(Inst.SPELL, new Inst1Fields());
		instMap.put(Inst.AUTOSPELL, new Inst1Fields());
		instMap.put(Inst.AUTOSPELLREPEAT, new Inst2Fields());
		instMap.put(Inst.RANDOMSPELL, new Inst2Fields());
		instMap.put(Inst.LUCK, new Inst4Fields());
		instMap.put(Inst.QUICKNESS, new Inst4Fields());
		instMap.put(Inst.BLESS, new Inst4Fields());
		instMap.put(Inst.BARKSKIN, new Inst4Fields());
		instMap.put(Inst.STONESKIN, new Inst4Fields());
		instMap.put(Inst.IRONSKIN, new Inst4Fields());
		instMap.put(Inst.MAPSPEED, new Inst2Fields());
		instMap.put(Inst.WATERBREATHING, new Inst4Fields());
		instMap.put(Inst.FLOAT, new Inst4Fields());
		instMap.put(Inst.FLY, new Inst4Fields());
		instMap.put(Inst.STORMIMMUNE, new Inst4Fields());
		instMap.put(Inst.RUN, new Inst4Fields());
		instMap.put(Inst.TRAMPLE, new Inst4Fields());
		instMap.put(Inst.BERS, new Inst4Fields());
		instMap.put(Inst.EXTRALIFE, new Inst4Fields());
		instMap.put(Inst.CHAMPPRIZE, new Inst4Fields());
		instMap.put(Inst.AUTOCOMPETE, new Inst4Fields());
		instMap.put(Inst.TAINTED, new Inst2Fields());
		instMap.put(Inst.CURSED, new Inst4Fields());
		instMap.put(Inst.CURSE, new Inst4Fields());
		instMap.put(Inst.DISEASE, new Inst4Fields());
		instMap.put(Inst.CHESTWOUND, new Inst4Fields());
		instMap.put(Inst.FEEBLEMIND, new Inst4Fields());
		instMap.put(Inst.MUTE, new Inst4Fields());
		instMap.put(Inst.NHWOUND, new Inst4Fields());
		instMap.put(Inst.CRIPPLED, new Inst4Fields());
		instMap.put(Inst.LOSEEYE, new Inst4Fields());
		instMap.put(Inst.SPECIALLOOK, new Inst2Fields());
		instMap.put(Inst.SINGLEBATTLE, new Inst4Fields());
		instMap.put(Inst.CHAOSREC, new Inst4Fields());
		instMap.put(Inst.STONEBEING, new Inst4Fields());
		instMap.put(Inst.NORIVERPASS, new Inst4Fields());
		instMap.put(Inst.UNTELEPORTABLE, new Inst4Fields());
		instMap.put(Inst.SEDUCE, new Inst2Fields());
		instMap.put(Inst.SUCCUBUS, new Inst2Fields());
		instMap.put(Inst.BECKON, new Inst2Fields());
		instMap.put(Inst.FALSEARMY, new Inst2Fields());
		instMap.put(Inst.FOOLSCOUTS, new Inst2Fields());
		instMap.put(Inst.SLASHRES, new Inst4Fields());
		instMap.put(Inst.PIERCERES, new Inst4Fields());
		instMap.put(Inst.BLUNTRES, new Inst4Fields());
		instMap.put(Inst.ICEPROT, new Inst2Fields());
		instMap.put(Inst.HEALER, new Inst2Fields());
		instMap.put(Inst.AUTOHEALER, new Inst2Fields());
		instMap.put(Inst.AUTODISHEALER, new Inst2Fields());
		instMap.put(Inst.AUTODISGRINDER, new Inst2Fields());
		instMap.put(Inst.HOMESICK, new Inst2Fields());
		instMap.put(Inst.UWDAMAGE, new Inst2Fields());
		instMap.put(Inst.REGENERATION, new Inst2Fields());
		instMap.put(Inst.REINVIGORATION, new Inst2Fields());
		instMap.put(Inst.WOUNDFEND, new Inst2Fields());
		instMap.put(Inst.HPOVERFLOW, new Inst4Fields());
		instMap.put(Inst.POISONCLOUD, new Inst2Fields());
		instMap.put(Inst.DISEASECLOUD, new Inst2Fields());
		instMap.put(Inst.ANIMALAWE, new Inst2Fields());
		instMap.put(Inst.AWE, new Inst2Fields());
		instMap.put(Inst.FEAR, new Inst2Fields());
		instMap.put(Inst.FIRESHIELD, new Inst2Fields());
		instMap.put(Inst.BANEFIRESHIELD, new Inst2Fields());
		instMap.put(Inst.DAMAGEREV, new Inst2Fields());
		instMap.put(Inst.BLOODVENGEANCE, new Inst2Fields());
		instMap.put(Inst.SLIMER, new Inst2Fields());
		instMap.put(Inst.DEATHCURSE, new Inst4Fields());
		instMap.put(Inst.DEATHDISEASE, new Inst2Fields());
		instMap.put(Inst.DEATHPARALYZE, new Inst2Fields());
		instMap.put(Inst.DEATHFIRE, new Inst2Fields());
		instMap.put(Inst.CHAOSPOWER, new Inst2Fields());
		instMap.put(Inst.FIREPOWER, new Inst2Fields());
		instMap.put(Inst.COLDPOWER, new Inst2Fields());
		instMap.put(Inst.MAGICPOWER, new Inst2Fields());
		instMap.put(Inst.STORMPOWER, new Inst2Fields());
		instMap.put(Inst.DARKPOWER, new Inst2Fields());
		instMap.put(Inst.SPRINGPOWER, new Inst2Fields());
		instMap.put(Inst.SUMMERPOWER, new Inst2Fields());
		instMap.put(Inst.FALLPOWER, new Inst2Fields());
		instMap.put(Inst.WINTERPOWER, new Inst2Fields());
		instMap.put(Inst.AMBIDEXTROUS, new Inst2Fields());
		instMap.put(Inst.BERSERK, new Inst2Fields());
		instMap.put(Inst.DARKVISION, new Inst2Fields());
		instMap.put(Inst.TRAMPSWALLOW, new Inst4Fields());
		instMap.put(Inst.DIGEST, new Inst2Fields());
		instMap.put(Inst.INCORPORATE, new Inst2Fields());
		instMap.put(Inst.CASTLEDEF, new Inst2Fields());
		instMap.put(Inst.SIEGEBONUS, new Inst2Fields());
		instMap.put(Inst.PATROLBONUS, new Inst2Fields());
		instMap.put(Inst.PILLAGEBONUS, new Inst2Fields());
		instMap.put(Inst.SUPPLYBONUS, new Inst2Fields());
		instMap.put(Inst.NOBADEVENTS, new Inst2Fields());
		instMap.put(Inst.INCPROVDEF, new Inst2Fields());
		instMap.put(Inst.INCUNREST, new Inst2Fields());
		instMap.put(Inst.LEPER, new Inst2Fields());
		instMap.put(Inst.POPKILL, new Inst2Fields());
		instMap.put(Inst.INQUISITOR, new Inst4Fields());
		instMap.put(Inst.HERETIC, new Inst2Fields());
		instMap.put(Inst.ELEGIST, new Inst2Fields());
		instMap.put(Inst.SPREADDOM, new Inst2Fields());
		instMap.put(Inst.SHATTEREDSOUL, new Inst2Fields());
		instMap.put(Inst.TAXCOLLECTOR, new Inst4Fields());
		instMap.put(Inst.GOLD, new Inst2Fields());
		instMap.put(Inst.INSPIRATIONAL, new Inst2Fields());
		instMap.put(Inst.BEASTMASTER, new Inst2Fields());
		instMap.put(Inst.TASKMASTER, new Inst2Fields());
		instMap.put(Inst.UNDISCIPLINED, new Inst4Fields());
		instMap.put(Inst.FORMATIONFIGHTER, new Inst2Fields());
		instMap.put(Inst.BODYGUARD, new Inst2Fields());
		instMap.put(Inst.STANDARD, new Inst2Fields());
		instMap.put(Inst.DOUSE, new Inst2Fields());
		instMap.put(Inst.RESEARCHBONUS, new Inst2Fields());
		instMap.put(Inst.DIVINEINS, new Inst2Fields());
		instMap.put(Inst.DRAINIMMUNE, new Inst4Fields());
		instMap.put(Inst.MAGICIMMUNE, new Inst4Fields());
		instMap.put(Inst.FORGEBONUS, new Inst2Fields());
		instMap.put(Inst.FIXFORGEBONUS, new Inst2Fields());
		instMap.put(Inst.CROSSBREEDER, new Inst2Fields());
		instMap.put(Inst.BONUSSPELLS, new Inst2Fields());
		instMap.put(Inst.COMSLAVE, new Inst4Fields());
		instMap.put(Inst.DEATHBANISH, new Inst2Fields());
		instMap.put(Inst.KOKYTOSRET, new Inst2Fields());
		instMap.put(Inst.INFERNORET, new Inst2Fields());
		instMap.put(Inst.VOIDRET, new Inst2Fields());
		instMap.put(Inst.ALLRET, new Inst2Fields());
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
		instMap.put(Inst.MAKEPEARLS, new Inst2Fields());
		instMap.put(Inst.TMPFIREGEMS, new Inst2Fields());
		instMap.put(Inst.TMPAIRGEMS, new Inst2Fields());
		instMap.put(Inst.TMPWATERGEMS, new Inst2Fields());
		instMap.put(Inst.TMPEARTHGEMS, new Inst2Fields());
		instMap.put(Inst.TMPASTRALGEMS, new Inst2Fields());
		instMap.put(Inst.TMPDEATHGEMS, new Inst2Fields());
		instMap.put(Inst.TMPNATUREGEMS, new Inst2Fields());
		instMap.put(Inst.TMPBLOODSLAVES, new Inst2Fields());
		instMap.put(Inst.DOMSUMMON, new Inst3Fields());
		instMap.put(Inst.DOMSUMMON2, new Inst3Fields());
		instMap.put(Inst.DOMSUMMON20, new Inst3Fields());
		instMap.put(Inst.RAREDOMSUMMON, new Inst3Fields());
		instMap.put(Inst.SUMMON1, new Inst3Fields());
		instMap.put(Inst.SUMMON2, new Inst3Fields());
		instMap.put(Inst.SUMMON3, new Inst3Fields());
		instMap.put(Inst.SUMMON4, new Inst3Fields());
		instMap.put(Inst.SUMMON5, new Inst3Fields());
		instMap.put(Inst.MAKEMONSTERS1, new Inst3Fields());
		instMap.put(Inst.MAKEMONSTERS2, new Inst3Fields());
		instMap.put(Inst.MAKEMONSTERS3, new Inst3Fields());
		instMap.put(Inst.MAKEMONSTERS4, new Inst3Fields());
		instMap.put(Inst.MAKEMONSTERS5, new Inst3Fields());
		instMap.put(Inst.BATTLESUM1, new Inst3Fields());
		instMap.put(Inst.BATTLESUM2, new Inst3Fields());
		instMap.put(Inst.BATTLESUM3, new Inst3Fields());
		instMap.put(Inst.BATTLESUM4, new Inst3Fields());
		instMap.put(Inst.BATTLESUM5, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM1, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM2, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM3, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM4, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM5, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM1D6, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM2D6, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM3D6, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM4D6, new Inst3Fields());
		instMap.put(Inst.BATSTARTSUM5D6, new Inst3Fields());

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
		s1.setText(Messages.getString("ItemDetailsSection.name")); //$NON-NLS-1$
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
		
		nameCheck = toolkit.createButton(nameComp, Messages.getString("ItemDetailsSection.mod.name"), SWT.CHECK); //$NON-NLS-1$
		nameCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.ITEM_CATEGORY, "name"));

		name = toolkit.createText(nameComp, null, SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
		name.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setItemname(doc, name.getText());
			}			
		});
		name.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setItemname(doc, name.getText());
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
					name.setText(getSelectItemname((Item)input));
					nameCheck.setFont(boldFont);
				} else {
					removeInst(Inst.NAME, doc);
					name.setEnabled(false);
					if (input instanceof SelectItemById || input instanceof SelectItemByName) {
						name.setText(getSelectItemname((Item)input));
					} else {
						name.setText("");
					}
					nameCheck.setFont(normalFont);
				}
			}
		});

		descrCheck = toolkit.createButton(nameComp, Messages.getString("ItemDetailsSection.mod.descr"), SWT.CHECK);
		descrCheck.setToolTipText(HelpTextHelper.getText(HelpTextHelper.ITEM_CATEGORY, "descr"));

		descr = toolkit.createText(nameComp, null, SWT.MULTI | SWT.BORDER | SWT.WRAP); //$NON-NLS-1$
		descr.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				setItemdescr(doc, descr.getText());
			}			
		});
		descr.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\r') {
					setItemdescr(doc, descr.getText());
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
					descr.setText(getSelectItemdescr((Item)input));
					descrCheck.setFont(boldFont);
				} else {
					removeInst(Inst.DESCR, doc);
					descr.setEnabled(false);
					descr.setBackground(toolkit.getColors().getInactiveBackground());
					descr.setText(getSelectItemdescr((Item)input));
					descrCheck.setFont(normalFont);
				}
			}
		});

		spriteLabel = toolkit.createLabel(nameComp, "", SWT.NONE);

		Composite leftColumn = null;
		Composite rightColumn = null;
		boolean isRight = false;
		for (final Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			final Inst key = fields.getKey();
			
			if (key.equals(Inst.COPYITEM) || 
				key.equals(Inst.STR) || 
				key.equals(Inst.RESTRICTED) || 
				key.equals(Inst.PEN) ||
				key.equals(Inst.MAPSPEED) ||
				key.equals(Inst.CHAMPPRIZE) ||
				key.equals(Inst.TAINTED) ||
				key.equals(Inst.SPECIALLOOK) ||
				key.equals(Inst.NORIVERPASS) ||
				key.equals(Inst.SEDUCE) ||
				key.equals(Inst.SLASHRES) ||
				key.equals(Inst.HEALER) ||
				key.equals(Inst.POISONCLOUD) ||
				key.equals(Inst.CHAOSPOWER) ||
				key.equals(Inst.SPRINGPOWER) ||
				key.equals(Inst.AMBIDEXTROUS) ||
				key.equals(Inst.CASTLEDEF) ||
				key.equals(Inst.INSPIRATIONAL) ||
				key.equals(Inst.DOUSE) ||
				key.equals(Inst.FIRERANGE) ||
				key.equals(Inst.MAKEPEARLS) ||
				key.equals(Inst.DOMSUMMON)){

				final Section expandable = toolkit.createSection(client, ExpandableComposite.TWISTIE | ExpandableComposite.TITLE_BAR);
				switch (key) {
				case COPYITEM:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.basic"));
					break;
				case STR:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.attributes"));
					break;
				case RESTRICTED:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.restrictions"));
					break;
				case PEN:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.magic"));
					break;
				case MAPSPEED:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.movement"));
					break;
				case CHAMPPRIZE:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.arena"));
					break;
				case TAINTED:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.curses"));
					break;
				case SPECIALLOOK:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.misc"));
					break;
				case NORIVERPASS:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.movement"));
					break;
				case SEDUCE:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.stealth"));
					break;
				case SLASHRES:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.damage"));
					break;
				case HEALER:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.healing"));
					break;
				case POISONCLOUD:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.auras"));
					break;
				case CHAOSPOWER:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.elemental"));
					break;
				case SPRINGPOWER:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.season"));
					break;
				case AMBIDEXTROUS:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.othercombat"));
					break;
				case CASTLEDEF:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.noncombat"));
					break;
				case INSPIRATIONAL:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.leadership"));
					break;
				case DOUSE:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.magic"));
					break;
				case FIRERANGE:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.range"));
					break;
				case MAKEPEARLS:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.gems"));
					break;
				case DOMSUMMON:
					expandable.setText(Messages.getString("ItemDetailsSection.mod.section.monster.summon"));
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
				if (key.equals(Inst.COPYITEM)) {
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
			check.setText(key.label);
			check.setToolTipText(HelpTextHelper.getText(HelpTextHelper.ITEM_CATEGORY, key.label));
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
							addInst3(key, doc, key.defaultValue);
						} else if (field instanceof Inst4Fields) {
							addInst4(key, doc);
						} else if (field instanceof Inst5Fields) {
							addInst5(key, doc, key.defaultValue, key.defaultValue2);
						} else if (field instanceof Inst6Fields) {
							addInst2(key, doc, key.defaultValue);
						}
					} else {
						removeInst(key, doc);
						check.setFont(normalFont);
					}
				}

			});

			Text myValue1 = null;
			if (field instanceof Inst1Fields ||	field instanceof Inst2Fields || field instanceof Inst3Fields) {
				final Text value = toolkit.createText(isRight?rightColumn:leftColumn, "", SWT.SINGLE | SWT.BORDER); //$NON-NLS-1$
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
							setInst3(key, doc, value.getText());
//						} else if (field instanceof Inst5Fields) {
//							setInst5(key, doc, value.getText(), null);
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
								setInst3(key, doc, value.getText());
//							} else if (field instanceof Inst5Fields) {
//								setInst5(key, doc, value.getText(), null);
							}
						}
					}
				});
				value.setEnabled(false);
				
				if (field instanceof Inst1Fields) {
					gd = new GridData(SWT.FILL, SWT.FILL, false, false);
					gd.widthHint = 120;
					gd.horizontalSpan = 3;
				} else if (field instanceof Inst2Fields || field instanceof Inst3Fields) {
					gd = new GridData(SWT.FILL, SWT.BEGINNING, false, false);
					if (fields.getKey() == Inst.WEAPON) {
						gd.widthHint = 120;
						gd.horizontalSpan = 3;
					} else {
						gd.widthHint = DEFAULT_VALUE_WIDTH;
					}
				}
				value.setLayoutData(gd);
				
			}
			MappedDynamicCombo myInst5Value1 = null;
			Text myInst5Value2 = null;
			if (field instanceof Inst5Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst5Value1 = value;
				
				check.addSelectionListener(new SelectionAdapter() {
					@SuppressWarnings("unchecked")
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
											if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value1.getData())) {
												((Inst5Fields)fields.getValue()).value1.setData(Boolean.TRUE);
												((Inst5Fields)fields.getValue()).value2.setData(Boolean.TRUE);
												((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
												((Inst5Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
												((Inst5Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
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
											if (Boolean.TRUE.equals(((Inst5Fields)fields.getValue()).value1.getData()) && !((Inst5Fields)fields.getValue()).value1.isEnabled()) {
												((Inst5Fields)fields.getValue()).value1.setData(Boolean.FALSE);
												((Inst5Fields)fields.getValue()).value2.setData(Boolean.FALSE);
												((Inst5Fields)fields.getValue()).check.setData(Boolean.FALSE);
												((Inst5Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
												((Inst5Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
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
						setInst5(key, doc, Integer.toString(val), null);
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

			MappedDynamicCombo myInst6Value1 = null;
			if (field instanceof Inst6Fields) {
				final MappedDynamicCombo value = new MappedDynamicCombo(isRight?rightColumn:leftColumn, SWT.READ_ONLY);
				myInst6Value1 = value;
				
				check.addSelectionListener(new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
						if (check.getSelection()) {
							value.setEnabled(true);
							if (key == Inst.MAINPATH) {
								value.setItems(new String[]{
									"Fire",	"Air", "Water", "Earth", "Astral", "Death",
									"Nature", "Blood",
									},
									new int[]{0, 1, 2, 3, 4, 5, 6, 7});
							} else if (key == Inst.SECONDARYPATH) {
								value.setItems(new String[]{
										"None", "Fire",	"Air", "Water", "Earth", "Astral", "Death",
										"Nature", "Blood",
										},
										new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7});
							} else if (key == Inst.TYPE) {
								value.setItems(new String[]{
										"1-handed weapon", "2-handed weapon", "missile weapon", "shield", "body armor", "helmet", "boots", "misc item"},
										new int[]{1, 2, 3, 4, 5, 6, 7, 8});
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
				if (key == Inst.TYPE) {
					gd.widthHint = DEFAULT_VALUE_WIDTH+60;
					gd.horizontalSpan = 2;
				} else {
					gd.widthHint = DEFAULT_VALUE_WIDTH;
				}
				value.setLayoutData(gd);
				
			}
				
			Label defaultLabel1 = null;
			
			if (field instanceof Inst1Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst2Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				gd.horizontalSpan = 3;
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst3Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				if (fields.getKey() != Inst.WEAPON) {
					gd.horizontalSpan = 3;
				}
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst4Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
				createSpacer(toolkit, isRight?rightColumn:leftColumn, 3);
			} else if (field instanceof Inst5Fields) {
//				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1 = new DynamicLabel(isRight?rightColumn:leftColumn, SWT.NONE);
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				defaultLabel1.setLayoutData(gd);
			} else if (field instanceof Inst6Fields) {
				defaultLabel1 = toolkit.createLabel(isRight?rightColumn:leftColumn, "");
				defaultLabel1.setEnabled(false);
				gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
				if (key == Inst.TYPE) {
					gd.horizontalSpan = 2;
				} else {
					gd.horizontalSpan = 3;
				}
				defaultLabel1.setLayoutData(gd);
			}
			
			Label defaultLabel2 = null;
			if (field instanceof Inst5Fields) {
				final Text value = new DynamicText(isRight?rightColumn:leftColumn, SWT.SINGLE | SWT.BORDER);
				myInst5Value2 = value;
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
						setInst5(key, doc, null, value.getText());
					}			
				});
				value.addKeyListener(new KeyAdapter() {
					@Override
					public void keyPressed(KeyEvent e) {
						if (e.character == '\r') {
							setInst5(key, doc, null, value.getText());
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
								myInst5Value1.setData(Boolean.TRUE);
								myInst5Value2.setData(Boolean.TRUE);
								check.setData(Boolean.TRUE);
								defaultLabel1.setData(Boolean.TRUE);
								defaultLabel2.setData(Boolean.TRUE);
							} else {
								myInst5Value1.setData(Boolean.FALSE);
								myInst5Value2.setData(Boolean.FALSE);
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
				((Inst3Fields)field).value = myValue1;
				((Inst3Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst4Fields) {
				((Inst4Fields)field).check = check;
				((Inst4Fields)field).defaultLabel = defaultLabel1;
			} else if (field instanceof Inst5Fields) {
				((Inst5Fields)field).check = check;
				((Inst5Fields)field).value1 = myInst5Value1;
				((Inst5Fields)field).defaultLabel1 = defaultLabel1;
				((Inst5Fields)field).value2 = myInst5Value2;
				((Inst5Fields)field).defaultLabel2 = defaultLabel2;
			} else if (field instanceof Inst6Fields) {
				((Inst6Fields)field).check = check;
				((Inst6Fields)field).value = myInst6Value1;
				((Inst6Fields)field).defaultLabel = defaultLabel1;
			}

			isRight = !isRight;
		}

		createSpacer(toolkit, isRight?rightColumn:leftColumn, 2);
	}
	
	private String getSelectItemname(Item item) {
		if (item instanceof SelectItemByName) {
			return ((SelectItemByName)item).getValue();
		} else if (item instanceof SelectItemById) {
			int id = ((SelectItemById)item).getValue();
			return Database.getItemName(id);
		}
		return null;
	}
	
	private String getSelectItemdescr(Item item) {
		if (item instanceof SelectItemByName) {
			return Database.getItemDescr(((SelectItemByName)item).getValue());
		} else if (item instanceof SelectItemById) {
			int id = ((SelectItemById)item).getValue();
			return Database.getItemDescr(Database.getItemName(id));
		}
		return "";
	}
	
	private int getSelectItemid(Item item) {
		if (item instanceof SelectItemByName) {
			ItemDB itemDB = Database.getItem(((SelectItemByName) item).getValue());
			return itemDB != null && itemDB.id != null ? itemDB.id.intValue() : 0;
		} else {
			return ((SelectItemById)item).getValue();
		}
	}
	
	public void update() {
		if (input != null) {
			String nameString = getInst1(Inst.NAME, (Item)input);
			
			String sprite = null;
			if (input instanceof SelectItemByName || input instanceof SelectItemById) {
				if (nameString != null) {
					name.setText(nameString);
					name.setEnabled(true);
					nameCheck.setSelection(true);
					nameCheck.setFont(boldFont);
				} else {
					name.setText(getSelectItemname((Item)input));
					name.setEnabled(false);
					nameCheck.setSelection(false);
					nameCheck.setFont(normalFont);
				}
				
				if (getInst3(Inst.COPYSPR, (Item)input) != null) {
					Object copyId = getInst3(Inst.COPYSPR, (Item)input);
					if (copyId instanceof Integer) {
						sprite = "item" + copyId + ".png";
					} else if (copyId instanceof String) {
						sprite = "item" + Database.getItem((String)copyId).id + ".png";
					}
				} else {
					int id = getSelectItemid((Item)input);
					if (id != 0) {
						sprite = "item" + id + ".png";
					}
				}
			} else {
				if (nameString != null) {
					name.setText(nameString);
					name.setEnabled(true);
					nameCheck.setSelection(true);
					nameCheck.setFont(boldFont);
				} else {
					String str = getItemname((Item)input);
					name.setText(str!=null?str:"");
					name.setEnabled(false);
					nameCheck.setSelection(false);
					nameCheck.setFont(normalFont);
				}
				nameCheck.setEnabled(false);
				
				if (getInst3(Inst.COPYSPR, (Item)input) != null) {
					Object copyId = getInst3(Inst.COPYSPR, (Item)input);
					if (copyId instanceof Integer) {
						sprite = "item" + copyId + ".png";
					} else if (copyId instanceof String) {
						sprite = "item" + Database.getItem((String)copyId).id + ".png";
					}
				}
			}
			
			if (sprite != null) {
				spriteLabel.setImage(getSpriteFromZip(sprite, "items"));
			} else {
				spriteLabel.setImage(null);
			}

			String description = getInst1(Inst.DESCR, (Item)input);
			final FormToolkit toolkit = mform.getToolkit();
			if (description != null) {
				descr.setText(description);
				descr.setEnabled(true);
				descr.setBackground(toolkit.getColors().getBackground());
				descrCheck.setSelection(true);
				descrCheck.setFont(boldFont);
			} else {
				descr.setText(getSelectItemdescr((Item)input));
				descr.setEnabled(false);
				descr.setBackground(toolkit.getColors().getInactiveBackground());
				descrCheck.setSelection(false);
				descrCheck.setFont(normalFont);
			}
			
		}
		ItemDB itemDB = new ItemDB();
		if (input instanceof SelectItemById) {
			itemDB = Database.getItem(((SelectItemById)input).getValue());
		} else if (input instanceof SelectItemByName) {
			itemDB = Database.getItem(((SelectItemByName)input).getValue());
		}
		Set<List<Inst>> dynamicFirstEmpty = new HashSet<List<Inst>>();
		for (Map.Entry<Inst, InstFields> fields : instMap.entrySet()) {
			String val1 = getInst1(fields.getKey(), (Item)input);
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
			Integer val = getInst2(fields.getKey(), (Item)input);
			if (val != null) {
				if (fields.getValue() instanceof Inst2Fields) {
					((Inst2Fields)fields.getValue()).value.setText(val.toString());
					((Inst2Fields)fields.getValue()).value.setEnabled(true);
					((Inst2Fields)fields.getValue()).check.setSelection(true);
					((Inst2Fields)fields.getValue()).check.setFont(boldFont);
				}
				if (fields.getValue() instanceof Inst6Fields) {
					if (fields.getKey() == Inst.MAINPATH) {
						((Inst6Fields)fields.getValue()).value.setItems(new String[]{
							"Fire",	"Air", "Water", "Earth", "Astral", "Death",
							"Nature", "Blood",
							},
							new int[]{0, 1, 2, 3, 4, 5, 6, 7});
					} else if (fields.getKey() == Inst.SECONDARYPATH) {
						((Inst6Fields)fields.getValue()).value.setItems(new String[]{
								"None", "Fire",	"Air", "Water", "Earth", "Astral", "Death",
								"Nature", "Blood",
								},
								new int[]{-1, 0, 1, 2, 3, 4, 5, 6, 7});
					} else if (fields.getKey() == Inst.TYPE) {
						((Inst6Fields)fields.getValue()).value.setItems(new String[]{
								"1-handed weapon", "2-handed weapon", "missile weapon", "shield", "body armor", "helmet", "boots", "misc item"},
								new int[]{1, 2, 3, 4, 5, 6, 7, 8});
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
				}
				if (fields.getValue() instanceof Inst6Fields) {
					((Inst6Fields)fields.getValue()).value.removeAll();
					((Inst6Fields)fields.getValue()).value.setEnabled(false);
					((Inst6Fields)fields.getValue()).check.setSelection(false);
					((Inst6Fields)fields.getValue()).check.setFont(normalFont);
				}
			}
			Object val3 = getInst3(fields.getKey(), (Item)input);
			if (val3 != null) {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value.setText(val3.toString());
					((Inst3Fields)fields.getValue()).value.setEnabled(true);
					((Inst3Fields)fields.getValue()).check.setSelection(true);
					((Inst3Fields)fields.getValue()).check.setFont(boldFont);
				}
			} else {
				if (fields.getValue() instanceof Inst3Fields) {
					((Inst3Fields)fields.getValue()).value.setText("");
					((Inst3Fields)fields.getValue()).value.setEnabled(false);
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
			Integer[] vals = getInst5(fields.getKey(), input);
			if (vals != null) {
				if (fields.getValue() instanceof Inst5Fields) {
					int selection = vals[0];
					((Inst5Fields)fields.getValue()).value1.setEnabled(true);
					setComboItems(fields.getKey(), ((Inst5Fields)fields.getValue()).value1);
					((Inst5Fields)fields.getValue()).value1.select(selection);
					((Inst5Fields)fields.getValue()).value2.setText(vals[1].toString());
					((Inst5Fields)fields.getValue()).value2.setEnabled(true);
					((Inst5Fields)fields.getValue()).check.setSelection(true);
					((Inst5Fields)fields.getValue()).check.setFont(boldFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value1.getData())) {
								((Inst5Fields)fields.getValue()).value1.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).value2.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
								((Inst5Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
							}
						}
					}
				}
			} else {
				if (fields.getValue() instanceof Inst5Fields) {
					((Inst5Fields)fields.getValue()).value1.setEnabled(true);
					((Inst5Fields)fields.getValue()).value1.removeAll();
					((Inst5Fields)fields.getValue()).value1.setEnabled(false);
					((Inst5Fields)fields.getValue()).value2.setText("");
					((Inst5Fields)fields.getValue()).value2.setEnabled(false);
					((Inst5Fields)fields.getValue()).check.setSelection(false);
					((Inst5Fields)fields.getValue()).check.setFont(normalFont);
					for (List<Inst> dynamic : dynamicFields) {
						if (dynamic.contains(fields.getKey())) {
							if (dynamicFirstEmpty.contains(dynamic)) {
								if (Boolean.TRUE.equals(((Inst5Fields)fields.getValue()).value1.getData())) {
									((Inst5Fields)fields.getValue()).value1.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).value2.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).check.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).defaultLabel1.setData(Boolean.FALSE);
									((Inst5Fields)fields.getValue()).defaultLabel2.setData(Boolean.FALSE);
								}
							} else {
								dynamicFirstEmpty.add(dynamic);
								if (Boolean.FALSE.equals(((Inst5Fields)fields.getValue()).value1.getData())) {
									((Inst5Fields)fields.getValue()).value1.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).value2.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).check.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).defaultLabel1.setData(Boolean.TRUE);
									((Inst5Fields)fields.getValue()).defaultLabel2.setData(Boolean.TRUE);
								}
							}
						}
					}
				}
			}

			if (input instanceof Item) {
				switch (fields.getKey()) {
				case ARMOR:
					if (itemDB.armor != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.armor));
						Inst.ARMOR.defaultValue = itemDB.armor;
					} else {
						((Inst1Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ARMOR.defaultValue = "";
					}
					break;
				case SPR:
					if (itemDB.spr != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.spr));
						Inst.SPR.defaultValue = itemDB.spr;
					} else {
						((Inst1Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPR.defaultValue = "";
					}
					break;
				case SPELL:
					if (itemDB.armor != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.spell));
						Inst.SPELL.defaultValue = itemDB.spell;
					} else {
						((Inst1Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPELL.defaultValue = "";
					}
					break;
				case AUTOSPELL:
					if (itemDB.autospell != null) {
						((Inst1Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autospell));
						Inst.AUTOSPELL.defaultValue = itemDB.autospell;
					} else {
						((Inst1Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTOSPELL.defaultValue = "";
					}
					break;
				
				case CONSTLEVEL:
					if (itemDB.constlevel != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.constlevel.toString()));
						Inst.CONSTLEVEL.defaultValue = itemDB.constlevel.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CONSTLEVEL.defaultValue = "";
					}
					break;
				case MAINLEVEL:
					if (itemDB.mainlevel != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.mainlevel.toString()));
						Inst.MAINLEVEL.defaultValue = itemDB.mainlevel.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAINLEVEL.defaultValue = "";
					}
					break;
				case SECONDARYLEVEL:
					if (itemDB.secondarylevel != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.secondarylevel.toString()));
						Inst.SECONDARYLEVEL.defaultValue = itemDB.secondarylevel.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SECONDARYLEVEL.defaultValue = "";
					}
					break;
				case STR:
					if (itemDB.str != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.str.toString()));
						Inst.STR.defaultValue = itemDB.str.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STR.defaultValue = "0";
					}
					break;
				case ATT:
					if (itemDB.att != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.att.toString()));
						Inst.ATT.defaultValue = itemDB.att.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ATT.defaultValue = "0";
					}
					break;
				case DEF:
					if (itemDB.def != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.def.toString()));
						Inst.DEF.defaultValue = itemDB.def.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEF.defaultValue = "0";
					}
					break;
				case PREC:
					if (itemDB.prec != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.prec.toString()));
						Inst.PREC.defaultValue = itemDB.prec.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PREC.defaultValue = "0";
					}
					break;
				case MR:
					if (itemDB.mr != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.mr.toString()));
						Inst.MR.defaultValue = itemDB.mr.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MR.defaultValue = "0";
					}
					break;
				case MORALE:
					if (itemDB.morale != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.morale.toString()));
						Inst.MORALE.defaultValue = itemDB.morale.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MORALE.defaultValue = "0";
					}
					break;
				case VOIDSANITY:
					if (itemDB.voidsanity != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.voidsanity.toString()));
						Inst.VOIDSANITY.defaultValue = itemDB.voidsanity.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.VOIDSANITY.defaultValue = "0";
					}
					break;
				case GIFTOFWATER:
					if (itemDB.giftofwater != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.giftofwater.toString()));
						Inst.GIFTOFWATER.defaultValue = itemDB.giftofwater.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GIFTOFWATER.defaultValue = "0";
					}
					break;
				case INVULNERABLE:
					if (itemDB.invulnerable != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.invulnerable.toString()));
						Inst.INVULNERABLE.defaultValue = itemDB.invulnerable.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INVULNERABLE.defaultValue = "0";
					}
					break;
				case INSPIRINGRES:
					if (itemDB.inspiringres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.inspiringres.toString()));
						Inst.INSPIRINGRES.defaultValue = itemDB.inspiringres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INSPIRINGRES.defaultValue = "0";
					}
					break;
				case FIRERES:
					if (itemDB.fireres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fireres.toString()));
						Inst.FIRERES.defaultValue = itemDB.fireres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRERES.defaultValue = "0";
					}
					break;
				case COLDRES:
					if (itemDB.coldres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.coldres.toString()));
						Inst.COLDRES.defaultValue = itemDB.coldres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLDRES.defaultValue = "0";
					}
					break;
				case SHOCKRES:
					if (itemDB.shockres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.shockres.toString()));
						Inst.SHOCKRES.defaultValue = itemDB.shockres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SHOCKRES.defaultValue = "0";
					}
					break;
				case POISONRES:
					if (itemDB.poisonres != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.poisonres.toString()));
						Inst.POISONRES.defaultValue = itemDB.poisonres.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POISONRES.defaultValue = "0";
					}
					break;
				case RESTRICTED:
					if (itemDB.restricted != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.restricted.toString()));
						Inst.RESTRICTED.defaultValue = itemDB.restricted.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RESTRICTED.defaultValue = "0";
					}
					break;
				case PEN:
					if (itemDB.pen != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.pen.toString()));
						Inst.PEN.defaultValue = itemDB.pen.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PEN.defaultValue = "0";
					}
					break;
				case AUTOSPELLREPEAT:
					if (itemDB.autospellrepeat != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autospellrepeat.toString()));
						Inst.AUTOSPELLREPEAT.defaultValue = itemDB.autospellrepeat.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTOSPELLREPEAT.defaultValue = "0";
					}
					break;
				case RANDOMSPELL:
					if (itemDB.randomspell != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.randomspell.toString()));
						Inst.RANDOMSPELL.defaultValue = itemDB.randomspell.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RANDOMSPELL.defaultValue = "0";
					}
					break;
				case MAPSPEED:
					if (itemDB.mapspeed != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.mapspeed.toString()));
						Inst.MAPSPEED.defaultValue = itemDB.mapspeed.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAPSPEED.defaultValue = "0";
					}
					break;
				case TAINTED:
					if (itemDB.tainted != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tainted.toString()));
						Inst.TAINTED.defaultValue = itemDB.tainted.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TAINTED.defaultValue = "0";
					}
					break;
				case SPECIALLOOK:
					if (itemDB.speciallook != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.speciallook.toString()));
						Inst.SPECIALLOOK.defaultValue = itemDB.speciallook.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPECIALLOOK.defaultValue = "0";
					}
					break;
				case SEDUCE:
					if (itemDB.seduce != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.seduce.toString()));
						Inst.SEDUCE.defaultValue = itemDB.seduce.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SEDUCE.defaultValue = "0";
					}
					break;
				case SUCCUBUS:
					if (itemDB.succubus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.succubus.toString()));
						Inst.SUCCUBUS.defaultValue = itemDB.succubus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUCCUBUS.defaultValue = "0";
					}
					break;
				case BECKON:
					if (itemDB.beckon != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.beckon.toString()));
						Inst.BECKON.defaultValue = itemDB.beckon.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BECKON.defaultValue = "0";
					}
					break;
				case FALSEARMY:
					if (itemDB.falsearmy != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.falsearmy.toString()));
						Inst.FALSEARMY.defaultValue = itemDB.falsearmy.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FALSEARMY.defaultValue = "0";
					}
					break;
				case FOOLSCOUTS:
					if (itemDB.foolscouts != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.foolscouts.toString()));
						Inst.FOOLSCOUTS.defaultValue = itemDB.foolscouts.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FOOLSCOUTS.defaultValue = "0";
					}
					break;
				case ICEPROT:
					if (itemDB.iceprot != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.iceprot.toString()));
						Inst.ICEPROT.defaultValue = itemDB.iceprot.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ICEPROT.defaultValue = "0";
					}
					break;
				case HEALER:
					if (itemDB.healer != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.healer.toString()));
						Inst.HEALER.defaultValue = itemDB.healer.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HEALER.defaultValue = "0";
					}
					break;
				case AUTOHEALER:
					if (itemDB.autohealer != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autohealer.toString()));
						Inst.AUTOHEALER.defaultValue = itemDB.autohealer.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTOHEALER.defaultValue = "0";
					}
					break;
				case AUTODISHEALER:
					if (itemDB.autodishealer != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autodishealer.toString()));
						Inst.AUTODISHEALER.defaultValue = itemDB.autodishealer.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTODISHEALER.defaultValue = "0";
					}
					break;
				case AUTODISGRINDER:
					if (itemDB.autodisgrinder != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autodisgrinder.toString()));
						Inst.AUTODISGRINDER.defaultValue = itemDB.autodisgrinder.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTODISGRINDER.defaultValue = "0";
					}
					break;
				case HOMESICK:
					if (itemDB.homesick != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.homesick.toString()));
						Inst.HOMESICK.defaultValue = itemDB.homesick.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HOMESICK.defaultValue = "0";
					}
					break;
				case UWDAMAGE:
					if (itemDB.uwdamage != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.uwdamage.toString()));
						Inst.UWDAMAGE.defaultValue = itemDB.uwdamage.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UWDAMAGE.defaultValue = "0";
					}
					break;
				case REGENERATION:
					if (itemDB.regeneration != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.regeneration.toString()));
						Inst.REGENERATION.defaultValue = itemDB.regeneration.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.REGENERATION.defaultValue = "0";
					}
					break;
				case REINVIGORATION:
					if (itemDB.reinvigoration != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.reinvigoration.toString()));
						Inst.REINVIGORATION.defaultValue = itemDB.reinvigoration.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.REINVIGORATION.defaultValue = "0";
					}
					break;
				case WOUNDFEND:
					if (itemDB.woundfend != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.woundfend.toString()));
						Inst.WOUNDFEND.defaultValue = itemDB.woundfend.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WOUNDFEND.defaultValue = "0";
					}
					break;
				case POISONCLOUD:
					if (itemDB.poisoncloud != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.poisoncloud.toString()));
						Inst.POISONCLOUD.defaultValue = itemDB.poisoncloud.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POISONCLOUD.defaultValue = "0";
					}
					break;
				case DISEASECLOUD:
					if (itemDB.diseasecloud != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.diseasecloud.toString()));
						Inst.DISEASECLOUD.defaultValue = itemDB.diseasecloud.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DISEASECLOUD.defaultValue = "0";
					}
					break;
				case ANIMALAWE:
					if (itemDB.animalawe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.animalawe.toString()));
						Inst.ANIMALAWE.defaultValue = itemDB.animalawe.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ANIMALAWE.defaultValue = "0";
					}
					break;
				case AWE:
					if (itemDB.awe != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.awe.toString()));
						Inst.AWE.defaultValue = itemDB.awe.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AWE.defaultValue = "0";
					}
					break;
				case FEAR:
					if (itemDB.fear != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fear.toString()));
						Inst.FEAR.defaultValue = itemDB.fear.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FEAR.defaultValue = "0";
					}
					break;
				case FIRESHIELD:
					if (itemDB.fireshield != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fireshield.toString()));
						Inst.FIRESHIELD.defaultValue = itemDB.fireshield.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRESHIELD.defaultValue = "0";
					}
					break;
				case BANEFIRESHIELD:
					if (itemDB.banefireshield != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.banefireshield.toString()));
						Inst.BANEFIRESHIELD.defaultValue = itemDB.banefireshield.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BANEFIRESHIELD.defaultValue = "0";
					}
					break;
				case DAMAGEREV:
					if (itemDB.damagerev != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.damagerev.toString()));
						Inst.DAMAGEREV.defaultValue = itemDB.damagerev.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DAMAGEREV.defaultValue = "0";
					}
					break;
				case BLOODVENGEANCE:
					if (itemDB.bloodvengeance != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bloodvengeance.toString()));
						Inst.BLOODVENGEANCE.defaultValue = itemDB.bloodvengeance.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLOODVENGEANCE.defaultValue = "0";
					}
					break;
				case SLIMER:
					if (itemDB.slimer != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.slimer.toString()));
						Inst.SLIMER.defaultValue = itemDB.slimer.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SLIMER.defaultValue = "0";
					}
					break;
				case DEATHDISEASE:
					if (itemDB.deathdisease != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathdisease.toString()));
						Inst.DEATHDISEASE.defaultValue = itemDB.deathdisease.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHDISEASE.defaultValue = "0";
					}
					break;
				case DEATHPARALYZE:
					if (itemDB.deathparalyze != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathparalyze.toString()));
						Inst.DEATHPARALYZE.defaultValue = itemDB.deathparalyze.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHPARALYZE.defaultValue = "0";
					}
					break;
				case DEATHFIRE:
					if (itemDB.deathfire != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathfire.toString()));
						Inst.DEATHFIRE.defaultValue = itemDB.deathfire.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHFIRE.defaultValue = "0";
					}
					break;
				case CHAOSPOWER:
					if (itemDB.chaospower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.chaospower.toString()));
						Inst.CHAOSPOWER.defaultValue = itemDB.chaospower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CHAOSPOWER.defaultValue = "0";
					}
					break;
				case FIREPOWER:
					if (itemDB.firepower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.firepower.toString()));
						Inst.FIREPOWER.defaultValue = itemDB.firepower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIREPOWER.defaultValue = "0";
					}
					break;
				case COLDPOWER:
					if (itemDB.coldpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.coldpower.toString()));
						Inst.COLDPOWER.defaultValue = itemDB.coldpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COLDPOWER.defaultValue = "0";
					}
					break;
				case MAGICPOWER:
					if (itemDB.magicpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.magicpower.toString()));
						Inst.MAGICPOWER.defaultValue = itemDB.magicpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAGICPOWER.defaultValue = "0";
					}
					break;
				case STORMPOWER:
					if (itemDB.stormpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.stormpower.toString()));
						Inst.STORMPOWER.defaultValue = itemDB.stormpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STORMPOWER.defaultValue = "0";
					}
					break;
				case DARKPOWER:
					if (itemDB.darkpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.darkpower.toString()));
						Inst.DARKPOWER.defaultValue = itemDB.darkpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DARKPOWER.defaultValue = "0";
					}
					break;
				case SPRINGPOWER:
					if (itemDB.springpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.springpower.toString()));
						Inst.SPRINGPOWER.defaultValue = itemDB.springpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPRINGPOWER.defaultValue = "0";
					}
					break;
				case SUMMERPOWER:
					if (itemDB.summerpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summerpower.toString()));
						Inst.SUMMERPOWER.defaultValue = itemDB.summerpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMERPOWER.defaultValue = "0";
					}
					break;
				case FALLPOWER:
					if (itemDB.fallpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fallpower.toString()));
						Inst.FALLPOWER.defaultValue = itemDB.fallpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FALLPOWER.defaultValue = "0";
					}
					break;
				case WINTERPOWER:
					if (itemDB.winterpower != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.winterpower.toString()));
						Inst.WINTERPOWER.defaultValue = itemDB.winterpower.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WINTERPOWER.defaultValue = "0";
					}
					break;
				case AMBIDEXTROUS:
					if (itemDB.ambidextrous != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.ambidextrous.toString()));
						Inst.AMBIDEXTROUS.defaultValue = itemDB.ambidextrous.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AMBIDEXTROUS.defaultValue = "0";
					}
					break;
				case BERSERK:
					if (itemDB.berserk != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.berserk.toString()));
						Inst.BERSERK.defaultValue = itemDB.berserk.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BERSERK.defaultValue = "0";
					}
					break;
				case DARKVISION:
					if (itemDB.darkvision != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.darkvision.toString()));
						Inst.DARKVISION.defaultValue = itemDB.darkvision.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DARKVISION.defaultValue = "0";
					}
					break;
				case DIGEST:
					if (itemDB.digest != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.digest.toString()));
						Inst.DIGEST.defaultValue = itemDB.digest.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DIGEST.defaultValue = "0";
					}
					break;
				case INCORPORATE:
					if (itemDB.incorporate != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.incorporate.toString()));
						Inst.INCORPORATE.defaultValue = itemDB.incorporate.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCORPORATE.defaultValue = "0";
					}
					break;
				case CASTLEDEF:
					if (itemDB.castledef != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.castledef.toString()));
						Inst.CASTLEDEF.defaultValue = itemDB.castledef.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CASTLEDEF.defaultValue = "0";
					}
					break;
				case SIEGEBONUS:
					if (itemDB.siegebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.siegebonus.toString()));
						Inst.SIEGEBONUS.defaultValue = itemDB.siegebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SIEGEBONUS.defaultValue = "0";
					}
					break;
				case PATROLBONUS:
					if (itemDB.patrolbonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.patrolbonus.toString()));
						Inst.PATROLBONUS.defaultValue = itemDB.patrolbonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PATROLBONUS.defaultValue = "0";
					}
					break;
				case PILLAGEBONUS:
					if (itemDB.pillagebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.pillagebonus.toString()));
						Inst.PILLAGEBONUS.defaultValue = itemDB.pillagebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PILLAGEBONUS.defaultValue = "0";
					}
					break;
				case SUPPLYBONUS:
					if (itemDB.supplybonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.supplybonus.toString()));
						Inst.SUPPLYBONUS.defaultValue = itemDB.supplybonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUPPLYBONUS.defaultValue = "0";
					}
					break;
				case NOBADEVENTS:
					if (itemDB.nobadevents != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nobadevents.toString()));
						Inst.NOBADEVENTS.defaultValue = itemDB.nobadevents.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOBADEVENTS.defaultValue = "0";
					}
					break;
				case INCPROVDEF:
					if (itemDB.incprovdef != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.incprovdef.toString()));
						Inst.INCPROVDEF.defaultValue = itemDB.incprovdef.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCPROVDEF.defaultValue = "0";
					}
					break;
				case INCUNREST:
					if (itemDB.incunrest != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.incunrest.toString()));
						Inst.INCUNREST.defaultValue = itemDB.incunrest.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INCUNREST.defaultValue = "0";
					}
					break;
				case LEPER:
					if (itemDB.leper != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.leper.toString()));
						Inst.LEPER.defaultValue = itemDB.leper.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.LEPER.defaultValue = "0";
					}
					break;
				case POPKILL:
					if (itemDB.popkill != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.popkill.toString()));
						Inst.POPKILL.defaultValue = itemDB.popkill.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.POPKILL.defaultValue = "0";
					}
					break;
				case HERETIC:
					if (itemDB.heretic != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.heretic.toString()));
						Inst.HERETIC.defaultValue = itemDB.heretic.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HERETIC.defaultValue = "0";
					}
					break;
				case ELEGIST:
					if (itemDB.elegist != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.elegist.toString()));
						Inst.ELEGIST.defaultValue = itemDB.elegist.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ELEGIST.defaultValue = "0";
					}
					break;
				case SPREADDOM:
					if (itemDB.spreaddom != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.spreaddom.toString()));
						Inst.SPREADDOM.defaultValue = itemDB.spreaddom.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SPREADDOM.defaultValue = "0";
					}
					break;
				case SHATTEREDSOUL:
					if (itemDB.shatteredsoul != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.shatteredsoul.toString()));
						Inst.SHATTEREDSOUL.defaultValue = itemDB.shatteredsoul.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SHATTEREDSOUL.defaultValue = "0";
					}
					break;
				case GOLD:
					if (itemDB.gold != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.gold.toString()));
						Inst.GOLD.defaultValue = itemDB.gold.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.GOLD.defaultValue = "0";
					}
					break;
				case INSPIRATIONAL:
					if (itemDB.inspirational != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.inspirational.toString()));
						Inst.INSPIRATIONAL.defaultValue = itemDB.inspirational.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INSPIRATIONAL.defaultValue = "0";
					}
					break;
				case BEASTMASTER:
					if (itemDB.beastmaster != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.beastmaster.toString()));
						Inst.BEASTMASTER.defaultValue = itemDB.beastmaster.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BEASTMASTER.defaultValue = "0";
					}
					break;
				case TASKMASTER:
					if (itemDB.taskmaster != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.taskmaster.toString()));
						Inst.TASKMASTER.defaultValue = itemDB.taskmaster.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TASKMASTER.defaultValue = "0";
					}
					break;
				case FORMATIONFIGHTER:
					if (itemDB.formationfighter != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.formationfighter.toString()));
						Inst.FORMATIONFIGHTER.defaultValue = itemDB.formationfighter.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FORMATIONFIGHTER.defaultValue = "0";
					}
					break;
				case BODYGUARD:
					if (itemDB.bodyguard != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bodyguard.toString()));
						Inst.BODYGUARD.defaultValue = itemDB.bodyguard.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BODYGUARD.defaultValue = "0";
					}
					break;
				case STANDARD:
					if (itemDB.standard != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.standard.toString()));
						Inst.STANDARD.defaultValue = itemDB.standard.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STANDARD.defaultValue = "0";
					}
					break;
				case DOUSE:
					if (itemDB.douse != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.douse.toString()));
						Inst.DOUSE.defaultValue = itemDB.douse.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOUSE.defaultValue = "0";
					}
					break;
				case RESEARCHBONUS:
					if (itemDB.researchbonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.researchbonus.toString()));
						Inst.RESEARCHBONUS.defaultValue = itemDB.researchbonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RESEARCHBONUS.defaultValue = "0";
					}
					break;
				case DIVINEINS:
					if (itemDB.divineins != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.divineins.toString()));
						Inst.DIVINEINS.defaultValue = itemDB.divineins.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DIVINEINS.defaultValue = "0";
					}
					break;
				case FORGEBONUS:
					if (itemDB.forgebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.forgebonus.toString()));
						Inst.FORGEBONUS.defaultValue = itemDB.forgebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FORGEBONUS.defaultValue = "0";
					}
					break;
				case FIXFORGEBONUS:
					if (itemDB.fixforgebonus != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fixforgebonus.toString()));
						Inst.FIXFORGEBONUS.defaultValue = itemDB.fixforgebonus.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIXFORGEBONUS.defaultValue = "0";
					}
					break;
				case CROSSBREEDER:
					if (itemDB.crossbreeder != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.crossbreeder.toString()));
						Inst.CROSSBREEDER.defaultValue = itemDB.crossbreeder.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CROSSBREEDER.defaultValue = "0";
					}
					break;
				case BONUSSPELLS:
					if (itemDB.bonusspells != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bonusspells.toString()));
						Inst.BONUSSPELLS.defaultValue = itemDB.bonusspells.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BONUSSPELLS.defaultValue = "0";
					}
					break;
				case DEATHBANISH:
					if (itemDB.deathbanish != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathbanish.toString()));
						Inst.DEATHBANISH.defaultValue = itemDB.deathbanish.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHBANISH.defaultValue = "0";
					}
					break;
				case KOKYTOSRET:
					if (itemDB.kokytosret != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.kokytosret.toString()));
						Inst.KOKYTOSRET.defaultValue = itemDB.kokytosret.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.KOKYTOSRET.defaultValue = "0";
					}
					break;
				case INFERNORET:
					if (itemDB.infernoret != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.infernoret.toString()));
						Inst.INFERNORET.defaultValue = itemDB.infernoret.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INFERNORET.defaultValue = "0";
					}
					break;
				case VOIDRET:
					if (itemDB.voidret != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.voidret.toString()));
						Inst.VOIDRET.defaultValue = itemDB.voidret.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.VOIDRET.defaultValue = "0";
					}
					break;
				case ALLRET:
					if (itemDB.allret != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.allret.toString()));
						Inst.ALLRET.defaultValue = itemDB.allret.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ALLRET.defaultValue = "0";
					}
					break;
				case FIRERANGE:
					if (itemDB.firerange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.firerange.toString()));
						Inst.FIRERANGE.defaultValue = itemDB.firerange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FIRERANGE.defaultValue = "0";
					}
					break;
				case AIRRANGE:
					if (itemDB.airrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.airrange.toString()));
						Inst.AIRRANGE.defaultValue = itemDB.airrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AIRRANGE.defaultValue = "0";
					}
					break;
				case WATERRANGE:
					if (itemDB.waterrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.waterrange.toString()));
						Inst.WATERRANGE.defaultValue = itemDB.waterrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WATERRANGE.defaultValue = "0";
					}
					break;
				case EARTHRANGE:
					if (itemDB.earthrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.earthrange.toString()));
						Inst.EARTHRANGE.defaultValue = itemDB.earthrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EARTHRANGE.defaultValue = "0";
					}
					break;
				case ASTRALRANGE:
					if (itemDB.astralrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.astralrange.toString()));
						Inst.ASTRALRANGE.defaultValue = itemDB.astralrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ASTRALRANGE.defaultValue = "0";
					}
					break;
				case DEATHRANGE:
					if (itemDB.deathrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathrange.toString()));
						Inst.DEATHRANGE.defaultValue = itemDB.deathrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHRANGE.defaultValue = "0";
					}
					break;
				case NATURERANGE:
					if (itemDB.naturerange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.naturerange.toString()));
						Inst.NATURERANGE.defaultValue = itemDB.naturerange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NATURERANGE.defaultValue = "0";
					}
					break;
				case BLOODRANGE:
					if (itemDB.bloodrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bloodrange.toString()));
						Inst.BLOODRANGE.defaultValue = itemDB.bloodrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLOODRANGE.defaultValue = "0";
					}
					break;
				case ELEMENTRANGE:
					if (itemDB.elementrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.elementrange.toString()));
						Inst.ELEMENTRANGE.defaultValue = itemDB.elementrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ELEMENTRANGE.defaultValue = "0";
					}
					break;
				case SORCERYRANGE:
					if (itemDB.sorceryrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.sorceryrange.toString()));
						Inst.SORCERYRANGE.defaultValue = itemDB.sorceryrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SORCERYRANGE.defaultValue = "0";
					}
					break;
				case ALLRANGE:
					if (itemDB.allrange != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.allrange.toString()));
						Inst.ALLRANGE.defaultValue = itemDB.allrange.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ALLRANGE.defaultValue = "0";
					}
					break;
				case MAKEPEARLS:
					if (itemDB.makepearls != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makepearls.toString()));
						Inst.MAKEPEARLS.defaultValue = itemDB.makepearls.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEPEARLS.defaultValue = "0";
					}
					break;
				case TMPFIREGEMS:
					if (itemDB.tmpfiregems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpfiregems.toString()));
						Inst.TMPFIREGEMS.defaultValue = itemDB.tmpfiregems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPFIREGEMS.defaultValue = "0";
					}
					break;
				case TMPAIRGEMS:
					if (itemDB.tmpairgems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpairgems.toString()));
						Inst.TMPAIRGEMS.defaultValue = itemDB.tmpairgems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPAIRGEMS.defaultValue = "0";
					}
					break;
				case TMPWATERGEMS:
					if (itemDB.tmpwatergems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpwatergems.toString()));
						Inst.TMPWATERGEMS.defaultValue = itemDB.tmpwatergems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPWATERGEMS.defaultValue = "0";
					}
					break;
				case TMPEARTHGEMS:
					if (itemDB.tmpearthgems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpearthgems.toString()));
						Inst.TMPEARTHGEMS.defaultValue = itemDB.tmpearthgems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPEARTHGEMS.defaultValue = "0";
					}
					break;
				case TMPASTRALGEMS:
					if (itemDB.tmpastralgems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpastralgems.toString()));
						Inst.TMPASTRALGEMS.defaultValue = itemDB.tmpastralgems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPASTRALGEMS.defaultValue = "0";
					}
					break;
				case TMPDEATHGEMS:
					if (itemDB.tmpdeathgems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpdeathgems.toString()));
						Inst.TMPDEATHGEMS.defaultValue = itemDB.tmpdeathgems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPDEATHGEMS.defaultValue = "0";
					}
					break;
				case TMPNATUREGEMS:
					if (itemDB.tmpnaturegems != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpnaturegems.toString()));
						Inst.TMPNATUREGEMS.defaultValue = itemDB.tmpnaturegems.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPNATUREGEMS.defaultValue = "0";
					}
					break;
				case TMPBLOODSLAVES:
					if (itemDB.tmpbloodslaves != null) {
						((Inst2Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.tmpbloodslaves.toString()));
						Inst.TMPBLOODSLAVES.defaultValue = itemDB.tmpbloodslaves.toString();
					} else {
						((Inst2Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TMPBLOODSLAVES.defaultValue = "0";
					}
					break;

				case WEAPON:
					if (itemDB.weapon != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.weapon.toString()));
						Inst.WEAPON.defaultValue = itemDB.weapon.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WEAPON.defaultValue = "";
					}
					break;
				case COPYITEM:
					if (itemDB.copyitem != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.copyitem.toString()));
						Inst.COPYITEM.defaultValue = itemDB.copyitem.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COPYITEM.defaultValue = "";
					}
					break;
				case DOMSUMMON:
					if (itemDB.domsummon != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.domsummon.toString()));
						Inst.DOMSUMMON.defaultValue = itemDB.domsummon.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON.defaultValue = "";
					}
					break;
				case DOMSUMMON2:
					if (itemDB.domsummon2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.domsummon2.toString()));
						Inst.DOMSUMMON2.defaultValue = itemDB.domsummon2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON2.defaultValue = "";
					}
					break;
				case DOMSUMMON20:
					if (itemDB.domsummon20 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.domsummon20.toString()));
						Inst.DOMSUMMON20.defaultValue = itemDB.domsummon20.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DOMSUMMON20.defaultValue = "";
					}
					break;
				case RAREDOMSUMMON:
					if (itemDB.raredomsummon != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.raredomsummon.toString()));
						Inst.RAREDOMSUMMON.defaultValue = itemDB.raredomsummon.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RAREDOMSUMMON.defaultValue = "";
					}
					break;
				case SUMMON1:
					if (itemDB.summon1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summon1.toString()));
						Inst.SUMMON1.defaultValue = itemDB.summon1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON1.defaultValue = "";
					}
					break;
				case SUMMON2:
					if (itemDB.summon2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summon2.toString()));
						Inst.SUMMON2.defaultValue = itemDB.summon2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON2.defaultValue = "";
					}
					break;
				case SUMMON3:
					if (itemDB.summon3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summon3.toString()));
						Inst.SUMMON3.defaultValue = itemDB.summon3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON3.defaultValue = "";
					}
					break;
				case SUMMON4:
					if (itemDB.summon4 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summon4.toString()));
						Inst.SUMMON4.defaultValue = itemDB.summon4.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON4.defaultValue = "";
					}
					break;
				case SUMMON5:
					if (itemDB.summon5 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.summon5.toString()));
						Inst.SUMMON5.defaultValue = itemDB.summon5.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SUMMON5.defaultValue = "";
					}
					break;
				case MAKEMONSTERS1:
					if (itemDB.makemonsters1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makemonsters1.toString()));
						Inst.MAKEMONSTERS1.defaultValue = itemDB.makemonsters1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTERS1.defaultValue = "";
					}
					break;
				case MAKEMONSTERS2:
					if (itemDB.makemonsters2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makemonsters2.toString()));
						Inst.MAKEMONSTERS2.defaultValue = itemDB.makemonsters2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTERS2.defaultValue = "";
					}
					break;
				case MAKEMONSTERS3:
					if (itemDB.makemonsters3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makemonsters3.toString()));
						Inst.MAKEMONSTERS3.defaultValue = itemDB.makemonsters3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTERS3.defaultValue = "";
					}
					break;
				case MAKEMONSTERS4:
					if (itemDB.makemonsters4 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makemonsters4.toString()));
						Inst.MAKEMONSTERS4.defaultValue = itemDB.makemonsters4.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTERS4.defaultValue = "";
					}
					break;
				case MAKEMONSTERS5:
					if (itemDB.makemonsters5 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.makemonsters5.toString()));
						Inst.MAKEMONSTERS5.defaultValue = itemDB.makemonsters5.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAKEMONSTERS5.defaultValue = "";
					}
					break;
				case BATTLESUM1:
					if (itemDB.battlesum1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.battlesum1.toString()));
						Inst.BATTLESUM1.defaultValue = itemDB.battlesum1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATTLESUM1.defaultValue = "";
					}
					break;
				case BATTLESUM2:
					if (itemDB.battlesum2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.battlesum2.toString()));
						Inst.BATTLESUM2.defaultValue = itemDB.battlesum2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATTLESUM2.defaultValue = "";
					}
					break;
				case BATTLESUM3:
					if (itemDB.battlesum3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.battlesum3.toString()));
						Inst.BATTLESUM3.defaultValue = itemDB.battlesum3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATTLESUM3.defaultValue = "";
					}
					break;
				case BATTLESUM4:
					if (itemDB.battlesum4 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.battlesum4.toString()));
						Inst.BATTLESUM4.defaultValue = itemDB.battlesum4.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATTLESUM4.defaultValue = "";
					}
					break;
				case BATTLESUM5:
					if (itemDB.battlesum5 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.battlesum5.toString()));
						Inst.BATTLESUM5.defaultValue = itemDB.battlesum5.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATTLESUM5.defaultValue = "";
					}
					break;
				case BATSTARTSUM1:
					if (itemDB.batstartsum1 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum1.toString()));
						Inst.BATSTARTSUM1.defaultValue = itemDB.batstartsum1.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM1.defaultValue = "";
					}
					break;
				case BATSTARTSUM2:
					if (itemDB.batstartsum2 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum2.toString()));
						Inst.BATSTARTSUM2.defaultValue = itemDB.batstartsum2.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM2.defaultValue = "";
					}
					break;
				case BATSTARTSUM3:
					if (itemDB.batstartsum3 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum3.toString()));
						Inst.BATSTARTSUM3.defaultValue = itemDB.batstartsum3.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM3.defaultValue = "";
					}
					break;
				case BATSTARTSUM4:
					if (itemDB.batstartsum4 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum4.toString()));
						Inst.BATSTARTSUM4.defaultValue = itemDB.batstartsum4.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM4.defaultValue = "";
					}
					break;
				case BATSTARTSUM5:
					if (itemDB.batstartsum5 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum5.toString()));
						Inst.BATSTARTSUM5.defaultValue = itemDB.batstartsum5.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM5.defaultValue = "";
					}
					break;
				case BATSTARTSUM1D6:
					if (itemDB.batstartsum1d6 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum1d6.toString()));
						Inst.BATSTARTSUM1D6.defaultValue = itemDB.batstartsum1d6.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM1D6.defaultValue = "";
					}
					break;
				case BATSTARTSUM2D6:
					if (itemDB.batstartsum2d6 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum2d6.toString()));
						Inst.BATSTARTSUM2D6.defaultValue = itemDB.batstartsum2d6.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM2D6.defaultValue = "";
					}
					break;
				case BATSTARTSUM3D6:
					if (itemDB.batstartsum3d6 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum3d6.toString()));
						Inst.BATSTARTSUM3D6.defaultValue = itemDB.batstartsum3d6.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM3D6.defaultValue = "";
					}
					break;
				case BATSTARTSUM4D6:
					if (itemDB.batstartsum4d6 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum4d6.toString()));
						Inst.BATSTARTSUM4D6.defaultValue = itemDB.batstartsum4d6.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM4D6.defaultValue = "";
					}
					break;
				case BATSTARTSUM5D6:
					if (itemDB.batstartsum5d6 != null) {
						((Inst3Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.batstartsum5d6.toString()));
						Inst.BATSTARTSUM5D6.defaultValue = itemDB.batstartsum5d6.toString();
					} else {
						((Inst3Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BATSTARTSUM5D6.defaultValue = "";
					}
					break;

				case ETHEREAL:
					if (itemDB.ethereal != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.ethereal));
						Inst.ETHEREAL.defaultValue = itemDB.ethereal.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ETHEREAL.defaultValue = "";
					}
					break;
				case NOMOUNTED:
					if (itemDB.nomounted != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nomounted));
						Inst.NOMOUNTED.defaultValue = itemDB.nomounted.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOMOUNTED.defaultValue = "";
					}
					break;
				case NOCOLDBLOOD:
					if (itemDB.nocoldblood != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nocoldblood));
						Inst.NOCOLDBLOOD.defaultValue = itemDB.nocoldblood.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOCOLDBLOOD.defaultValue = "";
					}
					break;
				case NODEMON:
					if (itemDB.nodemon != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nodemon));
						Inst.NODEMON.defaultValue = itemDB.nodemon.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NODEMON.defaultValue = "";
					}
					break;
				case NOUNDEAD:
					if (itemDB.noundead != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.noundead));
						Inst.NOUNDEAD.defaultValue = itemDB.noundead.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOUNDEAD.defaultValue = "";
					}
					break;
				case NOINANIM:
					if (itemDB.noinanim != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.noinanim));
						Inst.NOINANIM.defaultValue = itemDB.noinanim.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOINANIM.defaultValue = "";
					}
					break;
				case NOFEMALE:
					if (itemDB.nofemale != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nofemale));
						Inst.NOFEMALE.defaultValue = itemDB.nofemale.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOFEMALE.defaultValue = "";
					}
					break;
				case ONLYMOUNTED:
					if (itemDB.onlymounted != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlymounted));
						Inst.ONLYMOUNTED.defaultValue = itemDB.onlymounted.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYMOUNTED.defaultValue = "";
					}
					break;
				case ONLYCOLDBLOOD:
					if (itemDB.onlycoldblood != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlycoldblood));
						Inst.ONLYCOLDBLOOD.defaultValue = itemDB.onlycoldblood.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYCOLDBLOOD.defaultValue = "";
					}
					break;
				case ONLYDEMON:
					if (itemDB.onlydemon != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlydemon));
						Inst.ONLYDEMON.defaultValue = itemDB.onlydemon.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYDEMON.defaultValue = "";
					}
					break;
				case ONLYUNDEAD:
					if (itemDB.onlyundead != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlyundead));
						Inst.ONLYUNDEAD.defaultValue = itemDB.onlyundead.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYUNDEAD.defaultValue = "";
					}
					break;
				case ONLYINANIM:
					if (itemDB.onlyinanim != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlyinanim));
						Inst.ONLYINANIM.defaultValue = itemDB.onlyinanim.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYINANIM.defaultValue = "";
					}
					break;
				case ONLYFEMALE:
					if (itemDB.onlyfemale != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.onlyfemale));
						Inst.ONLYFEMALE.defaultValue = itemDB.onlyfemale.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.ONLYFEMALE.defaultValue = "";
					}
					break;
				case REQEYES:
					if (itemDB.reqeyes != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.reqeyes));
						Inst.REQEYES.defaultValue = itemDB.reqeyes.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.REQEYES.defaultValue = "";
					}
					break;
				case NOFIND:
					if (itemDB.nofind != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nofind));
						Inst.NOFIND.defaultValue = itemDB.nofind.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NOFIND.defaultValue = "";
					}
					break;
				case LUCK:
					if (itemDB.luck != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.luck));
						Inst.LUCK.defaultValue = itemDB.luck.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.LUCK.defaultValue = "";
					}
					break;
				case QUICKNESS:
					if (itemDB.quickness != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.quickness));
						Inst.QUICKNESS.defaultValue = itemDB.quickness.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.QUICKNESS.defaultValue = "";
					}
					break;
				case BLESS:
					if (itemDB.bless != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bless));
						Inst.BLESS.defaultValue = itemDB.bless.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLESS.defaultValue = "";
					}
					break;
				case BARKSKIN:
					if (itemDB.barkskin != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.barkskin));
						Inst.BARKSKIN.defaultValue = itemDB.barkskin.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BARKSKIN.defaultValue = "";
					}
					break;
				case STONESKIN:
					if (itemDB.stoneskin != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.stoneskin));
						Inst.STONESKIN.defaultValue = itemDB.stoneskin.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STONESKIN.defaultValue = "";
					}
					break;
				case IRONSKIN:
					if (itemDB.ironskin != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.ironskin));
						Inst.IRONSKIN.defaultValue = itemDB.ironskin.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.IRONSKIN.defaultValue = "";
					}
					break;
				case WATERBREATHING:
					if (itemDB.waterbreathing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.waterbreathing));
						Inst.WATERBREATHING.defaultValue = itemDB.waterbreathing.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.WATERBREATHING.defaultValue = "";
					}
					break;
				case FLOAT:
					if (itemDB.floatBoolean != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.floatBoolean));
						Inst.FLOAT.defaultValue = itemDB.floatBoolean.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FLOAT.defaultValue = "";
					}
					break;
				case FLY:
					if (itemDB.fly != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.fly));
						Inst.FLY.defaultValue = itemDB.fly.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FLY.defaultValue = "";
					}
					break;
				case STORMIMMUNE:
					if (itemDB.stormimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.stormimmune));
						Inst.STORMIMMUNE.defaultValue = itemDB.stormimmune.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STORMIMMUNE.defaultValue = "";
					}
					break;
				case RUN:
					if (itemDB.run != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.run));
						Inst.RUN.defaultValue = itemDB.run.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.RUN.defaultValue = "";
					}
					break;
				case TRAMPLE:
					if (itemDB.trample != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.trample));
						Inst.TRAMPLE.defaultValue = itemDB.trample.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TRAMPLE.defaultValue = "";
					}
					break;
				case BERS:
					if (itemDB.bers != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bers));
						Inst.BERS.defaultValue = itemDB.bers.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BERS.defaultValue = "";
					}
					break;
				case EXTRALIFE:
					if (itemDB.extralife != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.extralife));
						Inst.EXTRALIFE.defaultValue = itemDB.extralife.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.EXTRALIFE.defaultValue = "";
					}
					break;
				case CHAMPPRIZE:
					if (itemDB.champprize != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.champprize));
						Inst.CHAMPPRIZE.defaultValue = itemDB.champprize.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CHAMPPRIZE.defaultValue = "";
					}
					break;
				case AUTOCOMPETE:
					if (itemDB.autocompete != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.autocompete));
						Inst.AUTOCOMPETE.defaultValue = itemDB.autocompete.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.AUTOCOMPETE.defaultValue = "";
					}
					break;
				case CURSED:
					if (itemDB.cursed != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.cursed));
						Inst.CURSED.defaultValue = itemDB.cursed.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CURSED.defaultValue = "";
					}
					break;
				case CURSE:
					if (itemDB.curse != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.curse));
						Inst.CURSE.defaultValue = itemDB.curse.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CURSE.defaultValue = "";
					}
					break;
				case DISEASE:
					if (itemDB.disease != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.disease));
						Inst.DISEASE.defaultValue = itemDB.disease.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DISEASE.defaultValue = "";
					}
					break;
				case CHESTWOUND:
					if (itemDB.chestwound != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.chestwound));
						Inst.CHESTWOUND.defaultValue = itemDB.chestwound.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CHESTWOUND.defaultValue = "";
					}
					break;
				case FEEBLEMIND:
					if (itemDB.feeblemind != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.feeblemind));
						Inst.FEEBLEMIND.defaultValue = itemDB.feeblemind.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.FEEBLEMIND.defaultValue = "";
					}
					break;
				case MUTE:
					if (itemDB.mute != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.mute));
						Inst.MUTE.defaultValue = itemDB.mute.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MUTE.defaultValue = "";
					}
					break;
				case NHWOUND:
					if (itemDB.nhwound != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.nhwound));
						Inst.NHWOUND.defaultValue = itemDB.nhwound.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NHWOUND.defaultValue = "";
					}
					break;
				case CRIPPLED:
					if (itemDB.crippled != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.crippled));
						Inst.CRIPPLED.defaultValue = itemDB.crippled.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CRIPPLED.defaultValue = "";
					}
					break;
				case LOSEEYE:
					if (itemDB.loseeye != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.loseeye));
						Inst.LOSEEYE.defaultValue = itemDB.loseeye.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.LOSEEYE.defaultValue = "";
					}
					break;
				case SINGLEBATTLE:
					if (itemDB.singlebattle != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.singlebattle));
						Inst.SINGLEBATTLE.defaultValue = itemDB.singlebattle.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SINGLEBATTLE.defaultValue = "";
					}
					break;
				case CHAOSREC:
					if (itemDB.chaosrec != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.chaosrec));
						Inst.CHAOSREC.defaultValue = itemDB.chaosrec.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.CHAOSREC.defaultValue = "";
					}
					break;
				case STONEBEING:
					if (itemDB.stonebeing != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.stonebeing));
						Inst.STONEBEING.defaultValue = itemDB.stonebeing.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.STONEBEING.defaultValue = "";
					}
					break;
				case NORIVERPASS:
					if (itemDB.noriverpass != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.noriverpass));
						Inst.NORIVERPASS.defaultValue = itemDB.noriverpass.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.NORIVERPASS.defaultValue = "";
					}
					break;
				case UNTELEPORTABLE:
					if (itemDB.unteleportable != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.unteleportable));
						Inst.UNTELEPORTABLE.defaultValue = itemDB.unteleportable.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UNTELEPORTABLE.defaultValue = "";
					}
					break;
				case SLASHRES:
					if (itemDB.slashres != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.slashres));
						Inst.SLASHRES.defaultValue = itemDB.slashres.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SLASHRES.defaultValue = "";
					}
					break;
				case PIERCERES:
					if (itemDB.pierceres != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.pierceres));
						Inst.PIERCERES.defaultValue = itemDB.pierceres.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.PIERCERES.defaultValue = "";
					}
					break;
				case BLUNTRES:
					if (itemDB.bluntres != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.bluntres));
						Inst.BLUNTRES.defaultValue = itemDB.bluntres.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.BLUNTRES.defaultValue = "";
					}
					break;
				case HPOVERFLOW:
					if (itemDB.hpoverflow != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.hpoverflow));
						Inst.HPOVERFLOW.defaultValue = itemDB.hpoverflow.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.HPOVERFLOW.defaultValue = "";
					}
					break;
				case DEATHCURSE:
					if (itemDB.deathcurse != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.deathcurse));
						Inst.DEATHCURSE.defaultValue = itemDB.deathcurse.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DEATHCURSE.defaultValue = "";
					}
					break;
				case TRAMPSWALLOW:
					if (itemDB.trampswallow != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.trampswallow));
						Inst.TRAMPSWALLOW.defaultValue = itemDB.trampswallow.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TRAMPSWALLOW.defaultValue = "";
					}
					break;
				case INQUISITOR:
					if (itemDB.inquisitor != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.inquisitor));
						Inst.INQUISITOR.defaultValue = itemDB.inquisitor.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.INQUISITOR.defaultValue = "";
					}
					break;
				case TAXCOLLECTOR:
					if (itemDB.taxcollector != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.taxcollector));
						Inst.TAXCOLLECTOR.defaultValue = itemDB.taxcollector.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TAXCOLLECTOR.defaultValue = "";
					}
					break;
				case UNDISCIPLINED:
					if (itemDB.undisciplined != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.undisciplined));
						Inst.UNDISCIPLINED.defaultValue = itemDB.undisciplined.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.UNDISCIPLINED.defaultValue = "";
					}
					break;
				case DRAINIMMUNE:
					if (itemDB.drainimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.drainimmune));
						Inst.DRAINIMMUNE.defaultValue = itemDB.drainimmune.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.DRAINIMMUNE.defaultValue = "";
					}
					break;
				case MAGICIMMUNE:
					if (itemDB.magicimmune != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.magicimmune));
						Inst.MAGICIMMUNE.defaultValue = itemDB.magicimmune.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAGICIMMUNE.defaultValue = "";
					}
					break;
				case COMSLAVE:
					if (itemDB.comslave != null) {
						((Inst4Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.comslave));
						Inst.COMSLAVE.defaultValue = itemDB.comslave.toString();
					} else {
						((Inst4Fields)fields.getValue()).defaultLabel.setText("");
						Inst.COMSLAVE.defaultValue = "";
					}
					break;

				case MAGICBOOST1:
					((Inst5Fields)fields.getValue()).defaultLabel1.setText(itemDB.magicboost1 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.magicboost1) : "");
					((Inst5Fields)fields.getValue()).defaultLabel2.setText(itemDB.magicboost2 != null ? Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.magicboost2) : "");
					break;

				case MAINPATH:
					if (itemDB.mainpath != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(itemDB.mainpath)));
						Inst.MAINPATH.defaultValue = itemDB.mainpath.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.MAINPATH.defaultValue = "0";
					}
					break;
				case SECONDARYPATH:
					if (itemDB.secondarypath != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", getPathName(itemDB.secondarypath)));
						Inst.SECONDARYPATH.defaultValue = itemDB.secondarypath.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.SECONDARYPATH.defaultValue = "0";
					}
					break;
				case TYPE:
					if (itemDB.type != null) {
						((Inst6Fields)fields.getValue()).defaultLabel.setText(Messages.format("DetailsPage.DefaultLabel.fmt", itemDB.type.toString()));
						Inst.TYPE.defaultValue = itemDB.type.toString();
					} else {
						((Inst6Fields)fields.getValue()).defaultLabel.setText("");
						Inst.TYPE.defaultValue = "";
					}
					break;
				}
			}
		}
		name.getParent().getParent().getParent().layout(true, true);
	}
	
	private String getItemname(Item item) {
		EList<ItemMods> list = item.getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst1) {
				if (((ItemInst1)mod).isName()) {
					return ((ItemInst1)mod).getValue();
				}
			}
		}
		return null;
	}

	private void setItemname(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				Item itemToEdit = (Item)input;
				EList<ItemMods> mods = itemToEdit.getMods();
				boolean nameSet = false;
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst1) {
						if (((ItemInst1)mod).isName()) {
							((ItemInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					ItemInst1 nameInst = DmFactory.eINSTANCE.createItemInst1();
					nameInst.setName(true);
					nameInst.setValue(newName);
					mods.add(nameInst);
				}
			}  
		});

		updateSelection();
	}
	
	private void setItemdescr(final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				Item itemToEdit = (Item)input;
				EList<ItemMods> mods = itemToEdit.getMods();
				boolean nameSet = false;
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst1) {
						if (((ItemInst1)mod).isDescr()) {
							((ItemInst1)mod).setValue(newName);
							nameSet = true;
						}
					}
				}
				if (!nameSet) {
					ItemInst1 nameInst = DmFactory.eINSTANCE.createItemInst1();
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
			return "cannot be researched";
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
		}
		return "Unknown";
	}
	
	private void setComboItems(Inst key, MappedDynamicCombo combo) {
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
	}

	private String getInst1(Inst inst2, Item item) {
		EList<ItemMods> list = item.getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst1) {
				switch (inst2) {
				case NAME:
					if (((ItemInst1)mod).isName()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				case DESCR:
					if (((ItemInst1)mod).isDescr()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				case ARMOR:
					if (((ItemInst1)mod).isArmor()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				case SPR:
					if (((ItemInst1)mod).isSpr()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				case SPELL:
					if (((ItemInst1)mod).isSpell()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				case AUTOSPELL:
					if (((ItemInst1)mod).isAutospell()){
						return ((ItemInst1)mod).getValue();
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Integer getInst2(Inst inst2, Item item) {
		EList<ItemMods> list = item.getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst2) {
				switch (inst2) {
				case CONSTLEVEL:
					if (((ItemInst2)mod).isConstlevel()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MAINPATH:
					if (((ItemInst2)mod).isMainpath()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MAINLEVEL:
					if (((ItemInst2)mod).isMainlevel()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SECONDARYPATH:
					if (((ItemInst2)mod).isSecondarypath()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SECONDARYLEVEL:
					if (((ItemInst2)mod).isSecondarylevel()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TYPE:
					if (((ItemInst2)mod).isType()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case STR:
					if (((ItemInst2)mod).isStr()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ATT:
					if (((ItemInst2)mod).isAtt()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEF:
					if (((ItemInst2)mod).isDef()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case PREC:
					if (((ItemInst2)mod).isPrec()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MR:
					if (((ItemInst2)mod).isMr()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MORALE:
					if (((ItemInst2)mod).isMorale()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;					
				case VOIDSANITY:
					if (((ItemInst2)mod).isVoidsanity()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case GIFTOFWATER:
					if (((ItemInst2)mod).isGiftofwater()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INVULNERABLE:
					if (((ItemInst2)mod).isInvulnerable()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INSPIRINGRES:
					if (((ItemInst2)mod).isInspiringres()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FIRERES:
					if (((ItemInst2)mod).isFireres()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case COLDRES:
					if (((ItemInst2)mod).isColdres()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SHOCKRES:
					if (((ItemInst2)mod).isShockres()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case POISONRES:
					if (((ItemInst2)mod).isPoisonres()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case RESTRICTED:
					if (((ItemInst2)mod).isRestricted()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case PEN:
					if (((ItemInst2)mod).isPen()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AUTOSPELLREPEAT:
					if (((ItemInst2)mod).isAutospellrepeat()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case RANDOMSPELL:
					if (((ItemInst2)mod).isRandomspell()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MAPSPEED:
					if (((ItemInst2)mod).isMapspeed()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TAINTED:
					if (((ItemInst2)mod).isTainted()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SPECIALLOOK:
					if (((ItemInst2)mod).isSpeciallook()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SEDUCE:
					if (((ItemInst2)mod).isSeduce()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SUCCUBUS:
					if (((ItemInst2)mod).isSuccubus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BECKON:
					if (((ItemInst2)mod).isBeckon()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FALSEARMY:
					if (((ItemInst2)mod).isFalsearmy()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FOOLSCOUTS:
					if (((ItemInst2)mod).isFoolscouts()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ICEPROT:
					if (((ItemInst2)mod).isIceprot()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case HEALER:
					if (((ItemInst2)mod).isHealer()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AUTOHEALER:
					if (((ItemInst2)mod).isAutohealer()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AUTODISHEALER:
					if (((ItemInst2)mod).isAutodishealer()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AUTODISGRINDER:
					if (((ItemInst2)mod).isAutodisgrinder()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case HOMESICK:
					if (((ItemInst2)mod).isHomesick()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case UWDAMAGE:
					if (((ItemInst2)mod).isUwdamage()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case REGENERATION:
					if (((ItemInst2)mod).isRegeneration()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case REINVIGORATION:
					if (((ItemInst2)mod).isReinvigoration()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case WOUNDFEND:
					if (((ItemInst2)mod).isWoundfend()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case POISONCLOUD:
					if (((ItemInst2)mod).isPoisoncloud()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DISEASECLOUD:
					if (((ItemInst2)mod).isDiseasecloud()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ANIMALAWE:
					if (((ItemInst2)mod).isAnimalawe()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AWE:
					if (((ItemInst2)mod).isAwe()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FEAR:
					if (((ItemInst2)mod).isFear()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FIRESHIELD:
					if (((ItemInst2)mod).isFireshield()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BANEFIRESHIELD:
					if (((ItemInst2)mod).isBanefireshield()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DAMAGEREV:
					if (((ItemInst2)mod).isDamagerev()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BLOODVENGEANCE:
					if (((ItemInst2)mod).isBloodvengeance()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SLIMER:
					if (((ItemInst2)mod).isSlimer()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEATHDISEASE:
					if (((ItemInst2)mod).isDeathdisease()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEATHPARALYZE:
					if (((ItemInst2)mod).isDeathparalyze()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEATHFIRE:
					if (((ItemInst2)mod).isDeathfire()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case CHAOSPOWER:
					if (((ItemInst2)mod).isChaospower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FIREPOWER:
					if (((ItemInst2)mod).isFirepower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case COLDPOWER:
					if (((ItemInst2)mod).isColdpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MAGICPOWER:
					if (((ItemInst2)mod).isMagicpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case STORMPOWER:
					if (((ItemInst2)mod).isStormpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DARKPOWER:
					if (((ItemInst2)mod).isDarkpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SPRINGPOWER:
					if (((ItemInst2)mod).isSpringpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SUMMERPOWER:
					if (((ItemInst2)mod).isSummerpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FALLPOWER:
					if (((ItemInst2)mod).isFallpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case WINTERPOWER:
					if (((ItemInst2)mod).isWinterpower()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AMBIDEXTROUS:
					if (((ItemInst2)mod).isAmbidextrous()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BERSERK:
					if (((ItemInst2)mod).isBerserk()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DARKVISION:
					if (((ItemInst2)mod).isDarkvision()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DIGEST:
					if (((ItemInst2)mod).isDigest()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INCORPORATE:
					if (((ItemInst2)mod).isIncorporate()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case CASTLEDEF:
					if (((ItemInst2)mod).isCastledef()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SIEGEBONUS:
					if (((ItemInst2)mod).isSiegebonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case PATROLBONUS:
					if (((ItemInst2)mod).isPatrolbonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case PILLAGEBONUS:
					if (((ItemInst2)mod).isPillagebonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SUPPLYBONUS:
					if (((ItemInst2)mod).isSupplybonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case NOBADEVENTS:
					if (((ItemInst2)mod).isNobadevents()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INCPROVDEF:
					if (((ItemInst2)mod).isIncprovdef()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INCUNREST:
					if (((ItemInst2)mod).isIncunrest()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case LEPER:
					if (((ItemInst2)mod).isLeper()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case POPKILL:
					if (((ItemInst2)mod).isPopkill()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case HERETIC:
					if (((ItemInst2)mod).isHeretic()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ELEGIST:
					if (((ItemInst2)mod).isElegist()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SPREADDOM:
					if (((ItemInst2)mod).isSpreaddom()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SHATTEREDSOUL:
					if (((ItemInst2)mod).isShatteredsoul()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case GOLD:
					if (((ItemInst2)mod).isGold()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INSPIRATIONAL:
					if (((ItemInst2)mod).isInspirational()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BEASTMASTER:
					if (((ItemInst2)mod).isBeastmaster()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TASKMASTER:
					if (((ItemInst2)mod).isTaskmaster()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FORMATIONFIGHTER:
					if (((ItemInst2)mod).isFormationfighter()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BODYGUARD:
					if (((ItemInst2)mod).isBodyguard()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case STANDARD:
					if (((ItemInst2)mod).isStandard()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DOUSE:
					if (((ItemInst2)mod).isDouse()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case RESEARCHBONUS:
					if (((ItemInst2)mod).isResearchbonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DIVINEINS:
					if (((ItemInst2)mod).isDivineins()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FORGEBONUS:
					if (((ItemInst2)mod).isForgebonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FIXFORGEBONUS:
					if (((ItemInst2)mod).isFixforgebonus()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case CROSSBREEDER:
					if (((ItemInst2)mod).isCrossbreeder()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BONUSSPELLS:
					if (((ItemInst2)mod).isBonusspells()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEATHBANISH:
					if (((ItemInst2)mod).isDeathbanish()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case KOKYTOSRET:
					if (((ItemInst2)mod).isKokytosret()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case INFERNORET:
					if (((ItemInst2)mod).isInfernoret()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case VOIDRET:
					if (((ItemInst2)mod).isVoidret()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ALLRET:
					if (((ItemInst2)mod).isAllret()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case FIRERANGE:
					if (((ItemInst2)mod).isFirerange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case AIRRANGE:
					if (((ItemInst2)mod).isAirrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case WATERRANGE:
					if (((ItemInst2)mod).isWaterrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case EARTHRANGE:
					if (((ItemInst2)mod).isEarthrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ASTRALRANGE:
					if (((ItemInst2)mod).isAstralrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case DEATHRANGE:
					if (((ItemInst2)mod).isDeathrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case NATURERANGE:
					if (((ItemInst2)mod).isNaturerange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case BLOODRANGE:
					if (((ItemInst2)mod).isBloodrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ELEMENTRANGE:
					if (((ItemInst2)mod).isElementrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case SORCERYRANGE:
					if (((ItemInst2)mod).isSorceryrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case ALLRANGE:
					if (((ItemInst2)mod).isAllrange()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case MAKEPEARLS:
					if (((ItemInst2)mod).isMakepearls()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPFIREGEMS:
					if (((ItemInst2)mod).isTmpfiregems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPAIRGEMS:
					if (((ItemInst2)mod).isTmpairgems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPWATERGEMS:
					if (((ItemInst2)mod).isTmpwatergems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPEARTHGEMS:
					if (((ItemInst2)mod).isTmpearthgems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPASTRALGEMS:
					if (((ItemInst2)mod).isTmpastralgems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPDEATHGEMS:
					if (((ItemInst2)mod).isTmpdeathgems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPNATUREGEMS:
					if (((ItemInst2)mod).isTmpnaturegems()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				case TMPBLOODSLAVES:
					if (((ItemInst2)mod).isTmpbloodslaves()){
						return Integer.valueOf(((ItemInst2)mod).getValue());
					}
					break;
				}
			}
		}
		return null;
	}
	
	private Object getInst3(Inst inst2, Item item) {
		EList<ItemMods> list = item.getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst3) {
				switch (inst2) {
				case COPYSPR:
					if (((ItemInst3)mod).isCopyspr()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case WEAPON:
					if (((ItemInst3)mod).isWeapon()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case COPYITEM:
					if (((ItemInst3)mod).isCopyitem()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON:
					if (((ItemInst3)mod).isDomsummon()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON2:
					if (((ItemInst3)mod).isDomsummon2()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case DOMSUMMON20:
					if (((ItemInst3)mod).isDomsummon20()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case RAREDOMSUMMON:
					if (((ItemInst3)mod).isRaredomsummon()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON1:
					if (((ItemInst3)mod).isSummon1()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON2:
					if (((ItemInst3)mod).isSummon2()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON3:
					if (((ItemInst3)mod).isSummon3()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON4:
					if (((ItemInst3)mod).isSummon4()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case SUMMON5:
					if (((ItemInst3)mod).isSummon5()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTERS1:
					if (((ItemInst3)mod).isMakemonsters1()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTERS2:
					if (((ItemInst3)mod).isMakemonsters2()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTERS3:
					if (((ItemInst3)mod).isMakemonsters3()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTERS4:
					if (((ItemInst3)mod).isMakemonsters4()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case MAKEMONSTERS5:
					if (((ItemInst3)mod).isMakemonsters5()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATTLESUM1:
					if (((ItemInst3)mod).isBattlesum1()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATTLESUM2:
					if (((ItemInst3)mod).isBattlesum2()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATTLESUM3:
					if (((ItemInst3)mod).isBattlesum3()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATTLESUM4:
					if (((ItemInst3)mod).isBattlesum4()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATTLESUM5:
					if (((ItemInst3)mod).isBattlesum5()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM1:
					if (((ItemInst3)mod).isBatstartsum1()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM2:
					if (((ItemInst3)mod).isBatstartsum2()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM3:
					if (((ItemInst3)mod).isBatstartsum3()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM4:
					if (((ItemInst3)mod).isBatstartsum4()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM5:
					if (((ItemInst3)mod).isBatstartsum5()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM1D6:
					if (((ItemInst3)mod).isBatstartsum1d6()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM2D6:
					if (((ItemInst3)mod).isBatstartsum2d6()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM3D6:
					if (((ItemInst3)mod).isBatstartsum3d6()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM4D6:
					if (((ItemInst3)mod).isBatstartsum4d6()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
						if (strVal != null) {
							return strVal;
						}
						return intVal;
					}
					break;
				case BATSTARTSUM5D6:
					if (((ItemInst3)mod).isBatstartsum5d6()){
						String strVal = ((ItemInst3)mod).getValue1();
						Integer intVal = ((ItemInst3)mod).getValue2();
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

	private Boolean getInst4(Inst inst4, Object item) {
		EList<ItemMods> list = ((Item)item).getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst4) {
				switch (inst4) {
				case STONEBEING:
					if (((ItemInst4)mod).isStonebeing()){
						return Boolean.TRUE;
					}
					break;
				case STORMIMMUNE:
					if (((ItemInst4)mod).isStormimmune()){
						return Boolean.TRUE;
					}
					break;
				case ETHEREAL:
					if (((ItemInst4)mod).isEthereal()){
						return Boolean.TRUE;
					}
					break;
				case TRAMPLE:
					if (((ItemInst4)mod).isTrample()){
						return Boolean.TRUE;
					}
					break;
				case INQUISITOR:
					if (((ItemInst4)mod).isInquisitor()){
						return Boolean.TRUE;
					}
					break;
				case DRAINIMMUNE:
					if (((ItemInst4)mod).isDrainimmune()){
						return Boolean.TRUE;
					}
					break;
				case CHAOSREC:
					if (((ItemInst4)mod).isChaosrec()){
						return Boolean.TRUE;
					}
					break;
				case SINGLEBATTLE:
					if (((ItemInst4)mod).isSinglebattle()){
						return Boolean.TRUE;
					}
					break;
				case AUTOCOMPETE:
					if (((ItemInst4)mod).isAutocompete()){
						return Boolean.TRUE;
					}
					break;
				case WATERBREATHING:
					if (((ItemInst4)mod).isWaterbreathing()){
						return Boolean.TRUE;
					}
					break;
				case FLOAT:
					if (((ItemInst4)mod).isFloat()){
						return Boolean.TRUE;
					}
					break;
				case NORIVERPASS:
					if (((ItemInst4)mod).isNoriverpass()){
						return Boolean.TRUE;
					}
					break;
				case UNTELEPORTABLE:
					if (((ItemInst4)mod).isUnteleportable()){
						return Boolean.TRUE;
					}
					break;
				case HPOVERFLOW:
					if (((ItemInst4)mod).isHpoverflow()){
						return Boolean.TRUE;
					}
					break;
				case PIERCERES:
					if (((ItemInst4)mod).isPierceres()){
						return Boolean.TRUE;
					}
					break;
				case SLASHRES:
					if (((ItemInst4)mod).isSlashres()){
						return Boolean.TRUE;
					}
					break;
				case BLUNTRES:
					if (((ItemInst4)mod).isBluntres()){
						return Boolean.TRUE;
					}
					break;
				case DEATHCURSE:
					if (((ItemInst4)mod).isDeathcurse()){
						return Boolean.TRUE;
					}
					break;
				case TRAMPSWALLOW:
					if (((ItemInst4)mod).isTrampswallow()){
						return Boolean.TRUE;
					}
					break;
				case TAXCOLLECTOR:
					if (((ItemInst4)mod).isTaxcollector()){
						return Boolean.TRUE;
					}
					break;
				case UNDISCIPLINED:
					if (((ItemInst4)mod).isUndisciplined()){
						return Boolean.TRUE;
					}
					break;
				case MAGICIMMUNE:
					if (((ItemInst4)mod).isMagicimmune()){
						return Boolean.TRUE;
					}
					break;
				case COMSLAVE:
					if (((ItemInst4)mod).isComslave()){
						return Boolean.TRUE;
					}
					break;
				case NOMOUNTED:
					if (((ItemInst4)mod).isNomounted()){
						return Boolean.TRUE;
					}
					break;
				case NOCOLDBLOOD:
					if (((ItemInst4)mod).isNocoldblood()){
						return Boolean.TRUE;
					}
					break;
				case NODEMON:
					if (((ItemInst4)mod).isNodemon()){
						return Boolean.TRUE;
					}
					break;
				case NOUNDEAD:
					if (((ItemInst4)mod).isNoundead()){
						return Boolean.TRUE;
					}
					break;
				case NOINANIM:
					if (((ItemInst4)mod).isNoinanim()){
						return Boolean.TRUE;
					}
					break;
				case NOFEMALE:
					if (((ItemInst4)mod).isNofemale()){
						return Boolean.TRUE;
					}
					break;
				case ONLYMOUNTED:
					if (((ItemInst4)mod).isOnlymounted()){
						return Boolean.TRUE;
					}
					break;
				case ONLYCOLDBLOOD:
					if (((ItemInst4)mod).isOnlycoldblood()){
						return Boolean.TRUE;
					}
					break;
				case ONLYDEMON:
					if (((ItemInst4)mod).isOnlydemon()){
						return Boolean.TRUE;
					}
					break;
				case ONLYUNDEAD:
					if (((ItemInst4)mod).isOnlyundead()){
						return Boolean.TRUE;
					}
					break;
				case ONLYINANIM:
					if (((ItemInst4)mod).isOnlyinanim()){
						return Boolean.TRUE;
					}
					break;
				case ONLYFEMALE:
					if (((ItemInst4)mod).isOnlyfemale()){
						return Boolean.TRUE;
					}
					break;
				case REQEYES:
					if (((ItemInst4)mod).isReqeyes()){
						return Boolean.TRUE;
					}
					break;
				case NOFIND:
					if (((ItemInst4)mod).isNofind()){
						return Boolean.TRUE;
					}
					break;
				case LUCK:
					if (((ItemInst4)mod).isLuck()){
						return Boolean.TRUE;
					}
					break;
				case QUICKNESS:
					if (((ItemInst4)mod).isQuickness()){
						return Boolean.TRUE;
					}
					break;
				case BLESS:
					if (((ItemInst4)mod).isBless()){
						return Boolean.TRUE;
					}
					break;
				case BARKSKIN:
					if (((ItemInst4)mod).isBarkskin()){
						return Boolean.TRUE;
					}
					break;
				case STONESKIN:
					if (((ItemInst4)mod).isStoneskin()){
						return Boolean.TRUE;
					}
					break;
				case IRONSKIN:
					if (((ItemInst4)mod).isIronskin()){
						return Boolean.TRUE;
					}
					break;
				case FLY:
					if (((ItemInst4)mod).isFly()){
						return Boolean.TRUE;
					}
					break;
				case RUN:
					if (((ItemInst4)mod).isRun()){
						return Boolean.TRUE;
					}
					break;
				case BERS:
					if (((ItemInst4)mod).isBers()){
						return Boolean.TRUE;
					}
					break;
				case EXTRALIFE:
					if (((ItemInst4)mod).isExtralife()){
						return Boolean.TRUE;
					}
					break;
				case CHAMPPRIZE:
					if (((ItemInst4)mod).isChampprize()){
						return Boolean.TRUE;
					}
					break;
				case CURSED:
					if (((ItemInst4)mod).isCursed()){
						return Boolean.TRUE;
					}
					break;
				case CURSE:
					if (((ItemInst4)mod).isCurse()){
						return Boolean.TRUE;
					}
					break;
				case DISEASE:
					if (((ItemInst4)mod).isDisease()){
						return Boolean.TRUE;
					}
					break;
				case CHESTWOUND:
					if (((ItemInst4)mod).isChestwound()){
						return Boolean.TRUE;
					}
					break;
				case FEEBLEMIND:
					if (((ItemInst4)mod).isFeeblemind()){
						return Boolean.TRUE;
					}
					break;
				case MUTE:
					if (((ItemInst4)mod).isMute()){
						return Boolean.TRUE;
					}
					break;
				case NHWOUND:
					if (((ItemInst4)mod).isNhwound()){
						return Boolean.TRUE;
					}
					break;
				case CRIPPLED:
					if (((ItemInst4)mod).isCrippled()){
						return Boolean.TRUE;
					}
					break;
				case LOSEEYE:
					if (((ItemInst4)mod).isLoseeye()){
						return Boolean.TRUE;
					}
					break;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	private Integer[] getInst5(Inst inst, Object item) {
		int boostCount = 0;
		EList<ItemMods> list = ((Item)item).getMods();
		for (ItemMods mod : list) {
			if (mod instanceof ItemInst5) {
				switch (inst) {
				case MAGICBOOST1:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 1) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST2:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 2) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST3:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 3) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST4:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 4) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST5:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 5) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST6:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 6) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST7:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 7) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
					}
					break;
				case MAGICBOOST8:
					if (((ItemInst5)mod).isMagicboost()) {
						boostCount++;
						if (boostCount == 8) {
							return new Integer[]{Integer.valueOf(((ItemInst5)mod).getValue1()), Integer.valueOf(((ItemInst5)mod).getValue2())};
						}
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
			public void process(XtextResource state) throws Exception {
				Item itemToEdit = (Item)input;
				EList<ItemMods> mods = itemToEdit.getMods();				
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst1) {
						switch (inst2) {
						case NAME:
							if (((ItemInst1)mod).isName()) {
								((ItemInst1)mod).setValue(newName);
							}
							break;
						case DESCR:
							if (((ItemInst1)mod).isDescr()) {
								((ItemInst1)mod).setValue(newName);
							}
							break;
						case ARMOR:
							if (((ItemInst1)mod).isArmor()) {
								((ItemInst1)mod).setValue(newName);
							}
							break;
						case SPR:
							if (((ItemInst1)mod).isSpr()) {
								((ItemInst1)mod).setValue(newName);
							}
							break;
						case SPELL:
							if (((ItemInst1)mod).isSpell()) {
								((ItemInst1)mod).setValue(newName);
							}
							break;
						case AUTOSPELL:
							if (((ItemInst1)mod).isAutospell()) {
								((ItemInst1)mod).setValue(newName);
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
			public void process(XtextResource state) throws Exception {
				Item itemToEdit = (Item)input;
				EList<ItemMods> mods = itemToEdit.getMods();
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst2) {
						switch (inst2) {
						case CONSTLEVEL:
							if (((ItemInst2)mod).isConstlevel()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAINPATH:
							if (((ItemInst2)mod).isMainpath()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAINLEVEL:
							if (((ItemInst2)mod).isMainlevel()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SECONDARYPATH:
							if (((ItemInst2)mod).isSecondarypath()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SECONDARYLEVEL:
							if (((ItemInst2)mod).isSecondarylevel()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TYPE:
							if (((ItemInst2)mod).isType()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STR:
							if (((ItemInst2)mod).isStr()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ATT:
							if (((ItemInst2)mod).isAtt()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEF:
							if (((ItemInst2)mod).isDef()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PREC:
							if (((ItemInst2)mod).isPrec()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MR:
							if (((ItemInst2)mod).isMr()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MORALE:
							if (((ItemInst2)mod).isMorale()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;							
						case VOIDSANITY:
							if (((ItemInst2)mod).isVoidsanity()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GIFTOFWATER:
							if (((ItemInst2)mod).isGiftofwater()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INVULNERABLE:
							if (((ItemInst2)mod).isInvulnerable()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INSPIRINGRES:
							if (((ItemInst2)mod).isInspiringres()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRERES:
							if (((ItemInst2)mod).isFireres()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COLDRES:
							if (((ItemInst2)mod).isColdres()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SHOCKRES:
							if (((ItemInst2)mod).isShockres()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POISONRES:
							if (((ItemInst2)mod).isPoisonres()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESTRICTED:
							if (((ItemInst2)mod).isRestricted()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PEN:
							if (((ItemInst2)mod).isPen()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AUTOSPELLREPEAT:
							if (((ItemInst2)mod).isAutospellrepeat()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RANDOMSPELL:
							if (((ItemInst2)mod).isRandomspell()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAPSPEED:
							if (((ItemInst2)mod).isMapspeed()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TAINTED:
							if (((ItemInst2)mod).isTainted()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPECIALLOOK:
							if (((ItemInst2)mod).isSpeciallook()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SEDUCE:
							if (((ItemInst2)mod).isSeduce()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUCCUBUS:
							if (((ItemInst2)mod).isSuccubus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BECKON:
							if (((ItemInst2)mod).isBeckon()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FALSEARMY:
							if (((ItemInst2)mod).isFalsearmy()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FOOLSCOUTS:
							if (((ItemInst2)mod).isFoolscouts()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ICEPROT:
							if (((ItemInst2)mod).isIceprot()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HEALER:
							if (((ItemInst2)mod).isHealer()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AUTOHEALER:
							if (((ItemInst2)mod).isAutohealer()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AUTODISHEALER:
							if (((ItemInst2)mod).isAutodishealer()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AUTODISGRINDER:
							if (((ItemInst2)mod).isAutodisgrinder()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HOMESICK:
							if (((ItemInst2)mod).isHomesick()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case UWDAMAGE:
							if (((ItemInst2)mod).isUwdamage()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case REGENERATION:
							if (((ItemInst2)mod).isRegeneration()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case REINVIGORATION:
							if (((ItemInst2)mod).isReinvigoration()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WOUNDFEND:
							if (((ItemInst2)mod).isWoundfend()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POISONCLOUD:
							if (((ItemInst2)mod).isPoisoncloud()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DISEASECLOUD:
							if (((ItemInst2)mod).isDiseasecloud()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ANIMALAWE:
							if (((ItemInst2)mod).isAnimalawe()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AWE:
							if (((ItemInst2)mod).isAwe()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FEAR:
							if (((ItemInst2)mod).isFear()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRESHIELD:
							if (((ItemInst2)mod).isFireshield()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BANEFIRESHIELD:
							if (((ItemInst2)mod).isBanefireshield()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DAMAGEREV:
							if (((ItemInst2)mod).isDamagerev()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLOODVENGEANCE:
							if (((ItemInst2)mod).isBloodvengeance()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SLIMER:
							if (((ItemInst2)mod).isSlimer()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHDISEASE:
							if (((ItemInst2)mod).isDeathdisease()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHPARALYZE:
							if (((ItemInst2)mod).isDeathparalyze()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHFIRE:
							if (((ItemInst2)mod).isDeathfire()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CHAOSPOWER:
							if (((ItemInst2)mod).isChaospower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIREPOWER:
							if (((ItemInst2)mod).isFirepower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case COLDPOWER:
							if (((ItemInst2)mod).isColdpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAGICPOWER:
							if (((ItemInst2)mod).isMagicpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STORMPOWER:
							if (((ItemInst2)mod).isStormpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DARKPOWER:
							if (((ItemInst2)mod).isDarkpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPRINGPOWER:
							if (((ItemInst2)mod).isSpringpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUMMERPOWER:
							if (((ItemInst2)mod).isSummerpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FALLPOWER:
							if (((ItemInst2)mod).isFallpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WINTERPOWER:
							if (((ItemInst2)mod).isWinterpower()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AMBIDEXTROUS:
							if (((ItemInst2)mod).isAmbidextrous()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BERSERK:
							if (((ItemInst2)mod).isBerserk()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DARKVISION:
							if (((ItemInst2)mod).isDarkvision()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DIGEST:
							if (((ItemInst2)mod).isDigest()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INCORPORATE:
							if (((ItemInst2)mod).isIncorporate()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CASTLEDEF:
							if (((ItemInst2)mod).isCastledef()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SIEGEBONUS:
							if (((ItemInst2)mod).isSiegebonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PATROLBONUS:
							if (((ItemInst2)mod).isPatrolbonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case PILLAGEBONUS:
							if (((ItemInst2)mod).isPillagebonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SUPPLYBONUS:
							if (((ItemInst2)mod).isSupplybonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NOBADEVENTS:
							if (((ItemInst2)mod).isNobadevents()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INCPROVDEF:
							if (((ItemInst2)mod).isIncprovdef()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INCUNREST:
							if (((ItemInst2)mod).isIncunrest()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case LEPER:
							if (((ItemInst2)mod).isLeper()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case POPKILL:
							if (((ItemInst2)mod).isPopkill()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case HERETIC:
							if (((ItemInst2)mod).isHeretic()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ELEGIST:
							if (((ItemInst2)mod).isElegist()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SPREADDOM:
							if (((ItemInst2)mod).isSpreaddom()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SHATTEREDSOUL:
							if (((ItemInst2)mod).isShatteredsoul()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case GOLD:
							if (((ItemInst2)mod).isGold()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INSPIRATIONAL:
							if (((ItemInst2)mod).isInspirational()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BEASTMASTER:
							if (((ItemInst2)mod).isBeastmaster()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TASKMASTER:
							if (((ItemInst2)mod).isTaskmaster()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORMATIONFIGHTER:
							if (((ItemInst2)mod).isFormationfighter()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BODYGUARD:
							if (((ItemInst2)mod).isBodyguard()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case STANDARD:
							if (((ItemInst2)mod).isStandard()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DOUSE:
							if (((ItemInst2)mod).isDouse()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case RESEARCHBONUS:
							if (((ItemInst2)mod).isResearchbonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DIVINEINS:
							if (((ItemInst2)mod).isDivineins()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FORGEBONUS:
							if (((ItemInst2)mod).isForgebonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIXFORGEBONUS:
							if (((ItemInst2)mod).isFixforgebonus()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case CROSSBREEDER:
							if (((ItemInst2)mod).isCrossbreeder()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BONUSSPELLS:
							if (((ItemInst2)mod).isBonusspells()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHBANISH:
							if (((ItemInst2)mod).isDeathbanish()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case KOKYTOSRET:
							if (((ItemInst2)mod).isKokytosret()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case INFERNORET:
							if (((ItemInst2)mod).isInfernoret()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case VOIDRET:
							if (((ItemInst2)mod).isVoidret()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ALLRET:
							if (((ItemInst2)mod).isAllret()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case FIRERANGE:
							if (((ItemInst2)mod).isFirerange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case AIRRANGE:
							if (((ItemInst2)mod).isAirrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case WATERRANGE:
							if (((ItemInst2)mod).isWaterrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case EARTHRANGE:
							if (((ItemInst2)mod).isEarthrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ASTRALRANGE:
							if (((ItemInst2)mod).isAstralrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case DEATHRANGE:
							if (((ItemInst2)mod).isDeathrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case NATURERANGE:
							if (((ItemInst2)mod).isNaturerange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case BLOODRANGE:
							if (((ItemInst2)mod).isBloodrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ELEMENTRANGE:
							if (((ItemInst2)mod).isElementrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case SORCERYRANGE:
							if (((ItemInst2)mod).isSorceryrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case ALLRANGE:
							if (((ItemInst2)mod).isAllrange()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case MAKEPEARLS:
							if (((ItemInst2)mod).isMakepearls()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPFIREGEMS:
							if (((ItemInst2)mod).isTmpfiregems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPAIRGEMS:
							if (((ItemInst2)mod).isTmpairgems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPWATERGEMS:
							if (((ItemInst2)mod).isTmpwatergems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPEARTHGEMS:
							if (((ItemInst2)mod).isTmpearthgems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPASTRALGEMS:
							if (((ItemInst2)mod).isTmpastralgems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPDEATHGEMS:
							if (((ItemInst2)mod).isTmpdeathgems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPNATUREGEMS:
							if (((ItemInst2)mod).isTmpnaturegems()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						case TMPBLOODSLAVES:
							if (((ItemInst2)mod).isTmpbloodslaves()){
								((ItemInst2)mod).setValue(Integer.parseInt(newName));
							}
							break;
						}
					}
				}

			}  
		});

		updateSelection();
	}

	private void setInst3(final Inst inst2, final XtextEditor editor, final String newName) 
	{
		final IXtextDocument myDocument = editor.getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource state) throws Exception {
				Item itemToEdit = (Item)input;
				List<ItemMods> modsToRemove = new ArrayList<ItemMods>();
				List<ItemMods> modsToAdd = new ArrayList<ItemMods>();
				EList<ItemMods> mods = itemToEdit.getMods();
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst3) {
						Integer newValue = null;
						try {
							newValue = Integer.valueOf(newName);
						} catch (NumberFormatException e) {
							// is not a number
						}

						switch (inst2) {
						case COPYSPR:
							if (((ItemInst3)mod).isCopyspr()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setCopyspr(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case WEAPON:
							if (((ItemInst3)mod).isWeapon()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setWeapon(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case COPYITEM:
							if (((ItemInst3)mod).isCopyitem()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setCopyitem(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case DOMSUMMON:
							if (((ItemInst3)mod).isDomsummon()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
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
							if (((ItemInst3)mod).isDomsummon2()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
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
							if (((ItemInst3)mod).isDomsummon20()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setDomsummon20(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case RAREDOMSUMMON:
							if (((ItemInst3)mod).isRaredomsummon()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setRaredomsummon(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON1:
							if (((ItemInst3)mod).isSummon1()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setSummon1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON2:
							if (((ItemInst3)mod).isSummon2()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setSummon2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON3:
							if (((ItemInst3)mod).isSummon3()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setSummon3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON4:
							if (((ItemInst3)mod).isSummon4()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setSummon4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case SUMMON5:
							if (((ItemInst3)mod).isSummon5()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setSummon5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTERS1:
							if (((ItemInst3)mod).isMakemonsters1()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setMakemonsters1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTERS2:
							if (((ItemInst3)mod).isMakemonsters2()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setMakemonsters2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTERS3:
							if (((ItemInst3)mod).isMakemonsters3()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setMakemonsters3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTERS4:
							if (((ItemInst3)mod).isMakemonsters4()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setMakemonsters4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case MAKEMONSTERS5:
							if (((ItemInst3)mod).isMakemonsters5()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setMakemonsters5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATTLESUM1:
							if (((ItemInst3)mod).isBattlesum1()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBattlesum1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATTLESUM2:
							if (((ItemInst3)mod).isBattlesum2()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBattlesum2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATTLESUM3:
							if (((ItemInst3)mod).isBattlesum3()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBattlesum3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATTLESUM4:
							if (((ItemInst3)mod).isBattlesum4()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBattlesum4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATTLESUM5:
							if (((ItemInst3)mod).isBattlesum5()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBattlesum5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM1:
							if (((ItemInst3)mod).isBatstartsum1()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum1(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM2:
							if (((ItemInst3)mod).isBatstartsum2()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum2(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM3:
							if (((ItemInst3)mod).isBatstartsum3()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum3(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM4:
							if (((ItemInst3)mod).isBatstartsum4()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum4(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM5:
							if (((ItemInst3)mod).isBatstartsum5()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum5(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM1D6:
							if (((ItemInst3)mod).isBatstartsum1d6()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum1d6(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM2D6:
							if (((ItemInst3)mod).isBatstartsum2d6()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum2d6(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM3D6:
							if (((ItemInst3)mod).isBatstartsum3d6()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum3d6(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM4D6:
							if (((ItemInst3)mod).isBatstartsum4d6()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum4d6(true);
								if (newValue != null) {
									newMod.setValue2(Integer.parseInt(newName));
								} else {
									newMod.setValue1(newName);
								}
								modsToAdd.add(newMod);
							}
							break;
						case BATSTARTSUM5D6:
							if (((ItemInst3)mod).isBatstartsum5d6()){
								modsToRemove.add(mod);
								ItemInst3 newMod = DmFactory.eINSTANCE.createItemInst3();
								newMod.setBatstartsum5d6(true);
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

	private void setInst5(final Inst inst3, final XtextEditor editor, final String value1, final String value2) 
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
				int boostCount = 0;
				Item itemToEdit = (Item)input;
				EList<ItemMods> mods = itemToEdit.getMods();
				for (ItemMods mod : mods) {
					if (mod instanceof ItemInst5) {
						switch (inst3) {
						case MAGICBOOST1:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 1) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST2:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 2) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST3:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 3) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST4:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 4) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST5:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 5) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST6:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 6) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST7:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 7) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
									}
								}
							}
							break;
						case MAGICBOOST8:
							if (((ItemInst5)mod).isMagicboost()) {
								boostCount++;
								if (boostCount == 8) {
									if (value1 != null) {
										((ItemInst5)mod).setValue1(Integer.parseInt(value1));
									}
									if (value2 != null) {
										((ItemInst5)mod).setValue2(Integer.parseInt(value2));
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
					public void process(XtextResource state) throws Exception {
						EList<ItemMods> mods = ((Item)input).getMods();
						ItemInst1 type = DmFactory.eINSTANCE.createItemInst1();
						switch (inst) {
						case NAME:
							type.setName(true);
							break;
						case DESCR:
							type.setDescr(true);
							break;
						case ARMOR:
							type.setArmor(true);
							break;
						case SPR:
							type.setSpr(true);
							break;
						case SPELL:
							type.setSpell(true);
							break;
						case AUTOSPELL:
							type.setAutospell(true);
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
						EList<ItemMods> mods = ((Item)input).getMods();
						ItemInst2 type = DmFactory.eINSTANCE.createItemInst2();
						switch (inst) {
						case CONSTLEVEL:
							type.setConstlevel(true);
							break;
						case MAINPATH:
							type.setMainpath(true);
							break;
						case MAINLEVEL:
							type.setMainlevel(true);
							break;
						case SECONDARYPATH:
							type.setSecondarypath(true);
							break;
						case SECONDARYLEVEL:
							type.setSecondarylevel(true);
							break;
						case TYPE:
							type.setType(true);
							break;
						case STR:
							type.setStr(true);
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
						case MORALE:
							type.setMorale(true);
							break;							
						case VOIDSANITY:
							type.setVoidsanity(true);
							break;
						case GIFTOFWATER:
							type.setGiftofwater(true);
							break;
						case INVULNERABLE:
							type.setInvulnerable(true);
							break;
						case INSPIRINGRES:
							type.setInspiringres(true);
							break;							
						case FIRERES:
							type.setFireres(true);
							break;
						case COLDRES:
							type.setColdres(true);
							break;
						case SHOCKRES:
							type.setShockres(true);
							break;
						case POISONRES:
							type.setPoisonres(true);
							break;
						case RESTRICTED:
							type.setRestricted(true);
							break;
						case PEN:
							type.setPen(true);
							break;
						case AUTOSPELLREPEAT:
							type.setAutospellrepeat(true);
							break;
						case RANDOMSPELL:
							type.setRandomspell(true);
							break;
						case MAPSPEED:
							type.setMapspeed(true);
							break;
						case TAINTED:
							type.setTainted(true);
							break;
						case SPECIALLOOK:
							type.setSpeciallook(true);
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
						case FALSEARMY:
							type.setFalsearmy(true);
							break;
						case FOOLSCOUTS:
							type.setFoolscouts(true);
							break;
						case ICEPROT:
							type.setIceprot(true);
							break;
						case HEALER:
							type.setHealer(true);
							break;
						case AUTOHEALER:
							type.setAutohealer(true);
							break;
						case AUTODISHEALER:
							type.setAutodishealer(true);
							break;
						case AUTODISGRINDER:
							type.setAutodisgrinder(true);
							break;
						case HOMESICK:
							type.setHomesick(true);
							break;
						case UWDAMAGE:
							type.setUwdamage(true);
							break;
						case REGENERATION:
							type.setRegeneration(true);
							break;
						case REINVIGORATION:
							type.setReinvigoration(true);
							break;
						case WOUNDFEND:
							type.setWoundfend(true);
							break;
						case POISONCLOUD:
							type.setPoisoncloud(true);
							break;
						case DISEASECLOUD:
							type.setDiseasecloud(true);
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
						case FIRESHIELD:
							type.setFireshield(true);
							break;
						case BANEFIRESHIELD:
							type.setBanefireshield(true);
							break;
						case DAMAGEREV:
							type.setDamagerev(true);
							break;
						case BLOODVENGEANCE:
							type.setBloodvengeance(true);
							break;
						case SLIMER:
							type.setSlimer(true);
							break;
						case DEATHDISEASE:
							type.setDeathdisease(true);
							break;
						case DEATHPARALYZE:
							type.setDeathparalyze(true);
							break;
						case DEATHFIRE:
							type.setDeathfire(true);
							break;
						case CHAOSPOWER:
							type.setChaospower(true);
							break;
						case FIREPOWER:
							type.setFirepower(true);
							break;
						case COLDPOWER:
							type.setColdpower(true);
							break;
						case MAGICPOWER:
							type.setMagicpower(true);
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
						case BERSERK:
							type.setBerserk(true);
							break;
						case DARKVISION:
							type.setDarkvision(true);
							break;
						case DIGEST:
							type.setDigest(true);
							break;
						case INCORPORATE:
							type.setIncorporate(true);
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
						case SUPPLYBONUS:
							type.setSupplybonus(true);
							break;
						case NOBADEVENTS:
							type.setNobadevents(true);
							break;
						case INCPROVDEF:
							type.setIncprovdef(true);
							break;
						case INCUNREST:
							type.setIncunrest(true);
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
						case ELEGIST:
							type.setElegist(true);
							break;
						case SPREADDOM:
							type.setSpreaddom(true);
							break;
						case SHATTEREDSOUL:
							type.setShatteredsoul(true);
							break;
						case GOLD:
							type.setGold(true);
							break;
						case INSPIRATIONAL:
							type.setInspirational(true);
							break;
						case BEASTMASTER:
							type.setBeastmaster(true);
							break;
						case TASKMASTER:
							type.setTaskmaster(true);
							break;
						case FORMATIONFIGHTER:
							type.setFormationfighter(true);
							break;
						case BODYGUARD:
							type.setBodyguard(true);
							break;
						case STANDARD:
							type.setStandard(true);
							break;
						case DOUSE:
							type.setDouse(true);
							break;
						case RESEARCHBONUS:
							type.setResearchbonus(true);
							break;
						case DIVINEINS:
							type.setDivineins(true);
							break;
						case FORGEBONUS:
							type.setForgebonus(true);
							break;
						case FIXFORGEBONUS:
							type.setFixforgebonus(true);
							break;
						case CROSSBREEDER:
							type.setCrossbreeder(true);
							break;
						case BONUSSPELLS:
							type.setBonusspells(true);
							break;
						case DEATHBANISH:
							type.setDeathbanish(true);
							break;
						case KOKYTOSRET:
							type.setKokytosret(true);
							break;
						case INFERNORET:
							type.setInfernoret(true);
							break;
						case VOIDRET:
							type.setVoidret(true);
							break;
						case ALLRET:
							type.setAllret(true);
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
						case MAKEPEARLS:
							type.setMakepearls(true);
							break;
						case TMPFIREGEMS:
							type.setTmpfiregems(true);
							break;
						case TMPAIRGEMS:
							type.setTmpairgems(true);
							break;
						case TMPWATERGEMS:
							type.setTmpwatergems(true);
							break;
						case TMPEARTHGEMS:
							type.setTmpearthgems(true);
							break;
						case TMPASTRALGEMS:
							type.setTmpastralgems(true);
							break;
						case TMPDEATHGEMS:
							type.setTmpdeathgems(true);
							break;
						case TMPNATUREGEMS:
							type.setTmpnaturegems(true);
							break;
						case TMPBLOODSLAVES:
							type.setTmpbloodslaves(true);
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
	
	private void addInst3(final Inst inst, final XtextEditor editor, final String newName) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						EList<ItemMods> mods = ((Item)input).getMods();
						ItemInst3 type = DmFactory.eINSTANCE.createItemInst3();
						switch (inst) {
						case COPYSPR:
							type.setCopyspr(true);
							break;
						case WEAPON:
							type.setWeapon(true);
							break;
						case COPYITEM:
							type.setCopyitem(true);
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
						case RAREDOMSUMMON:
							type.setRaredomsummon(true);
							break;
						case SUMMON1:
							type.setSummon1(true);
							break;
						case SUMMON2:
							type.setSummon2(true);
							break;
						case SUMMON3:
							type.setSummon3(true);
							break;
						case SUMMON4:
							type.setSummon4(true);
							break;
						case SUMMON5:
							type.setSummon5(true);
							break;
						case MAKEMONSTERS1:
							type.setMakemonsters1(true);
							break;
						case MAKEMONSTERS2:
							type.setMakemonsters2(true);
							break;
						case MAKEMONSTERS3:
							type.setMakemonsters3(true);
							break;
						case MAKEMONSTERS4:
							type.setMakemonsters4(true);
							break;
						case MAKEMONSTERS5:
							type.setMakemonsters5(true);
							break;
						case BATTLESUM1:
							type.setBattlesum1(true);
							break;
						case BATTLESUM2:
							type.setBattlesum2(true);
							break;
						case BATTLESUM3:
							type.setBattlesum3(true);
							break;
						case BATTLESUM4:
							type.setBattlesum4(true);
							break;
						case BATTLESUM5:
							type.setBattlesum5(true);
							break;
						case BATSTARTSUM1:
							type.setBatstartsum1(true);
							break;
						case BATSTARTSUM2:
							type.setBatstartsum2(true);
							break;
						case BATSTARTSUM3:
							type.setBatstartsum3(true);
							break;
						case BATSTARTSUM4:
							type.setBatstartsum4(true);
							break;
						case BATSTARTSUM5:
							type.setBatstartsum5(true);
							break;
						case BATSTARTSUM1D6:
							type.setBatstartsum1d6(true);
							break;
						case BATSTARTSUM2D6:
							type.setBatstartsum2d6(true);
							break;
						case BATSTARTSUM3D6:
							type.setBatstartsum3d6(true);
							break;
						case BATSTARTSUM4D6:
							type.setBatstartsum4d6(true);
							break;
						case BATSTARTSUM5D6:
							type.setBatstartsum5d6(true);
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
	
	private void addInst4(final Inst inst, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<ItemMods> mods = ((Item)input).getMods();
						ItemInst4 type = DmFactory.eINSTANCE.createItemInst4();
						switch (inst) {
						case STONEBEING:
							type.setStonebeing(true);
							break;
						case STORMIMMUNE:
							type.setStormimmune(true);
							break;
						case ETHEREAL:
							type.setEthereal(true);
							break;
						case TRAMPLE:
							type.setTrample(true);
							break;
						case INQUISITOR:
							type.setInquisitor(true);
							break;
						case DRAINIMMUNE:
							type.setDrainimmune(true);
							break;
						case CHAOSREC:
							type.setChaosrec(true);
							break;				
						case SINGLEBATTLE:
							type.setSinglebattle(true);
							break;				
						case AUTOCOMPETE:
							type.setAutocompete(true);
							break;				
						case WATERBREATHING:
							type.setWaterbreathing(true);
							break;				
						case FLOAT:
							type.setFloat(true);
							break;				
						case NORIVERPASS:
							type.setNoriverpass(true);
							break;				
						case UNTELEPORTABLE:
							type.setUnteleportable(true);
							break;				
						case HPOVERFLOW:
							type.setHpoverflow(true);
							break;				
						case PIERCERES:
							type.setPierceres(true);
							break;				
						case SLASHRES:
							type.setSlashres(true);
							break;				
						case BLUNTRES:
							type.setBluntres(true);
							break;				
						case DEATHCURSE:
							type.setDeathcurse(true);
							break;				
						case TRAMPSWALLOW:
							type.setTrampswallow(true);
							break;				
						case TAXCOLLECTOR:
							type.setTaxcollector(true);
							break;				
						case UNDISCIPLINED:
							type.setUndisciplined(true);
							break;				
						case MAGICIMMUNE:
							type.setMagicimmune(true);
							break;				
						case COMSLAVE:
							type.setComslave(true);
							break;				
						case NOMOUNTED:
							type.setNomounted(true);
							break;
						case NOCOLDBLOOD:
							type.setNocoldblood(true);
							break;
						case NODEMON:
							type.setNodemon(true);
							break;
						case NOUNDEAD:
							type.setNoundead(true);
							break;
						case NOINANIM:
							type.setNoinanim(true);
							break;
						case NOFEMALE:
							type.setNofemale(true);
							break;
						case ONLYMOUNTED:
							type.setOnlymounted(true);
							break;
						case ONLYCOLDBLOOD:
							type.setOnlycoldblood(true);
							break;
						case ONLYDEMON:
							type.setOnlydemon(true);
							break;
						case ONLYUNDEAD:
							type.setOnlyundead(true);
							break;
						case ONLYINANIM:
							type.setOnlyinanim(true);
							break;
						case ONLYFEMALE:
							type.setOnlyfemale(true);
							break;
						case REQEYES:
							type.setReqeyes(true);
							break;
						case NOFIND:
							type.setNofind(true);
							break;
						case LUCK:
							type.setLuck(true);
							break;
						case QUICKNESS:
							type.setQuickness(true);
							break;
						case BLESS:
							type.setBless(true);
							break;
						case BARKSKIN:
							type.setBarkskin(true);
							break;
						case STONESKIN:
							type.setStoneskin(true);
							break;
						case IRONSKIN:
							type.setIronskin(true);
							break;
						case FLY:
							type.setFly(true);
							break;
						case RUN:
							type.setRun(true);
							break;
						case BERS:
							type.setBers(true);
							break;
						case EXTRALIFE:
							type.setExtralife(true);
							break;
						case CHAMPPRIZE:
							type.setChampprize(true);
							break;
						case CURSED:
							type.setCursed(true);
							break;
						case CURSE:
							type.setCurse(true);
							break;
						case DISEASE:
							type.setDisease(true);
							break;
						case CHESTWOUND:
							type.setChestwound(true);
							break;
						case FEEBLEMIND:
							type.setFeeblemind(true);
							break;
						case MUTE:
							type.setMute(true);
							break;
						case NHWOUND:
							type.setNhwound(true);
							break;
						case CRIPPLED:
							type.setCrippled(true);
							break;
						case LOSEEYE:
							type.setLoseeye(true);
							break;
						}
						mods.add(type);
					}  
				});

				updateSelection();
			}
		});
	}

	private void addInst5(final Inst inst, final XtextEditor editor, final String newName1, final String newName2) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource resource) throws Exception {
						EList<ItemMods> mods = ((Item)input).getMods();
						ItemInst5 type = DmFactory.eINSTANCE.createItemInst5();
						switch (inst) {
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
	
	private void removeInst(final Inst inst2, final XtextEditor editor) {
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			@Override
			public void run() {
				final IXtextDocument myDocument = editor.getDocument();
				myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
					@Override
					public void process(XtextResource state) throws Exception {
						ItemMods modToRemove = null;
						int boostCount = 0;
						EList<ItemMods> mods = ((Item)input).getMods();
						for (ItemMods mod : mods) {
							if (mod instanceof ItemInst1) {
								switch (inst2) {
								case NAME:
									if (((ItemInst1)mod).isName()){
										modToRemove = mod;
									}
									break;
								case DESCR:
									if (((ItemInst1)mod).isDescr()){
										modToRemove = mod;
									}
									break;
								case ARMOR:
									if (((ItemInst1)mod).isArmor()){
										modToRemove = mod;
									}
									break;
								case SPR:
									if (((ItemInst1)mod).isSpr()){
										modToRemove = mod;
									}
									break;
								case SPELL:
									if (((ItemInst1)mod).isSpell()){
										modToRemove = mod;
									}
									break;
								case AUTOSPELL:
									if (((ItemInst1)mod).isAutospell()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof ItemInst2) {
								switch (inst2) {
								case CONSTLEVEL:
									if (((ItemInst2)mod).isConstlevel()){
										modToRemove = mod;
									}
									break;
								case MAINPATH:
									if (((ItemInst2)mod).isMainpath()){
										modToRemove = mod;
									}
									break;
								case MAINLEVEL:
									if (((ItemInst2)mod).isMainlevel()){
										modToRemove = mod;
									}
									break;
								case SECONDARYPATH:
									if (((ItemInst2)mod).isSecondarypath()){
										modToRemove = mod;
									}
									break;
								case SECONDARYLEVEL:
									if (((ItemInst2)mod).isSecondarylevel()){
										modToRemove = mod;
									}
									break;
								case TYPE:
									if (((ItemInst2)mod).isType()){
										modToRemove = mod;
									}
									break;
								case STR:
									if (((ItemInst2)mod).isStr()){
										modToRemove = mod;
									}
									break;
								case ATT:
									if (((ItemInst2)mod).isAtt()){
										modToRemove = mod;
									}
									break;
								case DEF:
									if (((ItemInst2)mod).isDef()){
										modToRemove = mod;
									}
									break;
								case PREC:
									if (((ItemInst2)mod).isPrec()){
										modToRemove = mod;
									}
									break;
								case MR:
									if (((ItemInst2)mod).isMr()){
										modToRemove = mod;
									}
									break;
								case MORALE:
									if (((ItemInst2)mod).isMorale()){
										modToRemove = mod;
									}
									break;									
								case VOIDSANITY:
									if (((ItemInst2)mod).isVoidsanity()){
										modToRemove = mod;
									}
									break;
								case GIFTOFWATER:
									if (((ItemInst2)mod).isGiftofwater()){
										modToRemove = mod;
									}
									break;
								case INVULNERABLE:
									if (((ItemInst2)mod).isInvulnerable()){
										modToRemove = mod;
									}
									break;
								case INSPIRINGRES:
									if (((ItemInst2)mod).isInspiringres()){
										modToRemove = mod;
									}
									break;
								case FIRERES:
									if (((ItemInst2)mod).isFireres()){
										modToRemove = mod;
									}
									break;
								case COLDRES:
									if (((ItemInst2)mod).isColdres()){
										modToRemove = mod;
									}
									break;
								case SHOCKRES:
									if (((ItemInst2)mod).isShockres()){
										modToRemove = mod;
									}
									break;
								case POISONRES:
									if (((ItemInst2)mod).isPoisonres()){
										modToRemove = mod;
									}
									break;
								case RESTRICTED:
									if (((ItemInst2)mod).isRestricted()){
										modToRemove = mod;
									}
									break;
								case PEN:
									if (((ItemInst2)mod).isPen()){
										modToRemove = mod;
									}
									break;
								case AUTOSPELLREPEAT:
									if (((ItemInst2)mod).isAutospellrepeat()){
										modToRemove = mod;
									}
									break;
								case RANDOMSPELL:
									if (((ItemInst2)mod).isRandomspell()){
										modToRemove = mod;
									}
									break;
								case MAPSPEED:
									if (((ItemInst2)mod).isMapspeed()){
										modToRemove = mod;
									}
									break;
								case TAINTED:
									if (((ItemInst2)mod).isTainted()){
										modToRemove = mod;
									}
									break;
								case SPECIALLOOK:
									if (((ItemInst2)mod).isSpeciallook()){
										modToRemove = mod;
									}
									break;
								case SEDUCE:
									if (((ItemInst2)mod).isSeduce()){
										modToRemove = mod;
									}
									break;
								case SUCCUBUS:
									if (((ItemInst2)mod).isSuccubus()){
										modToRemove = mod;
									}
									break;
								case BECKON:
									if (((ItemInst2)mod).isBeckon()){
										modToRemove = mod;
									}
									break;
								case FALSEARMY:
									if (((ItemInst2)mod).isFalsearmy()){
										modToRemove = mod;
									}
									break;
								case FOOLSCOUTS:
									if (((ItemInst2)mod).isFoolscouts()){
										modToRemove = mod;
									}
									break;
								case ICEPROT:
									if (((ItemInst2)mod).isIceprot()){
										modToRemove = mod;
									}
									break;
								case HEALER:
									if (((ItemInst2)mod).isHealer()){
										modToRemove = mod;
									}
									break;
								case AUTOHEALER:
									if (((ItemInst2)mod).isAutohealer()){
										modToRemove = mod;
									}
									break;
								case AUTODISHEALER:
									if (((ItemInst2)mod).isAutodishealer()){
										modToRemove = mod;
									}
									break;
								case AUTODISGRINDER:
									if (((ItemInst2)mod).isAutodisgrinder()){
										modToRemove = mod;
									}
									break;
								case HOMESICK:
									if (((ItemInst2)mod).isHomesick()){
										modToRemove = mod;
									}
									break;
								case UWDAMAGE:
									if (((ItemInst2)mod).isUwdamage()){
										modToRemove = mod;
									}
									break;
								case REGENERATION:
									if (((ItemInst2)mod).isRegeneration()){
										modToRemove = mod;
									}
									break;
								case REINVIGORATION:
									if (((ItemInst2)mod).isReinvigoration()){
										modToRemove = mod;
									}
									break;
								case WOUNDFEND:
									if (((ItemInst2)mod).isWoundfend()){
										modToRemove = mod;
									}
									break;
								case POISONCLOUD:
									if (((ItemInst2)mod).isPoisoncloud()){
										modToRemove = mod;
									}
									break;
								case DISEASECLOUD:
									if (((ItemInst2)mod).isDiseasecloud()){
										modToRemove = mod;
									}
									break;
								case ANIMALAWE:
									if (((ItemInst2)mod).isAnimalawe()){
										modToRemove = mod;
									}
									break;
								case AWE:
									if (((ItemInst2)mod).isAwe()){
										modToRemove = mod;
									}
									break;
								case FEAR:
									if (((ItemInst2)mod).isFear()){
										modToRemove = mod;
									}
									break;
								case FIRESHIELD:
									if (((ItemInst2)mod).isFireshield()){
										modToRemove = mod;
									}
									break;
								case BANEFIRESHIELD:
									if (((ItemInst2)mod).isBanefireshield()){
										modToRemove = mod;
									}
									break;
								case DAMAGEREV:
									if (((ItemInst2)mod).isDamagerev()){
										modToRemove = mod;
									}
									break;
								case BLOODVENGEANCE:
									if (((ItemInst2)mod).isBloodvengeance()){
										modToRemove = mod;
									}
									break;
								case SLIMER:
									if (((ItemInst2)mod).isSlimer()){
										modToRemove = mod;
									}
									break;
								case DEATHDISEASE:
									if (((ItemInst2)mod).isDeathdisease()){
										modToRemove = mod;
									}
									break;
								case DEATHPARALYZE:
									if (((ItemInst2)mod).isDeathparalyze()){
										modToRemove = mod;
									}
									break;
								case DEATHFIRE:
									if (((ItemInst2)mod).isDeathfire()){
										modToRemove = mod;
									}
									break;
								case CHAOSPOWER:
									if (((ItemInst2)mod).isChaospower()){
										modToRemove = mod;
									}
									break;
								case FIREPOWER:
									if (((ItemInst2)mod).isFirepower()){
										modToRemove = mod;
									}
									break;
								case COLDPOWER:
									if (((ItemInst2)mod).isColdpower()){
										modToRemove = mod;
									}
									break;
								case MAGICPOWER:
									if (((ItemInst2)mod).isMagicpower()){
										modToRemove = mod;
									}
									break;
								case STORMPOWER:
									if (((ItemInst2)mod).isStormpower()){
										modToRemove = mod;
									}
									break;
								case DARKPOWER:
									if (((ItemInst2)mod).isDarkpower()){
										modToRemove = mod;
									}
									break;
								case SPRINGPOWER:
									if (((ItemInst2)mod).isSpringpower()){
										modToRemove = mod;
									}
									break;
								case SUMMERPOWER:
									if (((ItemInst2)mod).isSummerpower()){
										modToRemove = mod;
									}
									break;
								case FALLPOWER:
									if (((ItemInst2)mod).isFallpower()){
										modToRemove = mod;
									}
									break;
								case WINTERPOWER:
									if (((ItemInst2)mod).isWinterpower()){
										modToRemove = mod;
									}
									break;
								case AMBIDEXTROUS:
									if (((ItemInst2)mod).isAmbidextrous()){
										modToRemove = mod;
									}
									break;
								case BERSERK:
									if (((ItemInst2)mod).isBerserk()){
										modToRemove = mod;
									}
									break;
								case DARKVISION:
									if (((ItemInst2)mod).isDarkvision()){
										modToRemove = mod;
									}
									break;
								case DIGEST:
									if (((ItemInst2)mod).isDigest()){
										modToRemove = mod;
									}
									break;
								case INCORPORATE:
									if (((ItemInst2)mod).isIncorporate()){
										modToRemove = mod;
									}
									break;
								case CASTLEDEF:
									if (((ItemInst2)mod).isCastledef()){
										modToRemove = mod;
									}
									break;
								case SIEGEBONUS:
									if (((ItemInst2)mod).isSiegebonus()){
										modToRemove = mod;
									}
									break;
								case PATROLBONUS:
									if (((ItemInst2)mod).isPatrolbonus()){
										modToRemove = mod;
									}
									break;
								case PILLAGEBONUS:
									if (((ItemInst2)mod).isPillagebonus()){
										modToRemove = mod;
									}
									break;
								case SUPPLYBONUS:
									if (((ItemInst2)mod).isSupplybonus()){
										modToRemove = mod;
									}
									break;
								case NOBADEVENTS:
									if (((ItemInst2)mod).isNobadevents()){
										modToRemove = mod;
									}
									break;
								case INCPROVDEF:
									if (((ItemInst2)mod).isIncprovdef()){
										modToRemove = mod;
									}
									break;
								case INCUNREST:
									if (((ItemInst2)mod).isIncunrest()){
										modToRemove = mod;
									}
									break;
								case LEPER:
									if (((ItemInst2)mod).isLeper()){
										modToRemove = mod;
									}
									break;
								case POPKILL:
									if (((ItemInst2)mod).isPopkill()){
										modToRemove = mod;
									}
									break;
								case HERETIC:
									if (((ItemInst2)mod).isHeretic()){
										modToRemove = mod;
									}
									break;
								case ELEGIST:
									if (((ItemInst2)mod).isElegist()){
										modToRemove = mod;
									}
									break;
								case SPREADDOM:
									if (((ItemInst2)mod).isSpreaddom()){
										modToRemove = mod;
									}
									break;
								case SHATTEREDSOUL:
									if (((ItemInst2)mod).isShatteredsoul()){
										modToRemove = mod;
									}
									break;
								case GOLD:
									if (((ItemInst2)mod).isGold()){
										modToRemove = mod;
									}
									break;
								case INSPIRATIONAL:
									if (((ItemInst2)mod).isInspirational()){
										modToRemove = mod;
									}
									break;
								case BEASTMASTER:
									if (((ItemInst2)mod).isBeastmaster()){
										modToRemove = mod;
									}
									break;
								case TASKMASTER:
									if (((ItemInst2)mod).isTaskmaster()){
										modToRemove = mod;
									}
									break;
								case FORMATIONFIGHTER:
									if (((ItemInst2)mod).isFormationfighter()){
										modToRemove = mod;
									}
									break;
								case BODYGUARD:
									if (((ItemInst2)mod).isBodyguard()){
										modToRemove = mod;
									}
									break;
								case STANDARD:
									if (((ItemInst2)mod).isStandard()){
										modToRemove = mod;
									}
									break;
								case DOUSE:
									if (((ItemInst2)mod).isDouse()){
										modToRemove = mod;
									}
									break;
								case RESEARCHBONUS:
									if (((ItemInst2)mod).isResearchbonus()){
										modToRemove = mod;
									}
									break;
								case DIVINEINS:
									if (((ItemInst2)mod).isDivineins()){
										modToRemove = mod;
									}
									break;
								case FORGEBONUS:
									if (((ItemInst2)mod).isForgebonus()){
										modToRemove = mod;
									}
									break;
								case FIXFORGEBONUS:
									if (((ItemInst2)mod).isFixforgebonus()){
										modToRemove = mod;
									}
									break;
								case CROSSBREEDER:
									if (((ItemInst2)mod).isCrossbreeder()){
										modToRemove = mod;
									}
									break;
								case BONUSSPELLS:
									if (((ItemInst2)mod).isBonusspells()){
										modToRemove = mod;
									}
									break;
								case DEATHBANISH:
									if (((ItemInst2)mod).isDeathbanish()){
										modToRemove = mod;
									}
									break;
								case KOKYTOSRET:
									if (((ItemInst2)mod).isKokytosret()){
										modToRemove = mod;
									}
									break;
								case INFERNORET:
									if (((ItemInst2)mod).isInfernoret()){
										modToRemove = mod;
									}
									break;
								case VOIDRET:
									if (((ItemInst2)mod).isVoidret()){
										modToRemove = mod;
									}
									break;
								case ALLRET:
									if (((ItemInst2)mod).isAllret()){
										modToRemove = mod;
									}
									break;
								case FIRERANGE:
									if (((ItemInst2)mod).isFirerange()){
										modToRemove = mod;
									}
									break;
								case AIRRANGE:
									if (((ItemInst2)mod).isAirrange()){
										modToRemove = mod;
									}
									break;
								case WATERRANGE:
									if (((ItemInst2)mod).isWaterrange()){
										modToRemove = mod;
									}
									break;
								case EARTHRANGE:
									if (((ItemInst2)mod).isEarthrange()){
										modToRemove = mod;
									}
									break;
								case ASTRALRANGE:
									if (((ItemInst2)mod).isAstralrange()){
										modToRemove = mod;
									}
									break;
								case DEATHRANGE:
									if (((ItemInst2)mod).isDeathrange()){
										modToRemove = mod;
									}
									break;
								case NATURERANGE:
									if (((ItemInst2)mod).isNaturerange()){
										modToRemove = mod;
									}
									break;
								case BLOODRANGE:
									if (((ItemInst2)mod).isBloodrange()){
										modToRemove = mod;
									}
									break;
								case ELEMENTRANGE:
									if (((ItemInst2)mod).isElementrange()){
										modToRemove = mod;
									}
									break;
								case SORCERYRANGE:
									if (((ItemInst2)mod).isSorceryrange()){
										modToRemove = mod;
									}
									break;
								case ALLRANGE:
									if (((ItemInst2)mod).isAllrange()){
										modToRemove = mod;
									}
									break;
								case MAKEPEARLS:
									if (((ItemInst2)mod).isMakepearls()){
										modToRemove = mod;
									}
									break;
								case TMPFIREGEMS:
									if (((ItemInst2)mod).isTmpfiregems()){
										modToRemove = mod;
									}
									break;
								case TMPAIRGEMS:
									if (((ItemInst2)mod).isTmpairgems()){
										modToRemove = mod;
									}
									break;
								case TMPWATERGEMS:
									if (((ItemInst2)mod).isTmpwatergems()){
										modToRemove = mod;
									}
									break;
								case TMPEARTHGEMS:
									if (((ItemInst2)mod).isTmpearthgems()){
										modToRemove = mod;
									}
									break;
								case TMPASTRALGEMS:
									if (((ItemInst2)mod).isTmpastralgems()){
										modToRemove = mod;
									}
									break;
								case TMPDEATHGEMS:
									if (((ItemInst2)mod).isTmpdeathgems()){
										modToRemove = mod;
									}
									break;
								case TMPNATUREGEMS:
									if (((ItemInst2)mod).isTmpnaturegems()){
										modToRemove = mod;
									}
									break;
								case TMPBLOODSLAVES:
									if (((ItemInst2)mod).isTmpbloodslaves()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof ItemInst3) {
								switch (inst2) {
								case COPYSPR:
									if (((ItemInst3)mod).isCopyspr()){
										modToRemove = mod;
									}
									break;
								case WEAPON:
									if (((ItemInst3)mod).isWeapon()){
										modToRemove = mod;
									}
									break;
								case COPYITEM:
									if (((ItemInst3)mod).isCopyitem()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON:
									if (((ItemInst3)mod).isDomsummon()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON2:
									if (((ItemInst3)mod).isDomsummon2()){
										modToRemove = mod;
									}
									break;
								case DOMSUMMON20:
									if (((ItemInst3)mod).isDomsummon20()){
										modToRemove = mod;
									}
									break;
								case RAREDOMSUMMON:
									if (((ItemInst3)mod).isRaredomsummon()){
										modToRemove = mod;
									}
									break;
								case SUMMON1:
									if (((ItemInst3)mod).isSummon1()){
										modToRemove = mod;
									}
									break;
								case SUMMON2:
									if (((ItemInst3)mod).isSummon2()){
										modToRemove = mod;
									}
									break;
								case SUMMON3:
									if (((ItemInst3)mod).isSummon3()){
										modToRemove = mod;
									}
									break;
								case SUMMON4:
									if (((ItemInst3)mod).isSummon4()){
										modToRemove = mod;
									}
									break;
								case SUMMON5:
									if (((ItemInst3)mod).isSummon5()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTERS1:
									if (((ItemInst3)mod).isMakemonsters1()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTERS2:
									if (((ItemInst3)mod).isMakemonsters2()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTERS3:
									if (((ItemInst3)mod).isMakemonsters3()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTERS4:
									if (((ItemInst3)mod).isMakemonsters4()){
										modToRemove = mod;
									}
									break;
								case MAKEMONSTERS5:
									if (((ItemInst3)mod).isMakemonsters5()){
										modToRemove = mod;
									}
									break;
								case BATTLESUM1:
									if (((ItemInst3)mod).isBattlesum1()){
										modToRemove = mod;
									}
									break;
								case BATTLESUM2:
									if (((ItemInst3)mod).isBattlesum2()){
										modToRemove = mod;
									}
									break;
								case BATTLESUM3:
									if (((ItemInst3)mod).isBattlesum3()){
										modToRemove = mod;
									}
									break;
								case BATTLESUM4:
									if (((ItemInst3)mod).isBattlesum4()){
										modToRemove = mod;
									}
									break;
								case BATTLESUM5:
									if (((ItemInst3)mod).isBattlesum5()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM1:
									if (((ItemInst3)mod).isBatstartsum1()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM2:
									if (((ItemInst3)mod).isBatstartsum2()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM3:
									if (((ItemInst3)mod).isBatstartsum3()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM4:
									if (((ItemInst3)mod).isBatstartsum4()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM5:
									if (((ItemInst3)mod).isBatstartsum5()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM1D6:
									if (((ItemInst3)mod).isBatstartsum1d6()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM2D6:
									if (((ItemInst3)mod).isBatstartsum2d6()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM3D6:
									if (((ItemInst3)mod).isBatstartsum3d6()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM4D6:
									if (((ItemInst3)mod).isBatstartsum4d6()){
										modToRemove = mod;
									}
									break;
								case BATSTARTSUM5D6:
									if (((ItemInst3)mod).isBatstartsum5d6()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof ItemInst4) {
								switch (inst2) {
								case ETHEREAL:
									if (((ItemInst4)mod).isEthereal()){
										modToRemove = mod;
									}
									break;
								case NOMOUNTED:
									if (((ItemInst4)mod).isNomounted()){
										modToRemove = mod;
									}
									break;
								case NOCOLDBLOOD:
									if (((ItemInst4)mod).isNocoldblood()){
										modToRemove = mod;
									}
									break;
								case NODEMON:
									if (((ItemInst4)mod).isNodemon()){
										modToRemove = mod;
									}
									break;
								case NOUNDEAD:
									if (((ItemInst4)mod).isNoundead()){
										modToRemove = mod;
									}
									break;
								case NOINANIM:
									if (((ItemInst4)mod).isNoinanim()){
										modToRemove = mod;
									}
									break;
								case NOFEMALE:
									if (((ItemInst4)mod).isNofemale()){
										modToRemove = mod;
									}
									break;
								case ONLYMOUNTED:
									if (((ItemInst4)mod).isOnlymounted()){
										modToRemove = mod;
									}
									break;
								case ONLYCOLDBLOOD:
									if (((ItemInst4)mod).isOnlycoldblood()){
										modToRemove = mod;
									}
									break;
								case ONLYDEMON:
									if (((ItemInst4)mod).isOnlydemon()){
										modToRemove = mod;
									}
									break;
								case ONLYUNDEAD:
									if (((ItemInst4)mod).isOnlyundead()){
										modToRemove = mod;
									}
									break;
								case ONLYINANIM:
									if (((ItemInst4)mod).isOnlyinanim()){
										modToRemove = mod;
									}
									break;
								case ONLYFEMALE:
									if (((ItemInst4)mod).isOnlyfemale()){
										modToRemove = mod;
									}
									break;
								case REQEYES:
									if (((ItemInst4)mod).isReqeyes()){
										modToRemove = mod;
									}
									break;
								case NOFIND:
									if (((ItemInst4)mod).isNofind()){
										modToRemove = mod;
									}
									break;
								case LUCK:
									if (((ItemInst4)mod).isLuck()){
										modToRemove = mod;
									}
									break;
								case QUICKNESS:
									if (((ItemInst4)mod).isQuickness()){
										modToRemove = mod;
									}
									break;
								case BLESS:
									if (((ItemInst4)mod).isBless()){
										modToRemove = mod;
									}
									break;
								case BARKSKIN:
									if (((ItemInst4)mod).isBarkskin()){
										modToRemove = mod;
									}
									break;
								case STONESKIN:
									if (((ItemInst4)mod).isStoneskin()){
										modToRemove = mod;
									}
									break;
								case IRONSKIN:
									if (((ItemInst4)mod).isIronskin()){
										modToRemove = mod;
									}
									break;
								case WATERBREATHING:
									if (((ItemInst4)mod).isWaterbreathing()){
										modToRemove = mod;
									}
									break;
								case FLOAT:
									if (((ItemInst4)mod).isFloat()){
										modToRemove = mod;
									}
									break;
								case FLY:
									if (((ItemInst4)mod).isFly()){
										modToRemove = mod;
									}
									break;
								case STORMIMMUNE:
									if (((ItemInst4)mod).isStormimmune()){
										modToRemove = mod;
									}
									break;
								case RUN:
									if (((ItemInst4)mod).isRun()){
										modToRemove = mod;
									}
									break;
								case TRAMPLE:
									if (((ItemInst4)mod).isTrample()){
										modToRemove = mod;
									}
									break;
								case BERS:
									if (((ItemInst4)mod).isBers()){
										modToRemove = mod;
									}
									break;
								case EXTRALIFE:
									if (((ItemInst4)mod).isExtralife()){
										modToRemove = mod;
									}
									break;
								case CHAMPPRIZE:
									if (((ItemInst4)mod).isChampprize()){
										modToRemove = mod;
									}
									break;
								case AUTOCOMPETE:
									if (((ItemInst4)mod).isAutocompete()){
										modToRemove = mod;
									}
									break;
								case CURSED:
									if (((ItemInst4)mod).isCursed()){
										modToRemove = mod;
									}
									break;
								case CURSE:
									if (((ItemInst4)mod).isCurse()){
										modToRemove = mod;
									}
									break;
								case DISEASE:
									if (((ItemInst4)mod).isDisease()){
										modToRemove = mod;
									}
									break;
								case CHESTWOUND:
									if (((ItemInst4)mod).isChestwound()){
										modToRemove = mod;
									}
									break;
								case FEEBLEMIND:
									if (((ItemInst4)mod).isFeeblemind()){
										modToRemove = mod;
									}
									break;
								case MUTE:
									if (((ItemInst4)mod).isMute()){
										modToRemove = mod;
									}
									break;
								case NHWOUND:
									if (((ItemInst4)mod).isNhwound()){
										modToRemove = mod;
									}
									break;
								case CRIPPLED:
									if (((ItemInst4)mod).isCrippled()){
										modToRemove = mod;
									}
									break;
								case LOSEEYE:
									if (((ItemInst4)mod).isLoseeye()){
										modToRemove = mod;
									}
									break;
								case SINGLEBATTLE:
									if (((ItemInst4)mod).isSinglebattle()){
										modToRemove = mod;
									}
									break;
								case CHAOSREC:
									if (((ItemInst4)mod).isChaosrec()){
										modToRemove = mod;
									}
									break;
								case STONEBEING:
									if (((ItemInst4)mod).isStonebeing()){
										modToRemove = mod;
									}
									break;
								case NORIVERPASS:
									if (((ItemInst4)mod).isNoriverpass()){
										modToRemove = mod;
									}
									break;
								case UNTELEPORTABLE:
									if (((ItemInst4)mod).isUnteleportable()){
										modToRemove = mod;
									}
									break;
								case SLASHRES:
									if (((ItemInst4)mod).isSlashres()){
										modToRemove = mod;
									}
									break;
								case PIERCERES:
									if (((ItemInst4)mod).isPierceres()){
										modToRemove = mod;
									}
									break;
								case BLUNTRES:
									if (((ItemInst4)mod).isBluntres()){
										modToRemove = mod;
									}
									break;
								case HPOVERFLOW:
									if (((ItemInst4)mod).isHpoverflow()){
										modToRemove = mod;
									}
									break;
								case DEATHCURSE:
									if (((ItemInst4)mod).isDeathcurse()){
										modToRemove = mod;
									}
									break;
								case TRAMPSWALLOW:
									if (((ItemInst4)mod).isTrampswallow()){
										modToRemove = mod;
									}
									break;
								case INQUISITOR:
									if (((ItemInst4)mod).isInquisitor()){
										modToRemove = mod;
									}
									break;
								case TAXCOLLECTOR:
									if (((ItemInst4)mod).isTaxcollector()){
										modToRemove = mod;
									}
									break;
								case UNDISCIPLINED:
									if (((ItemInst4)mod).isUndisciplined()){
										modToRemove = mod;
									}
									break;
								case DRAINIMMUNE:
									if (((ItemInst4)mod).isDrainimmune()){
										modToRemove = mod;
									}
									break;
								case MAGICIMMUNE:
									if (((ItemInst4)mod).isMagicimmune()){
										modToRemove = mod;
									}
									break;
								case COMSLAVE:
									if (((ItemInst4)mod).isComslave()){
										modToRemove = mod;
									}
									break;
								}
							}
							if (mod instanceof ItemInst5) {
								switch (inst2) {
								case MAGICBOOST1:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 1) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST2:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 2) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST3:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 3) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST4:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 4) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST5:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 5) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST6:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 6) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST7:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 7) {
											modToRemove = mod;
										}
									}
									break;
								case MAGICBOOST8:
									if (((ItemInst5)mod).isMagicboost()){
										boostCount++;
										if (boostCount == 8) {
											modToRemove = mod;
										}
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
