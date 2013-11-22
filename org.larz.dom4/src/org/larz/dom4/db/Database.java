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
package org.larz.dom4.db;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DecimalFormat;
import java.text.Format;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.larz.dom4.Activator;

/**
 *
 */
public class Database {
	private static Connection connection = null;
	private static Map<Integer, String> monsterNameMap = new HashMap<Integer, String>();
	private static Map<Integer, MonsterDB> monsterDBIdMap = new HashMap<Integer, MonsterDB>();
	private static Map<String, MonsterDB> monsterDBNameMap = new HashMap<String, MonsterDB>();
	private static Map<Integer, String> monsterDescIdMap = new HashMap<Integer, String>();
	
	private static Map<Integer, String> armorNameMap = new HashMap<Integer, String>();
	private static Map<Integer, ArmorDB> armorDBIdMap = new HashMap<Integer, ArmorDB>();
	private static Map<String, ArmorDB> armorDBNameMap = new HashMap<String, ArmorDB>();

	private static Map<Integer, String> weaponNameMap = new HashMap<Integer, String>();
	private static Map<Integer, WeaponDB> weaponDBIdMap = new HashMap<Integer, WeaponDB>();
	private static Map<String, WeaponDB> weaponDBNameMap = new HashMap<String, WeaponDB>();

	private static Map<Integer, String> itemNameMap = new HashMap<Integer, String>();
	private static Map<Integer, ItemDB> itemDBIdMap = new HashMap<Integer, ItemDB>();
	private static Map<String, ItemDB> itemDBNameMap = new HashMap<String, ItemDB>();
	private static Map<String, String> itemDescrNameMap = new HashMap<String, String>();

	private static Map<Integer, String> siteNameMap = new HashMap<Integer, String>();
	private static Map<Integer, SiteDB> siteDBIdMap = new HashMap<Integer, SiteDB>();
	private static Map<String, SiteDB> siteDBNameMap = new HashMap<String, SiteDB>();

	private static Map<Integer, String> spellNameMap = new HashMap<Integer, String>();
	private static Map<Integer, SpellDB> spellDBIdMap = new HashMap<Integer, SpellDB>();
	private static Map<String, SpellDB> spellDBNameMap = new HashMap<String, SpellDB>();
	private static Map<String, String> spellDescrNameMap = new HashMap<String, String>();

	private static Map<Integer, String> nationNameMap = new HashMap<Integer, String>();
	private static Map<Integer, NationDB> nationDBIdMap = new HashMap<Integer, NationDB>();

	private static Format numberFormat = new DecimalFormat("0000");

	public static List<IDNameDB> getAllArmor() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"armorname\" FROM \"armor_base\"");

			while (rs.next())
			{
				IDNameDB armor = new IDNameDB();
				armor.id = rs.getInt("id");
				armor.name = rs.getString("armorname");
				list.add(armor);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllWeapon() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"weapon_name\" FROM \"weapons_base\"");

			while (rs.next())
			{
				IDNameDB weapon = new IDNameDB();
				weapon.id = rs.getInt("id");
				weapon.name = rs.getString("weapon_name");
				list.add(weapon);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllMonster() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"unitname\" FROM \"units_base\"");

			while (rs.next())
			{
				IDNameDB armor = new IDNameDB();
				armor.id = rs.getInt("id");
				armor.name = rs.getString("unitname");
				list.add(armor);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllItem() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"name\" FROM \"items\"");

			while (rs.next())
			{
				IDNameDB armor = new IDNameDB();
				armor.id = rs.getInt("id");
				armor.name = rs.getString("name");
				list.add(armor);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllSite() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"name\" FROM \"magic_sites\"");

			while (rs.next())
			{
				IDNameDB armor = new IDNameDB();
				armor.id = rs.getInt("id");
				armor.name = rs.getString("name");
				list.add(armor);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllNation() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"name\" FROM \"nations\"");

			while (rs.next())
			{
				IDNameDB nation = new IDNameDB();
				nation.id = rs.getInt("id");
				nation.name = rs.getString("name");
				list.add(nation);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static List<IDNameDB> getAllSpell() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"name\" FROM \"spells\"");

			while (rs.next())
			{
				IDNameDB spell = new IDNameDB();
				spell.id = rs.getInt("id");
				spell.name = rs.getString("name");
				list.add(spell);
			}

			statement.close();

		} catch (SQLException ex) {
			ex.printStackTrace();
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return list;
	}
	
	public static ArmorDB getArmor(int id) {
		ArmorDB armor = armorDBIdMap.get(id);
		if (armor == null) {
			armor = new ArmorDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"armor_base\" where \"id\"="+id);
				armor = getArmorDB(rs);
				statement.close();
				armorDBIdMap.put(id, armor);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return armor;
	}
	
	public static ArmorDB getArmor(String name) {
		ArmorDB armor = armorDBNameMap.get(name);
		if (armor == null) {
			armor = new ArmorDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"armor_base\" where \"armorname\" = '"+getSafeString(name)+"'");
				armor = getArmorDB(rs);
				statement.close();
				armorDBNameMap.put(name, armor);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return armor;
	}
	
	private static String getSafeString(String str) {
		str = str.replaceAll("'", "''");
		return str;
	}

	private static ArmorDB getArmorDB(ResultSet rs) throws SQLException {
		ArmorDB armor = new ArmorDB();
		if (rs.next()) {	
			armor.name = rs.getString("armorname");
			armor.id = rs.getInt("id");
			armor.def = rs.getInt("def");
			armor.enc = rs.getInt("enc");
			armor.rcost = rs.getInt("res");
			armor.type = rs.getInt("type");
			Integer head = rs.getInt("head");
			Integer body = rs.getInt("body");
			Integer shield = rs.getInt("shield");
			if (armor.type != null && armor.type == 4) {
				armor.prot = shield;
			} else if (armor.type != null && armor.type == 5) {
				armor.prot = body;
			} else if (armor.type != null) {
				armor.prot = head;
			}
		}
		return armor;

	}
	
	public static WeaponDB getWeapon(int id) {
		WeaponDB weapon = weaponDBIdMap.get(id);
		if (weapon == null) {
			weapon = new WeaponDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"weapons_base\" where \"id\"="+id);
				weapon = getWeaponDB(rs);
				statement.close();
				weaponDBIdMap.put(id, weapon);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return weapon;
	}

	public static WeaponDB getWeapon(String name) {
		WeaponDB weapon = weaponDBNameMap.get(name);
		if (weapon == null) {
			weapon = new WeaponDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"weapons_base\" where \"weapon_name\" = '"+getSafeString(name)+"'");
				weapon = getWeaponDB(rs);
				statement.close();
				weaponDBNameMap.put(name, weapon);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return weapon;
	}

	public static MonsterDB getMonster(int id) {
		MonsterDB monster = monsterDBIdMap.get(id);
		if (monster == null) {
			monster = new MonsterDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"units_base\" where \"id\"="+id);
				monster = getMonsterDB(rs);
				statement.close();
				monsterDBIdMap.put(id, monster);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return monster;
	}

	public static MonsterDB getMonster(String name) {
		MonsterDB monster = monsterDBNameMap.get(name);
		if (monster == null) {
			monster = new MonsterDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"units_base\" where \"unitname\" = '"+getSafeString(name)+"'");
				monster = getMonsterDB(rs);
				statement.close();
				monsterDBNameMap.put(name, monster);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return monster;
	}
	
	public static SiteDB getSite(int id) {
		SiteDB site = siteDBIdMap.get(id);
		if (site == null) {
			site = new SiteDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"magic_sites\" where \"id\"="+id);
				site = getSiteDB(rs);
				statement.close();
				siteDBIdMap.put(id, site);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return site;
	}

	public static SiteDB getSite(String name) {
		SiteDB site = siteDBNameMap.get(name);
		if (site == null) {
			site = new SiteDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"magic_sites\" where \"name\" = '"+getSafeString(name)+"'");
				site = getSiteDB(rs);
				statement.close();
				siteDBNameMap.put(name, site);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return site;
	}
	
	public static SpellDB getSpell(int id) {
		SpellDB spell = spellDBIdMap.get(id);
		if (spell == null) {
			spell = new SpellDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"spells\" where \"id\"="+id);
				spell = getSpellDB(rs);
				statement.close();
				spellDBIdMap.put(id, spell);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return spell;
	}

	public static SpellDB getSpell(String name) {
		SpellDB spell = spellDBIdMap.get(name);
		if (spell == null) {
			spell = new SpellDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"spells\" where \"name\" = '"+getSafeString(name)+"'");
				spell = getSpellDB(rs);
				statement.close();
				spellDBNameMap.put(name, spell);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return spell;
	}
	
	public static NationDB getNation(int id) {
		NationDB nation = nationDBIdMap.get(id);
		if (nation == null) {
			nation = new NationDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"nations\" where \"id\"="+id);

				nation = getNationDB(rs);
				statement.close();
				nationDBIdMap.put(id, nation);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return nation;
	}
	
	private static SiteDB getSiteDB(ResultSet rs) throws SQLException {
		SiteDB site = new SiteDB();
		if (rs.next()) {
			site.id = rs.getInt("id");
			site.name = rs.getString("name");
			String type = rs.getString("type");
			if (type != null) {
				if (type.equals("Fire")) {
					site.path = 0;
				} else if (type.equals("Water")) {
					site.path = 2;
				} else if (type.equals("Earth")) {
					site.path = 3;
				} else if (type.equals("Astral")) {
					site.path = 4;
				} else if (type.equals("Nature")) {
					site.path = 6;
				} else if (type.equals("Air")) {
					site.path = 1;
				} else if (type.equals("Death")) {
					site.path = 5;
				} else if (type.equals("Blood")) {
					site.path = 7;
				} else if (type.equals("Holy")) {
					site.path = 8;
				}
			}
			site.level = rs.getInt("lvl");
			site.rarity = rs.getInt("frq");
			site.loc = rs.getInt("mask");
			site.gold = rs.getInt("gold");
			site.res = rs.getInt("res");
			
			String F = rs.getString("F");
			String W = rs.getString("W");
			String A = rs.getString("A");
			String E = rs.getString("E");
			String S = rs.getString("S");
			String D = rs.getString("D");
			String N = rs.getString("N");
			String B = rs.getString("B");

			List<Integer[]> gem = new ArrayList<Integer[]>();
			if (F != null && !F.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(F), Integer.valueOf(0)});
			}
			if (W != null && !W.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(W), Integer.valueOf(2)});
			}
			if (A != null && !A.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(A), Integer.valueOf(1)});
			}
			if (E != null && !E.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(E), Integer.valueOf(3)});
			}
			if (S != null && !S.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(S), Integer.valueOf(4)});
			}
			if (D != null && !D.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(D), Integer.valueOf(5)});
			}
			if (N != null && !N.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(N), Integer.valueOf(6)});
			}
			if (B != null && !B.equals("")) {
				gem.add(new Integer[]{Integer.valueOf(B), Integer.valueOf(7)});
			}
			
			int gemCount = 0;
			for (Integer[] gemArray : gem) {
				gemCount ++;
				switch (gemCount) {
				case 1:
					site.gemsamt1 = gemArray[0];
					site.gemspath1 = gemArray[1];
					break;
				case 2:
					site.gemsamt2 = gemArray[0];
					site.gemspath2 = gemArray[1];
					break;
				case 3:
					site.gemsamt3 = gemArray[0];
					site.gemspath3 = gemArray[1];
					break;
				}
			}
			
			String scale1 = rs.getString("scale1");
			String scale2 = rs.getString("scale2");
			if (scale1 != null) {
				if (scale1.equals("Turmoil")) {
					site.incscale1 = 0;
				} else if (scale1.equals("Order")) {
					site.decscale1 = 0;
				} else if (scale1.equals("Sloth")) {
					site.incscale1 = 1;
				} else if (scale1.equals("Productivity")) {
					site.decscale1 = 1;
				} else if (scale1.equals("Cold")) {
					site.incscale1 = 2;
				} else if (scale1.equals("Heat")) {
					site.decscale1 = 2;
				} else if (scale1.equals("Death")) {
					site.incscale1 = 3;
				} else if (scale1.equals("Growth")) {
					site.decscale1 = 3;
				} else if (scale1.equals("Misfortune")) {
					site.incscale1 = 4;
				} else if (scale1.equals("Luck")) {
					site.decscale1 = 4;
				} else if (scale1.equals("Drain")) {
					site.incscale1 = 5;
				} else if (scale1.equals("Magic")) {
					site.decscale1 = 5;
				}
			}
			if (scale2 != null) {
				if (scale2.equals("Turmoil")) {
					site.incscale2 = 0;
				} else if (scale2.equals("Order")) {
					site.decscale2 = 0;
				} else if (scale2.equals("Sloth")) {
					site.incscale2 = 1;
				} else if (scale2.equals("Productivity")) {
					site.decscale2 = 1;
				} else if (scale2.equals("Cold")) {
					site.incscale2 = 2;
				} else if (scale2.equals("Heat")) {
					site.decscale2 = 2;
				} else if (scale2.equals("Death")) {
					site.incscale2 = 3;
				} else if (scale2.equals("Growth")) {
					site.decscale2 = 3;
				} else if (scale2.equals("Misfortune")) {
					site.incscale2 = 4;
				} else if (scale2.equals("Luck")) {
					site.decscale2 = 4;
				} else if (scale2.equals("Drain")) {
					site.incscale2 = 5;
				} else if (scale2.equals("Magic")) {
					site.decscale2 = 5;
				}
			}

//			public Integer homemon;
//			public Integer homecom;
//			public Integer mon;
//			public Integer com;
//			public Integer homemon;
//			public Integer homecom;
//			public Integer mon1;
//			public Integer mon2;
//			public Integer mon3;
//			public Integer mon4;
//			public Integer mon5;
//			public Integer com1;
//			public Integer com2;
//			public Integer com3;
//			public Integer com4;
//			public Integer com5;
//			public Integer incscale1;
//			public Integer incscale2;
//			public Integer decscale1;
//			public Integer decscale2;
//			public Integer gemspath1;
//			public Integer gemsamt1;
//			public Integer gemspath2;
//			public Integer gemsamt2;
//			public Integer gemspath3;
//			public Integer gemsamt3;
//			public Boolean clear;
//			public Integer bloodcost;
//			public Integer conjcost;
//			public Integer altcost;
//			public Integer evocost;
//			public Integer constcost;
//			public Integer enchcost;
//			public Integer thaucost;	
//			public Integer decunrest;
//			public Integer supply;
//			public Integer summon;
//			public Integer voidgate;
//			public Boolean temple;
//			public Integer fort;
//			public Integer fortpart;
//			public Integer scry;
//			public Integer firerange;
//			public Integer airrange;
//			public Integer waterrange;
//			public Integer earthrange;
//			public Integer astralrange;
//			public Integer deathrange;
//			public Integer naturerange;
//			public Integer bloodrange;
//			public Integer elementrange;
//			public Integer sorceryrange;
//			public Integer allrange;
//			public Integer xp;
//			public Integer adventureruin;
//			public Boolean claim;
//			public Integer cluster;
//			public Integer dominion;
//			public Integer goddomchaos;
//			public Integer goddomlazy;
//			public Integer goddomcold;
//			public Integer goddomdeath;
//			public Integer goddommisfortune;
//			public Integer goddomdrain;
//			public Integer blesshp;
//			public Integer blessmr;
//			public Integer blessmor;
//			public Integer blessstr;
//			public Integer blessatt;
//			public Integer blessdef;
//			public Integer blessprec;
//			public Integer blessfireres;
//			public Integer blesscoldres;
//			public Integer blessshockres;
//			public Integer blesspoisres;
//			public Integer blessairshld;	
//			public Integer blessreinvig;
//			public Integer blessdtv;
//			public Integer blessanimawe;
//			public Integer blessawe;
//			public Integer blessdarkvis;
//			public Boolean evil;
//			public Boolean wild;
//			public Boolean lab;
		}
		return site;
	}

	private static NationDB getNationDB(ResultSet rs) throws SQLException {
		NationDB nation = new NationDB();
		if (rs.next()) {
			nation.id = rs.getInt("id");
			nation.name = rs.getString("name");
			nation.epithet = rs.getString("epithet");
			nation.era = rs.getInt("era");
			nation.startsite1 = getSiteName(rs.getInt("site1"));
			nation.startsite2 = getSiteName(rs.getInt("site2"));
			nation.startsite3 = getSiteName(rs.getInt("site3"));
			nation.startsite4 = getSiteName(rs.getInt("site4"));
//			int heroCount = 1;
//			int hero = rs.getInt("hero"+heroCount);
//			while (hero != 0 ) {
//				switch (heroCount) {
//				case 1:
//					nation.hero1 = hero;
//					break;
//				case 2:
//					nation.hero2 = hero;
//					break;
//				case 3:
//					nation.hero3 = hero;
//					break;
//				case 4:
//					nation.hero4 = hero;
//					break;
//				case 5:
//					nation.hero5 = hero;
//					break;
//				case 6:
//					nation.hero6 = hero;
//					break;
//				}
//				heroCount++;
//				hero = rs.getInt("hero"+heroCount);
//			}
//
//			int unitCount = 1;
//			int unit = rs.getInt("unit"+unitCount);
//			List<Integer> units = new ArrayList<Integer>();
//			while (unit != 0 && unitCount < 16) {
//				units.add(unit);
//				unitCount++;
//				unit = rs.getInt("unit"+unitCount);
//			}
//			nation.addrecunit = new Integer[units.size()];
//			for (int i = 0; i < units.size(); i++) {
//				nation.addrecunit[i] = units.get(i);
//			}
//
//			int comCount = 1;
//			int com = rs.getInt("com"+comCount);
//			List<Integer> cmdrs = new ArrayList<Integer>();
//			while (com != 0 && comCount < 12) {
//				cmdrs.add(com);
//				comCount++;
//				com = rs.getInt("com"+comCount);
//			}
//			nation.addreccom = new Integer[cmdrs.size()];
//			for (int i = 0; i < cmdrs.size(); i++) {
//				nation.addreccom[i] = cmdrs.get(i);
//			}
		}
		return nation;
	}

	private static WeaponDB getWeaponDB(ResultSet rs) throws SQLException {
		WeaponDB weapon = new WeaponDB();
		if (rs.next()) {
			weapon.id = rs.getInt("id");
			weapon.name = rs.getString("weapon_name");
			weapon.dmg = rs.getInt("dmg");
			weapon.nratt = rs.getInt("n_att");
			weapon.att = rs.getInt("att");
			weapon.def = rs.getInt("def");
			weapon.len = rs.getInt("lgt");
			weapon.range = rs.getInt("rng");
			weapon.ammo = rs.getInt("shots");
			weapon.rcost = rs.getInt("res");
			//weapon.sound = rs.getInt("");
			weapon.aoe = rs.getInt("aoe");
			weapon.secondaryeffect = rs.getInt("2nd_effect");
			weapon.secondaryeffectalways = rs.getInt("effauto");
			//weapon.explspr = rs.getInt("");
			//weapon.flyspr1 = rs.getInt("");
			//weapon.flyspr2 = rs.getInt("");
			weapon.twohanded = rs.getInt("2h") == 1;
			weapon.armorpiercing = rs.getInt("ap") == 1;
			weapon.armornegating = rs.getInt("an") == 1;
			weapon.magic = rs.getInt("magic") == 1;
			weapon.dt_normal = rs.getInt("dt_norm") == 1;
			weapon.dt_stun = rs.getInt("dt_stun") == 1;
			weapon.dt_paralyze = rs.getInt("dt_paralyze") == 1;
			weapon.dt_poison = rs.getInt("dt_poison") == 1;
			weapon.dt_cap = rs.getInt("dt_cap") == 1;
			weapon.dt_demon = rs.getInt("dt_demon") == 1;
			weapon.dt_demononly = rs.getInt("dt_demononly") == 1;
			weapon.dt_holy = rs.getInt("dt_holy") == 1;
			weapon.dt_magic = rs.getInt("dt_magic") == 1;
			weapon.dt_small = rs.getInt("dt_small") == 1;
			weapon.dt_large = rs.getInt("dt_large") == 1;
			weapon.dt_constructonly = rs.getInt("dt_constronly") == 1;
			weapon.dt_raise = rs.getInt("dt_raise") == 1;
			weapon.mind = rs.getInt("mind") == 1;
			weapon.cold = rs.getInt("cold") == 1;
			weapon.fire = rs.getInt("fire") == 1;
			weapon.shock = rs.getInt("shock") == 1;
			weapon.poison = rs.getInt("poison") == 1;
			weapon.bonus = rs.getInt("bonus") == 1;
			weapon.charge = rs.getInt("charge") == 1;
			weapon.flail = rs.getInt("flail") == 1;
			weapon.nostr = rs.getInt("nostr") == 1;
			weapon.mrnegates = rs.getInt("mres") == 1;
			//weapon.mrnegateseasily = rs.getInt("") == 1;
			weapon.slash = rs.getInt("dt_s") == 1;
			weapon.pierce = rs.getInt("dt_p") == 1;
			weapon.blunt = rs.getInt("dt_b") == 1;
			weapon.acid = rs.getInt("acid") == 1;
			//weapon.hardmrneg;
			weapon.sizeresist = rs.getInt("sizeres") == 1;
			//weapon.undeadimmune;
			//weapon.inanimateimmune;
			//weapon.flyingimmune;
			//weapon.enemyimmune;
			//weapon.friendlyimmune;
			weapon.undeadonly = rs.getInt("dt_undeadonly") == 1;
			weapon.norepel = rs.getInt("norepel") == 1;
			weapon.unrepel = rs.getInt("unrepel") == 1;
			//weapon.beam;
			//weapon.range050;
			//weapon.range0;
			//weapon.melee50;
			//weapon.skip;
			//weapon.skip2;
			
			//weapon.dt_weakness;
			weapon.dt_drain = rs.getInt("drain") == 1;
			//weapon.dt_weapondrain;
			weapon.sacredonly = rs.getInt("dt_sacred") == 1;
			//weapon.dt_sizestun;
			weapon.demonundead = rs.getInt("dt_demonundead") == 1;
		}
		return weapon;
	}
	
	private static MonsterDB getMonsterDB(ResultSet rs) throws SQLException {
		MonsterDB monster = new MonsterDB();
		if (rs.next()) {
			monster.id = rs.getInt("id");
			monster.name = rs.getString("unitname");
			//monster.spr1 = rs.getString("spr1");
			//monster.spr2 = rs.getString("spr2");
			//monster.descr = rs.getString("descr");
			monster.armor1 = rs.getInt("helm") != 0 ? Integer.toString(rs.getInt("helm")) : null;
			monster.armor2 = rs.getInt("armor") != 0 ? Integer.toString(rs.getInt("armor")) : null;
			monster.armor3 = rs.getInt("shield") != 0 ? Integer.toString(rs.getInt("shield")) : null;
			//monster.speciallook = rs.getInt("speciallook") != 0 ? rs.getInt("speciallook") : null;
			monster.ap = rs.getInt("ap");
			monster.mapmove = rs.getInt("map");
			monster.hp = rs.getInt("hp");
			monster.prot = rs.getInt("prot");
			monster.size = rs.getInt("sz");
			monster.ressize = rs.getInt("resz") != 0 ? rs.getInt("resz") : null;
			monster.str = rs.getInt("str");
			monster.enc = rs.getInt("enc");
			monster.att = rs.getInt("att");
			monster.def = rs.getInt("def");
			monster.prec = rs.getInt("prec");
			monster.mr = rs.getInt("mr");
			monster.mor = rs.getInt("mor");
			
			// TODO gold cost
			monster.gcost = rs.getInt("basecost");
			
			monster.rcost = rs.getInt("rsrc");
			monster.pathcost = rs.getInt("path");
			monster.startdom = rs.getInt("dom");
			//monster.eyes = rs.getInt("eyes") != 0 ? rs.getInt("eyes") : 2;
			//monster.copystats = rs.getInt("copystats");
			//monster.copyspr = rs.getInt("copyspr");
//			monster.restrictedgod = rs.getInt("restrictedgod");
			monster.shatteredsoul = rs.getInt("soulsh");
			monster.coldres = rs.getInt("rescold");
			monster.fireres = rs.getInt("resfire");
			monster.poisonres = rs.getInt("respois");
			monster.shockres = rs.getInt("resshck");
			monster.darkvision = rs.getInt("dv");
			monster.stealthy = rs.getInt("stealth");
			monster.seduce = rs.getInt("seduce");
			monster.succubus = rs.getInt("succubus");
			//monster.beckon = rs.getInt("beckon");
			monster.startage = rs.getInt("agestrt");
			monster.maxage = rs.getInt("ageold");
			//monster.older = rs.getInt("older");
			monster.healer = rs.getInt("heal");
			//monster.startaff = rs.getInt("startaff");
			monster.supplybonus = rs.getInt("sup");
			//monster.uwdamage = rs.getInt("uwdamage");
			monster.coldpower = rs.getInt("pwcold");
			monster.firepower = rs.getInt("pwfire");
			monster.stormpower = rs.getInt("pwstrm");
			monster.darkpower = rs.getInt("pwdark");
			monster.springpower = rs.getInt("spring");
			monster.summerpower = rs.getInt("summer");
			monster.fallpower = rs.getInt("fall");
			monster.winterpower = rs.getInt("winter");
			monster.ambidextrous = rs.getInt("adx");
			monster.banefireshield = rs.getInt("bfshld");
			monster.berserk = rs.getInt("brsrk");
			monster.standard = rs.getInt("std");
			monster.animalawe = rs.getInt("aawe");
			monster.awe = rs.getInt("awe");
			monster.fear = rs.getInt("fear");
			monster.regeneration = rs.getInt("regen");
			monster.reinvigoration = rs.getInt("inv");
			monster.fireshield = rs.getInt("fshld");
			monster.heat = rs.getInt("heat");
			monster.cold = rs.getInt("chill");
			monster.iceprot = rs.getInt("iceprot");
			monster.poisoncloud = rs.getInt("pcloud");
			monster.diseasecloud = rs.getInt("plague");
			//monster.bloodvengeance = rs.getInt("bloodvengeance");
			monster.castledef = rs.getInt("cdef");
			monster.siegebonus = rs.getInt("siege");
			monster.patrolbonus = rs.getInt("ptrl");
			monster.pillagebonus = rs.getInt("pllg");
			monster.researchbonus = rs.getInt("research");
			monster.forgebonus = rs.getInt("forge");
			monster.douse = rs.getInt("bloodsearch");
			monster.nobadevents = rs.getInt("fortune");
			monster.incunrest = rs.getInt("unrest");
			//monster.spreaddom = rs.getInt("spreaddom");
			//monster.leper = rs.getInt("leper");
			monster.popkill = rs.getInt("popkill");
			monster.heretic = rs.getInt("her");
			
			int hand = rs.getInt("hand");
			int head = rs.getInt("head");
			int body = rs.getInt("body");
			int foot = rs.getInt("foot");
			int misc = rs.getInt("misc1");
			int slots = 0;
			int handmask = 0;
			int headmask = 0;
			int bodymask = 0;
			int footmask = 0;
			int miscmask = 0;
			if (hand == 1) {
				handmask = 0x02;
			} else if (hand == 2) {
				handmask = 0x06;
			} else if (hand == 3) {
				handmask = 0x0e;
			} else if (hand == 4) {
				handmask = 0x1e;
			}
			if (head == 1) {
				headmask = 0x80;
			} else if (head == 2) {
				headmask = 0x180;
			}
			if (body == 1) {
				bodymask = 0x400;
			}
			if (foot == 1) {
				footmask = 0x800;
			}
			if (misc == 1) {
				miscmask = 0x1000;
			} else if (misc == 2) {
				miscmask = 0x3000;
			} else if (misc == 3) {
				miscmask = 0x7000;
			} else if (misc == 4) {
				miscmask = 0xf000;
			}
			slots |= handmask;
			slots |= headmask;
			slots |= bodymask;
			slots |= footmask;
			slots |= miscmask;
			monster.itemslots = slots;
			
			monster.nametype = rs.getInt("nametype");
			
			String F = rs.getString("F");
			String W = rs.getString("W");
			String A = rs.getString("A");
			String E = rs.getString("E");
			String S = rs.getString("S");
			String D = rs.getString("D");
			String N = rs.getString("N");
			String B = rs.getString("B");
			String H = rs.getString("H");

			List<Integer[]> magicSkill = new ArrayList<Integer[]>();
			if (F != null && !F.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(F), Integer.valueOf(0)});
			}
			if (W != null && !W.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(W), Integer.valueOf(2)});
			}
			if (A != null && !A.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(A), Integer.valueOf(1)});
			}
			if (E != null && !E.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(E), Integer.valueOf(3)});
			}
			if (S != null && !S.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(S), Integer.valueOf(4)});
			}
			if (D != null && !D.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(D), Integer.valueOf(5)});
			}
			if (N != null && !N.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(N), Integer.valueOf(6)});
			}
			if (B != null && !B.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(B), Integer.valueOf(7)});
			}
			if (H != null && !H.equals("")) {
				magicSkill.add(new Integer[]{Integer.valueOf(H), Integer.valueOf(8)});
			}
			
			int magicCount = 0;
			for (Integer[] magArray : magicSkill) {
				magicCount ++;
				switch (magicCount) {
				case 1:
					monster.magicskilllevel1 = magArray[0];
					monster.magicskillpath1 = magArray[1];
					break;
				case 2:
					monster.magicskilllevel2 = magArray[0];
					monster.magicskillpath2 = magArray[1];
					break;
				case 3:
					monster.magicskilllevel3 = magArray[0];
					monster.magicskillpath3 = magArray[1];
					break;
				case 4:
					monster.magicskilllevel4 = magArray[0];
					monster.magicskillpath4 = magArray[1];
					break;
				}
			}
			
			List<Integer[]> customList = new ArrayList<Integer[]>();
			for (int loop = 1; loop <= 4; loop++) {
				int nbr = rs.getInt("nbr" + loop);
				int mask = rs.getInt("mask" + loop);
				int chance = rs.getInt("rand" + loop);
				if (chance != 0) {
					chance *= nbr;
					customList.add(new Integer[]{mask, chance});
				}
			}
			
			int customCount = 0;
			for (Integer[] custArray : customList) {
				customCount ++;
				switch (customCount) {
				case 1:
					monster.custommagicpath1 = custArray[0];
					monster.custommagicchance1 = custArray[1];
					break;
				case 2:
					monster.custommagicpath2 = custArray[0];
					monster.custommagicchance2 = custArray[1];
					break;
				case 3:
					monster.custommagicpath3 = custArray[0];
					monster.custommagicchance3 = custArray[1];
					break;
				case 4:
					monster.custommagicpath4 = custArray[0];
					monster.custommagicchance4 = custArray[1];
					break;
				case 5:
					monster.custommagicpath5 = custArray[0];
					monster.custommagicchance5 = custArray[1];
					break;
				case 6:
					monster.custommagicpath6 = custArray[0];
					monster.custommagicchance6 = custArray[1];
					break;
				case 7:
					monster.custommagicpath7 = custArray[0];
					monster.custommagicchance7 = custArray[1];
					break;
				case 8:
					monster.custommagicpath8 = custArray[0];
					monster.custommagicchance8 = custArray[1];
					break;
				}
			}
			
//			int magicboostAll = rs.getInt("magicboost_all");
//			if (magicboostAll != 0) {
//				monster.magicboost1 = 53;
//				monster.magicboost2 = magicboostAll;
//			} else {
//				int magicboost = rs.getInt("n_magicboost");
//				if (magicboost != 0) {
//					String path = rs.getString("magicboost");
//					if (path.equals("F")) {
//						monster.magicboost1 = 0;
//					} else if (path.equals("W")) {
//						monster.magicboost1 = 2;
//					} else if (path.equals("E")) {
//						monster.magicboost1 = 3;
//					} else if (path.equals("S")) {
//						monster.magicboost1 = 4;
//					} else if (path.equals("N")) {
//						monster.magicboost1 = 6;
//					} else if (path.equals("A")) {
//						monster.magicboost1 = 1;
//					} else if (path.equals("D")) {
//						monster.magicboost1 = 5;
//					} else if (path.equals("B")) {
//						monster.magicboost1 = 7;
//					}
//
//					monster.magicboost2 = magicboost;
//				}
//			}
//			
//			int gemprod = rs.getInt("n_gemprod");
//			if (gemprod != 0) {
//				String path = rs.getString("gemprod");
//				if (path.equals("F")) {
//					monster.gemprod1 = 0;
//				} else if (path.equals("W")) {
//					monster.gemprod1 = 2;
//				} else if (path.equals("E")) {
//					monster.gemprod1 = 3;
//				} else if (path.equals("S")) {
//					monster.gemprod1 = 4;
//				} else if (path.equals("N")) {
//					monster.gemprod1 = 6;
//				} else if (path.equals("A")) {
//					monster.gemprod1 = 1;
//				} else if (path.equals("D")) {
//					monster.gemprod1 = 5;
//				} else if (path.equals("B")) {
//					monster.gemprod1 = 7;
//				}
//
//				monster.gemprod2 = gemprod;
//			}
			
//			monster.clear = rs.getInt("clear") == 1;
//			monster.clearmagic = rs.getInt("clearmagic") == 1;
//			monster.clearspec = rs.getInt("clearspec") == 1;
			monster.female = rs.getInt("female") == 1;
			monster.mounted = rs.getInt("mount") == 1;
			monster.holy = rs.getInt("holy") == 1;
			monster.animal = rs.getInt("animal") == 1;
			monster.undead = rs.getInt("undead") == 1;
			monster.demon = rs.getInt("demon") == 1;
			monster.magicbeing = rs.getInt("mgcbng") == 1;
			
			monster.stonebeing = rs.getInt("stone") == 1;
			monster.inanimate = rs.getInt("lifeless") == 1;
			monster.coldblood = rs.getInt("cldbld") == 1;
			monster.immortal = rs.getInt("imm") == 1;
			monster.blind = rs.getInt("blind") == 1;
			monster.unique = rs.getInt("unique") == 1;
			monster.immobile = rs.getInt("immob") == 1;
			monster.aquatic = rs.getInt("aqua") == 1;
			monster.amphibian = rs.getInt("amphi") == 1;
			monster.pooramphibian = rs.getInt("pamph") == 1;
			monster.flying = rs.getInt("fly") == 1;
			monster.stormimmune = rs.getInt("strmfly") == 1;
			monster.sailing1 = (rs.getInt("sailsz") == 0 ? null : rs.getInt("sailsz"));
			monster.sailing2 = rs.getInt("sailmaxsz");
			monster.forestsurvival = rs.getInt("forest") == 1;
			monster.mountainsurvival = rs.getInt("mount") == 1;
			monster.swampsurvival = rs.getInt("swamp") == 1;
			monster.wastesurvival = rs.getInt("waste") == 1;
			monster.illusion = rs.getInt("disbel") == 1;
			monster.spy = rs.getInt("spy") == 1;
			monster.assassin = rs.getInt("assassin") == 1;
			monster.heal = rs.getInt("rec") == 1;
			monster.noheal = rs.getInt("noheal") == 1;
			monster.neednoteat = rs.getInt("noeat") == 1;
			monster.ethereal = rs.getInt("eth") == 1;
			monster.trample = rs.getInt("trmpl") == 1;
			//monster.entangle = rs.getInt("entangle") == 1;
			monster.eyeloss = rs.getInt("eyeloss") == 1;
			//monster.horrormark = rs.getInt("horrormark") == 1;
			monster.poisonarmor = rs.getInt("barbs") == 1;
			monster.inquisitor = rs.getInt("barbs") == 1;
//			monster.noitem = rs.getInt("noitem") == 1;
			
			int normalLeadership = rs.getInt("ldr-n");
			switch (normalLeadership) {
			case 0:
				monster.noleader = true;
				break;
			case 10:
				monster.poorleader = true;
				break;
			case 40:
				monster.okleader = true;
				break;
			case 80:
				monster.goodleader = true;
				break;
			case 120:
				monster.expertleader = true;
				break;
			case 160:
				monster.superiorleader = true;
				break;
			}
			int magicLeadership = rs.getInt("ldr-m");
			switch (magicLeadership) {
			case 0:
				monster.nomagicleader = true;
				break;
			case 10:
				monster.poormagicleader = true;
				break;
			case 40:
				monster.okmagicleader = true;
				break;
			case 80:
				monster.goodmagicleader = true;
				break;
			case 120:
				monster.expertmagicleader = true;
				break;
			case 160:
				monster.superiormagicleader = true;
				break;
			}
			int undeadLeadership = rs.getInt("ldr-u");
			switch (undeadLeadership) {
			case 0:
				monster.noundeadleader = true;
				break;
			case 10:
				monster.poorundeadleader = true;
				break;
			case 40:
				monster.okundeadleader = true;
				break;
			case 80:
				monster.goodundeadleader = true;
				break;
			case 120:
				monster.expertundeadleader = true;
				break;
			case 160:
				monster.superiorundeadleader = true;
				break;
			}
			
			monster.weapon1 = rs.getInt("wpn1") != 0 ? Integer.toString(rs.getInt("wpn1")) : null;
			monster.weapon2 = rs.getInt("wpn2") != 0 ? Integer.toString(rs.getInt("wpn2")) : null;
			monster.weapon3 = rs.getInt("wpn3") != 0 ? Integer.toString(rs.getInt("wpn3")) : null;
			monster.weapon4 = rs.getInt("wpn4") != 0 ? Integer.toString(rs.getInt("wpn4")) : null;
			
			//monster.onebattlespell = rs.getInt("onebattlespell") != 0 ? Integer.toString(rs.getInt("onebattlespell")) : null;
			
			monster.firstshape = Integer.toString(rs.getInt("1shape"));
			monster.secondshape = Integer.toString(rs.getInt("2shape"));
			monster.secondtmpshape = Integer.toString(rs.getInt("tmp2shape"));
			monster.shapechange = Integer.toString(rs.getInt("shapechange"));
			monster.landshape = Integer.toString(rs.getInt("landshape"));
			monster.watershape = Integer.toString(rs.getInt("seashape"));
			monster.forestshape = Integer.toString(rs.getInt("forestshape"));
			//monster.plainshape = Integer.toString(rs.getInt("plainshape"));
			
			int domNum = rs.getInt("n_domsum");
			if (domNum == 1) {
				monster.domsummon = Integer.toString(rs.getInt("domsum"));
			} else if (domNum == 2) {
				monster.domsummon2 = Integer.toString(rs.getInt("domsum"));
			} else if (domNum == 20) {
				monster.domsummon20 = Integer.toString(rs.getInt("domsum"));
			}
			
			int mmNum = rs.getInt("n_autosum");
			if (mmNum == 1) {
				monster.makemonsters1 = Integer.toString(rs.getInt("autosum"));
			} else if (mmNum == 2) {
				monster.makemonsters2 = Integer.toString(rs.getInt("autosum"));
			} else if (mmNum == 3) {
				monster.makemonsters3 = Integer.toString(rs.getInt("autosum"));
			} else if (mmNum == 4) {
				monster.makemonsters4 = Integer.toString(rs.getInt("autosum"));
			} else if (mmNum == 5) {
				monster.makemonsters5 = Integer.toString(rs.getInt("autosum"));
			}
			
			int sumNum = rs.getInt("n_sum");
			if (sumNum == 1) {
				monster.summon1 = Integer.toString(rs.getInt("sum"));
			} else if (sumNum == 5) {
				monster.summon5 = Integer.toString(rs.getInt("sum"));
			}
		}
		return monster;
	}
	
	private static ItemDB getItemDB(ResultSet rs) throws SQLException {
		ItemDB item = new ItemDB();
		if (rs.next()) {
			item.id = rs.getInt("id");
			item.name = rs.getString("name");
			String armorId = rs.getString("armor");
			if (armorId != null) {
				try {
					item.armor = getArmorName(rs.getInt("armor"));
				} catch (NumberFormatException e) {
					System.err.println("Incorrect armor value in item DB: " + rs.getString("armor"));
				}
			}
			item.constlevel = rs.getInt("con");
			String mainPath = rs.getString("p1");
			if (mainPath != null) {
				if (mainPath.equals("F")) {
					item.mainpath = 0;
				} else if (mainPath.equals("W")) {
					item.mainpath = 2;
				} else if (mainPath.equals("E")) {
					item.mainpath = 3;
				} else if (mainPath.equals("S")) {
					item.mainpath = 4;
				} else if (mainPath.equals("N")) {
					item.mainpath = 6;
				} else if (mainPath.equals("A")) {
					item.mainpath = 1;
				} else if (mainPath.equals("D")) {
					item.mainpath = 5;
				} else if (mainPath.equals("B")) {
					item.mainpath = 7;
				}
			}
			item.mainlevel = rs.getInt("lv1");
			
			String secondarypath = rs.getString("p2");
			if (secondarypath != null) {
				if (secondarypath.equals("F")) {
					item.secondarypath = 0;
				} else if (secondarypath.equals("W")) {
					item.secondarypath = 2;
				} else if (secondarypath.equals("E")) {
					item.secondarypath = 3;
				} else if (secondarypath.equals("S")) {
					item.secondarypath = 4;
				} else if (secondarypath.equals("N")) {
					item.secondarypath = 6;
				} else if (secondarypath.equals("A")) {
					item.secondarypath = 1;
				} else if (secondarypath.equals("D")) {
					item.secondarypath = 5;
				} else if (secondarypath.equals("B")) {
					item.secondarypath = 7;
				}
			}
			item.secondarylevel = rs.getInt("lv2");
			
			String type = rs.getString("type");
			if (type != null) {
				if (type.equals("1-h wpn")) {
					item.type = 1;
				} else if (type.equals("2-h wpn")) {
					item.type = 2;
				} else if (type.equals("missile")) {
					item.type = 3;
				} else if (type.equals("shield")) {
					item.type = 4;
				} else if (type.equals("armor")) {
					item.type = 5;
				} else if (type.equals("helm")) {
					item.type = 6;
				} else if (type.equals("boots")) {
					item.type = 7;
				} else if (type.equals("misc")) {
					item.type = 8;
				}
			}
			item.weapon = rs.getInt("wpn") != 0 ? rs.getInt("wpn") : null;

//			public Integer type;
//			public Integer weapon;
//			public Integer str;
//			public Integer att;
//			public Integer def;
//			public Integer prec;
//			public Integer mr;
//			public Integer morale;
//			public Integer voidsanity;
//			public Integer giftofwater;
//			public Integer invulnerable;
//			public Integer inspiringres;
//			public Integer fireres;
//			public Integer coldres;
//			public Integer shockres;
//			public Integer poisonres;
//			public Integer restricted;
//			public Integer pen;
//			public Integer autospellrepeat;
//			public Integer randomspell;
//			public Integer mapspeed;
//			public Integer tainted;
//			public Integer speciallook;
//			public Integer seduce;
//			public Integer succubus;
//			public Integer beckon;
//			public Integer falsearmy;
//			public Integer foolscouts;
//			public Integer iceprot;
//			public Integer healer;
//			public Integer autohealer;
//			public Integer autodishealer;
//			public Integer autodisgrinder;
//			public Integer homesick;
//			public Integer uwdamage;
//			public Integer regeneration;
//			public Integer reinvigoration;
//			public Integer woundfend;
//			public Integer poisoncloud;
//			public Integer diseasecloud;
//			public Integer animalawe;
//			public Integer awe;
//			public Integer fear;
//			public Integer fireshield;
//			public Integer banefireshield;
//			public Integer damagerev;
//			public Integer bloodvengeance;
//			public Integer slimer;
//			public Integer deathdisease;
//			public Integer deathparalyze;
//			public Integer deathfire;
//			public Integer chaospower;
//			public Integer firepower;
//			public Integer coldpower;
//			public Integer magicpower;
//			public Integer stormpower;
//			public Integer darkpower;
//			public Integer springpower;
//			public Integer summerpower;
//			public Integer fallpower;
//			public Integer winterpower;
//			public Integer ambidextrous;
//			public Integer berserk;
//			public Integer darkvision;
//			public Integer digest;
//			public Integer incorporate;
//			public Integer castledef;
//			public Integer siegebonus;
//			public Integer patrolbonus;
//			public Integer pillagebonus;
//			public Integer supplybonus;
//			public Integer nobadevents;
//			public Integer incprovdef;
//			public Integer incunrest;
//			public Integer leper;
//			public Integer popkill;
//			public Integer heretic;
//			public Integer elegist;
//			public Integer spreaddom;
//			public Integer shatteredsoul;
//			public Integer gold;
//			public Integer inspirational;
//			public Integer beastmaster;
//			public Integer taskmaster;
//			public Integer formationfighter;
//			public Integer bodyguard;
//			public Integer standard;
//			public Integer douse;
//			public Integer researchbonus;
//			public Integer divineins;
//			public Integer forgebonus;
//			public Integer fixforgebonus;
//			public Integer crossbreeder;
//			public Integer bonusspells;
//			public Integer deathbanish;
//			public Integer kokytosret;
//			public Integer infernoret;
//			public Integer voidret;
//			public Integer allret;
//			public Integer firerange;
//			public Integer airrange;
//			public Integer waterrange;
//			public Integer earthrange;
//			public Integer astralrange;
//			public Integer deathrange;
//			public Integer naturerange;
//			public Integer bloodrange;
//			public Integer elementrange;
//			public Integer sorceryrange;
//			public Integer allrange;
//			public Integer makepearls;
//			public Integer tmpfiregems;
//			public Integer tmpairgems;
//			public Integer tmpwatergems;
//			public Integer tmpearthgems;
//			public Integer tmpastralgems;
//			public Integer tmpdeathgems;
//			public Integer tmpnaturegems;
//			public Integer tmpbloodslaves;
//			public String copyitem;
//			public String domsummon;
//			public String domsummon2;
//			public String domsummon20;
//			public String raredomsummon;
//			public String summon1;
//			public String summon2;
//			public String summon3;
//			public String summon4;
//			public String summon5;
//			public String makemonsters1;
//			public String makemonsters2;
//			public String makemonsters3;
//			public String makemonsters4;
//			public String makemonsters5;
//			public String battlesum1;
//			public String battlesum2;
//			public String battlesum3;
//			public String battlesum4;
//			public String battlesum5;
//			public String batstartsum1;
//			public String batstartsum2;
//			public String batstartsum3;
//			public String batstartsum4;
//			public String batstartsum5;
//			public String batstartsum1d6;
//			public String batstartsum2d6;
//			public String batstartsum3d6;
//			public String batstartsum4d6;
//			public String batstartsum5d6;
//			public Boolean ethereal;
//			public Boolean nomounted;
//			public Boolean nocoldblood;
//			public Boolean nodemon;
//			public Boolean noundead;
//			public Boolean noinanim;
//			public Boolean nofemale;
//			public Boolean onlymounted;
//			public Boolean onlycoldblood;
//			public Boolean onlydemon;
//			public Boolean onlyundead;
//			public Boolean onlyinanim;
//			public Boolean onlyfemale;
//			public Boolean reqeyes;
//			public Boolean nofind;
//			public Boolean luck;
//			public Boolean quickness;
//			public Boolean bless;
//			public Boolean barkskin;
//			public Boolean stoneskin;
//			public Boolean ironskin;
//			public Boolean waterbreathing;
//			public Boolean floatBoolean;
//			public Boolean fly;
//			public Boolean stormimmune;
//			public Boolean run;
//			public Boolean trample;
//			public Boolean bers;
//			public Boolean extralife;
//			public Boolean champprize;
//			public Boolean autocompete;
//			public Boolean cursed;
//			public Boolean curse;
//			public Boolean disease;
//			public Boolean chestwound;
//			public Boolean feeblemind;
//			public Boolean mute;
//			public Boolean nhwound;
//			public Boolean crippled;
//			public Boolean loseeye;
//			public Boolean singlebattle;
//			public Boolean chaosrec;
//			public Boolean stonebeing;
//			public Boolean noriverpass;
//			public Boolean unteleportable;
//			public Boolean slashres;
//			public Boolean pierceres;
//			public Boolean bluntres;
//			public Boolean hpoverflow;
//			public Boolean deathcurse;
//			public Boolean trampswallow;
//			public Boolean inquisitor;
//			public Boolean taxcollector;
//			public Boolean undisciplined;
//			public Boolean drainimmune;
//			public Boolean magicimmune;
//			public Boolean comslave;
//			public String magicboost1;
//			public String magicboost2;
		}
		return item;
	}

	public static String getArmorName(int id) {
		String armorName = armorNameMap.get(Integer.valueOf(id));
		if (armorName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"armorname\" FROM \"armor_base\" where \"id\"="+id);

				while (rs.next()) {			
					armorName = rs.getString("armorname");
				}

				statement.close();
				armorNameMap.put(Integer.valueOf(id), armorName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return armorName;
	}
	
	private static SpellDB getSpellDB(ResultSet rs) throws SQLException {
		SpellDB spell = new SpellDB();
		if (rs.next()) {
			spell.id = rs.getInt("id");
			spell.name = rs.getString("name");
			
			String school = rs.getString("school");
			int schoolNum = -1;
			if (school != null) {
				if (school.equals("Conjuration")) {
					schoolNum = 0;
				} else if (school.equals("Alteration")) {
					schoolNum = 1;
				} else if (school.equals("Evocation")) {
					schoolNum = 2;
				} else if (school.equals("Construction")) {
					schoolNum = 3;
				} else if (school.equals("Enchantment")) {
					schoolNum = 4;
				} else if (school.equals("Thaumaturgy")) {
					schoolNum = 5;
				} else if (school.equals("Blood")) {
					schoolNum = 6;
				} else if (school.equals("Divine")) {
					schoolNum = 7;
				}
				
			}
			spell.school = schoolNum;
			
			spell.researchlevel = rs.getInt("research");
			int path1 = -1;
			String p1 = rs.getString("p1");
			if (p1 != null) {
				if (p1.equals("F")) {
					path1 = 0;
				} else if (p1.equals("A")) {
					path1 = 1;
				} else if (p1.equals("W")) {
					path1 = 2;
				} else if (p1.equals("E")) {
					path1 = 3;
				} else if (p1.equals("S")) {
					path1 = 4;
				} else if (p1.equals("D")) {
					path1 = 5;
				} else if (p1.equals("N")) {
					path1 = 6;
				} else if (p1.equals("B")) {
					path1 = 7;
				} else if (p1.equals("H")) {
					path1 = 8;
				}
			}
			spell.path1 = path1;
			
			int path2 = -1;
			String p2 = rs.getString("p2");
			if (p2 != null) {
				if (p2.equals("F")) {
					path2 = 0;
				} else if (p2.equals("A")) {
					path2 = 1;
				} else if (p2.equals("W")) {
					path2 = 2;
				} else if (p2.equals("E")) {
					path2 = 3;
				} else if (p2.equals("S")) {
					path2 = 4;
				} else if (p2.equals("D")) {
					path2 = 5;
				} else if (p2.equals("N")) {
					path2 = 6;
				} else if (p2.equals("B")) {
					path2 = 7;
				} else if (p2.equals("H")) {
					path2 = 8;
				}
			}
			spell.path2 = path2;

			int level1 = rs.getInt("lv1");
			if (level1 != 0) {
				spell.pathlevel1 = level1;
			}
			int level2 = rs.getInt("lv2");
			if (level2 != 0) {
				spell.pathlevel2 = level2;
			}
			
			spell.fatiguecost = rs.getInt("fatigue");			
			
			String aoe = rs.getString("aoe_s");
			if (aoe != null && aoe.equals("bf")) {
				spell.aoe = 666;
			} else {
				spell.aoe = rs.getInt("aoe_s");
			}
			
			//spell.effect = rs.getInt("effect");
			spell.range = rs.getInt("rng_bat");
			spell.precision = rs.getInt("prec");
			spell.damage = rs.getInt("dmg");
			spell.nreff = rs.getInt("n_eff");
			//spell.spec = rs.getInt("spec");
			//spell.nextspell = rs.getInt("nextspell");
			
			//spell.restricted1 = rs.getInt("restricted1");
			//spell.restricted2 = rs.getInt("restricted2");
			//spell.restricted3 = rs.getInt("restricted3");
			//spell.damagemon;
			spell.provrange = rs.getInt("rng_prov");
			//spell.onlygeosrc;
			//spell.onlygeodst;
			//spell.onlyfriendlydst;
			//spell.onlyowndst;
			//spell.nowatertrace;
			//spell.nolandtrace;
			//spell.walkable;
		}
		return spell;
	}

	public static String getWeaponName(int id) {
		String weaponName = weaponNameMap.get(Integer.valueOf(id));
		if (weaponName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"weapon_name\" FROM \"weapons_base\" where \"id\"="+id);

				while (rs.next()) {			
					weaponName = rs.getString("weapon_name");
				}

				statement.close();
				weaponNameMap.put(Integer.valueOf(id), weaponName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return weaponName;
	}
	
	public static String getMonsterName(int id) {
		String monsterName = monsterNameMap.get(Integer.valueOf(id));
		if (monsterName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"unitname\" FROM \"units_base\" where \"id\"="+id);

				while (rs.next()) {			
					monsterName = rs.getString("unitname");
				}

				statement.close();
				monsterNameMap.put(Integer.valueOf(id), monsterName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return monsterName;
	}
	
	public static String getMonsterDescr(int id) {
		String descr = monsterDescIdMap.get(id);
		if (descr == null) {
			descr = "";
			BufferedReader reader = null;
			try {
				Path path = new Path("$nl$/data/unitdescr/"+numberFormat.format(id)+".txt");
				URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
				if (url != null) {
					String dbPath = FileLocator.toFileURL(url).getPath();
					File descrFile = new File(dbPath);
					reader = new BufferedReader(new FileReader(descrFile));
					descr = reader.readLine();
					descr = descr.replaceAll("<p>", "");
					descr = descr.replaceAll("</p>", "");
				}
				monsterDescIdMap.put(id, descr);
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				if (reader != null) {
					try {
						reader.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return descr;
	}
	
	public static String getItemName(int id) {
		String itemName = itemNameMap.get(Integer.valueOf(id));
		if (itemName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"items\" where \"id\"="+id);

				while (rs.next()) {			
					itemName = rs.getString("name");
				}

				statement.close();
				itemNameMap.put(Integer.valueOf(id), itemName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return itemName;
	}
	
	public static String getItemDescr(String name) {
		String descr = itemDescrNameMap.get(name);
		if (descr == null) {
			descr = "";
			name = name.replaceAll(" ", "");
			name = name.replaceAll("�", "");
			name = name.replaceAll("'", "");
			BufferedReader reader = null;
			try {
				Path path = new Path("$nl$/data/itemdescr/"+name+".txt");
				URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
				if (url != null) {
					String dbPath = FileLocator.toFileURL(url).getPath();
					File descrFile = new File(dbPath);
					reader = new BufferedReader(new FileReader(descrFile));
					descr = reader.readLine();
					descr = descr.replaceAll("<p>", "");
					descr = descr.replaceAll("</p>", "");
				}
				itemDescrNameMap.put(name, descr);
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				if (reader != null) {
					try {
						reader.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return descr;
	}
	
	public static String getSpellName(int id) {
		String spellName = spellNameMap.get(Integer.valueOf(id));
		if (spellName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"spells\" where \"id\"="+id);

				while (rs.next()) {			
					spellName = rs.getString("name");
				}

				statement.close();
				spellNameMap.put(id, spellName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return spellName;
	}
	
	public static String getSpellDescr(String name) {
		String descr = spellDescrNameMap.get(name);
		if (descr == null) {
			descr = "";
			name = name.replaceAll(" ", "");
			name = name.replaceAll("�", "");
			name = name.replaceAll("'", "");
			BufferedReader reader = null;
			try {
				Path path = new Path("$nl$/data/spelldescr/"+name+".txt");
				URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
				if (url != null) {
					String dbPath = FileLocator.toFileURL(url).getPath();
					File descrFile = new File(dbPath);
					reader = new BufferedReader(new FileReader(descrFile));
					descr = reader.readLine();
					descr = descr.replaceAll("<p>", "");
					descr = descr.replaceAll("</p>", "");
				}
				spellDescrNameMap.put(name, descr);
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				if (reader != null) {
					try {
						reader.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return descr;
	}
	
	public static ItemDB getItem(int id) {
		ItemDB item = itemDBIdMap.get(id);
		if (item == null) {
			item = new ItemDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"items\" where \"id\"="+id);
				item = getItemDB(rs);
				statement.close();
				itemDBIdMap.put(id, item);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return item;
	}

	public static ItemDB getItem(String name) {
		ItemDB item = itemDBNameMap.get(name);
		if (item == null) {
			item = new ItemDB();
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"items\" where \"name\" = '"+getSafeString(name)+"'");
				item = getItemDB(rs);
				statement.close();
				itemDBNameMap.put(name, item);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
		return item;
	}
	
	public static String getNationName(int id) {
		String nationName = nationNameMap.get(Integer.valueOf(id));
		if (nationName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\", \"era\" FROM \"nations\" where \"id\"="+id);

				while (rs.next()) {			
					nationName = rs.getString("name");
					int era = rs.getInt("era");
					switch (era) {
					case 1:
						nationName = "EA " + nationName;
						break;
					case 2:
						nationName = "MA " + nationName;
						break;
					case 3:
						nationName = "LA " + nationName;
						break;
					default:
						nationName = "Era " + era + " " + nationName;
						break;
					}
				}

				statement.close();
				nationNameMap.put(id, nationName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return nationName;
	}
	
	public static String getSiteName(int id) {
		String siteName = siteNameMap.get(Integer.valueOf(id));
		if (siteName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"magic_sites\" where \"id\"="+id);

				while (rs.next()) {			
					siteName = rs.getString("name");
				}

				statement.close();
				siteNameMap.put(id, siteName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return siteName;
	}
	
	public static PoptypeDB getPoptype(int id) {
		return new PoptypeDB();
	}
	
	private static Connection getConnection() throws IOException, ClassNotFoundException, SQLException {
		if (connection == null) {
			Path path = new Path("$nl$/data/dom4db.data");
			URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
			String dbPath = FileLocator.toFileURL(url).getPath();
			dbPath = dbPath.substring(0, dbPath.length()-5);

			Class.forName("org.hsqldb.jdbcDriver");
			connection = DriverManager.getConnection("jdbc:hsqldb:file:" + dbPath, "sa", "");
		}
		return connection;
	}

}
