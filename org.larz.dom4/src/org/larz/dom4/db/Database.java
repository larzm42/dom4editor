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
	private static Connection connection2 = null;
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

	private static Map<Integer, String> poptypeNameMap = new HashMap<Integer, String>();
	
	private static Format numberFormat = new DecimalFormat("0000");
	
	private static long notnostr =		0x00000000001l;
	private static long twohanded =		0x00000000002l;
	private static long flail =			0x00000000004l;
	private static long demononly = 	0x00000000008l;
	//private static long xx = 			0x00000000010l;
	private static long fire = 			0x00000000020l;
	private static long armorpiercing = 0x00000000040l;
	private static long armornegating = 0x00000000080l;
	//private static long xx = 			0x00000000100l;
	private static long cold = 			0x00000000200l;
	//private static long xx = 			0x00000000400l;
	private static long shock = 		0x00000000800l;
	private static long mrneg = 		0x00000001000l;
	private static long poison =		0x00000002000l;
	//private static long xx = 			0x00000004000l;
	private static long sacredonly = 	0x00000008000l;
	//private static long xx = 			0x00000010000l;
	private static long mind = 			0x00000020000l;
	private static long friendimmune = 	0x00000040000l;
	private static long undeadimmune = 	0x00000080000l;
	private static long flyingimmue = 	0x00000100000l;
	private static long notmagic = 		0x00000200000l;
	private static long enemyimmune = 	0x00000400000l;
	//private static long xx = 			0x00000800000l;
	private static long mrnegeasy = 	0x00010000000l;
	//private static long xx = 			0x00002000000l;
	private static long undeadonly = 	0x00004000000l;
	private static long bonus = 		0x00008000000l;
	//private static long xx = 			0x00010000000l;
	//private static long xx = 			0x00020000000l;
	//private static long xx = 			0x00040000000l;
	private static long charge = 		0x00080000000l;
	//private static long xx = 			0x00100000000l;
	//private static long xx = 			0x00200000000l;
	//private static long xx = 			0x00400000000l;
	//private static long xx = 			0x00800000000l;
	//private static long xx = 			0x01000000000l;
	private static long unrepel = 		0x02000000000l;
	private static long pierce = 		0x04000000000l;
	private static long blunt = 		0x08000000000l;
	private static long slash = 		0x10000000000l;
	private static long acid = 			0x20000000000l;
	private static long sizeresist = 	0x40000000000l;

	public static List<IDNameDB> getAllArmor() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection2().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"number\", \"name\" FROM \"armors\"");

			while (rs.next())
			{
				IDNameDB armor = new IDNameDB();
				armor.id = rs.getInt("number");
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
	
	public static List<IDNameDB> getAllWeapon() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection2().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"number\", \"name\" FROM \"weapons\"");

			while (rs.next())
			{
				IDNameDB weapon = new IDNameDB();
				weapon.id = rs.getInt("number");
				weapon.name = rs.getString("name");
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
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"name\" FROM \"units_base\"");

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
			Statement statement = getConnection2().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"number\", \"name\" FROM \"nations\"");

			while (rs.next())
			{
				IDNameDB nation = new IDNameDB();
				nation.id = rs.getInt("number");
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
			Statement statement = getConnection2().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"number\", \"name\" FROM \"spells\"");

			while (rs.next())
			{
				IDNameDB spell = new IDNameDB();
				spell.id = rs.getInt("number");
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
	
	public static List<IDNameDB> getAllPoptype() {
		List<IDNameDB> list = new ArrayList<IDNameDB>();
		try {
			Statement statement = getConnection().createStatement();
			ResultSet rs = statement.executeQuery("SELECT \"id\", \"type\" FROM \"poptypes\"");

			while (rs.next())
			{
				IDNameDB poptype = new IDNameDB();
				poptype.id = rs.getInt("id");
				poptype.name = rs.getString("type");
				list.add(poptype);
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
				Statement statement = getConnection2().createStatement();
				Statement statement2 = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"armors\" where \"number\"="+id);
				ResultSet prots = statement2.executeQuery("SELECT * FROM \"protections_by_armor\" where \"armor_number\"="+id);
				armor = getArmorDB(rs, prots);
				statement.close();
				statement2.close();
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
				Statement statement = getConnection2().createStatement();
				Statement statement2 = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"armors\" where \"name\" = '"+getSafeString(name)+"'");
				rs.next();
				int id = rs.getInt("number");
				rs.beforeFirst();
				ResultSet prots = statement2.executeQuery("SELECT * FROM \"protections_by_armor\" where \"armor_number\"="+id);
				armor = getArmorDB(rs, prots);
				statement.close();
				statement2.close();
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

	private static ArmorDB getArmorDB(ResultSet rs, ResultSet prots) throws SQLException {
		ArmorDB armor = new ArmorDB();
		if (rs.next()) {	
			armor.name = rs.getString("name");
			armor.id = rs.getInt("number");
			armor.def = rs.getInt("defense");
			armor.enc = rs.getInt("encumbrance");
			armor.rcost = rs.getInt("resource_cost");
			armor.type = rs.getInt("armor_type");
			while (prots.next()) {
				if (prots.getInt("zone_number") == 1 || 
					prots.getInt("zone_number") == 2 || 
					prots.getInt("zone_number") == 5 || 
					prots.getInt("zone_number") == 6) {
					armor.prot = prots.getInt("protection");
					break;
				}
			}

		}
		return armor;

	}
	
	public static WeaponDB getWeapon(int id) {
		WeaponDB weapon = weaponDBIdMap.get(id);
		if (weapon == null) {
			weapon = new WeaponDB();
			try {
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"weapons\" where \"number\"="+id);
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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"weapons\" where \"name\" = '"+getSafeString(name)+"'");
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
				ResultSet rs = statement.executeQuery("SELECT * FROM \"units_base\" where \"name\" = '"+getSafeString(name)+"'");
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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"spells\" where \"number\"="+id);
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
				Statement statement = getConnection2().createStatement();
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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT * FROM \"nations\" where \"number\"="+id);

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
			String type = rs.getString("path");
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
			site.level = rs.getInt("level");
			site.rarity = rs.getInt("rarity");
			site.loc = rs.getInt("loc");
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

	private static NationDB getNationDB(ResultSet rs) throws SQLException, ClassNotFoundException, IOException {
		NationDB nation = new NationDB();
		if (rs.next()) {
			nation.id = rs.getInt("number");
			nation.name = rs.getString("name");
			nation.epithet = rs.getString("epithet");
			String fileName = rs.getString("file_name_base");
			if (fileName.startsWith("early")) {
				nation.era = 1;
			} else if (fileName.startsWith("mid")) {
				nation.era = 2;
			} else if (fileName.startsWith("late")) {
				nation.era = 3;
			}
			
			int numSites = 0;
			Statement statement2 = getConnection2().createStatement();
			ResultSet rsAttributesByNation = statement2.executeQuery("SELECT * FROM \"attributes_by_nation\" where \"nation_number\"="+nation.id);
			while (rsAttributesByNation.next()) {
				int attrRecId = rsAttributesByNation.getInt("attribute_record_id");
				
				Statement statement4 = getConnection2().createStatement();
				ResultSet rsAttributes = statement4.executeQuery("SELECT * FROM \"attributes\" where \"record_id\"="+attrRecId);
				if (rsAttributes.next()) {
					if (rsAttributes.getInt("attribute_number") == 52 ||
						rsAttributes.getInt("attribute_number") == 100) {
						numSites++;
						switch (numSites) {
						case 1:
							nation.startsite1 = getSiteName(rsAttributes.getInt("raw_value"));
							break;
						case 2:
							nation.startsite2 = getSiteName(rsAttributes.getInt("raw_value"));
							break;
						case 3:
							nation.startsite3 = getSiteName(rsAttributes.getInt("raw_value"));
							break;
						case 4:
							nation.startsite4 = getSiteName(rsAttributes.getInt("raw_value"));
							break;
						}
					}
				}

			}

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

	private static WeaponDB getWeaponDB(ResultSet rs) throws SQLException, ClassNotFoundException, IOException {
		WeaponDB weapon = new WeaponDB();
		if (rs.next()) {
			int effectId = rs.getInt("effect_record_id");
			
			Statement statement = getConnection2().createStatement();
			ResultSet rsEffect = statement.executeQuery("SELECT * FROM \"effects\" where \"record_id\"="+effectId);
			rsEffect.next();
			
			weapon.id = rs.getInt("number");
			weapon.name = rs.getString("name");
			int effectNumber = rsEffect.getInt("effect_number");
			if (effectNumber != 11) {
				weapon.dmg = rsEffect.getInt("raw_argument");
			}
			switch (effectNumber) {
			case 2:
				weapon.dt_normal = true;
				break;
			case 3:
				weapon.dt_stun = true;
				break;
			case 7:
				weapon.dt_poison = true;
				break;
			case 24:
				weapon.dt_holy = true;
				break;
			case 32:
				weapon.dt_large = true;
				break;
			case 33:
				weapon.dt_small = true;
				break;
			case 66:
				weapon.dt_paralyze = true;
				break;
			case 67:
				weapon.dt_weakness = true;
				break;
			case 73:
				weapon.dt_magic = true;
				break;
			case 96:
				weapon.dt_constructonly = true;
				break;
			case 103:
				weapon.dt_drain = true;
				break;
			case 104:
				weapon.dt_weapondrain = true;
				break;
			case 107:
				weapon.dt_demon = true;
				break;
			case 109:
				weapon.dt_cap = true;
				break;
			}
//			weapon.dt_raise = rs.getInt("dt_raise") == 1;
//			weapon.dt_sizestun;
//			weapon.demonundead = rs.getInt("dt_demonundead") == 1;

			weapon.nratt = rs.getInt("attack_rate");
			weapon.att = rs.getInt("attack");
			weapon.def = rs.getInt("defense");
			weapon.len = rs.getInt("length");
			
			if (rsEffect.getObject("range_base") != null) {
				weapon.range = rsEffect.getInt("range_base");
			} else {
				weapon.range = -rsEffect.getInt("range_strength_divisor");
			}
			
			weapon.ammo = rs.getInt("attacks_total");
			weapon.rcost = rs.getInt("resource_cost");
			weapon.sound = rsEffect.getInt("sound_number");
			weapon.aoe = rsEffect.getInt("area_base");
			weapon.secondaryeffect = rs.getInt("secondary_effect_on_hit");
			weapon.secondaryeffectalways = rs.getInt("secondary_effect_always");
			if (rsEffect.getObject("explosion_sprite_number") != null) {
				weapon.explspr = rsEffect.getInt("explosion_sprite_number");
			}
			if (rsEffect.getObject("flight_sprite_number") != null) {
				weapon.flyspr1 = rsEffect.getInt("flight_sprite_number");
			}
			if (rsEffect.getObject("flight_sprite_length") != null) {
				weapon.flyspr2 = rsEffect.getInt("flight_sprite_length");
			}
			
			long bitMask = rsEffect.getLong("modifiers_mask");
			if ((bitMask & twohanded) != 0) {
				weapon.twohanded = true;
			}
			if ((bitMask & armorpiercing) != 0) {
				weapon.armorpiercing = true;
			}
			if ((bitMask & armornegating) != 0) {
				weapon.armornegating = true;
			}
			if ((bitMask & notmagic) == 0) {
				weapon.magic = true;
			}
			if ((bitMask & mind) != 0) {
				weapon.mind = true;
			}
			if ((bitMask & cold) != 0) {
				weapon.cold = true;
			}
			if ((bitMask & fire) != 0) {
				weapon.fire = true;
			}
			if ((bitMask & shock) != 0) {
				weapon.shock = true;
			}
			if ((bitMask & poison) != 0) {
				weapon.poison = true;
			}
			if ((bitMask & bonus) != 0) {
				weapon.bonus = true;
			}
			if ((bitMask & charge) != 0) {
				weapon.charge = true;
			}
			if ((bitMask & flail) != 0) {
				weapon.flail = true;
			}
			if ((bitMask & notnostr) == 0) {
				weapon.nostr = true;
			}
			if ((bitMask & mrneg) != 0) {
				weapon.mrnegates = true;
			}
			if ((bitMask & mrnegeasy) != 0) {
				weapon.mrnegateseasily = true;
			}
			if ((bitMask & slash) != 0) {
				weapon.slash = true;
			}
			if ((bitMask & pierce) != 0) {
				weapon.pierce = true;
			}
			if ((bitMask & blunt) != 0) {
				weapon.blunt = true;
			}
			if ((bitMask & acid) != 0) {
				weapon.acid = true;
			}
//			//weapon.hardmrneg;
			if ((bitMask & sizeresist) != 0) {
				weapon.sizeresist = true;
			}
//			//weapon.inanimateimmune;
			if ((bitMask & undeadimmune) != 0) {
				weapon.undeadimmune = true;
			}
			if ((bitMask & flyingimmue) != 0) {
				weapon.flyingimmune = true;
			}
			if ((bitMask & enemyimmune) != 0) {
				weapon.enemyimmune = true;
			}
			if ((bitMask & friendimmune) != 0) {
				weapon.friendlyimmune = true;
			}
			if ((bitMask & undeadonly) != 0) {
				weapon.undeadonly = true;
			}
//			weapon.norepel = rs.getInt("norepel") == 1;
			if ((bitMask & unrepel) != 0) {
				weapon.unrepel = true;
			}
//			weapon.beam;
//			weapon.range050;
//			weapon.range0;
//			weapon.melee50;
//			weapon.skip;
//			weapon.skip2;
			if ((bitMask & sacredonly) != 0) {
				weapon.sacredonly = true;
			}
		statement.close();
		}
		return weapon;
	}
	
	private static MonsterDB getMonsterDB(ResultSet rs) throws SQLException {
		MonsterDB monster = new MonsterDB();
		if (rs.next()) {
			monster.id = rs.getInt("id");
			monster.name = rs.getString("name");
			//monster.spr1 = rs.getString("spr1");
			//monster.spr2 = rs.getString("spr2");
			//monster.descr = rs.getString("descr");
			monster.armor1 = rs.getInt("armor1") != 0 ? Integer.toString(rs.getInt("armor1")) : null;
			monster.armor2 = rs.getInt("armor2") != 0 ? Integer.toString(rs.getInt("armor2")) : null;
			monster.armor3 = rs.getInt("armor3") != 0 ? Integer.toString(rs.getInt("armor3")) : null;
			//monster.speciallook = rs.getInt("speciallook") != 0 ? rs.getInt("speciallook") : null;
			monster.ap = rs.getInt("ap");
			monster.mapmove = rs.getInt("mapmove");
			monster.hp = rs.getInt("hp");
			monster.prot = rs.getInt("prot");
			monster.size = rs.getInt("size");
			monster.ressize = rs.getInt("ressize") != 0 ? rs.getInt("ressize") : null;
			monster.str = rs.getInt("str");
			monster.enc = rs.getInt("enc");
			monster.att = rs.getInt("att");
			monster.def = rs.getInt("def");
			monster.prec = rs.getInt("prec");
			monster.mr = rs.getInt("mr");
			monster.mor = rs.getInt("mor");
			monster.gcost = rs.getInt("basecost");
			monster.rcost = rs.getInt("rcost");
			monster.pathcost = rs.getInt("pathcost");
			monster.startdom = rs.getInt("startdom");
			//monster.eyes = rs.getInt("eyes") != 0 ? rs.getInt("eyes") : 2;
			//monster.copystats = rs.getInt("copystats");
			//monster.copyspr = rs.getInt("copyspr");
//			monster.restrictedgod = rs.getInt("restrictedgod");
			monster.shatteredsoul = rs.getInt("shatteredsoul");
			monster.coldres = rs.getInt("coldres");
			monster.fireres = rs.getInt("fireres");
			monster.poisonres = rs.getInt("poisonres");
			monster.shockres = rs.getInt("shockres");
			monster.darkvision = rs.getInt("darkvision");
			monster.stealthy = rs.getInt("stealthy");
			monster.seduce = rs.getInt("seduce");
			monster.succubus = rs.getInt("succubus");
			//monster.beckon = rs.getInt("beckon");
			monster.startage = rs.getInt("startage");
			monster.maxage = rs.getInt("startage");
			//monster.older = rs.getInt("older");
			monster.healer = rs.getInt("heal");
			//monster.startaff = rs.getInt("startaff");
			monster.supplybonus = rs.getInt("supplybonus");
			//monster.uwdamage = rs.getInt("uwdamage");
			monster.coldpower = rs.getInt("coldpower");
			monster.firepower = rs.getInt("firepower");
			monster.stormpower = rs.getInt("stormpower");
			monster.darkpower = rs.getInt("darkpower");
			monster.springpower = rs.getInt("springpower");
			monster.summerpower = rs.getInt("summerpower");
			monster.fallpower = rs.getInt("fallpower");
			monster.winterpower = rs.getInt("winterpower");
			monster.ambidextrous = rs.getInt("ambidextrous");
			monster.banefireshield = rs.getInt("banefireshield");
			monster.berserk = rs.getInt("berserk");
			monster.standard = rs.getInt("standard");
			monster.animalawe = rs.getInt("animalawe");
			monster.awe = rs.getInt("awe");
			monster.fear = rs.getInt("fear");
			monster.regeneration = rs.getInt("regeneration");
			monster.reinvigoration = rs.getInt("reinvigoration");
			monster.fireshield = rs.getInt("fireshield");
			monster.heat = rs.getInt("heat");
			monster.cold = rs.getInt("cold");
			monster.iceprot = rs.getInt("iceprot");
			monster.poisoncloud = rs.getInt("poisoncloud");
			monster.diseasecloud = rs.getInt("diseasecloud");
			//monster.bloodvengeance = rs.getInt("bloodvengeance");
			monster.castledef = rs.getInt("castledef");
			monster.siegebonus = rs.getInt("siegebonus");
			monster.patrolbonus = rs.getInt("patrolbonus");
			monster.pillagebonus = rs.getInt("pillagebonus");
			monster.researchbonus = rs.getInt("researchbonus");
			monster.forgebonus = rs.getInt("forgebonus");
			monster.douse = rs.getInt("douse");
			monster.nobadevents = rs.getInt("nobadevents");
			monster.incunrest = rs.getInt("incunrest");
			//monster.spreaddom = rs.getInt("spreaddom");
			//monster.leper = rs.getInt("leper");
			monster.popkill = rs.getInt("popkill");
			monster.heretic = rs.getInt("heretic");
			
			int hand = rs.getInt("hand");
			int head = rs.getInt("head");
			int body = rs.getInt("body");
			int foot = rs.getInt("foot");
			int misc = rs.getInt("misc");
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
//			String gemgen = rs.getString("gemgen");
//			if (gemgen != null) {
//				int gemprod = Integer.valueOf(gemgen.substring(0, 1));
//				String path = gemgen.substring(1, 2);
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
			monster.mounted = rs.getInt("mounted") == 1;
			monster.holy = rs.getInt("holy") == 1;
			monster.animal = rs.getInt("animal") == 1;
			monster.undead = rs.getInt("undead") == 1;
			monster.demon = rs.getInt("demon") == 1;
			monster.magicbeing = rs.getInt("magicbeing") == 1;
			
			monster.stonebeing = rs.getInt("stonebeing") == 1;
			monster.inanimate = rs.getInt("inanimate") == 1;
			monster.coldblood = rs.getInt("coldblood") == 1;
			monster.immortal = rs.getInt("immortal") == 1;
			monster.blind = rs.getInt("blind") == 1;
			monster.unique = rs.getInt("unique") == 1;
			monster.immobile = rs.getInt("immobile") == 1;
			monster.aquatic = rs.getInt("aquatic") == 1;
			monster.amphibian = rs.getInt("amphibian") == 1;
			monster.pooramphibian = rs.getInt("pooramphibian") == 1;
			monster.flying = rs.getInt("flying") == 1;
			monster.stormimmune = rs.getInt("stormimmune") == 1;
			monster.sailing1 = (rs.getInt("sailsz") == 0 ? null : rs.getInt("sailsz"));
			monster.sailing2 = rs.getInt("sailmaxsz");
			monster.forestsurvival = rs.getInt("forestsurvival") == 1;
			monster.mountainsurvival = rs.getInt("mountainsurvival") == 1;
			monster.swampsurvival = rs.getInt("swampsurvival") == 1;
			monster.wastesurvival = rs.getInt("wastesurvival") == 1;
			monster.illusion = rs.getInt("illusion") == 1;
			monster.spy = rs.getInt("spy") == 1;
			monster.assassin = rs.getInt("assassin") == 1;
			//monster.heal = rs.getInt("rec") == 1;
			monster.noheal = rs.getInt("noheal") == 1;
			monster.neednoteat = rs.getInt("neednoteat") == 1;
			monster.ethereal = rs.getInt("ethereal") == 1;
			monster.trample = rs.getInt("trample") == 1;
			//monster.entangle = rs.getInt("entangle") == 1;
			monster.eyeloss = rs.getInt("eyeloss") == 1;
			//monster.horrormark = rs.getInt("horrormark") == 1;
//			monster.poisonarmor = rs.getInt("poisonarmor") == 1;
			monster.inquisitor = rs.getInt("inquisitor") == 1;
//			monster.noitem = rs.getInt("noitem") == 1;
			
			int normalLeadership = rs.getInt("leader");
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
			int magicLeadership = rs.getInt("magicleader");
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
			int undeadLeadership = rs.getInt("undeadleader");
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
			
			monster.firstshape = Integer.toString(rs.getInt("firstshape"));
			monster.secondshape = Integer.toString(rs.getInt("secondshape"));
			monster.secondtmpshape = Integer.toString(rs.getInt("secondtmpshape"));
			monster.shapechange = Integer.toString(rs.getInt("shapechange"));
			monster.landshape = Integer.toString(rs.getInt("landshape"));
			monster.watershape = Integer.toString(rs.getInt("watershape"));
			monster.forestshape = Integer.toString(rs.getInt("forestshape"));
			//monster.plainshape = Integer.toString(rs.getInt("plainshape"));
			
//			int domNum = rs.getInt("n_domsum");
//			if (domNum == 1) {
//				monster.domsummon = Integer.toString(rs.getInt("domsum"));
//			} else if (domNum == 2) {
//				monster.domsummon2 = Integer.toString(rs.getInt("domsum"));
//			} else if (domNum == 20) {
//				monster.domsummon20 = Integer.toString(rs.getInt("domsum"));
//			}
//			
//			int mmNum = rs.getInt("n_autosum");
//			if (mmNum == 1) {
//				monster.makemonsters1 = Integer.toString(rs.getInt("autosum"));
//			} else if (mmNum == 2) {
//				monster.makemonsters2 = Integer.toString(rs.getInt("autosum"));
//			} else if (mmNum == 3) {
//				monster.makemonsters3 = Integer.toString(rs.getInt("autosum"));
//			} else if (mmNum == 4) {
//				monster.makemonsters4 = Integer.toString(rs.getInt("autosum"));
//			} else if (mmNum == 5) {
//				monster.makemonsters5 = Integer.toString(rs.getInt("autosum"));
//			}
//			
//			int sumNum = rs.getInt("n_sum");
//			if (sumNum == 1) {
//				monster.summon1 = Integer.toString(rs.getInt("sum"));
//			} else if (sumNum == 5) {
//				monster.summon5 = Integer.toString(rs.getInt("sum"));
//			}
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
			item.constlevel = rs.getInt("constlevel");
			String mainPath = rs.getString("mainpath");
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
			item.mainlevel = rs.getInt("mainlevel");
			
			String secondarypath = rs.getString("secondarypath");
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
			item.secondarylevel = rs.getInt("secondarylevel");
			
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
			item.weapon = rs.getInt("weapon") != 0 ? rs.getInt("weapon") : null;

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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"armors\" where \"number\"="+id);

				while (rs.next()) {			
					armorName = rs.getString("name");
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
	
	private static SpellDB getSpellDB(ResultSet rs) throws SQLException, ClassNotFoundException, IOException {
		SpellDB spell = new SpellDB();
		if (rs.next()) {
			spell.id = rs.getInt("number");
			spell.name = rs.getString("name");
			spell.school = rs.getInt("school");
			spell.researchlevel = rs.getInt("research_level");
			spell.path1 = rs.getInt("path_0");
			spell.path2 = rs.getInt("path_1");

			int level1 = rs.getInt("path_level_0");
			if (level1 != 0) {
				spell.pathlevel1 = level1;
			}
			int level2 = rs.getInt("path_level_1");
			if (level2 != 0) {
				spell.pathlevel2 = level2;
			}
			
			spell.fatiguecost = rs.getInt("fatigue") + 100 * rs.getInt("gem_cost");			
			
			int effectId = rs.getInt("effect_record_id");
			Statement statement = getConnection2().createStatement();
			ResultSet rsEffect = statement.executeQuery("SELECT * FROM \"effects\" where \"record_id\"="+effectId);
			rsEffect.next();

			if (rsEffect.getObject("area_base") != null) {
				spell.aoe = rsEffect.getInt("area_base");
				spell.aoe = spell.aoe + 1000 * rsEffect.getInt("area_per_level");
			} else {
				int pct = rsEffect.getInt("area_battlefield_pct");
				switch (pct) {
				case 100:
					spell.aoe = 666;
					break;
				case 50:
					spell.aoe = 663;
					break;
				case 10:
					spell.aoe = 664;
					break;
				}
			}

			spell.effect = rsEffect.getInt("effect_number") + 10000;
			spell.range = rsEffect.getInt("range_base") + 1000 * rsEffect.getInt("range_per_level");;
			spell.precision = rs.getInt("precision");
			spell.damage = rsEffect.getInt("raw_argument");
			spell.nreff = rs.getInt("effects_count");
			spell.spec = rsEffect.getInt("modifiers_mask");
			spell.nextspell = rs.getInt("next_spell");
			
			if (rsEffect.getObject("flight_sprite_number") != null) {
				spell.flightspr = rsEffect.getInt("flight_sprite_number");
			}
			if (rsEffect.getObject("explosion_sprite_number") != null) {
				spell.explspr = rsEffect.getInt("explosion_sprite_number");
			}
			spell.sound = rsEffect.getInt("sound_number");
			
			int numRestricted = 0;
			Statement statement2 = getConnection2().createStatement();
			ResultSet rsAttributesBySpell = statement2.executeQuery("SELECT * FROM \"attributes_by_spell\" where \"spell_number\"="+spell.id);
			while (rsAttributesBySpell.next()) {
				int attrRecId = rsAttributesBySpell.getInt("attribute_record_id");
				
				Statement statement3 = getConnection2().createStatement();
				ResultSet rsRestrict = statement3.executeQuery("SELECT * FROM \"restrict_to_nations_by_attribute\" where \"attribute_record_id\"="+attrRecId);
				if (rsRestrict.next()) {
					numRestricted++;
					switch (numRestricted) {
					case 1:
						spell.restricted1 = rsRestrict.getInt("nation_number");
						break;
					case 2:
						spell.restricted2 = rsRestrict.getInt("nation_number");
						break;
					case 3:
						spell.restricted3 = rsRestrict.getInt("nation_number");
						break;
					}
				}
				statement3.close();
				
				Statement statement4 = getConnection2().createStatement();
				ResultSet rsAttributes = statement4.executeQuery("SELECT * FROM \"attributes\" where \"record_id\"="+attrRecId);
				if (rsAttributes.next()) {
					if (rsAttributes.getInt("attribute_number") == 700) {
						spell.provrange = rsAttributes.getInt("raw_value");
					}
					if (rsAttributes.getInt("attribute_number") == 702) {
						spell.onlygeosrc = rsAttributes.getInt("raw_value");
					}
					if (rsAttributes.getInt("attribute_number") == 703) {
						spell.onlyowndst = rsAttributes.getInt("raw_value");
					}
					if (rsAttributes.getInt("attribute_number") == 706) {
						spell.nolandtrace = rsAttributes.getInt("raw_value");
					}
				}
				statement4.close();
			}
			//spell.damagemon;
			//spell.onlygeodst;
			//spell.onlyfriendlydst;
			//spell.nowatertrace;
			//spell.walkable;
			statement2.close();
			statement.close();
		}
		return spell;
	}

	public static String getWeaponName(int id) {
		String weaponName = weaponNameMap.get(Integer.valueOf(id));
		if (weaponName == null) {
			try {
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"weapons\" where \"number\"="+id);

				while (rs.next()) {			
					weaponName = rs.getString("name");
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
	
	public static String getPoptypeName(int id) {
		String poptypeName = poptypeNameMap.get(Integer.valueOf(id));
		if (poptypeName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"type\" FROM \"poptypes\" where \"id\"="+id);

				while (rs.next()) {			
					poptypeName = rs.getString("type");
				}

				statement.close();
				poptypeNameMap.put(Integer.valueOf(id), poptypeName);

			} catch (SQLException ex) {
				ex.printStackTrace();
			} catch (ClassNotFoundException ex) {
				ex.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return poptypeName;
	}
	
	public static String getMonsterName(int id) {
		String monsterName = monsterNameMap.get(Integer.valueOf(id));
		if (monsterName == null) {
			try {
				Statement statement = getConnection().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"units_base\" where \"id\"="+id);

				while (rs.next()) {			
					monsterName = rs.getString("name");
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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\" FROM \"spells\" where \"number\"="+id);

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
				Statement statement = getConnection2().createStatement();
				ResultSet rs = statement.executeQuery("SELECT \"name\", \"file_name_base\" FROM \"nations\" where \"number\"="+id);

				while (rs.next()) {			
					nationName = rs.getString("name");
					String fileName = rs.getString("file_name_base");
					if (fileName.startsWith("early")) {
						nationName = "EA " + nationName;
					} else if (fileName.startsWith("mid")) {
						nationName = "MA " + nationName;
					} else if (fileName.startsWith("late")) {
						nationName = "LA " + nationName;
					} else {
						nationName = "Unknown Era " + nationName;
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

	private static Connection getConnection2() throws IOException, ClassNotFoundException, SQLException {
		if (connection2 == null) {
			Path path = new Path("$nl$/data/Dominions.sqlite");
			URL url = FileLocator.find(Activator.getDefault().getBundle(), path, null);
			String dbPath = FileLocator.toFileURL(url).getPath();
			dbPath = dbPath.substring(1, dbPath.length());

			Class.forName("org.sqlite.JDBC");
			connection2 = DriverManager.getConnection("jdbc:sqlite:" + dbPath);
		}
		return connection2;
	}
}
