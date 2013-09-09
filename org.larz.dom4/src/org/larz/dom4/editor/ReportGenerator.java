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
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.larz.dom4.db.ArmorDB;
import org.larz.dom4.db.Database;
import org.larz.dom4.db.ItemDB;
import org.larz.dom4.db.MonsterDB;
import org.larz.dom4.db.NationDB;
import org.larz.dom4.db.SiteDB;
import org.larz.dom4.db.SpellDB;
import org.larz.dom4.db.WeaponDB;
import org.larz.dom4.dm.dm.AbstractElement;
import org.larz.dom4.dm.dm.Armor;
import org.larz.dom4.dm.dm.ArmorInst1;
import org.larz.dom4.dm.dm.ArmorInst2;
import org.larz.dom4.dm.dm.ArmorMods;
import org.larz.dom4.dm.dm.Dom4Mod;
import org.larz.dom4.dm.dm.Item;
import org.larz.dom4.dm.dm.ItemInst1;
import org.larz.dom4.dm.dm.ItemInst2;
import org.larz.dom4.dm.dm.ItemInst3;
import org.larz.dom4.dm.dm.ItemMods;
import org.larz.dom4.dm.dm.Monster;
import org.larz.dom4.dm.dm.MonsterInst1;
import org.larz.dom4.dm.dm.MonsterInst2;
import org.larz.dom4.dm.dm.MonsterInst3;
import org.larz.dom4.dm.dm.MonsterInst4;
import org.larz.dom4.dm.dm.MonsterInst5;
import org.larz.dom4.dm.dm.MonsterInst6;
import org.larz.dom4.dm.dm.MonsterMods;
import org.larz.dom4.dm.dm.Nation;
import org.larz.dom4.dm.dm.NationInst1;
import org.larz.dom4.dm.dm.NationInst2;
import org.larz.dom4.dm.dm.NationInst3;
import org.larz.dom4.dm.dm.NationInst4;
import org.larz.dom4.dm.dm.NationInst5;
import org.larz.dom4.dm.dm.NationMods;
import org.larz.dom4.dm.dm.NewArmor;
import org.larz.dom4.dm.dm.NewItem;
import org.larz.dom4.dm.dm.NewMonster;
import org.larz.dom4.dm.dm.NewSite;
import org.larz.dom4.dm.dm.NewSpell;
import org.larz.dom4.dm.dm.NewWeapon;
import org.larz.dom4.dm.dm.SelectArmorById;
import org.larz.dom4.dm.dm.SelectArmorByName;
import org.larz.dom4.dm.dm.SelectItemById;
import org.larz.dom4.dm.dm.SelectItemByName;
import org.larz.dom4.dm.dm.SelectMonsterById;
import org.larz.dom4.dm.dm.SelectMonsterByName;
import org.larz.dom4.dm.dm.SelectNation;
import org.larz.dom4.dm.dm.SelectSiteById;
import org.larz.dom4.dm.dm.SelectSiteByName;
import org.larz.dom4.dm.dm.SelectSpellById;
import org.larz.dom4.dm.dm.SelectSpellByName;
import org.larz.dom4.dm.dm.SelectWeaponById;
import org.larz.dom4.dm.dm.SelectWeaponByName;
import org.larz.dom4.dm.dm.Site;
import org.larz.dom4.dm.dm.SiteInst1;
import org.larz.dom4.dm.dm.SiteInst2;
import org.larz.dom4.dm.dm.SiteInst3;
import org.larz.dom4.dm.dm.SiteInst4;
import org.larz.dom4.dm.dm.SiteMods;
import org.larz.dom4.dm.dm.Spell;
import org.larz.dom4.dm.dm.SpellInst1;
import org.larz.dom4.dm.dm.SpellInst2;
import org.larz.dom4.dm.dm.SpellInst3;
import org.larz.dom4.dm.dm.SpellInst4;
import org.larz.dom4.dm.dm.SpellInst5;
import org.larz.dom4.dm.dm.SpellMods;
import org.larz.dom4.dm.dm.Weapon;
import org.larz.dom4.dm.dm.WeaponInst1;
import org.larz.dom4.dm.dm.WeaponInst2;
import org.larz.dom4.dm.dm.WeaponInst3;
import org.larz.dom4.dm.dm.WeaponInst4;
import org.larz.dom4.dm.dm.WeaponMods;

import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Chunk;
import com.itextpdf.text.Document;
import com.itextpdf.text.Element;
import com.itextpdf.text.Font;
import com.itextpdf.text.Font.FontFamily;
import com.itextpdf.text.PageSize;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;

public class ReportGenerator {
	public static final Font SUBTITLE = new Font(FontFamily.HELVETICA, 10);
	public static final Font TITLE = new Font(FontFamily.HELVETICA, 14, Font.BOLD);
	public static final Font TEXT = new Font(FontFamily.HELVETICA, 8);
	public static final Font BOLD_TEXT = new Font(FontFamily.HELVETICA, 8, Font.BOLD);
	
	public static final String ARMOR = "Armor";
	public static final String WEAPONS = "Weapons";
	public static final String MONSTERS = "Monsters";
	public static final String SPELLS = "Spells";
	public static final String ITEMS = "Items";
	public static final String SITES = "Sites";
	public static final String NATIONS = "Nations";

	public static void generateReport(XtextEditor sourcePage, final Shell shell) {
		final IXtextDocument myDocument = ((XtextEditor)sourcePage).getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				Map<String, Map<String, ModObject>> cellMap = new HashMap<String, Map<String, ModObject>>();
				
				Dom4Mod dom4Mod = (Dom4Mod)resource.getContents().get(0);
				EList<AbstractElement> elements = dom4Mod.getElements();
				for (AbstractElement element : elements) {
					if (element instanceof SelectArmorById || element instanceof SelectArmorByName) {
						String name = getSelectArmorname((Armor)element);
						if (name == null) continue;
						String id = getArmorid((Armor)element);

						Map<String, ModObject> map = cellMap.get(ARMOR);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(ARMOR, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Armor)element, modObject.propertyMap);
					} else if (element instanceof NewArmor) {
						String name = getArmorname((Armor)element);
						String id = getArmorid((Armor)element);

						Map<String, ModObject> map = cellMap.get(ARMOR);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(ARMOR, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Armor)element, modObject.propertyMap);
					} else if (element instanceof SelectWeaponById || element instanceof SelectWeaponByName) {
						String name = getSelectWeaponname((Weapon)element);
						String id = getWeaponid((Weapon)element);

						Map<String, ModObject> map = cellMap.get(WEAPONS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(WEAPONS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Weapon)element, modObject.propertyMap);
					} else if (element instanceof NewWeapon) {
						String name = getWeaponname((Weapon)element);
						String id = getWeaponid((Weapon)element);

						Map<String, ModObject> map = cellMap.get(WEAPONS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(WEAPONS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Weapon)element, modObject.propertyMap);
					} else if (element instanceof SelectMonsterById || element instanceof SelectMonsterByName) {
						String name = getSelectMonstername((Monster)element);
						String id = getMonsterid((Monster)element);

						Map<String, ModObject> map = cellMap.get(MONSTERS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(MONSTERS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Monster)element, modObject.propertyMap);
					} else if (element instanceof NewMonster) {
						String name = getMonstername((Monster)element);
						String id = getMonsterid((Monster)element);

						Map<String, ModObject> map = cellMap.get(MONSTERS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(MONSTERS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Monster)element, modObject.propertyMap);
					} else if (element instanceof SelectSpellById || element instanceof SelectSpellByName) {
						String name = getSelectSpellname((Spell)element);
						String id = getSpellid((Spell)element);

						Map<String, ModObject> map = cellMap.get(SPELLS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(SPELLS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Spell)element, modObject.propertyMap);
					} else if (element instanceof NewSpell) {
						String name = getSpellname((Spell)element);
						//String id = getSpellid((Spell)element);

						Map<String, ModObject> map = cellMap.get(SPELLS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(SPELLS, map);
						}
						ModObject modObject = map.get(name);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = "" + name;
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(name, modObject);
						}
						setPropertyValues((Spell)element, modObject.propertyMap);
					} else if (element instanceof SelectItemById || element instanceof SelectItemByName) {
						String name = getSelectItemname((Item)element);
						String id = getItemid((Item)element);

						Map<String, ModObject> map = cellMap.get(ITEMS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(ITEMS, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Item)element, modObject.propertyMap);
					} else if (element instanceof NewItem) {
						String name = getItemname((Item)element);
						//String id = getItemid((Item)element);

						Map<String, ModObject> map = cellMap.get(ITEMS);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(ITEMS, map);
						}
						ModObject modObject = map.get(name);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name;
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(name, modObject);
						}
						setPropertyValues((Item)element, modObject.propertyMap);
					} else if (element instanceof SelectSiteById || element instanceof SelectSiteByName) {
						String name = getSelectSitename((Site)element);
						String id = getSiteid((Site)element);

						Map<String, ModObject> map = cellMap.get(SITES);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(SITES, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Site)element, modObject.propertyMap);
					} else if (element instanceof NewSite) {
						String name = getSitename((Site)element);
						String id = getSiteid((Site)element);

						Map<String, ModObject> map = cellMap.get(SITES);
						if (map == null) {
							map = new HashMap<String, ModObject>();
							cellMap.put(SITES, map);
						}
						ModObject modObject = map.get(id);
						if (modObject == null) {
							modObject = new ModObject();
							modObject.title = name + " (" + id + ")";
							modObject.propertyMap = new HashMap<String, PropertyValues>();
							map.put(id, modObject);
						}
						setPropertyValues((Site)element, modObject.propertyMap);
					} else if (element instanceof SelectNation) {
//						String name = getSelectNationname((Nation)element);
//						String id = getNationid((Nation)element);
//
//						Map<String, ModObject> map = cellMap.get(NATIONS);
//						if (map == null) {
//							map = new HashMap<String, ModObject>();
//							cellMap.put(NATIONS, map);
//						}
//						ModObject modObject = map.get(id);
//						if (modObject == null) {
//							modObject = new ModObject();
//							modObject.title = name + " (" + id + ")";
//							modObject.propertyMap = new HashMap<String, PropertyValues>();
//							map.put(id, modObject);
//						}
//						setPropertyValues((SelectNation)element, modObject.propertyMap, resource);
					}
					
				}

				try {
					// step 1
					Document document = new Document(PageSize.LETTER.rotate());
					// step 2
					File tempFile = File.createTempFile("dom4editor", ".pdf");
					tempFile.deleteOnExit();
					FileOutputStream tempFileOutputStream = new FileOutputStream(tempFile);
							
					PdfWriter.getInstance(document, tempFileOutputStream);
					// step 3
					document.open();
					
					List <Map.Entry<String, Map<String, ModObject>>> cellList = new ArrayList<Map.Entry<String, Map<String, ModObject>>>();
					for (Map.Entry<String, Map<String, ModObject>> innerEntry : cellMap.entrySet()) {
						cellList.add(innerEntry);
					}
				    Collections.sort(cellList, new Comparator<Map.Entry<String, Map<String, ModObject>>>() {
						@Override
						public int compare(Map.Entry<String, Map<String, ModObject>> o1, Map.Entry<String, Map<String, ModObject>> o2) {
							return o1.getKey().compareTo(o2.getKey());
						}
					});

					for (Map.Entry<String, Map<String, ModObject>> entry : cellList) {
						PdfPTable table = new PdfPTable(1);
						table.setWidthPercentage(100f);

						PdfPCell cell = new PdfPCell(new Phrase(entry.getKey(), TITLE));
					    cell.setBackgroundColor(BaseColor.LIGHT_GRAY);
					    cell.setFixedHeight(26f);
					    cell.setHorizontalAlignment(Element.ALIGN_CENTER);
					    cell.setVerticalAlignment(Element.ALIGN_BOTTOM);
					    table.addCell(cell);
					    table.setHeaderRows(1);

					    List<Map.Entry<String, ModObject>> list = new ArrayList<Map.Entry<String, ModObject>>();
						for (Map.Entry<String, ModObject> innerEntry : entry.getValue().entrySet()) {
							list.add(innerEntry);
						}
					    
					    Collections.sort(list, new Comparator<Map.Entry<String, ModObject>>() {
							@Override
							public int compare(Map.Entry<String, ModObject> o1, Map.Entry<String, ModObject> o2) {
								return o1.getValue().title.compareTo(o2.getValue().title);
							}
						});
					    
					    PdfPTable propertyTable = null;
					    if (entry.getKey().equals(ARMOR)) {
					    	propertyTable = getTable(new String[]{"name","type","prot","def","enc","rcost"}, 
					    			new String[]{"Name","Type","Prot","Def","Enc","Rcost"}, 
					    			new ValueTranslator[]{null, new ValueTranslator() {
										@Override
										public String translate(String value) {
											if (value == null) return null;
											if (value.equals("4")) return "Shield";
											if (value.equals("5")) return "Body Armor";
											if (value.equals("6")) return "Helmet";
											return "Unknown";
										}
									}, null, null, null, null},
									null,
					    			list);
						    propertyTable.setWidths(new float[]{5,1,1,1,1,1});
					    }
					    if (entry.getKey().equals(WEAPONS)) {
					    	propertyTable = getTable(new String[]{"name","dmg","att","nratt","def","len","range","ammo","rcost"},
					    			new String[]{"Name","Dmg","Att","Nratt","Def","Len","Range","Ammo","Rcost"},
					    			null,
					    			null,
					    			list);
						    propertyTable.setWidths(new float[]{5,1,1,1,1,1,1,1,1});
					    }
					    if (entry.getKey().equals(MONSTERS)) {
						    propertyTable = getTable(new String[]{"name","hp","prot","MOVE","size","ressize","str","enc","att","def","prec","mr","mor","gcost","rcost"},
						    		new String[]{"Name","HP","Prot","Move","Size","Rsize","Str","Enc","Att","Def","Prec","MR","Mor","Gcost","Rcost"},
						    		null,
						    		new ValueCombiner[]{null, null, null, new ValueCombiner() {
										@Override
										public String translate(String[] value) {
											if (value[0] == null && value[1] == null) return null;
											return value[0] + "/" + value[1];
										}
										@Override
										public String[] getNeededColumns() {
											return new String[]{"mapmove", "ap"};
										}
									}, null, null, null, null, null, null, null, null, null, null, null},
						    		list);
						    propertyTable.setWidths(new float[]{5,1,1,1,1,1,1,1,1,1,1,1,1,1,1});
						}
					    if (entry.getKey().equals(ITEMS)) {
					    	propertyTable = getTable(new String[]{"name", "constlevel", "PATH", "type", "weapon", "armor"}, 
					    			new String[]{"Name", "Constlevel", "Path Req", "Type", "Weapon", "Armor"},
					    			new ValueTranslator[]{null, null, null, new ValueTranslator() {
										@Override
										public String translate(String value) {
											if (value == null) return null;
											if (value.equals("1")) return "1-h Weapon";
											if (value.equals("2")) return "2-h Weapon";
											if (value.equals("3")) return "Missile Weapon";
											if (value.equals("4")) return "Shield";
											if (value.equals("5")) return "Body Armor";
											if (value.equals("6")) return "Helmet";
											if (value.equals("7")) return "Boots";
											if (value.equals("8")) return "Misc";
											return "Unknown";
										}
									}, null, null},
						    		new ValueCombiner[]{null, null, new ValueCombiner() {
										@Override
										public String translate(String[] value) {
											if (value[0] == null && value[1] == null && value[2] == null && value[3] == null) return null;
											StringBuffer buf = new StringBuffer();
											if (value[0] != null && !value[0].equals("null")) {
												buf.append(getPathName(Integer.parseInt(value[0])) + value[1]);
											}
											if (value[2] != null && !value[2].equals("null") && !value[2].equals("-1")) {
												buf.append(getPathName(Integer.parseInt(value[2])) + value[3]);
											}
											return buf.toString();
										}
										@Override
										public String[] getNeededColumns() {
											return new String[]{"mainpath", "mainlevel", "secondarypath", "secondarylevel"};
										}
									}, null, null, null},
					    			list);
					    	propertyTable.setWidths(new float[]{2.5f,1,1,1,2.5f,2.5f});
					    }
					    if (entry.getKey().equals(SPELLS)) {
					    	propertyTable = getTable(new String[]{"name", "school", "researchlevel", "aoe", "damage", "effect", "fatiguecost", "nreff", "range", "precision", "spec", "nextspell"},
					    			new String[]{"Name", "School", "Research", "AOE", "Damage", "Effect", "Fatigue", "Nreff", "Range", "Precision", "Spec", "Nextspell"},
					    			null,
					    			null,
					    			list);
					    	propertyTable.setWidths(new float[]{3,1,1,1,1,1,1,1,1,1,1,1});
					    }
					    if (entry.getKey().equals(NATIONS)) {
					    	propertyTable = getTable(new String[]{"name", "startsite1", "startsite2", "startsite3", "startsite4", "era", "startfort"}, list);
					    	propertyTable.setWidths(new float[]{5,1,1,1,1,1,1});
					    }
					    if (entry.getKey().equals(SITES)) {
					    	propertyTable = getTable(new String[]{"name", "path", "level", "rarity", "loc", "homemon", "homecom", "gold", "res"},
					    			new String[]{"Name", "Path", "Level", "Rarity", "Loc", "Homemon", "Homecom", "Gold", "Res"},
					    			new ValueTranslator[]{null, new ValueTranslator() {
										@Override
										public String translate(String value) {
											if (value == null) return null;
											if (value.equals("0")) return "Fire";
											if (value.equals("1")) return "Air";
											if (value.equals("2")) return "Water";
											if (value.equals("3")) return "Earth";
											if (value.equals("4")) return "Astral";
											if (value.equals("5")) return "Death";
											if (value.equals("6")) return "Nature";
											if (value.equals("7")) return "Blood";
											return "Unknown";
										}
									}, null, null, null, null, null, null, null},
									null,
					    			list);
					    	propertyTable.setWidths(new float[]{5,1,1,1,1,1,1,1,1});
					    }
					    PdfPCell innerCell = new PdfPCell();
					    innerCell.addElement(propertyTable);
					    innerCell.setBorder(PdfPCell.NO_BORDER);
					    innerCell.setHorizontalAlignment(Element.ALIGN_LEFT);

					    table.addCell(innerCell);
						document.add(table);
						document.newPage();
					}
					document.close();
					
		    		tempFileOutputStream.flush();
		    		tempFileOutputStream.close();

		    		Program pdfViewer = Program.findProgram("pdf");
			    	if (pdfViewer != null) {
			    		pdfViewer.execute(tempFile.getAbsolutePath());
			    	} else {			 
			    		FileDialog dialog = new FileDialog(shell, SWT.SAVE);
			    		dialog.setFilterExtensions(new String[]{"*.pdf"});
			    		if (dialog.open() != null) {
							FileInputStream from = null;
							FileOutputStream to = null;
							try {
								String filterPath =  dialog.getFilterPath();
					    		String name =  dialog.getFileName();

								from = new FileInputStream(new File(tempFile.getAbsolutePath()));
								to = new FileOutputStream(new File(filterPath + File.separator + name));
								byte[] buffer = new byte[4096];
								int bytesRead;

								while ((bytesRead = from.read(buffer)) != -1) {
									to.write(buffer, 0, bytesRead); // write
								}
							} finally {
								if (from != null) {
									from.close();
								}
								if (to != null) {
									to.close();
								}
							}
			    		}
			    	}

				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	private static String getArmorname(Armor armor) {
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

	private static String getSelectArmorname(Armor armor) {
		String name = null;
		if (armor instanceof SelectArmorByName) {
			name = ((SelectArmorByName)armor).getValue();
		} else if (armor instanceof SelectArmorById) {
			int id = ((SelectArmorById)armor).getValue();
			name = Database.getArmorName(id);
		}
		if (getArmorname(armor) != null) {
			name = getArmorname(armor);
		}
		return name;
	}
	
	private static String getArmorid(Armor armor) {
		if (armor instanceof SelectArmorByName) {
			ArmorDB armorDB = Database.getArmor(((SelectArmorByName)armor).getValue());
			if (armorDB != null && armorDB.id != null) {
				return Integer.toString(armorDB.id);
			}
		} else if (armor instanceof SelectArmorById) {
			return Integer.toString(((SelectArmorById)armor).getValue());
		} else if (armor instanceof NewArmor) {
			return Integer.toString(((NewArmor)armor).getValue());
		}
		return null;
	}

	private static String getWeaponname(Weapon weapon) {
		EList<WeaponMods> list = weapon.getMods();
		for (WeaponMods mod : list) {
			if (mod instanceof WeaponInst1) {
				if (((WeaponInst1)mod).isName()) {
					return ((WeaponInst1)mod).getValue();
				}
			}
		}
		return null;
	}

	private static String getMonstername(Monster monster) {
		EList<MonsterMods> list = monster.getMods();
		for (MonsterMods mod : list) {
			if (mod instanceof MonsterInst1) {
				if (((MonsterInst1)mod).isName()) {
					return ((MonsterInst1)mod).getValue();
				}
			}
		}
		return null;
	}

	private static String getSpellname(Spell spell) {
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

	private static String getItemname(Item item) {
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

	private static String getSitename(Site site) {
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

	private static String getNationname(SelectNation monster) {
		EList<NationMods> list = monster.getMods();
		for (NationMods mod : list) {
			if (mod instanceof NationInst1) {
				if (((NationInst1)mod).isName()) {
					return ((NationInst1)mod).getValue();
				}
			}
		}
		return null;
	}

	private static String getSelectWeaponname(Weapon weapon) {
		String name = null;
		if (weapon instanceof SelectWeaponByName) {
			name = ((SelectWeaponByName)weapon).getValue();
		} else if (weapon instanceof SelectWeaponById) {
			int id = ((SelectWeaponById)weapon).getValue();
			name = Database.getWeaponName(id);
		}
		if (getWeaponname(weapon) != null) {
			name = getWeaponname(weapon);
		}
		return name;
	}
	
	private static String getWeaponid(Weapon weapon) {
		if (weapon instanceof SelectWeaponByName) {
			WeaponDB weaponDB = Database.getWeapon(((SelectWeaponByName)weapon).getValue());
			if (weaponDB != null && weaponDB.id != null) {
				return Integer.toString(weaponDB.id);
			}
		} else if (weapon instanceof SelectWeaponById) {
			return Integer.toString(((SelectWeaponById)weapon).getValue());
		} else if (weapon instanceof NewWeapon) {
			return Integer.toString(((NewWeapon)weapon).getValue());
		}
		return null;
	}

	private static String getSelectMonstername(Monster monster) {
		String name = null;
		if (monster instanceof SelectMonsterByName) {
			name = ((SelectMonsterByName)monster).getValue();
		} else if (monster instanceof SelectMonsterById) {
			int id = ((SelectMonsterById)monster).getValue();
			name = Database.getMonsterName(id);
		}
		if (getMonstername(monster) != null) {
			name = getMonstername(monster);
		}
		return name;
	}
	
	private static String getMonsterid(Monster monster) {
		if (monster instanceof SelectMonsterByName) {
			MonsterDB monsterDB = Database.getMonster(((SelectMonsterByName)monster).getValue());
			if (monsterDB != null && monsterDB.id != null) {
				return Integer.toString(monsterDB.id);
			}
		} else if (monster instanceof SelectMonsterById) {
			return Integer.toString(((SelectMonsterById)monster).getValue());
		} else if (monster instanceof NewMonster) {
			return Integer.toString(((NewMonster)monster).getValue());
		}
		return null;
	}

	private static String getSelectSitename(Site site) {
		String name = null;
		if (site instanceof SelectSiteByName) {
			name = ((SelectSiteByName)site).getValue();
		} else if (site instanceof SelectSiteById) {
			int id = ((SelectSiteById)site).getValue();
			name = Database.getSiteName(id);
		}
		if (getSitename(site) != null) {
			name = getSitename(site);
		}
		return name;
	}
	
	private static String getSiteid(Site site) {
		if (site instanceof SelectSiteByName) {
			SiteDB siteDB = Database.getSite(((SelectSiteByName)site).getValue());
			if (siteDB != null && siteDB.id != null) {
				return Integer.toString(siteDB.id);
			}
		} else if (site instanceof SelectSiteById) {
			return Integer.toString(((SelectSiteById)site).getValue());
		} else if (site instanceof NewSite) {
			return Integer.toString(((NewSite)site).getValue());
		}
		return null;
	}

	private static String getSelectNationname(Nation nation) {
		String name = null;
		if (nation instanceof SelectNation) {
			int id = ((SelectNation)nation).getValue();
			name = Database.getNationName(id);
		}
		if (getNationname((SelectNation)nation) != null) {
			name = getNationname((SelectNation)nation);
		}
		return name;
	}
	
	private static String getNationid(Nation nation) {
		if (nation instanceof SelectNation) {
			return Integer.toString(((SelectNation)nation).getValue());
		}
		return null;
	}

	private static String getSelectSpellname(Spell spell) {
		String name = null;
		if (spell instanceof SelectSpellByName) {
			name = ((SelectSpellByName)spell).getValue();
		} else if (spell instanceof SelectSpellById) {
			int id = ((SelectSpellById)spell).getValue();
			name = Database.getSpellName(id);
		}
		if (getSpellname(spell) != null) {
			name = getSpellname(spell);
		}
		return name;
	}
	
	private static String getSpellid(Spell spell) {
		if (spell instanceof SelectSpellByName) {
			SpellDB spellDB = Database.getSpell(((SelectSpellByName)spell).getValue());
			if (spellDB != null && spellDB.id != null) {
				return Integer.toString(spellDB.id);
			}
		} else if (spell instanceof SelectSpellById) {
			return Integer.toString(((SelectSpellById)spell).getValue());
		}
		return null;
	}

	private static String getSelectItemname(Item item) {
		String name = null;
		if (item instanceof SelectItemByName) {
			name = ((SelectItemByName)item).getValue();
		} else if (item instanceof SelectItemById) {
			int id = ((SelectItemById)item).getValue();
			name = Database.getItemName(id);
		}
		if (getItemname(item) != null) {
			name = getItemname(item);
		}
		return name;
	}
	
	private static String getItemid(Item item) {
		if (item instanceof SelectItemByName) {
			ItemDB itemDB = Database.getItem(((SelectItemByName)item).getValue());
			if (itemDB != null && itemDB.id != null) {
				return Integer.toString(itemDB.id);
			}
		} else if (item instanceof SelectMonsterById) {
			return Integer.toString(((SelectItemById)item).getValue());
		}
		return null;
	}

	private static void setPropertyValues(Armor armor, Map<String, PropertyValues> propertyMap) {
		EList<ArmorMods> list = armor.getMods();
		for (ArmorMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getArmorid(armor) != null ? getArmorid(armor) : "0");
					if (id != 0) {
						ArmorDB armorDB = Database.getArmor(id);
						if (armorDB != null && armorDB.id != null) {
							try {
								oldVal = "" + armorDB.getClass().getField(field.getName()).get(armorDB);
							} catch (NoSuchFieldException e) {
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof ArmorInst1) {
							newVal = ((ArmorInst1)mod).getValue();
						} else if (mod instanceof ArmorInst2) {
							newVal = ""+((ArmorInst2)mod).getValue();
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}
		}
	}

	private static void setPropertyValues(Weapon weapon, Map<String, PropertyValues> propertyMap) {
		EList<WeaponMods> list = weapon.getMods();
		for (WeaponMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getWeaponid(weapon) != null ? getWeaponid(weapon) : "0");
					if (id != 0) {
						WeaponDB weaponDB = Database.getWeapon(id);
						if (weaponDB != null && weaponDB.id != null) {
							try {
								oldVal = "" + weaponDB.getClass().getField(field.getName()).get(weaponDB);
							} catch (NoSuchFieldException e) {
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof WeaponInst1) {
							newVal = ((WeaponInst1)mod).getValue();
						} else if (mod instanceof WeaponInst2) {
							newVal = ""+((WeaponInst2)mod).getValue();
						} else if (mod instanceof WeaponInst3) {
							newVal = ""+((WeaponInst3)mod).getValue1()+", " + ((WeaponInst3)mod).getValue2();
						} else if (mod instanceof WeaponInst4) {
							newVal = "true";
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private static void setPropertyValues(Monster monster, Map<String, PropertyValues> propertyMap) {
		EList<MonsterMods> list = monster.getMods();
		for (MonsterMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			int weaponCount = 1;
			int armorCount = 1;
			int magicBoostCount = 1;
			int gemprodCount = 1;
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getMonsterid(monster) != null ? getMonsterid(monster) : "0");
					if (id != 0) {
						MonsterDB monsterDB = Database.getMonster(id);
						if (monsterDB != null && monsterDB.id != null) {
							String fieldName = field.getName();
							if (fieldName.equals("weapon")) {
								fieldName += weaponCount++;
							} else if (fieldName.equals("armor")) {
								fieldName += armorCount++;
							} else if (fieldName.equals("magicboost")) {
								fieldName += magicBoostCount++;
							} else if (fieldName.equals("gemprod")) {
								fieldName += gemprodCount++;
							} 
							if (fieldName.equals("magicskill")) {
								int path = ((MonsterInst3)mod).getValue1();
								if (monsterDB.magicskillpath1 != null && monsterDB.magicskillpath1.intValue() == path) {
									oldVal = monsterDB.magicskillpath1 + ", " + monsterDB.magicskilllevel1;
								} else if (monsterDB.magicskillpath2 != null && monsterDB.magicskillpath2.intValue() == path) {
									oldVal = monsterDB.magicskillpath2 + ", " + monsterDB.magicskilllevel2;
								} else if (monsterDB.magicskillpath3 != null && monsterDB.magicskillpath3.intValue() == path) {
									oldVal = monsterDB.magicskillpath3 + ", " + monsterDB.magicskilllevel3;
								} else if (monsterDB.magicskillpath4 != null && monsterDB.magicskillpath4.intValue() == path) {
									oldVal = monsterDB.magicskillpath4 + ", " + monsterDB.magicskilllevel4;
								}
							} else if (!fieldName.equals("cleararmor") && !fieldName.equals("clearweapons") && !fieldName.equals("custommagic")) {
								try {
									oldVal = "" + monsterDB.getClass().getField(fieldName).get(monsterDB);
								} catch (NoSuchFieldException e) {
								}
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof MonsterInst1) {
							newVal = ((MonsterInst1)mod).getValue();
						} else if (mod instanceof MonsterInst2) {
							newVal = ""+((MonsterInst2)mod).getValue();
						} else if (mod instanceof MonsterInst3) {
							newVal = ""+((MonsterInst3)mod).getValue1()+", " + ((MonsterInst3)mod).getValue2();
						} else if (mod instanceof MonsterInst4) {
							newVal = "true";
						} else if (mod instanceof MonsterInst5) {
							if (((MonsterInst5)mod).getValue1() != null) {
								newVal = ((MonsterInst5)mod).getValue1();
							} else {
								newVal = ""+((MonsterInst5)mod).getValue2();
							}
						} else if (mod instanceof MonsterInst6) {
							newVal = ""+((MonsterInst6)mod).getValue();
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private static void setPropertyValues(Spell spell, Map<String, PropertyValues> propertyMap) {
		EList<SpellMods> list = spell.getMods();
		for (SpellMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getSpellid(spell) != null ? getSpellid(spell) : "0");
					if (id != 0) {
						SpellDB spellDB = Database.getSpell(id);
						if (spellDB != null && spellDB.id != null) {
							String fieldName = field.getName();
							if (fieldName.equals("path")) {
								if (((SpellInst3)mod).getValue1() == 0) {
									oldVal = "0, " + spellDB.path1;
								} else if (((SpellInst3)mod).getValue1() == 1) {
									oldVal = "1, " + spellDB.path2;
								} else {
									System.out.println("invalid path: " + ((SpellInst3)mod).getValue1());
								}
							} else if (fieldName.equals("pathlevel")) {
								if (((SpellInst3)mod).getValue1() == 0) {
									oldVal = "0, " + spellDB.pathlevel1;
								} else if (((SpellInst3)mod).getValue1() == 1) {
									oldVal = "1, " + spellDB.pathlevel2;
								} else {
									System.out.println("invalid pathlevel: " + ((SpellInst3)mod).getValue1());
								}
							} else {
								try {
									oldVal = "" + spellDB.getClass().getField(fieldName).get(spellDB);
								} catch (NoSuchFieldException e) {
								}
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof SpellInst1) {
							newVal = ((SpellInst1)mod).getValue();
						} else if (mod instanceof SpellInst2) {
							newVal = ""+((SpellInst2)mod).getValue();
						} else if (mod instanceof SpellInst3) {
							newVal = ""+((SpellInst3)mod).getValue1()+", " + ((SpellInst3)mod).getValue2();
						} else if (mod instanceof SpellInst4) {
							newVal = "true";
						} else if (mod instanceof SpellInst5) {
							newVal = ((SpellInst5)mod).getValue1()+", " + ((SpellInst5)mod).getValue2();
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}
		}
	}

	private static void setPropertyValues(Item item, Map<String, PropertyValues> propertyMap) {
		EList<ItemMods> list = item.getMods();
		for (ItemMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getItemid(item) != null ? getItemid(item) : "0");
					if (id != 0) {
						ItemDB itemDB = Database.getItem(id);
						if (itemDB != null && itemDB.id != null) {
							try {
								oldVal = "" + itemDB.getClass().getField(field.getName()).get(itemDB);
							} catch (NoSuchFieldException e) {
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof ItemInst1) {
							newVal = ((ItemInst1)mod).getValue();
						} else if (mod instanceof ItemInst2) {
							newVal = ""+((ItemInst2)mod).getValue();
						} else if (mod instanceof ItemInst3) {
							if (((ItemInst3)mod).getValue1() != null) {
								newVal = ((ItemInst3)mod).getValue1();
							} else {
								newVal = "" + ((ItemInst3)mod).getValue2();
							}
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private static void setPropertyValues(Site site, Map<String, PropertyValues> propertyMap) {
		EList<SiteMods> list = site.getMods();
		for (SiteMods mod : list) {
			int comCount = 1;
			int monCount = 1;
			int gemCount = 1;
			Field[] fields = mod.getClass().getDeclaredFields();
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					String property = field.getName();
					if ("name".equals(property)) {
						continue;
					}

					String oldVal = null;
					int id = Integer.valueOf(getSiteid(site) != null ? getSiteid(site) : "0");
					if (id != 0) {
						SiteDB siteDB = Database.getSite(id);
						if (siteDB != null && siteDB.id != null) {
							if (property.equals("com")) {
								property += comCount++;
							} else if (property.equals("mon")) {
								property += monCount++;
							}
							if (property.equals("gems")) {
								property += gemCount++;
								int path = ((SiteInst3)mod).getValue1();
								if (siteDB.gemspath1 != null && siteDB.gemspath1.intValue() == path) {
									oldVal = siteDB.gemspath1 + ", " + siteDB.gemsamt1;
								} else if (siteDB.gemspath2 != null && siteDB.gemspath2.intValue() == path) {
									oldVal = siteDB.gemspath2 + ", " + siteDB.gemsamt2;
								} else if (siteDB.gemspath3 != null && siteDB.gemspath3.intValue() == path) {
									oldVal = siteDB.gemspath3 + ", " + siteDB.gemsamt3;
								}								
							} else {
								try {
									oldVal = "" + siteDB.getClass().getField(property).get(siteDB);
								} catch (NoSuchFieldException e) {
								}
							}
						}
					}

					String newVal = null;
					if (field.getBoolean(mod)) {
						if (mod instanceof SiteInst1) {
							newVal = ((SiteInst1)mod).getValue();
						} else if (mod instanceof SiteInst2) {
							newVal = ""+((SiteInst2)mod).getValue();
						} else if (mod instanceof SiteInst3) {
							newVal = ""+((SiteInst3)mod).getValue1()+", " + ((SiteInst3)mod).getValue2();
						} else if (mod instanceof SiteInst4) {
							newVal = "true";
						}
					}
					if (oldVal != null || newVal != null) {
						PropertyValues propertyValues = propertyMap.get(property);
						if (propertyValues == null) {
							propertyValues = new PropertyValues();
							propertyMap.put(property, propertyValues);
						}
						propertyValues.oldValue = oldVal;
						propertyValues.newValue = newVal;
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				}
			}

		}

	}
	
	private static void setPropertyValues(SelectNation nation, Map<String, PropertyValues> propertyMap, XtextResource resource) {
		int addreccomCount = 1;
		int addrecunitCount = 1;
		EList<NationMods> list = nation.getMods();
		for (NationMods mod : list) {
			Field[] fields = mod.getClass().getDeclaredFields();
			int startsiteCount = 1;
			for (Field field : fields) {
				try {
					field.setAccessible(true);
					if (field.getBoolean(mod)) {
						String property = field.getName();
						if ("name".equals(property)) {
							continue;
						}
						if ("addrecunit".equals(property)) {
							property += addrecunitCount < 10 ? "0" + addrecunitCount : addrecunitCount;
							addrecunitCount++;
						}
						if ("addreccom".equals(property)) {
							property += addreccomCount < 10 ? "0" + addreccomCount : addreccomCount;
							addreccomCount++;
						}
						String oldVal = null;
						int id = Integer.valueOf(getNationid(nation) != null ? getNationid(nation) : "0");
						//if (id != 0) {
							NationDB nationDB = Database.getNation(id);
							if (nationDB != null && nationDB.id != null) {
								String fieldName = field.getName();
								if (fieldName.equals("startsite")) {
									fieldName += startsiteCount++;
								}
								if (!fieldName.equals("clearsites")) {
									if (fieldName.equals("addrecunit")) {
										if (nationDB.addrecunit.length > addrecunitCount-2) {
											oldVal = nationDB.addrecunit[addrecunitCount-2] + " (" + Database.getMonsterName(nationDB.addrecunit[addrecunitCount-2]) + ")";
										}
									} else if (fieldName.equals("addreccom")) {
										if (nationDB.addreccom.length > addreccomCount-2) {
											oldVal = nationDB.addreccom[addreccomCount-2] + " (" + Database.getMonsterName(nationDB.addreccom[addreccomCount-2]) + ")";
										}
									} else {
										Field field2 = nationDB.getClass().getField(fieldName);
										oldVal = "" + field2.get(nationDB);
										if (mod instanceof NationInst2) {
											if (((NationInst2)mod).isHero1() ||
												((NationInst2)mod).isHero2() ||
												((NationInst2)mod).isHero3() ||
												((NationInst2)mod).isHero4() ||
												((NationInst2)mod).isHero5() ||
												((NationInst2)mod).isHero6()) {
												if (field2.get(nationDB) != null) {
													getMonsterName(resource, mod);
													oldVal = field2.get(nationDB) + " (" + Database.getMonsterName(((Integer)field2.get(nationDB)).intValue()) + ")";
												}
											}
										}
									}
								}
							}
						//}
						
						String newVal = null;
						if (mod instanceof NationInst1) {
							newVal = ((NationInst1)mod).getValue();
						} else if (mod instanceof NationInst2) {
							if (((NationInst2)mod).isHero1() ||
								((NationInst2)mod).isHero2() ||
								((NationInst2)mod).isHero3() ||
								((NationInst2)mod).isHero4() ||
								((NationInst2)mod).isHero5() ||
								((NationInst2)mod).isHero6() ||
								((NationInst2)mod).isMultihero1() ||
								((NationInst2)mod).isMultihero2()) {
								newVal = ((NationInst2)mod).getValue() + " (" + getMonsterName(resource, mod) + ")";
							} else {
								newVal = ""+((NationInst2)mod).getValue();
							}
						} else if (mod instanceof NationInst3) {
							newVal = "true";
						} else if (mod instanceof NationInst4) {
							if (((NationInst4)mod).getValue1() != null) {
								newVal = ((NationInst4)mod).getValue1();
							} else {
								newVal = ((NationInst4)mod).getValue2() + " (" + getMonsterName(resource, mod) + ")";
							}
						} else if (mod instanceof NationInst5) {
							newVal = ""+((NationInst5)mod).getValue1()+", " + ((NationInst5)mod).getValue2()+", " + ((NationInst5)mod).getValue3();
						}
						//if (!compareStrings(oldVal, newVal)) {
							PropertyValues propertyValues = propertyMap.get(property);
							if (propertyValues == null) {
								propertyValues = new PropertyValues();
								propertyMap.put(property, propertyValues);
							}
							propertyValues.oldValue = oldVal;
							propertyValues.newValue = newVal;
						//}
					}
				} catch (IllegalArgumentException e) {
					//e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				} catch (NoSuchFieldException e) {
					e.printStackTrace();
				}
			}
			
		}

	}
	
	private static String getMonsterName(XtextResource resource, NationMods mod) {
		Dom4Mod dom4Mod = (Dom4Mod)resource.getContents().get(0);
		EList<AbstractElement> elements = dom4Mod.getElements();
		int id = 0;
		if (mod instanceof NationInst2) {
			id = ((NationInst2)mod).getValue();
		} else if (mod instanceof NationInst4) {
			id = ((NationInst4)mod).getValue2();
		}
		String name = Database.getMonsterName(id);
		for (AbstractElement element : elements) {
			if (element instanceof SelectMonsterById || element instanceof SelectMonsterByName || element instanceof NewMonster) {
				if (id == Integer.valueOf(getMonsterid((Monster)element))) {
					name = getSelectMonstername((Monster)element);
					break;
				}
			}
		}
		return name;
	}
	
	private static PdfPTable getTable(String[] columns, List<Map.Entry<String, ModObject>> list) {
		return getTable(columns, columns, null, null, list);
	}
	
	private static PdfPTable getTable(String[] columns, String[] columnNames, ValueTranslator[] trans, ValueCombiner[] combine, List<Map.Entry<String, ModObject>> list) {
		PdfPTable table = new PdfPTable(columns.length);
		table.setWidthPercentage(100f);
		table.setHorizontalAlignment(Element.ALIGN_LEFT);
		for (String col : columnNames) {
			PdfPCell c = new PdfPCell(new Phrase(col, SUBTITLE));
			c.setBackgroundColor(BaseColor.LIGHT_GRAY);
			table.addCell(c);
		}
		table.setHeaderRows(1);

		for (Map.Entry<String, ModObject> innerEntry : list) {
			String name = innerEntry.getValue().title;
			Map<String, PropertyValues> map = innerEntry.getValue().propertyMap;

			List<Map.Entry<String, PropertyValues>> list2 = new ArrayList<Map.Entry<String, PropertyValues>>();
			for (Map.Entry<String, PropertyValues> innerEntry2 : map.entrySet()) {
				list2.add(innerEntry2);
			}
			Collections.sort(list2, new Comparator<Map.Entry<String, PropertyValues>>() {
				@Override
				public int compare(Map.Entry<String, PropertyValues> o1, Map.Entry<String, PropertyValues> o2) {
					return o1.getKey().compareTo(o2.getKey());
				}
			});
			
			if (list2.size() == 0) continue;
			
			PdfPCell[] cells = new PdfPCell[columns.length];
			cells[0] = new PdfPCell();
			cells[0].addElement(new Phrase(name, BOLD_TEXT));

			for (int i = 1; i < cells.length; i++) {
				cells[i] = new PdfPCell();
				if (combine != null && combine[i] != null) {
					String[] neededCols = combine[i].getNeededColumns();
					String[] oldValues = new String[neededCols.length];
					String[] newValues = new String[neededCols.length];
					for (int j = 0; j < neededCols.length; j++) {
						for (Map.Entry<String, PropertyValues> entry : list2) {
							if (entry.getKey().equals(neededCols[j])) {
								oldValues[j] = entry.getValue().oldValue;
								newValues[j] = entry.getValue().newValue;
								break;
							}
						}
					}
					// Put old values into null new values
					boolean hasNew = false;
					for (int k = 0; k < newValues.length; k++) {
						if (newValues[k] != null) {
							hasNew = true;
							break;
						}
					}
					if (hasNew) {
						for (int k = 0; k < newValues.length; k++) {
							if (newValues[k] == null) {
								newValues[k] = oldValues[k];
							}
						}
					}
					String newValue = combine[i].translate(newValues);
					String oldValue = combine[i].translate(oldValues);
					if (newValue != null) {
						Phrase phrase = new Phrase();
						phrase.add(new Chunk(newValue, BOLD_TEXT));
						if (oldValue != null) {
							phrase.add(new Chunk(" ("+oldValue+")", TEXT));
						}
						cells[i].addElement(phrase);
					} else if (oldValue != null) {
						cells[i].addElement(new Phrase(oldValue, TEXT));
					}
				} else {
					for (Map.Entry<String, PropertyValues> entry : list2) {
						if (entry.getKey().equals(columns[i])) {
							String oldValue = entry.getValue().oldValue;
							String newValue = entry.getValue().newValue;
							if (trans != null && trans.length > i && trans[i] != null) {
								oldValue = trans[i].translate(oldValue);
								newValue = trans[i].translate(newValue);
							}
							if (newValue != null) {
								Phrase phrase = new Phrase();
								phrase.add(new Chunk(newValue, BOLD_TEXT));
								if (oldValue != null && !oldValue.equals("null")) {
									phrase.add(new Chunk(" ("+oldValue+")", TEXT));
								}
								cells[i].addElement(phrase);
							} else if (oldValue != null && !oldValue.equals("null")) {
								cells[i].addElement(new Phrase(oldValue, TEXT));
							}
							break;
						}
					}
				}
			}
			
			for (PdfPCell cell : cells) {
				table.addCell(cell);
			}
		}
		return table;
	}
	private static String getPathName(int id) {
		switch (id) {
		case -1:
			return "cannot be researched";
		case 0:
			return "F";
		case 1:
			return "A";
		case 2:
			return "W";
		case 3:
			return "E";
		case 4:
			return "S";
		case 5:
			return "D";
		case 6:
			return "N";
		case 7:
			return "B";
		}
		return "Unknown";
	}

}

class ModObject {
	String title;
	Map<String, PropertyValues> propertyMap;
}

class PropertyValues {
	String oldValue;
	String newValue;
}

interface ValueTranslator {
	public String translate(String value);
}

interface ValueCombiner {
	public String[] getNeededColumns();
	public String translate(String[] value);
}
