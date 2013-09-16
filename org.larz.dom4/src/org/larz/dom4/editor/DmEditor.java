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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.util.EContentAdapter;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextSyntaxDiagnostic;
import org.eclipse.xtext.ui.MarkerTypes;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.XtextDocumentUtil;
import org.eclipse.xtext.ui.editor.validation.MarkerCreator;
import org.eclipse.xtext.ui.editor.validation.MarkerIssueProcessor;
import org.eclipse.xtext.ui.editor.validation.ValidationJob;
import org.eclipse.xtext.ui.validation.MarkerTypeProvider;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.larz.dom4.dm.dm.AbstractElement;
import org.larz.dom4.dm.dm.Dom4Mod;
import org.larz.dom4.dm.dm.Item;
import org.larz.dom4.dm.dm.ItemInst3;
import org.larz.dom4.dm.dm.ItemMods;
import org.larz.dom4.dm.dm.Monster;
import org.larz.dom4.dm.dm.MonsterInst5;
import org.larz.dom4.dm.dm.MonsterMods;
import org.larz.dom4.dm.dm.NationInst2;
import org.larz.dom4.dm.dm.NationInst4;
import org.larz.dom4.dm.dm.NationMods;
import org.larz.dom4.dm.dm.NewArmor;
import org.larz.dom4.dm.dm.NewMonster;
import org.larz.dom4.dm.dm.NewSite;
import org.larz.dom4.dm.dm.NewWeapon;
import org.larz.dom4.dm.dm.SelectArmorById;
import org.larz.dom4.dm.dm.SelectArmorByName;
import org.larz.dom4.dm.dm.SelectItemById;
import org.larz.dom4.dm.dm.SelectItemByName;
import org.larz.dom4.dm.dm.SelectMonsterById;
import org.larz.dom4.dm.dm.SelectMonsterByName;
import org.larz.dom4.dm.dm.SelectName;
import org.larz.dom4.dm.dm.SelectNation;
import org.larz.dom4.dm.dm.SelectSiteById;
import org.larz.dom4.dm.dm.SelectSiteByName;
import org.larz.dom4.dm.dm.SelectSpellById;
import org.larz.dom4.dm.dm.SelectSpellByName;
import org.larz.dom4.dm.dm.SelectWeaponById;
import org.larz.dom4.dm.dm.SelectWeaponByName;
import org.larz.dom4.dm.dm.Site;
import org.larz.dom4.dm.dm.SiteInst5;
import org.larz.dom4.dm.dm.SiteMods;
import org.larz.dom4.dm.dm.Spell;
import org.larz.dom4.dm.dm.SpellInst2;
import org.larz.dom4.dm.dm.SpellMods;
import org.larz.dom4.dm.ui.editor.DmXtextEditor;
import org.larz.dom4.dm.ui.internal.DmActivator;
import org.larz.dom4.dm.validation.DmJavaValidator;

public class DmEditor extends FormEditor implements IMenuListener, IGotoMarker {
	
	protected IEditorPart sourcePage;
	protected MasterFormPage masterDetailsPage;
	private boolean	docChanged = false;


	/**
	 * This is the method used by the framework to install your own controls.
	 */
	@Override
	public void addPages() {
		IExtensionPoint ep = RegistryFactory.getRegistry().getExtensionPoint("org.eclipse.ui.editors");
		final IExtension[ ] extensions = ep.getExtensions();
		BusyIndicator.showWhile(Display.getDefault(), new Runnable() {
			
			@Override
			public void run() {
				IExtension ex;
				IConfigurationElement confElem = null;
				for (int i = 0; i < extensions.length; i++) {
					ex = extensions[i];
					if (ex.getContributor().getName().equals("org.larz.dom4.dm.ui")) {
						for (IConfigurationElement c : ex.getConfigurationElements()) {
							if (c.getName().equals("editor")) {
								confElem = c;
								break;
							}
						}
					}
				}
				try {
					// create the xtext editor
					sourcePage = (IEditorPart) confElem.createExecutableExtension("class");
					masterDetailsPage = new MasterFormPage(DmEditor.this, (XtextEditor)sourcePage);
					addPage(masterDetailsPage);

					int index = addPage(sourcePage, getEditorInput());
					setPageText(index, Messages.getString("MasterDetailsPage.source.label"));

					getSite().setSelectionProvider(((XtextEditor)sourcePage).getSelectionProvider());
					
					DmEditor.this.addPageChangedListener(new IPageChangedListener() {
						@Override
						public void pageChanged(PageChangedEvent event) {
							if (docChanged) {
								refresh();
								docChanged = false;
							}
						}
					});
					
					final IXtextDocument document = ((XtextEditor)sourcePage).getDocument();
					document.readOnly(new IUnitOfWork.Void<XtextResource>(){       
						public void process(XtextResource resource) { 
							resource.eAdapters().add(new SyntaxAdapter());
							Dom4Mod dom4Mod = (Dom4Mod)resource.getContents().get(0);
							EList<Adapter> eAdapters = dom4Mod.eAdapters();
							eAdapters.add(new ValidationAdapter());
						} 
					});
					document.addDocumentListener(new IDocumentListener() {
						@Override
						public void documentChanged(DocumentEvent event) {
							docChanged = true;
						}
						@Override
						public void documentAboutToBeChanged(DocumentEvent event) {
						}
					});
				} catch (CoreException e1) {
					e1.printStackTrace();
					return;
				}
			}
		});

	}
	
	private class SyntaxAdapter extends EContentAdapter {
		@SuppressWarnings("rawtypes")
		@Override
		public void notifyChanged(Notification notification) {
			if (notification.getNewValue() instanceof ArrayList) {
				ArrayList list = (ArrayList)notification.getNewValue();
				if (list != null && list.size() > 0) {
					for (Object object : list) {
						if (object instanceof XtextSyntaxDiagnostic) {
							try {
								((XtextEditor)sourcePage).getResource().deleteMarkers(MarkerTypes.EXPENSIVE_VALIDATION, true, IResource.DEPTH_INFINITE);
								break;
							} catch (CoreException e) {
								e.printStackTrace();
							}
						}
					}
				}
			}
			if (notification.getNewValue() instanceof XtextSyntaxDiagnostic) {
				try {
					((XtextEditor)sourcePage).getResource().deleteMarkers(MarkerTypes.EXPENSIVE_VALIDATION, true, IResource.DEPTH_INFINITE);
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
			final IXtextDocument document = ((XtextEditor)sourcePage).getDocument();
			if (document != null) {
				document.readOnly(new IUnitOfWork.Void<XtextResource>(){       
					public void process(XtextResource resource) { 
						if (resource.getContents() != null && resource.getContents().size() > 0) {
							Dom4Mod dom4Mod = (Dom4Mod)resource.getContents().get(0);
							EList<Adapter> eAdapters = dom4Mod.eAdapters();
							for (Adapter adapter : eAdapters) {
								if (adapter instanceof ValidationAdapter) {
									return;
								}
							}
							eAdapters.add(new ValidationAdapter());
						}
					} 
				});
			}
		}
	}
	
	private class ValidationAdapter extends EContentAdapter {
		@Override
		public void notifyChanged(Notification notification) {
			if (notification.getNewValue() instanceof NewArmor ||
				notification.getNewValue() instanceof SelectArmorById || 
				notification.getNewValue() instanceof SelectArmorByName ||
				notification.getNewValue() instanceof NewWeapon ||
				notification.getNewValue() instanceof SelectWeaponById ||
				notification.getNewValue() instanceof SelectWeaponByName ||
				notification.getNewValue() instanceof NewMonster ||
				notification.getNewValue() instanceof SelectMonsterById ||
				notification.getNewValue() instanceof SelectMonsterByName ||
				notification.getNewValue() instanceof SelectName ||
				notification.getNewValue() instanceof NewSite ||
				notification.getNewValue() instanceof SelectNation) {
				
				runValidation();
			}
		}
	}
	
	private void runValidation() {
		IResourceValidator resourceValidator = DmActivator.getInstance().getInjector("org.larz.dom4.dm.Dm").getInstance(IResourceValidator.class);
		MarkerCreator markerCreator = DmActivator.getInstance().getInjector("org.larz.dom4.dm.Dm").getInstance(MarkerCreator.class);
		XtextEditor xtextEditor = ((XtextEditor)sourcePage);
		if (xtextEditor != null) {
			MarkerIssueProcessor markerIssueProcessor = new MarkerIssueProcessor(xtextEditor.getResource(),	markerCreator, new MarkerTypeProvider());
			IXtextDocument xtextDocument = XtextDocumentUtil.get(xtextEditor);
			ValidationJob validationJob = new ValidationJob(resourceValidator, xtextDocument, markerIssueProcessor, CheckMode.EXPENSIVE_ONLY);
			validationJob.schedule();
		}
	}
	
	private void setToCurrentSelection() {
		masterDetailsPage.block.viewer.setSelection(new StructuredSelection(((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()));
	}
	
	private void refresh() {
		if (masterDetailsPage.block != null && masterDetailsPage.block.viewer != null) {
			masterDetailsPage.block.viewer.refresh();
			masterDetailsPage.update();
			if (((SummaryList)masterDetailsPage.block).getDetailsPart() != null) {
				if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() != null) {
					if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof ArmorDetailsPage) {
						((ArmorDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((ArmorDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectArmorById && two instanceof SelectArmorById) {
								if (((SelectArmorById)one).getValue() == ((SelectArmorById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectArmorByName && two instanceof SelectArmorByName) {
								if (((SelectArmorByName)one).getValue().equals(((SelectArmorByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else if (one instanceof NewArmor && two instanceof NewArmor) {
								if (((NewArmor)one).getValue() == ((NewArmor)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof WeaponDetailsPage) {
						((WeaponDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((WeaponDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectWeaponById && two instanceof SelectWeaponById) {
								if (((SelectWeaponById)one).getValue() == ((SelectWeaponById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectWeaponByName && two instanceof SelectWeaponByName) {
								if (((SelectWeaponByName)one).getValue().equals(((SelectWeaponByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else if (one instanceof NewWeapon && two instanceof NewWeapon) {
								if (((NewWeapon)one).getValue() == ((NewWeapon)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof MonsterDetailsPage) {
						((MonsterDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((MonsterDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectMonsterById && two instanceof SelectMonsterById) {
								if (((SelectMonsterById)one).getValue() == ((SelectMonsterById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectMonsterByName && two instanceof SelectMonsterByName) {
								if (((SelectMonsterByName)one).getValue().equals(((SelectMonsterByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else if (one instanceof NewMonster && two instanceof NewMonster) {
								if (((NewMonster)one).getValue() == ((NewMonster)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof NationDetailsPage) {
						((NationDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((NationDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectNation && two instanceof SelectNation) {
								if (((SelectNation)one).getValue() == ((SelectNation)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof SpellDetailsPage) {
						((SpellDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((SpellDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectSpellById && two instanceof SelectSpellById) {
								if (((SelectSpellById)one).getValue() == ((SelectSpellById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectSpellByName && two instanceof SelectSpellByName) {
								if (((SelectSpellByName)one).getValue().equals(((SelectSpellByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof ItemDetailsPage) {
						((ItemDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((ItemDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectItemById && two instanceof SelectItemById) {
								if (((SelectItemById)one).getValue() == ((SelectItemById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectItemByName && two instanceof SelectItemByName) {
								if (((SelectItemByName)one).getValue().equals(((SelectItemByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof SiteDetailsPage) {
						((SiteDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((SiteDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectSiteById && two instanceof SelectSiteById) {
								if (((SelectSiteById)one).getValue() == ((SelectSiteById)two).getValue()) {
									setToCurrentSelection();
								}
							} else if (one instanceof SelectSiteByName && two instanceof SelectSiteByName) {
								if (((SelectSiteByName)one).getValue().equals(((SelectSiteByName)two).getValue())) {
									setToCurrentSelection();
								}
							} else if (one instanceof NewSite && two instanceof NewSite) {
								if (((NewSite)one).getValue() == ((NewSite)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					} else if (((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage() instanceof NameDetailsPage) {
						((NameDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).update();

						Object one = ((NameDetailsPage)((SummaryList)masterDetailsPage.block).getDetailsPart().getCurrentPage()).getInput();
						Object two = ((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement() != null ? ((AbstractElementWrapper)((IStructuredSelection)masterDetailsPage.block.viewer.getSelection()).getFirstElement()).getElement() : null;
						if (one != two) {
							if (one instanceof SelectName && two instanceof SelectName) {
								if (((SelectName)one).getValue() == ((SelectName)two).getValue()) {
									setToCurrentSelection();
								}
							} else {
								masterDetailsPage.block.viewer.setSelection(null);
							}
						}
					}
				}
			}
		}

	}
	
	@Override
	public void setActivePage(int index) {
		super.setActivePage(index);
	}
	
	public IEditorPart getSourcePage() {
		return sourcePage;
	}
	
	/**
	 * This is how the framework determines which interfaces we implement.
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public Object getAdapter(Class key) {
		return ((DmXtextEditor)sourcePage).getAdapter(key);
	}

	/**
	 * This is for implementing {@link IEditorPart} and simply tests the command stack.
	 */
	@Override
	public boolean isDirty() {
		return sourcePage.isDirty();
	}

	/**
	 * This is for implementing {@link IEditorPart} and simply saves the model file.
	 */
	@Override
	public void doSave(IProgressMonitor progressMonitor) {
		sourcePage.doSave(progressMonitor);
	}

	/**
	 * This always returns true because it is not currently supported.
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	/**
	 * This also changes the editor's input.
	 */
	@Override
	public void doSaveAs() {
		sourcePage.doSaveAs();
	}

	public void gotoMarker(IMarker marker) {
		IGotoMarker gotoMarker = (IGotoMarker)sourcePage.getAdapter(IGotoMarker.class);
		gotoMarker.gotoMarker(marker);
	}

	/**
	 * This is called during startup.
	 */
	@Override
	public void init(IEditorSite site, IEditorInput editorInput) {
		setSite(site);
		setInputWithNotify(editorInput);
		setPartName(editorInput.getName());
	}

	/**
	 * This implements {@link org.eclipse.jface.action.IMenuListener} to help fill the context menus with contributions from the Edit menu.
	 */
	public void menuAboutToShow(IMenuManager menuManager) {
		((IMenuListener)getEditorSite().getActionBarContributor()).menuAboutToShow(menuManager);
	}
	
	public void fixIdNumbers() {
		final IXtextDocument myDocument = ((XtextEditor)sourcePage).getDocument();
		myDocument.modify(new IUnitOfWork.Void<XtextResource>() {
			@Override
			public void process(XtextResource resource) throws Exception {
				// Fix Armor IDs
				Set<Integer> armorIds = new HashSet<Integer>();
				List<NewArmor> armorsToFix = new ArrayList<NewArmor>();
				Dom4Mod dom4Mod = (Dom4Mod)resource.getContents().get(0);
				EList<AbstractElement> elements = dom4Mod.getElements();
				for (AbstractElement element : elements) {
					if (element instanceof NewArmor) {
						if (armorIds.contains(((NewArmor)element).getValue()) ||
							((NewArmor)element).getValue() < DmJavaValidator.MIN_ARMOR_ID ||
							((NewArmor)element).getValue() > DmJavaValidator.MAX_ARMOR_ID) {
							armorsToFix.add((NewArmor)element);
						} else {
							armorIds.add(((NewArmor)element).getValue());
						}
					}
				}
				for (NewArmor armor : armorsToFix) {
					for (int i = DmJavaValidator.MIN_ARMOR_ID; i <= DmJavaValidator.MAX_ARMOR_ID; i++) {
						if (!armorIds.contains(i)) {
							armor.setValue(i);
							break;
						}
					}
				}

				// Fix Weapon IDs
				Set<Integer> weaponIds = new HashSet<Integer>();
				List<NewWeapon> weaponsToFix = new ArrayList<NewWeapon>();
				for (AbstractElement element : elements) {
					if (element instanceof NewWeapon) {
						if (weaponIds.contains(((NewWeapon)element).getValue()) ||
							((NewWeapon)element).getValue() < DmJavaValidator.MIN_WEAPON_ID ||
							((NewWeapon)element).getValue() > DmJavaValidator.MAX_WEAPON_ID) {
							weaponsToFix.add((NewWeapon)element);
						} else {
							weaponIds.add(((NewWeapon)element).getValue());
						}
					}
				}
				for (NewWeapon weapon : weaponsToFix) {
					int oldValue = weapon.getValue();
					for (int i = DmJavaValidator.MIN_WEAPON_ID; i <= DmJavaValidator.MAX_WEAPON_ID; i++) {
						if (!weaponIds.contains(i)) {
							// Change the weapon id
							weapon.setValue(i);
							
							// Need to change references to this weapon as well
							boolean foundWeapon = false;
							for (AbstractElement element : elements) {
								if (element instanceof NewWeapon) {
									if (((NewWeapon)element).getValue() == i) {
										foundWeapon = true;
									}
								}
								if (foundWeapon) {
									if (element instanceof Monster) {
										EList<MonsterMods> mods = ((Monster)element).getMods();
										for (MonsterMods mod : mods) {
											if (mod instanceof MonsterInst5 && ((MonsterInst5)mod).isWeapon()) {
												if (((MonsterInst5)mod).getValue2() == oldValue) {
													((MonsterInst5)mod).setValue2(i);
												}
											}
										}
									} else if (element instanceof Item) {
										EList<ItemMods> mods = ((Item)element).getMods();
										for (ItemMods mod : mods) {
											if (mod instanceof ItemInst3 && ((ItemInst3)mod).isWeapon()) {
												if (((ItemInst3)mod).getValue2() == oldValue) {
													((ItemInst3)mod).setValue2(i);
												}
											}
										}
									}
								}
							}
							break;
						}
					}
				}

				// Fix Monster IDs
				Set<Integer> monsterIds = new HashSet<Integer>();
				List<NewMonster> monstersToFix = new ArrayList<NewMonster>();
				for (AbstractElement element : elements) {
					if (element instanceof NewMonster) {
						if (monsterIds.contains(((NewMonster)element).getValue()) ||
							((NewMonster)element).getValue() < DmJavaValidator.MIN_MONSTER_ID ||
							((NewMonster)element).getValue() > DmJavaValidator.MAX_MONSTER_ID) {
							monstersToFix.add((NewMonster)element);
						} else {
							monsterIds.add(((NewMonster)element).getValue());
						}
					}
				}
				for (NewMonster monster : monstersToFix) {
					int oldValue = monster.getValue();
					for (int i = DmJavaValidator.MIN_MONSTER_ID; i <= DmJavaValidator.MAX_MONSTER_ID; i++) {
						if (!monsterIds.contains(i)) {
							monster.setValue(i);

							// Need to change references to this monster as well
							boolean foundMonster = false;
							for (AbstractElement element : elements) {
								if (element instanceof NewMonster) {
									if (((NewMonster)element).getValue() == i) {
										foundMonster = true;
									}
								}
								if (foundMonster) {
									if (element instanceof Monster) {
										EList<MonsterMods> mods = ((Monster)element).getMods();
										for (MonsterMods mod : mods) {
											if (mod instanceof MonsterInst5 && 
											   (((MonsterInst5)mod).isFirstshape() ||
												((MonsterInst5)mod).isSecondshape() ||
												((MonsterInst5)mod).isSecondtmpshape() ||
												((MonsterInst5)mod).isShapechange() ||
												((MonsterInst5)mod).isLandshape() ||
												((MonsterInst5)mod).isForestshape() ||
												((MonsterInst5)mod).isWatershape() ||
												((MonsterInst5)mod).isPlainshape() ||
												((MonsterInst5)mod).isDomsummon() ||
												((MonsterInst5)mod).isDomsummon2() ||
												((MonsterInst5)mod).isDomsummon20() ||
												((MonsterInst5)mod).isMakemonster1() ||
												((MonsterInst5)mod).isMakemonster2() ||
												((MonsterInst5)mod).isMakemonster3() ||
												((MonsterInst5)mod).isMakemonster4() ||
												((MonsterInst5)mod).isMakemonster5() ||
												((MonsterInst5)mod).isSummon1() ||
												((MonsterInst5)mod).isSummon5())) {
												if (((MonsterInst5)mod).getValue2() == oldValue) {
													((MonsterInst5)mod).setValue2(i);
												}
											}
										}
									} else if (element instanceof Site) {
										EList<SiteMods> mods = ((Site)element).getMods();
										for (SiteMods mod : mods) {
											if (mod instanceof SiteInst5 && 
											   (((SiteInst5)mod).isHomecom() ||
												((SiteInst5)mod).isHomemon() ||
												((SiteInst5)mod).isCom() ||
												((SiteInst5)mod).isMon() )) {
												if (((SiteInst5)mod).getValue2() == oldValue) {
													((SiteInst5)mod).setValue2(i);
												}
											}
										}
									} else if (element instanceof SelectNation) {
										EList<NationMods> mods = ((SelectNation)element).getMods();
										for (NationMods mod : mods) {
											if (mod instanceof NationInst4 && 
											   (((NationInst4)mod).isStartcom() ||
												((NationInst4)mod).isStartscout() ||
												((NationInst4)mod).isStartunittype1() ||
												((NationInst4)mod).isStartunittype2() ||
												((NationInst4)mod).isAddrecunit() ||
												((NationInst4)mod).isAddreccom() ||
												((NationInst4)mod).isUwunit1() ||
												((NationInst4)mod).isUwunit2() ||
												((NationInst4)mod).isUwunit3() ||
												((NationInst4)mod).isUwunit4() ||
												((NationInst4)mod).isUwunit5() ||
												((NationInst4)mod).isUwcom1() ||
												((NationInst4)mod).isUwcom2() ||
												((NationInst4)mod).isUwcom3() ||
												((NationInst4)mod).isUwcom4() ||
												((NationInst4)mod).isUwcom5() ||
												((NationInst4)mod).isDefcom1() ||
												((NationInst4)mod).isDefcom2() ||
												((NationInst4)mod).isDefunit1() ||
												((NationInst4)mod).isDefunit1b() ||
												((NationInst4)mod).isDefunit2() ||
												((NationInst4)mod).isDefunit2b() ||
												((NationInst4)mod).isAddrecunit() ||
												((NationInst4)mod).isAddrecunit() )) {
												if (((NationInst4)mod).getValue2() == oldValue) {
													((NationInst4)mod).setValue2(i);
												}
											}
											if (mod instanceof NationInst2 && 
											   (((NationInst2)mod).isHero1() ||
											    ((NationInst2)mod).isHero2() ||
											    ((NationInst2)mod).isHero3() ||
											    ((NationInst2)mod).isHero4() ||
											    ((NationInst2)mod).isHero5() ||
											    ((NationInst2)mod).isHero6() ||
											    ((NationInst2)mod).isMultihero1() ||
												((NationInst2)mod).isMultihero2() )) {
												if (((NationInst2)mod).getValue() == oldValue) {
													((NationInst2)mod).setValue(i);
												}
											}
										}
									} else if (element instanceof Spell) {
										EList<SpellMods> mods = ((Spell)element).getMods();
										for (SpellMods mod : mods) {
											if (mod instanceof SpellInst2 && 
											   (((SpellInst2)mod).isDamage() )) {
												if (((SpellInst2)mod).getValue() == oldValue) {
													((SpellInst2)mod).setValue(i);
												}
											}
										}
									}
								}
							}
							break;
						}
					}
				}

				// Fix Site IDs
				Set<Integer> siteIds = new HashSet<Integer>();
				List<NewSite> sitesToFix = new ArrayList<NewSite>();
				for (AbstractElement element : elements) {
					if (element instanceof NewSite) {
						if (siteIds.contains(((NewSite)element).getValue()) ||
							((NewSite)element).getValue() < DmJavaValidator.MIN_SITE_ID ||
							((NewSite)element).getValue() > DmJavaValidator.MAX_SITE_ID) {
							sitesToFix.add((NewSite)element);
						} else {
							siteIds.add(((NewSite)element).getValue());
						}
					}
				}
				for (NewSite site : sitesToFix) {
					for (int i = DmJavaValidator.MIN_SITE_ID; i <= DmJavaValidator.MAX_SITE_ID; i++) {
						if (!siteIds.contains(i)) {
							site.setValue(i);
							break;
						}
					}
				}
			}
			
		});
		
		runValidation();
		
		refresh();
	}
	
	public void generateReport() {
		ReportGenerator.generateReport((XtextEditor)sourcePage, sourcePage.getSite().getShell());
	}

}
