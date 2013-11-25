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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.PageBook;
import org.larz.dom4.db.IDNameDB;

public class AddDialog extends Dialog {
	public enum TypeToAdd {
		WEAPON(Messages.getString("AddDialog.typelist.weapon")),
		ARMOR(Messages.getString("AddDialog.typelist.armor")),
		MONSTER(Messages.getString("AddDialog.typelist.monster")),
		SPELL(Messages.getString("AddDialog.typelist.spell")),
		ITEM(Messages.getString("AddDialog.typelist.item")),
		NAME(Messages.getString("AddDialog.typelist.name")),
		SITE(Messages.getString("AddDialog.typelist.site")),
		NATION(Messages.getString("AddDialog.typelist.nation")),
		MERCENARY(Messages.getString("AddDialog.typelist.mercenary")),
		POPTYPE(Messages.getString("AddDialog.typelist.poptype"));
		String text;
		TypeToAdd(String text) {
			this.text = text;
		}
	}
	
	public TypeToAdd type;
	public boolean select;
	public String name;
	public String id;
	
	private Composite[] composites = new Composite[TypeToAdd.values().length];
	private Button[] selectButton = new Button[TypeToAdd.values().length];
	private Button[] newButton = new Button[TypeToAdd.values().length];
	private Text[] idText = new Text[TypeToAdd.values().length];
	private Text[] nameText = new Text[TypeToAdd.values().length];
	
	/**
	 * @param parentShell
	 */
	public AddDialog(Shell parentShell) {
		super(parentShell);
        setShellStyle(getShellStyle() | SWT.RESIZE); 
	}

	/**
	 * @param parentShell
	 */
	public AddDialog(IShellProvider parentShell) {
		super(parentShell);
        setShellStyle(getShellStyle() | SWT.RESIZE); 
	}
	
	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(Messages.getString("AddDialog.title"));
	}

	@Override
	protected Point getInitialSize() {
		return new Point(500, 300);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
		
		Label label = new Label(comp, SWT.NONE);
		label.setText(Messages.getString("AddDialog.instructions"));
		
		Composite composite = new Composite(comp, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		final List list = new List(composite, SWT.SINGLE | SWT.BORDER);
		list.setItems(new String[]{TypeToAdd.ARMOR.text, TypeToAdd.WEAPON.text, TypeToAdd.MONSTER.text, TypeToAdd.SPELL.text, TypeToAdd.ITEM.text, TypeToAdd.NAME.text, TypeToAdd.SITE.text, TypeToAdd.NATION.text, TypeToAdd.MERCENARY.text, TypeToAdd.POPTYPE.text});
		list.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false));
		
		final PageBook book = new PageBook(composite, SWT.NONE);
		book.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		
		for (TypeToAdd add : TypeToAdd.values()) {
			addComposite(book, add);
		}
		
		list.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (list.getSelection().length == 1) {
					if (list.getSelection()[0].equals(TypeToAdd.ARMOR.text)) {
						book.showPage(composites[TypeToAdd.ARMOR.ordinal()]);
						type = TypeToAdd.ARMOR;
					} else if (list.getSelection()[0].equals(TypeToAdd.WEAPON.text)) {
						book.showPage(composites[TypeToAdd.WEAPON.ordinal()]);
						type = TypeToAdd.WEAPON;
					} else if (list.getSelection()[0].equals(TypeToAdd.MONSTER.text)) {
						book.showPage(composites[TypeToAdd.MONSTER.ordinal()]);
						type = TypeToAdd.MONSTER;
					} else if (list.getSelection()[0].equals(TypeToAdd.SPELL.text)) {
						book.showPage(composites[TypeToAdd.SPELL.ordinal()]);
						type = TypeToAdd.SPELL;
					} else if (list.getSelection()[0].equals(TypeToAdd.ITEM.text)) {
						book.showPage(composites[TypeToAdd.ITEM.ordinal()]);
						type = TypeToAdd.ITEM;
					} else if (list.getSelection()[0].equals(TypeToAdd.NAME.text)) {
						book.showPage(composites[TypeToAdd.NAME.ordinal()]);
						type = TypeToAdd.NAME;
					} else if (list.getSelection()[0].equals(TypeToAdd.SITE.text)) {
						book.showPage(composites[TypeToAdd.SITE.ordinal()]);
						type = TypeToAdd.SITE;
					} else if (list.getSelection()[0].equals(TypeToAdd.NATION.text)) {
						book.showPage(composites[TypeToAdd.NATION.ordinal()]);
						type = TypeToAdd.NATION;
					} else if (list.getSelection()[0].equals(TypeToAdd.MERCENARY.text)) {
						book.showPage(composites[TypeToAdd.MERCENARY.ordinal()]);
						type = TypeToAdd.MERCENARY;
					} else if (list.getSelection()[0].equals(TypeToAdd.POPTYPE.text)) {
						book.showPage(composites[TypeToAdd.POPTYPE.ordinal()]);
						type = TypeToAdd.POPTYPE;
					}
				}
			}
			
		});
		
		
		return composite;
	}
	
	private void addComposite(Composite book, final TypeToAdd add) {
		composites[add.ordinal()] = new Composite(book, SWT.NONE);
		Composite armorComp = composites[add.ordinal()];
		armorComp.setLayout(new GridLayout(4, false));
		armorComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		selectButton[add.ordinal()] = new Button(armorComp, SWT.RADIO);
		String selectStr = "";
		String newStr = "";
		switch (add) {
		case ARMOR:
			selectStr = Messages.getString("AddDialog.typelist.select.armor");
			newStr = Messages.getString("AddDialog.typelist.new.armor");
			break;
		case WEAPON:
			selectStr = Messages.getString("AddDialog.typelist.select.weapon");
			newStr = Messages.getString("AddDialog.typelist.new.weapon");
			break;
		case MONSTER:
			selectStr = Messages.getString("AddDialog.typelist.select.monster");
			newStr = Messages.getString("AddDialog.typelist.new.monster");
			break;
		case SPELL:
			selectStr = Messages.getString("AddDialog.typelist.select.spell");
			newStr = Messages.getString("AddDialog.typelist.new.spell");
			break;
		case ITEM:
			selectStr = Messages.getString("AddDialog.typelist.select.item");
			newStr = Messages.getString("AddDialog.typelist.new.item");
			break;
		case NAME:
			selectStr = Messages.getString("AddDialog.typelist.select.name");
			newStr = Messages.getString("AddDialog.typelist.new.name");
			break;
		case SITE:
			selectStr = Messages.getString("AddDialog.typelist.select.site");
			newStr = Messages.getString("AddDialog.typelist.new.site");
			break;
		case NATION:
			selectStr = Messages.getString("AddDialog.typelist.select.nation");
			newStr = Messages.getString("AddDialog.typelist.new.nation");
			break;
		case MERCENARY:
			selectStr = Messages.getString("AddDialog.typelist.select.mercenary");
			newStr = Messages.getString("AddDialog.typelist.new.mercenary");
			break;
		case POPTYPE:
			selectStr = Messages.getString("AddDialog.typelist.select.poptype");
			newStr = Messages.getString("AddDialog.typelist.new.poptype");
			break;
		}
		selectButton[add.ordinal()].setText(selectStr);
		selectButton[add.ordinal()].setSelection(true);
		
		Label idLabel = new Label(armorComp, SWT.NONE);
		idLabel.setText(Messages.getString("AddDialog.search.id"));
		idLabel.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));
		idText[add.ordinal()] = new Text(armorComp, SWT.BORDER);
		idText[add.ordinal()].setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
		final Button idSearch = new Button(armorComp, SWT.PUSH);
		idSearch.setText(Messages.getString("AddDialog.search.button"));
		idSearch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				SearchDialog dialog = new SearchDialog(getShell(), selectButton[add.ordinal()].getSelection(), type);
				if (dialog.open() == Window.OK) {
					IDNameDB armorDB = dialog.getSelected();
					if (armorDB != null) {
						idText[add.ordinal()].setText(Integer.toString(armorDB.id));
					}
				}
			}
		});
		
		newButton[add.ordinal()] = new Button(armorComp, SWT.RADIO);
		newButton[add.ordinal()].setText(newStr);

		Label nameLabel = new Label(armorComp, SWT.NONE);
		nameLabel.setText(Messages.getString("AddDialog.search.name"));
		nameText[add.ordinal()] = new Text(armorComp, SWT.BORDER);
		nameText[add.ordinal()].setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
		final Button nameSearch = new Button(armorComp, SWT.PUSH);
		nameSearch.setText(Messages.getString("AddDialog.search.button"));
		nameSearch.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				SearchDialog dialog = new SearchDialog(getShell(), selectButton[add.ordinal()].getSelection(), type);
				if (dialog.open() == Window.OK) {
					IDNameDB armorDB = dialog.getSelected();
					nameText[add.ordinal()].setText(armorDB.name);
				}
			}
		});
		newButton[add.ordinal()].addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (newButton[add.ordinal()].getSelection()) {
					if (add == TypeToAdd.NATION) {
						idText[add.ordinal()].setText("");
						idText[add.ordinal()].setEnabled(false);
						nameText[add.ordinal()].setText("");
						nameText[add.ordinal()].setEnabled(true);
						nameSearch.setEnabled(false);
					} else {
						idText[add.ordinal()].setText("");
						nameText[add.ordinal()].setText("");
						nameText[add.ordinal()].setEnabled(true);
						nameSearch.setEnabled(false);
					}
				}
			}
		});
		selectButton[add.ordinal()].addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (selectButton[add.ordinal()].getSelection()) {
					if (add == TypeToAdd.NATION) {
						idText[add.ordinal()].setEnabled(true);
						idText[add.ordinal()].setText("");
						nameText[add.ordinal()].setText("");
						nameText[add.ordinal()].setEnabled(false);
						nameSearch.setEnabled(true);
					} else {
						idText[add.ordinal()].setText("");
						nameText[add.ordinal()].setText("");
						nameText[add.ordinal()].setEnabled(true);
						nameSearch.setEnabled(true);
					}
				}
			}
		});

		idText[add.ordinal()].addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				if (idText[add.ordinal()].getText().length() > 0 && selectButton[add.ordinal()].getSelection()) {
					if (add != TypeToAdd.NATION) {
						nameText[add.ordinal()].setEnabled(false);
						nameSearch.setEnabled(false);
					}
				} else {
					if (selectButton[type.ordinal()].getSelection()) {
						nameText[add.ordinal()].setEnabled(true);
						nameSearch.setEnabled(true);
					}
				}
				setOKEnablement(add);
			}
		});
		nameText[add.ordinal()].addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				if (nameText[add.ordinal()].getText().length() > 0 && selectButton[add.ordinal()].getSelection()) {
					if (add != TypeToAdd.NATION) {
						idText[add.ordinal()].setEnabled(false);
						idSearch.setEnabled(false);
					}
				} else {
					if (add != TypeToAdd.NATION &&
						add != TypeToAdd.MERCENARY) {
						idText[add.ordinal()].setEnabled(true);
						idSearch.setEnabled(true);
					}
				}
				setOKEnablement(add);
			}
		});
		if (add == TypeToAdd.NAME) {
			newButton[add.ordinal()].setEnabled(false);
			nameText[add.ordinal()].setEnabled(false);
		}
		if (add == TypeToAdd.NATION) {
			nameText[add.ordinal()].setEnabled(false);
		}
		if (add == TypeToAdd.MERCENARY) {
			newButton[add.ordinal()].setSelection(true);
			selectButton[add.ordinal()].setSelection(false);
			selectButton[add.ordinal()].setEnabled(false);
			idText[add.ordinal()].setEnabled(false);
			idSearch.setEnabled(false);
		}
		if (add == TypeToAdd.POPTYPE) {
			newButton[add.ordinal()].setEnabled(false);
			nameText[add.ordinal()].setEnabled(false);
			nameSearch.setEnabled(false);
		}

	}
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		getButton(IDialogConstants.OK_ID).setEnabled(false);
	}

	private void setOKEnablement(TypeToAdd add) {
		if (selectButton[type.ordinal()].getSelection() && (nameText[type.ordinal()].getText().length() > 0 || idText[type.ordinal()].getText().length() > 0)) {
			getButton(IDialogConstants.OK_ID).setEnabled(true);
		} else if (newButton[type.ordinal()].getSelection() && nameText[type.ordinal()].getText().length() > 0 && idText[type.ordinal()].getText().length() > 0) {
			getButton(IDialogConstants.OK_ID).setEnabled(true);
		} else if (newButton[type.ordinal()].getSelection() && nameText[type.ordinal()].getText().length() > 0 && (add == TypeToAdd.NATION || add == TypeToAdd.MERCENARY || add == TypeToAdd.MONSTER)) {
			getButton(IDialogConstants.OK_ID).setEnabled(true);
		} else {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
		}

	}

	@Override
	protected void okPressed() {
		select = selectButton[type.ordinal()].getSelection();
		name = nameText[type.ordinal()].getText();
		id = idText[type.ordinal()].getText();
		super.okPressed();
	}

}
