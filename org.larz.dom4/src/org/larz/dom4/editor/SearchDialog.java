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
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.larz.dom4.db.Database;
import org.larz.dom4.db.IDNameDB;
import org.larz.dom4.dm.validation.DmJavaValidator;
import org.larz.dom4.editor.AddDialog.TypeToAdd;

/**
 *
 */
public class SearchDialog extends Dialog {
	private TableViewer viewer;
	private IDNameDB selected;
	private boolean select;
	private TypeToAdd type;
	
	/**
	 * @param parentShell
	 */
	public SearchDialog(Shell parentShell, boolean select, TypeToAdd type) {
		super(parentShell);
		this.select = select;
		this.type = type;
	}

	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText("Search");
	}

	@Override
	protected Point getInitialSize() {
		return new Point(300, 300);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite)super.createDialogArea(parent);
		viewer = new TableViewer(comp, SWT.BORDER | SWT.FULL_SELECTION);
		viewer.getTable().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		viewer.setContentProvider(new ArrayContentProvider());
		viewer.getTable().setLinesVisible(true);
		viewer.getTable().setHeaderVisible(true); 
		
		TableViewerColumn column = new TableViewerColumn(viewer,SWT.NONE);
		column.getColumn().setWidth(40);
		column.getColumn().setText(Messages.getString("SearchDialog.id"));
		column.setLabelProvider(new ColumnLabelProvider(){
			@Override
			public String getText(Object element) {
				return Integer.toString(((IDNameDB)element).id);
			}
		});
		ColumnViewerSorter cSorter = new ColumnViewerSorter(viewer,column) {
			protected int doCompare(Viewer viewer, Object e1, Object e2) {
				IDNameDB p1 = (IDNameDB) e1;
				IDNameDB p2 = (IDNameDB) e2;
				return Integer.valueOf(p1.id).compareTo(Integer.valueOf(p2.id));
			}
		};
		 	
		if (select) {
			column = new TableViewerColumn(viewer,SWT.NONE);
			column.getColumn().setWidth(180);
			column.getColumn().setText(Messages.getString("SearchDialog.name")); 
			column.setLabelProvider(new ColumnLabelProvider(){
				@Override
				public String getText(Object element) {
					return ((IDNameDB)element).name;
				}
			});
			new ColumnViewerSorter(viewer,column) {
				protected int doCompare(Viewer viewer, Object e1, Object e2) {
					IDNameDB p1 = (IDNameDB) e1;
					IDNameDB p2 = (IDNameDB) e2;
					return p1.name.compareTo(p2.name);
				}
			};
		}

		if (select) {
			switch (type) {
			case ARMOR:
				viewer.setInput(Database.getAllArmor().toArray());
				break;
			case WEAPON:
				viewer.setInput(Database.getAllWeapon().toArray());
				break;
			case MONSTER:
				viewer.setInput(Database.getAllMonster().toArray());
				break;
			case ITEM:
				viewer.setInput(Database.getAllItem().toArray());
				break;
			case SITE:
				viewer.setInput(Database.getAllSite().toArray());
				break;
			case NATION:
				viewer.setInput(Database.getAllNation().toArray());
				break;
			case SPELL:
				viewer.setInput(Database.getAllSpell().toArray());
				break;
			case NAME:
				List<IDNameDB> ids = new ArrayList<IDNameDB>();
				for (int i = 100; i <= 200; i++) {
					IDNameDB armorDB = new IDNameDB();
					armorDB.id = i;
					ids.add(armorDB);
				}
				viewer.setInput(ids.toArray());
				break;
			}
		} else {
			int startId = 0;
			int endId = -1;
			switch (type) {
			case ARMOR:
				startId = DmJavaValidator.MIN_ARMOR_ID;
				endId = DmJavaValidator.MAX_ARMOR_ID;
				break;
			case WEAPON:
				startId = DmJavaValidator.MIN_WEAPON_ID;
				endId = DmJavaValidator.MAX_WEAPON_ID;
				break;
			case MONSTER:
				startId = DmJavaValidator.MIN_MONSTER_ID;
				endId = DmJavaValidator.MAX_MONSTER_ID;
				break;
			case SITE:
				startId = DmJavaValidator.MIN_SITE_ID;
				endId = DmJavaValidator.MAX_SITE_ID;
				break;
			case NATION:
				startId = DmJavaValidator.MIN_NATION_ID;
				endId = DmJavaValidator.MAX_NATION_ID;
				break;
			}
			List<IDNameDB> ids = new ArrayList<IDNameDB>();
			for (int i = startId; i <= endId; i++) {
				IDNameDB armorDB = new IDNameDB();
				armorDB.id = i;
				ids.add(armorDB);
			}
			viewer.setInput(ids.toArray());
		}
		cSorter.setSorter(cSorter, ColumnViewerSorter.ASC);
		
		return comp;
	}
	
	@Override
	protected void okPressed() {
		IStructuredSelection selection = (IStructuredSelection)viewer.getSelection();
		if (!selection.isEmpty()) {
			selected = (IDNameDB)selection.getFirstElement();
		}
		super.okPressed();
	}
	
	public IDNameDB getSelected() {
		return selected;
	}

	private static abstract class ColumnViewerSorter extends ViewerComparator {
		public static final int ASC = 1;
		public static final int NONE = 0;
		public static final int DESC = -1;
		private int direction = 0;
		private TableViewerColumn column;
		private ColumnViewer viewer;

		public ColumnViewerSorter(ColumnViewer viewer, TableViewerColumn column) {
			this.column = column;
			this.viewer = viewer;
			this.column.getColumn().addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					if( ColumnViewerSorter.this.viewer.getComparator() != null ) {
						if( ColumnViewerSorter.this.viewer.getComparator() == ColumnViewerSorter.this ) {
							int tdirection = ColumnViewerSorter.this.direction;

							if( tdirection == ASC ) {
								setSorter(ColumnViewerSorter.this, DESC);
							} else if( tdirection == DESC ) {
								setSorter(ColumnViewerSorter.this, NONE);
							}
						} else {
							setSorter(ColumnViewerSorter.this, ASC);
						}
					} else {
						setSorter(ColumnViewerSorter.this, ASC);
					}
				}
			});
		}


		public void setSorter(ColumnViewerSorter sorter, int direction) {
			if( direction == NONE ) {
				column.getColumn().getParent().setSortColumn(null);
				column.getColumn().getParent().setSortDirection(SWT.NONE);
				viewer.setComparator(null);
			} else {
				column.getColumn().getParent().setSortColumn(column.getColumn());
				sorter.direction = direction;

				if( direction == ASC ) {
					column.getColumn().getParent().setSortDirection(SWT.DOWN);
				} else {
					column.getColumn().getParent().setSortDirection(SWT.UP);
				}

				if( viewer.getComparator() == sorter ) {
					viewer.refresh();
				} else {
					viewer.setComparator(sorter);
				}

			}
		}

		public int compare(Viewer viewer, Object e1, Object e2) {
			return direction * doCompare(viewer, e1, e2);
		}

		protected abstract int doCompare(Viewer viewer, Object e1, Object e2);

	}


}
