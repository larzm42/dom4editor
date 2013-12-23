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
package org.larz.dom4;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class ActionWiper implements IStartup, IPerspectiveListener
{
	private static final String[]	ACTIONS_2_WIPE	= new String[] {
			"org.eclipse.search.searchActionSet", //$NON-NLS-1$
			"org.eclipse.debug.ui.breakpointActionSet", //$NON-NLS-1$
			"org.eclipse.debug.ui.debugActionSet", //$NON-NLS-1$
			"org.eclipse.debug.ui.launchActionSet", //$NON-NLS-1$
			"org.eclipse.debug.ui.profileActionSet", //$NON-NLS-1$
			"org.eclipse.ui.externaltools.ExternalToolsSet", //$NON-NLS-1$
			"org.eclipse.ui.edit.text.actionSet.navigation",
			"org.eclipse.ui.edit.text.actionSet.convertLineDelimitersTo",
			"org.eclipse.update.ui.softwareUpdates",
			"org.eclipse.ui.edit.text.actionSet.openExternalFile",
			"org.eclipse.ui.actionSet.openFiles",
			"org.eclipse.ui.edit.text.actionSet.annotationNavigation"
//			"org.eclipse.ui.edit.text.actionSet.presentation",
			};

	public void earlyStartup()
	{
		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
		if (windows.length == 0) {
			Job job = new Job("remove uneeded actions") {

				@Override
				protected IStatus run(IProgressMonitor monitor) {
					IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
					for (int i = 0; i < windows.length; i++)
					{
						IWorkbenchPage page = windows[i].getActivePage();
						if (page != null)
						{
							wipeActions(page);
						}
						windows[i].addPerspectiveListener(ActionWiper.this);
					}
					return null;
				}
				
			};
			job.schedule(1000);
			
		}
		for (int i = 0; i < windows.length; i++)
		{
			IWorkbenchPage page = windows[i].getActivePage();
			if (page != null)
			{
				wipeActions(page);
			}
			windows[i].addPerspectiveListener(this);
		}
	}

	private void wipeActions(final IWorkbenchPage page)
	{
		Display.getDefault().syncExec(new Runnable() {
			public void run()
			{
				for (int i = 0; i < ACTIONS_2_WIPE.length; i++)
				{
					page.hideActionSet(ACTIONS_2_WIPE[i]);
				}
			}
		});
	}

	public void perspectiveActivated(IWorkbenchPage page,
			IPerspectiveDescriptor perspective)
	{
		wipeActions(page);
	}

	public void perspectiveChanged(IWorkbenchPage page,
			IPerspectiveDescriptor perspective, String changeId)
	{
	}
}
