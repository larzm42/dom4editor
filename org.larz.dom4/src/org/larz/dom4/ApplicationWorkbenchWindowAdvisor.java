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

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

public class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

    /**
     * @param configurer
     */
    public ApplicationWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
        super(configurer);
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.application.WorkbenchWindowAdvisor#preWindowOpen()
     */
    public void preWindowOpen() {
        IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
        configurer.setShowCoolBar(true);
        configurer.setShowStatusLine(false);
        
        // Load in some classes needed later
		IExtensionPoint ep = RegistryFactory.getRegistry().getExtensionPoint("org.eclipse.ui.editors");
		final IExtension[ ] extensions = ep.getExtensions();
		IConfigurationElement confElem = null;
		for (int i = 0; i < extensions.length; i++) {
			IExtension ex = extensions[i];
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
			confElem.createExecutableExtension("class");
		} catch (Exception e) {
			e.printStackTrace();
		}
    }

	@Override  
	public boolean preWindowShellClose() {  
		try {  
			// save the full workspace before quit  
			ResourcesPlugin.getWorkspace().save(true, null);  
		} catch (final CoreException e) {  
			// log exception, if required  
			e.printStackTrace();
		}  

		return true;  
	}  

}
