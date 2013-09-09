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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.larz.dom4.dm.dm.AbstractElement;

public class AbstractElementWrapper implements AbstractElement {
	private AbstractElement element;
	private int id = -1;
	
	public AbstractElementWrapper(AbstractElement element, int id) {
		this.element = element;
		this.id = id;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AbstractElementWrapper other = (AbstractElementWrapper) obj;
		if (id != other.id)
			return false;
		return true;
	}

	public AbstractElement getElement() {
		return element;
	}

	@Override
	public TreeIterator<EObject> eAllContents() {
		return element.eAllContents();
	}

	@Override
	public EClass eClass() {
		return element.eClass();
	}

	@Override
	public EObject eContainer() {
		return element.eContainer();
	}

	@Override
	public EStructuralFeature eContainingFeature() {
		return element.eContainingFeature();
	}

	@Override
	public EReference eContainmentFeature() {
		return element.eContainmentFeature();
	}

	@Override
	public EList<EObject> eContents() {
		return element.eContents();
	}

	@Override
	public EList<EObject> eCrossReferences() {
		return element.eCrossReferences();
	}

	@Override
	public Object eGet(EStructuralFeature arg0, boolean arg1) {
		return element.eGet(arg0, arg1);
	}

	@Override
	public Object eGet(EStructuralFeature arg0) {
		return element.eGet(arg0);
	}

	@Override
	public Object eInvoke(EOperation arg0, EList<?> arg1)
			throws InvocationTargetException {
		return element.eInvoke(arg0, arg1);
	}

	@Override
	public boolean eIsProxy() {
		return element.eIsProxy();
	}

	@Override
	public boolean eIsSet(EStructuralFeature arg0) {
		return element.eIsSet(arg0);
	}

	@Override
	public Resource eResource() {
		return element.eResource();
	}

	@Override
	public void eSet(EStructuralFeature arg0, Object arg1) {
		element.eSet(arg0, arg1);
	}

	@Override
	public void eUnset(EStructuralFeature arg0) {
		element.eUnset(arg0);
	}

	@Override
	public EList<Adapter> eAdapters() {
		return element.eAdapters();
	}

	@Override
	public boolean eDeliver() {
		return element.eDeliver();
	}

	@Override
	public void eNotify(Notification arg0) {
		element.eNotify(arg0);
	}

	@Override
	public void eSetDeliver(boolean arg0) {
		element.eSetDeliver(arg0);
	}

}

