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
package org.larz.dom4.dm.ui.comment;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.parsetree.reconstr.impl.DefaultCommentAssociater;
import org.eclipse.xtext.parsetree.reconstr.impl.NodeIterator;
import org.eclipse.xtext.util.Pair;

/**
 */
public class DmCommentAssociater extends DefaultCommentAssociater {

	/**
	 * This implementation associates each comment with the next following semantic token's EObject, except for the
	 * case, where a line of the document end by a semantic element followed by a comment. Then, the the comment is
	 * associated with this preceding semantic token.
	 */
	@Override
	protected void associateCommentsWithSemanticEObjects(Map<ILeafNode, EObject> mapping, ICompositeNode rootNode) {
		EObject currentEObject = null;
		List<ILeafNode> currentComments = new ArrayList<ILeafNode>();

		NodeIterator nodeIterator = new NodeIterator(rootNode);
		// rewind to previous token with token owner 
		while (nodeIterator.hasPrevious() && currentEObject == null) {
			INode node = nodeIterator.previous();
			if (tokenUtil.isToken(node)) {
				currentEObject = tokenUtil.getTokenOwner(node);
			}
		}
		while (nodeIterator.hasNext()) {
			INode o = nodeIterator.next();
			if (o instanceof INode) {
				INode node = (INode) o;
				if (tokenUtil.isCommentNode(node)) {
					currentComments.add((ILeafNode) node);
				}
				boolean isToken = tokenUtil.isToken(node);
				if ((node instanceof ILeafNode || isToken) && node.getStartLine() != node.getEndLine() && currentEObject != null) {
					// found a newline -> associating existing comments with currentEObject
					//addMapping(mapping, currentComments, currentEObject);
					currentEObject = null;
					continue;
				}
				if (isToken) {
					Pair<List<ILeafNode>, List<ILeafNode>> leadingAndTrailingHiddenTokens = tokenUtil
							.getLeadingAndTrailingHiddenTokens(node);
					for (ILeafNode leadingHiddenNode : leadingAndTrailingHiddenTokens.getFirst()) {
						if (tokenUtil.isCommentNode(leadingHiddenNode)) {
							currentComments.add(leadingHiddenNode);
						}
					}
					nodeIterator.prune();
					currentEObject = tokenUtil.getTokenOwner(node);
					if (currentEObject != null) {
						addMapping(mapping, currentComments, currentEObject);
						if (node.getOffset() > rootNode.getOffset() + rootNode.getLength()) {
							// found next EObject outside rootNode
							break;
						}
					}
				}
			}
		}
		if (!currentComments.isEmpty()) {
			if (currentEObject != null) {
				addMapping(mapping, currentComments, currentEObject);
			} else {
				EObject objectForRemainingComments = getEObjectForRemainingComments(rootNode);
				if (objectForRemainingComments != null) {
					addMapping(mapping, currentComments, objectForRemainingComments);
				}
			}
		}
	}

}
