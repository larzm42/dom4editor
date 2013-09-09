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
package org.larz.dom4.image;

import java.awt.geom.AffineTransform;
import java.awt.image.AffineTransformOp;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;

public abstract class ImageLoader
{
	public abstract InputStream getStream() throws IOException;
	
    public BufferedImage loadImage() throws Exception {
        // set no caching for ImageIO as there should be no rewinds
        // NOTE:  having caching enabled *severely* slows down image reading
        //        performance
        // NOTE:  this does not need to be set each time since it's local to
        //        each thread group.
        ImageIO.setUseCache(false /*don't use file caching*/);

        InputStream inputStream = null;
        ImageInputStream imageStream = null; 
        BufferedImage bufferedImage = null;
        try
        {
            // NOTE:  due to a re-emergence of:
            //           http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6331418
            //        ImageIO.read() cannot be used directly.  This is 
            //        effectively a copy of ImageIO.read() that catches the 
            //        IllegalArgumentException and tries the next available
            //        ImageReader.
            // TODO:  determine if there is a way (via classpath tricks) to set
            //        the order of the service providers so that precedence can
            //        be given to the RI provider
        	inputStream = getStream();
        	if (inputStream != null) {
                imageStream = ImageIO.createImageInputStream(inputStream);
        	}
            if(imageStream == null)
                throw new Exception("Image could not be loaded.  Probably an unknown format.");
            /* else -- there is an ImageInputStream for the image */
            final Iterator<ImageReader> readers = ImageIO.getImageReaders(imageStream);
            if(!readers.hasNext())
                throw new Exception("Image could not be loaded.  Probably an unknown format.");
            /* else -- there are ImageReader's for the image */

            // try ImageReaders until a non-null BufferedImage is loaded
            while(readers.hasNext())
            {
                try
                {
                    final ImageReader reader = readers.next();
                    reader.setInput(imageStream, true/*seek forward only*/, true/*ignore metadata*/);
                    bufferedImage = reader.read(0, reader.getDefaultReadParam());
                    reader.dispose();
                    
                    if(bufferedImage != null)
                        break;
                    /* else -- the image was not loaded */
                } catch(final IllegalArgumentException iae)
                {
                    // NOTE:  this is explicitly caught for bug:
                    //          http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6331418

                    // try the next ImageReader
                	inputStream.close();
                	inputStream = getStream();
                    imageStream = ImageIO.createImageInputStream(inputStream);
                    continue;
                }
            }

// NOTE:  intentionally commented out (see above NOTEs)
//            // read the image from the URL
//            bufferedImage = ImageIO.read(imageUrl);
        } catch(IOException ioe)
        {
            // the image could not be read or there was an error reading it
            throw new Exception(ioe);
        } finally
        {
            try
            {
                if(imageStream != null)
                    imageStream.close();
                /* else -- the ImageStream was never created */
                
                if(inputStream != null)
                	inputStream.close();
            } catch(final IOException ioe)
            {
                // an error occurred while closing
                // NOTE:  an outer IOException may have been thrown in which case
                //        this exception would shadow it.  It is acceptible in
                //        this case since they are the same exception (though
                //        the potentially more meaningful reason would be lost).
                throw new Exception(ioe);
            }
        }

        // if the buffered image is null then it could not be loaded
        if(bufferedImage == null)
            throw new Exception("Image could not be loaded.  Probably an unknown format.");
        /* else -- the buffered image is loaded */

        // flip the image vertically
        // NOTE:  OpenGL specifies that (0, 0) is in the lower-left corner 
        //        whereas a BufferedImage's (0, 0) is in the *upper*-left corner.
        final AffineTransform transform = AffineTransform.getScaleInstance(1, -1);
                              transform.translate(0, -bufferedImage.getHeight(null));
        final AffineTransformOp op =
            new AffineTransformOp(transform, AffineTransformOp.TYPE_NEAREST_NEIGHBOR);
        bufferedImage = op.filter(bufferedImage, null);

        return bufferedImage;
    }
}