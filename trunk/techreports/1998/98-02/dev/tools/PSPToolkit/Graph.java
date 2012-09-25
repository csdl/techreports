

import java.awt.*;
import java.util.*;
//import javax.swing.*;
//import com.sun.java.swing.*;

/**
 * 
 *
 * @author Johan Forsman
 * @version 0.0
 *
 * History:  9805?? Johan Forsman  implementation
 *           990113 Anders Östman  modified to use Swing
 *           990209 Anders Östman  bug-fix in setMaxMinPoints()
 *           990220 Anders Östman  modified to support "data x-labels"
 *           990224 Anders Östman  modified to support unit labels on each axis
 *           990224 Anders Östman  workin to add support for heading / title
 */

/**
 *  the graph can only visualize integer data (GraphData)
 */

public class Graph extends Panel {
  private Color paper;
  private Color frame;
  private Dimension d;

  private String baseString;                   // text at the bottom of the diagram
  private String yMaxString;                   // label for max y value
  private String yMinString;                   // label for min y value
  private String xMaxString;                   // label for max x value
  private String xMinString;                   // label for min x value
  private String xUnitLabel = "";              // label for the x-axis "unit"
  private String yUnitLabel = "";              // label for the y-axis "unit"
  private String graphTitle = "";              // graph title

  private int margin = 5;                      // margin around the drawing area
  private int rightMargin = margin;            // margin on the right side of the graph
  private int fontHeight;                      // height for the current font
  private int baseLine;                        // bottom of all graph objects
  private int yScaleWidth;                     // left margin + y-label text width + margin
  private int xScaleBaseLine;                  // bottom of x-label text
  private int xScale;                          // y-position for the x-axis
  private int minLabelNumber=0 ;               // don't use custom label with lower index
  private int maxLabelNumber = -1;             // don't use custom label with higher index

  private Point baseStringPoint = new Point(0, 0);  // bottom left of base text
  private Point xScaleStart = new Point(0, 0);      // x-axis start
  private Point xScaleStop = new Point(0, 0);       // x-axis stop
  private Point yScaleStart = new Point(0, 0);      // y-axis start
  private Point yScaleStop = new Point(0, 0);       // y-axis stop
  private Point xMinStringAt = new Point(0, 0);     // bottom left of x-axis minimum label
  private Point xMaxStringAt = new Point(0, 0);     // bottom left of x-axis maximum label
  private Point yMinStringAt = new Point(0, 0);     // bottom left of y-axis minimum label
  private Point yMaxStringAt = new Point(0, 0);     // bottom left of y-axis maximum label
  private Point origo = new Point(0, 0);            // x-axis and y-axis intersection
  private Point delta = new Point(0, 0);            // drawable area from min to max
  private Point maxPoint = new Point(100, 100);     // true x and y max values
  private Point minPoint = new Point(0, 0);         // true x and y min valuse

  private Vector values;                         // graph series data
  private Vector xLabels;                        // custom labels for the x-axis


  /**
   *
   */
  public Graph() {
    paper = new Color(255, 255, 255);
    frame = new Color(0, 0, 0);
    baseString = new String("");
    yMaxString = new String("100");
    yMinString = new String("0");
    xMinString = new String("0");
    xMaxString = new String("100");
    values = new Vector();
    xLabels = new Vector();
  }  // end Graph()



  /**
   * Set base text.
   *
   * @param baseText the text at the baseof the diagram.
   */
  public void setBaseText(String baseText) {
    baseString = new String(baseText);
  }  // end setBaseText()



  /**
   * Add a new data serie.
   *
   */
  public void addSeries(GraphData gd) {
    values.addElement(gd);
    repaint();
  }  // end addSeries()



  /**
   * Add a new graph title
   * @param String label for the title
   * @note Clear the title by adding an empty string
   */  
  public void addTitle(String st) {
    graphTitle = new String(st);
  }



  /**
   * Add a new set of x-labels.
   * @param Vector with GraphLabel objects
   * @note Clear the labels by sending a new, empty Vector to this method
   * @note the elements of the vector MUST be sorted in asscending order on their value 
   */
  public void addXLabels(Vector gl) {
    xLabels = gl;
    if(xLabels.size()==0) {
      minLabelNumber = 0;
      maxLabelNumber = -1;
    }
    repaint();
  }  // end addXLabel()




  /**
   * Add a new x-axis unit label
   * @param String label for the x-axis
   * @note Clear the label by adding an empty string
   */  
	public void addXUnitLabel(String xst) {
		xUnitLabel = new String(xst);
	}


  /**
   * Add a new y-axis unit label
   * @param String label for the y-axis
   * @note Clear the label by adding an empty string
   */  
  public void addYUnitLabel(String yst) {
    yUnitLabel = new String(yst);
  }

 

  /**
   * Paints the graphs.
   *
   * @param g the graphics context.
   */
  public void paint(Graphics g) {
    setMaxMinPoints();                                 // find max and min values
    setMetrics();                                      // calculate some useful values
    drawBackground(g);                                 // create a background
    drawLabels(g);                                     // print x- and y-axis labels etc.

    int i, j, x, y;
    int oldX = 0, oldY = 0;
    GraphData gd;

    for (i = 0; i < values.size(); i++) {              // for every data serie
      gd = (GraphData)values.elementAt(i);
      g.setColor(gd.color());
      for (j = 0; j < gd.size(); j++) {                // draw every data point
        if ((maxPoint.x - minPoint.x) == 0)
          maxPoint.x++;                                // separate x max and min values

        if ((maxPoint.y - minPoint.y) == 0)
          maxPoint.y++;                                // separate y max and min values

        x = posToX( ((Point)gd.elementAt(j)).x );      // find x-position for data point

        y = posToY( ((Point)gd.elementAt(j)).y );      // find y-position for data point

        if (gd.type == gd.CROSS) {
          g.drawLine(x - 2, y - 2, x + 2, y + 2);
          g.drawLine(x + 2, y - 2, x - 2, y + 2);
        }

        else if (gd.type == gd.BOX) {
          g.drawRect(x - 2, y - 2, 4, 4);
        }

        else if (gd.type == gd.CIRCLE) {
          g.drawOval(x - 2, y - 2, 4, 4);
        }

        else if (gd.type == gd.DASH) {
          g.drawLine(x - 2, y, x+2, y);
        }

        else if (gd.type == gd.PLUS) {
          g.drawLine(x - 2, y, x+2, y);
          g.drawLine(x, y-2, x, y+2);
        }

        else if (gd.type == gd.LINE) {
          if (j == 0) {     // if it's the first data point
            oldX = x;
            oldY = y;
          }
          g.drawLine(x, y, oldX, oldY);
          oldX = x;
          oldY = y;
        }

        else if (gd.type == gd.BAR) {
          g.fillRect(x - 2, y, 5, origo.y - y + 1 );   
        }

      }  // end for(j...
    }  // end for(i...

  }  // end paint()



  /**
   * Sets max and min values.
   */
  private void setMaxMinPoints() {
    Point tmpPoint;
    GraphData gd;

    minPoint.x=0;  maxPoint.x=0;
    minPoint.y=0;  maxPoint.y=0;

    for (int i = 0; i < values.size(); i++) {   // go through all GraphData's
      gd = (GraphData)values.elementAt(i);
      for (int j = 0; j < gd.size(); j++) {     // go through all Point's
        tmpPoint = gd.elementAt(j);
        if (tmpPoint.x < minPoint.x)
          minPoint.x = tmpPoint.x;
        if (tmpPoint.x > maxPoint.x)
          maxPoint.x = tmpPoint.x;

        if (tmpPoint.y < minPoint.y)
          minPoint.y = tmpPoint.y;
        if (tmpPoint.y > maxPoint.y)
          maxPoint.y = tmpPoint.y;
      }
    }

    yMaxString = Integer.toString(maxPoint.y);
    yMinString = Integer.toString(minPoint.y);
    xMinString = Integer.toString(minPoint.x);
    xMaxString = Integer.toString(maxPoint.x);

    if(xLabels.size() > 0) {           // find index limits for visible custom labels
      for(int n = 0; (n<xLabels.size())
                    && ( ((GraphLabel)xLabels.elementAt(n)).value <= maxPoint.x) ; n++) {
        if( ((GraphLabel)xLabels.elementAt(n)).value < minPoint.x)
          minLabelNumber = n+1;
        maxLabelNumber = n;
      }
    }
  }  // end setMaxMinPoints()



  /**
   * Sets the metrics of the panel.
   */
  private void setMetrics() {
    d = getSize();
    fontHeight = getFontMetrics(getFont()).getHeight();

    baseLine = d.height - margin;                       // bottom of all graph objects
    baseStringPoint.x = (d.width - getFontMetrics(getFont()).stringWidth(baseString)) / 2;
    baseStringPoint.y = baseLine;
                                                     // calculate space needed on the right
    if( (xLabels.size()==0) || (maxLabelNumber<minLabelNumber) )  
      rightMargin = margin + (getFontMetrics(getFont()).stringWidth(xMaxString)/2) ;
    else {
      GraphLabel gl = (GraphLabel)xLabels.elementAt(maxLabelNumber);       // last visible label
      rightMargin = margin + (getFontMetrics(getFont()).stringWidth(gl.text)/2) ;
    }

    if(xUnitLabel.length()!=0) {                    // just for clearity
      rightMargin += margin + getFontMetrics(getFont()).stringWidth(xUnitLabel);
    }

    int tmp1 = margin + getFontMetrics(getFont()).stringWidth(yMinString);
    int tmp2 = margin + getFontMetrics(getFont()).stringWidth(yMaxString);
    yScaleWidth = (tmp1 > tmp2) ? tmp1 : tmp2;

    xScaleBaseLine = baseLine - fontHeight;     // bottom of x-labels text
    xScale = xScaleBaseLine - fontHeight;       // position for x-axis in y-direction

    xScaleStart.x = yScaleWidth + margin;       // x-axis start position
    xScaleStart.y = xScale;                     // -"-
    xScaleStop.x  = d.width - rightMargin;      // x-axis stop position
    xScaleStop.y  = xScaleStart.y;              // -"-

    yScaleStart.x = yScaleWidth + margin;       // y-axis start position
    yScaleStart.y = xScale;                     // -"-
    yScaleStop.x  = yScaleStart.x;              // y-axis stop position
    yScaleStop.y  = margin;                     // -"-
    if(yUnitLabel.length()!=0)
      yScaleStop.y  += margin + fontHeight;     // adjust y-axis stop for y-axis unit label
    if(graphTitle.length()!=0)
      yScaleStop.y  += margin + fontHeight;     // adjust y-axis stop for graph title

    xMinStringAt.x = xScaleStart.x;
    xMinStringAt.y = xScaleBaseLine;
    xMaxStringAt.x = xScaleStop.x - getFontMetrics(getFont()).stringWidth(xMaxString)/2;
    xMaxStringAt.y = xScaleBaseLine;

    yMinStringAt.x = margin;
    yMinStringAt.y = xScale+3;                   // (+3) -> text middle at y-axis bottom
    yMaxStringAt.x = margin;
    yMaxStringAt.y = yScaleStop.y + (fontHeight/2);  // +(font.../2) ->text middle at y-axis top

    origo.x = yScaleWidth + margin + 2;         // (+2) -> adjust for line width ??
    origo.y = xScale - 2;                       // (-2) ->   -"-

    delta.x = xScaleStop.x - origo.x;                   // drawable area along x-axis
    delta.y = origo.y - yScaleStop.y - 1;               // drawable area along y-axis
  }  // end setMetrics()



  /**
   *  create a background for graph drawing
   */
  private void drawBackground(Graphics g) {
    g.setColor(paper);                             //
    g.fillRect(0, 0, d.width, d.height);           // white background
    g.setColor(frame);                             // black lines
    g.drawLine(xScaleStart.x, xScaleStart.y, xScaleStop.x, xScaleStop.y);  // draw x-axis
    g.drawLine(yScaleStart.x, yScaleStart.y, yScaleStop.x, yScaleStop.y);  // draw y-axis
  }  // end drawBackground()


  
  /**
   *  print labels on the graph
   */
  private void drawLabels(Graphics g) {
    if(graphTitle.length()!=0) 
     g.drawString(graphTitle, 
                  d.width/2 - getFontMetrics(getFont()).stringWidth(graphTitle)/2 ,
                  margin + fontHeight);                              // print graph main title

    g.drawString(baseString, baseStringPoint.x, baseStringPoint.y);  // print graph sub-title

    if( (xLabels.size()==0) || (maxLabelNumber < minLabelNumber) ) {
      g.drawString(xMinString, xMinStringAt.x, xMinStringAt.y);      // print x-axis labels
      g.drawString(xMaxString, xMaxStringAt.x, xMaxStringAt.y);      //   -"-
      xTick(g,minPoint.x);                                           // scale marks
      xTick(g,maxPoint.x);
    }
    else {                                                        // user specified labels
      for(int n=minLabelNumber; n <= maxLabelNumber; n++) {
        GraphLabel gl = (GraphLabel)xLabels.elementAt(n);
        g.drawString(gl.text,
                     posToX(gl.value)
                       - getFontMetrics(getFont()).stringWidth(gl.text)/2 ,
                     xMinStringAt.y);                          // print custom x-axis labels
       
        xTick(g, gl.value);                                    // scale mark for label
      }
    }

    if(xUnitLabel.length()!=0)
     g.drawString(xUnitLabel, xScaleStop.x + margin , xScaleStop.y); // unit label on x-axis

    if(yUnitLabel.length()!=0)
     g.drawString(yUnitLabel, yScaleStop.x, yScaleStop.y-margin);    // unit label on y-axis

    g.drawString(yMinString, yMinStringAt.x, yMinStringAt.y);      // print y-axis labels
    g.drawString(yMaxString, yMaxStringAt.x, yMaxStringAt.y);      //   -"-
    yTick(g);

  }  // end drawLabels()



  /**
   * convert from 'normal' coordinates to 'pixel' coordinates
   * @param normal x coordinate
   * @return x pixel position
   */
  private int posToX(int Xpos) {
    if(minPoint.x==maxPoint.x)
      return(origo.x);                    // to avoid division by zero
    else
      return ( ((Xpos - minPoint.x) * delta.x / (maxPoint.x - minPoint.x)) + origo.x );
  }  // end posToX()



  /**
   * convert from 'normal' coordinates to 'pixel' coordinates
   * @param normal y coordinate
   * @return y pixel position
   */
  private int posToY(int yPos) {
    if(minPoint.y==maxPoint.y)
      return(origo.y);                    // to avoid division by zero
    else
      return( ((yPos - minPoint.y) * delta.y * (-1) / (maxPoint.y - minPoint.y))+ origo.y );
  }  // end posToY()



  /**
   *  make scale mark on the x-axis
   */
  private void xTick(Graphics g, int x) {
    if( (x >= minPoint.x) && (x <= maxPoint.x) )
      g.drawLine(posToX(x), xScaleStart.y, posToX(x), xScaleStart.y+3 );    // make a mark
  }  // end xTick()



  /**
   *  make 5 scale marks on the y-axis
   */
  private void yTick(Graphics g) {
    double step = (yScaleStop.y - yScaleStart.y )/5.0;
 
    for(int n=0; n<5+1 ; n++) {
      g.drawLine(yScaleStart.x-3, yScaleStart.y + (int)Math.round(step*n) ,
                 yScaleStart.x , yScaleStart.y + (int)Math.round(step*n)  );  // make a mark
    }
  }  // end yTick()


}  // end Graph




