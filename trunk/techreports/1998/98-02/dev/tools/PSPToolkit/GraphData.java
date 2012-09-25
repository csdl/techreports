
import java.awt.Color;
import java.awt.Point;
import java.util.Vector;



/**
 * 
 *
 * @author Johan Forsman
 * @version 0.0
 *
 * History: 9805?? Johan Forsman  implementation
 *          990113 Anders Östman  modified point-types
 *
 */
public class GraphData extends Object {

  private Vector v = new Vector();

  public int type = 0;                           //The current graph type.

  public Color color = new Color(0, 0, 0);       // The current color of the points.

  static public final int CROSS = 0;                    // x -point graph.

  static public final int BOX = 1;                      // [] -point graph.

  static public final int CIRCLE = 2;                   // o -point graph.

  static public final int DASH = 3;                     // - -point graph  (short line).

  static public final int PLUS = 4;                     // + -point graph.

  static public final int LINE = 10;                    // Line graph.

  static public final int BAR = 11;                     // Bar graph.


  /**
   * Returns the size of this graph vector.
   *
   *
   * @return the size of this graph vector.
   */
  public int size() {
    return v.size();
  }



  /**
   * Adds a point to the graph vector.
   *
   * @param p the point.
   *
   */
  public void addElement(Point p) {
    v.addElement(p);
  }



  /**
   * Gets a point from the graph vector.
   *
   * @param index the index of the point.
   * @return the point or null.
   */
  public Point elementAt(int index) {
    if ( (0 <= index)
         && (index < v.size())) {
      int x = ((Point)v.elementAt(index)).x;
      int y = ((Point)v.elementAt(index)).y;
      return new Point(x, y);
    }
    else
      return null;
  }



  /**
   * Sets the color of the points in the graph vector.
   *
   * @param c the color.
   *
   */
  public void color(Color c) {
    color = c;
  }



  /**
   * Returns the color of the points in the graph vector.
   *
   *
   * @return the color of the points.
   */
  public Color color() {
    return color;
  }


}  // end Graphdata



