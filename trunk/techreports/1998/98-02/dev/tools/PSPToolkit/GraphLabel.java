

/**
 *  History:
 *  990219 Anders Östman  implementation
 *
 */
public class GraphLabel extends Object {

  public String text;        // text for label
  public int  value;         // this is the value where the label will be placed on the x-axis

  GraphLabel(String s, int i) {
    text = new String(s);
    value = i;
  }

}  // end GraphLabel



