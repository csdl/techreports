import java.io.*;
import java.util.*;



/**
 * 
 * SummaryList.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 * @updated
 *   It is now possible to remove history items permanently.
 *   Removed Object inheritance.
 *
 */
public class SummaryList {
    private boolean hasChanged;
    private Vector plans;
    private Vector actuals;
    private Vector files;
    private Vector paths;
    private SummaryData planData;
    private SummaryData actualData;



    /**
     *
     * Create a new SummaryList.
     * 
     *
     */
    public SummaryList() {
		hasChanged = false;
		plans = new Vector();
		actuals = new Vector();
		files = new Vector();
		paths = new Vector();
    }



    /**
     * Check if any values have been changed.
     *
     *
     * @return true if changes have been made.
     */
    public boolean hasChanged() {
		return hasChanged;
    }



    /**
     * Set the changed flag.
     *
     * @param value the boolena value.
     */
    public void setChanged(boolean value) {
		hasChanged = value;
    }



    /**
     * Add a path and a file.
     *
     * @param path the path.
     * @param file the file.
     */
    public void addPathFile(String path, String file) {
		paths.addElement(path);
		files.addElement(file);
		read();
		hasChanged = true;
    }



    /**
     * Get a file name.
     *
     * @param index the index of the file name.
     * @return the file name if it exists, else null.
     */
    public String getFile(int index) {
		if ((0 <= index)
		&& (index < files.size()))
			return new String((String)files.elementAt(index));
		return null;
    }



    /**
     * Get a path.
     *
     * @param index the index of the path.
     * @return the path if it exists, else null.
     */
    public String getPath(int index) {
		if ((0 <= index)
		&& (index < paths.size()))
			return new String((String)paths.elementAt(index));
		return null;
    }



    /**
     * Get an old plan.
     *
     * @param index the index of the plan.
     * @return the plan.
     */
    public SummaryData getPlan(int index) {
		if ((0 <= index)
		&& (index < plans.size()))
			return (SummaryData)plans.elementAt(index);
		return null;
    }



    /**
     * Get an old actual.
     *
     * @param index the index of the actual.
     * @return the actual.
     */
    public SummaryData getActual(int index) {
		if ((0 <= index)
		&& (index < actuals.size()))
			return (SummaryData)actuals.elementAt(index);
		return null;
    }


    
    /**
     * Get a path and a file.
     *
     * @param index the index of the path and the file.
     * @return the path and the file.
     */
    public String getPathFile(int index) {
		if ((0 <= index)
		&& (index < files.size())
		&& (index < paths.size()))
			return new String((String)paths.elementAt(index)
			    + (String)files.elementAt(index));
		return null;
    }



    /**
     * Get the size of the list.
     *
     *
     * @return the size of the list.
     */
    public int size() {
		return files.size();
    }



    /**
     * Remove an element from the list.
     *
     * @param index the index of the element in the list.
     * @return true if it was removed.
     */
    public boolean removeElementAt(int index) {
		if ((0 <= index)
		&& (index < files.size())) {
			plans.removeElementAt(index);
			actuals.removeElementAt(index);
			paths.removeElementAt(index);
			files.removeElementAt(index);
			hasChanged = true;
		}
		return true;
    }



    /**
     * Return a string representation of the list.
     *
     *
     * @return a string representation of the list.
     */
    public String toString() {
		StringBuffer sb = new StringBuffer();
		int siz = files.size();
		for (int index = 0; index < siz; index++) {
			sb.append(paths.elementAt(index));
			sb.append("\t");
			sb.append(files.elementAt(index));
			sb.append("\n");
		}
		return new String(sb.toString());
    }



    /**
     * Read old files.
     *
     *
     * @return true if the files were read.
     */
    public boolean read() {
		plans.removeAllElements();
		actuals.removeAllElements();
		int siz = files.size();
		if (siz <= 0) { return true; }
		for (int index = 0; index < siz; index++) {
			planData = new SummaryData();
			actualData = new SummaryData();
			readFile((String)paths.elementAt(index)
				, (String)files.elementAt(index));
			planData.update();
			actualData.update();
			plans.addElement(planData);
			actuals.addElement(actualData);
		}
		return true;
    }



    /**
     * Read a file.
     *
     * @param path the path.
     * @param file the file.
     * @return true if the file was read.
     */
    private boolean readFile(String path, String file) {
		File f = new File(path, file);
		if (f.exists() == true) {
			String rowPlan;
			String rowActual;
			try {
				RandomAccessFile accessFile = new RandomAccessFile(f, "r");
				rowPlan = new String(accessFile.readLine());
				if (rowPlan == null) { return false; }
				while (rowPlan.startsWith("PLN") == false) {
					if (rowPlan == null) { return false; }
					rowPlan = new String(accessFile.readLine());
				}
				rowActual = new String(accessFile.readLine());
				if (rowActual == null) { return false; }
				while (rowActual.startsWith("ACT") == false) {
					if (rowActual == null) { return false; }
					rowActual = new String(accessFile.readLine());
				}
			}
			catch (IOException e) { return false; }
			catch (NullPointerException e) { return false; }
			StringTokenizer st1 = new StringTokenizer(rowPlan, "\t");
			StringTokenizer st2 = new StringTokenizer(rowActual, "\t");
			if ((st1.countTokens() != 45)
			|| (st2.countTokens() != 45)) { return false; }
			int i;
			st1.nextToken();
			st2.nextToken();
			for (i = 0; i < 10; i++) {
				planData.setSize(i, new Integer(st1.nextToken()).intValue());
				actualData.setSize(i, new Integer(st2.nextToken()).intValue());
			}
			for (i = 0; i < 13; i++) {
				planData.setTimeInPhase(i, new Integer(st1.nextToken()).intValue());
				actualData.setTimeInPhase(i, new Integer(st2.nextToken()).intValue());
			}
			for (i = 0; i < 10; i++) {
				planData.setInjected(i, new Integer(st1.nextToken()).intValue());
				actualData.setInjected(i, new Integer(st2.nextToken()).intValue());
			}
			for (i = 0; i < 11; i++) {
				planData.setRemoved(i, new Integer(st1.nextToken()).intValue());
				actualData.setRemoved(i, new Integer(st2.nextToken()).intValue());
			}
			return true;
		}
		return false;
	}
}
