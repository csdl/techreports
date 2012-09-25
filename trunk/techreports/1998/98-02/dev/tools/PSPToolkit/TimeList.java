import java.util.*;


/**
 * 
 * TimeList.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Exchanged TimeListException for Exception.
 *   Removed Object inheritance.
 *
 */
public class TimeList {
    private Vector timeList;				// time vector
    public int planTime = 0;
    public int hLDesignTime = 0;
    public int hLDesignReviewTime = 0;
    public int designTime = 0;
    public int designReviewTime = 0;
    public int codeTime = 0;
    public int codeReviewTime = 0;
    public int compileTime = 0;
    public int testTime = 0;
    public int postMortemTime = 0;



    /**
     * Create a new TimeList.
     *
     * 
     *
     */
    public TimeList() {
		timeList = new Vector();
    }



    /**
     * Add a TimeData element to the list.
     *
     * @param timeData the TimeData to add to the list.
     * @return true if the data was added.
     * @exception when date is overlapping.
     */
    public boolean addElement(TimeData timeData) throws Exception {
		int i = timeList.size();
		if (i == 0) {
			timeList.addElement(timeData);
		}
		else if (timeData.after((TimeData)timeList.elementAt(i-1))) {
			timeList.addElement(timeData);
		}
		else {
			while ((i > 0)
			&& (timeData.before((TimeData)timeList.elementAt(i-1)))) {
				i--;
			}
			if ((i == 0) 
			|| (timeData.after((TimeData)timeList.elementAt(i-1)))) {
				timeList.insertElementAt(timeData,i);
				return true;
			}
			else throw new Exception("Overlapping times!");
		}
		return true;
    }



    /**
     * Remove an element from the list.
     *
     * @param index of the element to be remove.
     * @return true if the element was removed.
     */
    public boolean removeElement(int index) {
		if ((index >= 0)
		&& (index < timeList.size())) {
			timeList.removeElementAt(index);
			return true;
		}
		else return false;
    }



    /**
     * Remove all elements of the list.
     *
     */
    public void removeAll() {
		timeList.removeAllElements();
	}



    /**
     * Take an element from the list (removes).
     *
     * @param index of the element to take.
     * @return the element.
     */
    public TimeData getElement(int index) {
		if ((index >= 0)
		&& (index < timeList.size())) {
			TimeData td;
			td = (TimeData)timeList.elementAt(index);
			timeList.removeElementAt(index);
			return td;
		}
		return null;
    }



    /**
     * Get an element in the list (a pointer to the element).
     *
     * @param index of the element.
     * @return the element.
     */
    public TimeData inspectElement(int index) {
		if ((index >= 0)
		&& (index < timeList.size())) {
			TimeData td;
			td = (TimeData)timeList.elementAt(index);
			return td;
		}
		return null;
    }



    /**
     * Get the size of this list.
     *
     *
     * @return the size of the list.
     */
    public int size() {
		return timeList.size();
    }



    /**
     * Get the string representation of an element.
     *
     * @param index of the element.
     * @return the string representing this element.
     */
    public String getElementString(int index) {
		if ((index >= 0)
		&& (index < timeList.size())) {
			return new String(timeList.elementAt(index).toString());
		}
		return new String("");
    }



    /**
     * Create a string representation of TimeList.
     *
     *
     * @return the string representation of TimeList.
     */
    public String toString() {
		StringBuffer s = new StringBuffer("");
		int i = 0;
		while (i < timeList.size()) {
			s.append(getElementString(i) + "\n");
			i++;
		}
		return new String(s.toString());
    }



    /**
     * Calculate total times.
     *
     *
     * @return true if calculation done.
     */
    public boolean calculateTimes() {
		planTime = 0;
		hLDesignTime = 0;
		hLDesignReviewTime = 0;
		designTime = 0;
		designReviewTime = 0;
		codeTime = 0;
		codeReviewTime = 0;
		compileTime = 0;
		testTime = 0;
		postMortemTime = 0;
		int siz = timeList.size();
		if (siz < 0)
			return false;
		TimeData td;
		Datum d1;
		Datum d2;
		String phase;
		for (int i = 0; i < siz; i++) {
			td = (TimeData)timeList.elementAt(i);
			d1 = td.getStartDate();
			d2 = td.getStopDate();
			phase = new String(td.getPhase());
			if (phase.compareTo("Planning") == 0)
				planTime = planTime
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("High Level Design") == 0)
				hLDesignTime = hLDesignTime
			    + new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("High Level Design Review") == 0)
				hLDesignReviewTime = hLDesignReviewTime
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Design") == 0)
				designTime = designTime 
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Design Review") == 0)
				designReviewTime = designReviewTime
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Code") == 0)
				codeTime = codeTime
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Code Review") == 0)
				codeReviewTime = codeReviewTime
			    + new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Compile") == 0)
				compileTime = compileTime
			    + new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Test") == 0)
				testTime = testTime
				+ new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
			if (phase.compareTo("Post Mortem") == 0)
				postMortemTime = postMortemTime
			    + new Long((d2.getTime()
				- d1.getTime())
				/ 60000).intValue();
		}
		return true;
    }



    /**
     * Return the date of the first compile.
     *
     *
     * @return the date of the first compile, if no compile null.
     */
    public Datum getFirstCompileDate() {
		int siz = timeList.size();
		TimeData td;
		if (siz == 0)
			return null;
		for (int i = 0; i < siz; i++) {
			td = (TimeData)timeList.elementAt(i);
			if (td.getPhase().compareTo("Compile") == 0)
				return td.getStartDate();
		}
		return null;
    }
}
