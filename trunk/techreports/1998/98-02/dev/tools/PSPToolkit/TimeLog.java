


/**
 *
 * TimeLog.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Exchanged TimeListException for Exception.
 *   Removed Object inheritance.
 *
 */
public class TimeLog {
    private boolean hasChanged;					// has changed
    private TimeList timeList;					// time list



    /**
     * Create a new TimeLog.
     *
     * 
     *
     */
    public TimeLog() {
		hasChanged = false;
		timeList = new TimeList();
    }



    /**
     * Get an element from the list.
     *
     *
     *
     */
    public TimeData getElement(int index) {
		if ((index >= 0)
		&& (index < timeList.size())) {
			hasChanged = true;
			return timeList.getElement(index);
		}
		else return null;
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
			timeList.removeElement(index);
			hasChanged = true;
			return true;
		}
		else return false;
    }



    /**
     * Get the size of this log.
     *
     *
     * @return the size of the log.
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
			return new String(timeList.getElementString(index));
		}
		else return new String("");
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
     * Check if any values have been changed.
     *
     *
     * @return true if changes have been made.
     */
    public boolean hasChanged() {
		return hasChanged;
    }



    /**
     * Create a string that represents this object.
     *
     * 
     * @return a string representation of this object.
     */
    public String toString() {
		return new String(timeList.toString());
    }



    /**
     * Add a TimeData to the time list.
     *
     * @param timeData the time data to add.
     * @exception if an error occured.
     */
    public void add(TimeData timeData) throws Exception {
		try {
			timeList.addElement(timeData);
		}
		catch (Exception e) {
			throw e;
		}
		hasChanged = true;
    }



    /**
     * Get the current time list.
     *
     *
     * @return the current time list.
     */
    public TimeList getTimeList() {
		return timeList;
    }
}
