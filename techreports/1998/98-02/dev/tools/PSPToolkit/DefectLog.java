


/**
 * 
 * DefectLog.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed Object inheritance.
 *
 */
public class DefectLog {
    private boolean hasChanged;					// has changed
    private DefectList defectList;				// time list



    /**
     * Creates a new DefectLog.
     *
     * 
     *
     */
    public DefectLog() {
		hasChanged = false;
		defectList = new DefectList();
    }



    /**
     * Gets the size of this log.
     *
     *
     * @return the size of the log.
     */
    public int size() {
		return defectList.size();
    }



    /**
     * Gets the string representation of an element.
     *
     * @param index of the element.
     * @return the string representing this element.
     */
    public String getElementString(int index) {
		if ((index >= 0)
		&& (index < defectList.size())) {
			return new String(defectList.getElementString(index));
		}
		return new String("");
    }



    /**
     * Get an element from the list.
     *
     *
     *
     */
    public DefectData getElement(int index) {
		if ((index >= 0)
		&& (index < defectList.size())) {
			hasChanged = true;
			return defectList.getElement(index);
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
		&& (index < defectList.size())) {
			defectList.removeElement(index);
			hasChanged = true;
			return true;
		}
		return false;
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
		return new String(defectList.toString());
    }



    /**
     * Add a DefectData to the time list.
     *
     * @param defectData the time data to add.
     * @return true of the data was added.
     */
    public boolean add(DefectData defectData) {
		if (defectList.addElement(defectData) == false)
			return false;
		hasChanged = true;
		return true;
    }



    /**
     * Get the current defect list.
     *
     *
     * @return the current defect list.
     */
    public DefectList getDefectList() {
		return defectList;
    }
}
