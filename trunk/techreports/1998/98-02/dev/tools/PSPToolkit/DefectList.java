import java.util.Vector;



/**
 * 
 * DefectList.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed Object inheritance.
 *
 */
public class DefectList {
    private Vector defectList;				// time vector
    public int injectedPlanning = 0;
    public int injectedHighLevelDesign = 0;
    public int injectedHighLevelDesignReview = 0;
    public int injectedDesign = 0;
    public int injectedDesignReview = 0;
    public int injectedCode = 0;
    public int injectedCodeReview = 0;
    public int injectedCompile = 0;
    public int injectedTest = 0;
    public int removedPlanning = 0;
    public int removedHighLevelDesign = 0;
    public int removedHighLevelDesignReview = 0;
    public int removedDesign = 0;
    public int removedDesignReview = 0;
    public int removedCode = 0;
    public int removedCodeReview = 0;
    public int removedCompile = 0;
    public int removedTest = 0;



    /**
     * Creates a new TimeList.
     *
     * 
     *
     */
    public DefectList() {
		defectList = new Vector();
    }



    /**
     * Adds a DefectData element to the list.
     *
     * @param defectData the DefectData to add to the list.
     * @return true if the data was added.
     */
    public boolean addElement(DefectData defectData) {
		if (defectData.getFoundDate() == null)
			return false;
		int i = defectList.size();
		if (i == 0) {
			defectList.addElement(defectData);
			return true;
		}
		else if (this.inspectElement(i - 1).before(defectData)) {
			defectList.addElement(defectData);
			return true;
		}
		else {
			while ((i > 0)
			&& (defectData.before((DefectData)defectList.elementAt(i - 1)))) {
				i--;
			}
		}
		defectList.insertElementAt(defectData, i);
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
		&& (index < defectList.size())) {
			defectList.removeElementAt(index);
			return true;
		}
		else return false;
    }



    /**
     * Remove all elements of the list.
     *
     */
    public void removeAll() {
		defectList.removeAllElements();
    }



    /**
     * Take an element from the list (removes).
     *
     * @param index of the element to take.
     * @return the element.
     */
    public DefectData getElement(int index) {
		if ((index >= 0)
		&& (index < defectList.size())) {
			DefectData td;
			td = (DefectData)defectList.elementAt(index);
			defectList.removeElementAt(index);
			return td;
		}
		return null;
    }



    /**
     * Gets an element in the list (a pointer to the element).
     *
     * @param index of the element.
     * @return the element.
     */
    public DefectData inspectElement(int index) {
	if ((index >= 0)
	 && (index < defectList.size())) {
	    DefectData td;
	    td = (DefectData)defectList.elementAt(index);
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
		return defectList.size();
    }



    /**
     * Get the string representation of an element.
     *
     * @param index of the element.
     * @return the string representing this element.
     */
    public String getElementString(int index) {
		if ((index >= 0)
		&& (index < defectList.size())) {
			DefectData dd = (DefectData)defectList.elementAt(index);
			if (dd.getFixDate() != null)
				return new String(dd.getFoundDate() + ", "
					+ dd.getInjectedPhase() + ", "
					+ dd.getFixDate() + ", "
					+ dd.getRemovedPhase() + ", "
					+ dd.getDefectType() + ", "
					+ dd.getDescription());
			else if (dd.getDefectType() != null)
				return new String(dd.getFoundDate() + ", "
					+ dd.getInjectedPhase() + ", , , "
					+ dd.getDefectType() + ", "
					+ dd.getDescription());
			else
				return new String(dd.getFoundDate() + ", "
					+ dd.getInjectedPhase() + ", , , , "
					+ dd.getDescription());
		}
		else return new String("");
    }



    /**
     * Count the defects injected and removed.
     *
     *
     * @return true if the defects were counted.
     */
    public boolean count() {
		injectedPlanning = 0;
		injectedHighLevelDesign = 0;
		injectedHighLevelDesignReview = 0;
		injectedDesign = 0;
		injectedDesignReview = 0;
		injectedCode = 0;
		injectedCodeReview = 0;
		injectedCompile = 0;
		injectedTest = 0;
		removedPlanning = 0;
		removedHighLevelDesign = 0;
		removedHighLevelDesignReview = 0;
		removedDesign = 0;
		removedDesignReview = 0;
		removedCode = 0;
		removedCodeReview = 0;
		removedCompile = 0;
		removedTest = 0;
		int siz = defectList.size();
		if (siz < 0)
			return true;
		String injectedPhase;
		String removedPhase;
		for (int i = 0; i < siz; i++) {
			injectedPhase = inspectElement(i).getInjectedPhase();
			removedPhase = inspectElement(i).getRemovedPhase();
			if (injectedPhase.compareTo("Planning") == 0)
				injectedPlanning++;
			if (injectedPhase.compareTo("High Level Design") == 0)
				injectedHighLevelDesign++;
			if (injectedPhase.compareTo("High Level Design Review") == 0)
				injectedHighLevelDesignReview++;
			if (injectedPhase.compareTo("Design") == 0)
				injectedDesign++;
			if (injectedPhase.compareTo("Design Review") == 0)
				injectedDesignReview++;
			if (injectedPhase.compareTo("Code") == 0)
				injectedCode++;
			if (injectedPhase.compareTo("Code Review") == 0)
				injectedCodeReview++;
			if (injectedPhase.compareTo("Compile") == 0)
				injectedCompile++;
			if (injectedPhase.compareTo("Test") == 0)
				injectedTest++;
			if (removedPhase.compareTo("Planning") == 0)
				removedPlanning++;
			if (removedPhase.compareTo("High Level Design") == 0)
				removedHighLevelDesign++;
			if (removedPhase.compareTo("High Level Design Review") == 0)
				removedHighLevelDesignReview++;
			if (removedPhase.compareTo("Design") == 0)
				removedDesign++;
			if (removedPhase.compareTo("Design Review") == 0)
				removedDesignReview++;
			if (removedPhase.compareTo("Code") == 0)
				removedCode++;
			if (removedPhase.compareTo("Code Review") == 0)
				removedCodeReview++;
			if (removedPhase.compareTo("Compile") == 0)
				removedCompile++;
			if (removedPhase.compareTo("Test") == 0)
				removedTest++;
		}
		return true;
    }



    /**
     * Create a string representation of DefectList.
     *
     *
     * @return the string representation of DefectList.
     */
    public String toString() {
		StringBuffer s = new StringBuffer("");
		int i = 0;
		while (i < defectList.size()) {
			s.append(getElementString(i) + "\n");
			i++;
		}
		return new String(s.toString());
    }



    /**
     * Returns the number of defects removed before a specific date.
     *
     * @param date the date.
     * @return the number of defects removed before the date.
     */
    public int defectsRemovedBefore(Datum date) {
		int siz = defectList.size();
		if (siz == 0)
			return 0;
		int num = 0;
		DefectData dd;
		for (int i = 0; i < siz; i++) {
			dd = (DefectData)defectList.elementAt(i);
			if ((dd.getFoundDate() == null)
			&& (dd.getFoundDate().getTime() < date.getTime()))
				num++;
		}
		return num;
    }



    /**
     * Return the number of defects injected before a specific date.
     *
     * @param date the date.
     * @return the number of defects injected before the date.
     */
    public int defectsInjectedBefore(Datum date) {
		int siz = defectList.size();
		if (siz == 0)
			return 0;
		int num = 0;
		DefectData dd;
		for (int i = 0; i < siz; i++) {
			dd = (DefectData)defectList.elementAt(i);
			if ((dd.getFixDate() != null)
			&& (dd.getFixDate().getTime() <  date.getTime()))
				num++;
		}
		return num;
    }
}