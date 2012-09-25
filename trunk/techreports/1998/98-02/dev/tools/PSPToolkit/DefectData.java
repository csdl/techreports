import java.util.*;


/**
 * 
 * DefectData.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Made class compatible with updates/bug-fixes in DLScreen.java.
 *   Exchanged DatumException for Exception.
 *   Exchanged DefectDataException for Exception.
 *   Removed Object inheritance.
 *
 */
public class DefectData {
    private Datum foundDate;
    private Datum fixDate;
    private String removedPhase;
    private String injectedPhase;
    private String defectType;
    private String description;



    /**
     * Create a new DefectData.
     *
     *
     *
     */
    public DefectData() {
		foundDate = null;
		fixDate = null;
		removedPhase = new String("");
		injectedPhase = new String("");
		defectType = new String("");
		description = new String("");
    }



    /**
     * Return the date when this defect was found.
     *
     *
     * @return the date when this defect was found.
     */
    public Datum getFoundDate() {
		return foundDate;
    }



    /**
     * Return the date when this defect was fixed.
     *
     *
     * @return the date when this defect was fixed.
     */
    public Datum getFixDate() {
		return fixDate;
    }



    /**
     * Return the phase when this defect was removed.
     *
     *
     * @return the phase when this defect was removed.
     */
    public String getRemovedPhase() {
		return new String(removedPhase);
    }



    /**
     * Return the phase when this defect was injected.
     *
     *
     * @return the phase when this defect was injected.
     */
    public String getInjectedPhase() {
		return new String(injectedPhase);
    }



    /**
     * Return the type of this defect.
     *
     *
     * @return the type of this defect.
     */
    public String getDefectType() {
		return new String(defectType);
    }



    /**
     * Return the description of this defect.
     *
     *
     * @return the description of this defect.
     */
    public String getDescription() {
		return makeLine(description);
    }


    /**
     * Return the description of this defect.
     *
     *
     * @return the description of this defect.
     */
	public String getDesc() {
		return description;
	}


    /**
     * Set the defect data.
     *
     * @param foundDate the date when the defect was found.
     * @param removedPhase the phase where the defect was removed.
     * @param fixDate the date when the defect was fixed.
     * @param injectedPhase the phase where the defect was injected.
     * @param defectType the defect type.
     * @param description description of the defect.
     * @return true if the data was set.
     * @exception Exception is the data not was set.
     */
    public boolean setData(String foundDate
				 , String injectedPhase
				 , String fixDate
				 , String removedPhase
				 , String defectType
				 , String description)
		   throws Exception {
		int i = 0;
		if (foundDate.compareTo("") != 0)
			i = i + 1;
		if (injectedPhase.compareTo("") != 0)
			i = i + 2;
		if (fixDate.compareTo("") != 0)
			i = i + 4;
		if (removedPhase.compareTo("") != 0)
		    i = i + 8;
		if (defectType.compareTo("") != 0)
		    i = i + 16;
		if ((i == 3)
		|| (i == 19)
		|| (i == 31)) {
//			if (checkRemovedPhase(removedPhase) == false)
//				throw new Exception("No valid phase selected");
			Datum tmpDate1 = null;
			Datum tmpDate2 = null;
			try {
				tmpDate1 = new Datum(foundDate);
			}
			catch (Exception e) {
				throw new Exception("Illegal date format: " + foundDate);
			}
			if (i == 31) {
				try {
					tmpDate2 = new Datum(fixDate);
				}
				catch (Exception e) {
					throw new Exception("Illegal date format: " + fixDate);
				}
//				if ((checkInjectedPhase(injectedPhase) == false)
//				|| (checkDefectType(defectType) == false))
//					throw new Exception("Wrong defect type!");
			}
			if (tmpDate2 != null) {
				if (tmpDate1.getTime() > tmpDate2.getTime())
					throw new Exception("You can't fix a defect before it's found!");
			}
			this.foundDate = tmpDate1;
			this.fixDate = tmpDate2;
			this.removedPhase = new String(removedPhase);
			this.injectedPhase = new String(injectedPhase);
			this.defectType = new String(defectType);
			this.description = makeDesc(description);
			return true;
		}
		if (i == 0)
			throw new Exception("No data supplied!");
		if ((i & 1) != 1)
			throw new Exception("No found date supplied!");
		if ((i & 2) != 2)
			throw new Exception("No injection phase supplied!");
		if ((i & 4) != 4)
			throw new Exception("No fix date supplied!");
		if ((i & 8) != 8)
			throw new Exception("No removal phase supplied!");
		if ((i & 16) != 16)
			throw new Exception("No defect type supplied!");
		throw new Exception("Unknown error!");
    }



    /**
     * Checks if the removal phase is valid.
     *
     * @param removedPhase the phase to check.
     * @return true if it was a valid phase.
     */
//    public boolean checkRemovedPhase(String removedPhase) {
//		if (removedPhase.compareTo("") == 0)
//			return false;
//		else return checkInjectedPhase(removedPhase);
  //  }



    /**
     * Checks if the injection phase is valid.
     *
     * @param injectedPhase the phase to check.
     * @return true if it was a valid phase.
     */
//    public boolean checkInjectedPhase(String injectedPhase) {
//	if ((injectedPhase.compareTo("") == 0)
//	 || (injectedPhase.compareTo("Planning") == 0)
//	 || (injectedPhase.compareTo("High Level Design") == 0)
//	 || (injectedPhase.compareTo("High Level Design Review") == 0)
//	 || (injectedPhase.compareTo("Design") == 0)
//	 || (injectedPhase.compareTo("Design Review") == 0)
//	 || (injectedPhase.compareTo("Code") == 0)
//	 || (injectedPhase.compareTo("Code Review") == 0)
//	 || (injectedPhase.compareTo("Compile") == 0)
//	 || (injectedPhase.compareTo("Test") == 0))
//	    return true;
//	return false;
  //  }



    /**
     * Checks i the defect type is valid.
     *
     * @param defectType the defect type to check.
     * @return true if it was a valid type.
     */
//    public boolean checkDefectType(String defectType) {
//	if ((defectType.compareTo("") == 0)
//	 || (defectType.compareTo("Documentation") == 0)
//	 || (defectType.compareTo("Syntax") == 0)
//	 || (defectType.compareTo("Build/Package") == 0)
//	 || (defectType.compareTo("Assignment") == 0)
//	 || (defectType.compareTo("Interface") == 0)
//	 || (defectType.compareTo("Checking") == 0)
//	 || (defectType.compareTo("Data") == 0)
//	 || (defectType.compareTo("Function") == 0)
//	 || (defectType.compareTo("System") == 0)
//	 || (defectType.compareTo("Environment") == 0))
//	    return true;
//	return false;
//    }



    /**
     * Checks if this DefectData is before an other DefectData.
     *
     * @param defectData the other DefectData.
     * @return true if this is before the other.
     */
    public boolean before(DefectData defectData) {
		if ((this.foundDate == null)
		|| (defectData.getFoundDate() == null)
		|| (this.foundDate.getTime() > defectData.getFoundDate().getTime()))
			return false;
		return true;
    }


	/**
	 * Convert a string to one line.
	 *
	 *
	 */
	private String makeLine(String t) {
		if (t != null) {
			StringTokenizer st =  new StringTokenizer(t,"\n");
			String tmp;
			if (st.countTokens() > 1) {
				tmp = new String(st.nextToken());
				while (st.hasMoreTokens()) {
					tmp += "~~" + st.nextToken();
				}
			}
			else
				tmp = new String(t);
			return tmp;
		}
		else return null;
	}
	

	/**
	 * Convert a string back to multiple lines (see above).
	 *
	 *
	 */
	private String makeDesc(String t) {
		if (t != null) {
			StringTokenizer st =  new StringTokenizer(t,"~~");
			String tmp;
			if (st.countTokens() > 1) {
				tmp = new String(st.nextToken());
				while (st.hasMoreTokens()) {
					tmp += "\n" + st.nextToken();
				}
			}
			else
				tmp = new String(t);
			return tmp;
		}
		else return null;
	}



    /**
     * Create a string that represents this object.
     *
     *
     * @return a string representation of this object.
     */
    public String toString() {
	return new String("FOD#" + foundDate + "\t"
			+ "REP#" + removedPhase + "\t"
			+ "FID#" + fixDate + "\t"
			+ "INP#" + injectedPhase + "\t"
			+ "DET#" + defectType + "\t"
			+ "DES#" + makeLine(description));
    }
}
