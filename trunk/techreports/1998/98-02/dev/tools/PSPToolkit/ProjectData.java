
/**
 *
 * ProjectData.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Exchanged DatumException for Exception.
 *   Removed Object inheritance.
 *	 PSP 1.2 changed to PSP 1.1 (generated corrupted files).
 *
 */
public class ProjectData {
    private boolean hasChanged;				// has changed
    private String level;					// PSP level
    private String userName;				// user name
    private String projectName;				// project name
    private String projectNumber;			// project number
    private String language;				// language
	private Datum datum;					// date



    /**
     * Create a new project
     *
     *
     *
     */
    public ProjectData() {
		level = new String("PSP 0");		// PSP level
		userName = new String("");			// user name
		projectName = new String("");		// project name
		projectNumber = new String("");		// project number
		language = new String("");			// language
		datum = new Datum();				// date
		hasChanged = false;					// has changed
    }



    /**
     * Test if the PSP level is valid.
     *
     * @param level the level to test.
     * @return true if it was a valid level.
     */
    private boolean testLevel(String level) {
		return ((level.compareTo("PSP 0") == 0)
		|| (level.compareTo("PSP 0.1") == 0)
		|| (level.compareTo("PSP 1") == 0)
		|| (level.compareTo("PSP 1.1") == 0)
		|| (level.compareTo("PSP 2") == 0)
		|| (level.compareTo("PSP 2.1") == 0)
		|| (level.compareTo("PSP 3") == 0));
    }



    /**
     * Set the current PSP level.
     *
     * @param level the new PSP level.
     * @return true if level was set.
     */
    public boolean setLevel (String level) {
		if (testLevel(level)) {
			level = new String(level);
			return true;
		}
		else
			return false;
    }



    /**
     * Get the current PSP level.
     *
     * 
     * @return the current PSP level.
     */
    public String getLevel() {
		return level;
    }



    /**
     * Sets the users name.
     *
     * @param name the name of the user.
     * @return true if name was set.
     */
    public void setUserName(String name) {
		userName = new String(name);
    }



    /**
     * Get the users name.
     *
     * 
     * @return the current user name.
     */
    public String getUserName() {
		return userName;
    }



    /**
     * Set the project name.
     *
     * @param projectName the project name.
     * @return true if name was set.
     */
    public void setProjectName(String projectName) {
		this.projectName = projectName;
    }



    /**
     * Get the project name.
     *
     * 
     * @return the project name.
     */
    public String getProjectName() {
		return projectName;
    }



    /**
     * Set the project number.
     *
     * @param projectNumber the project nnumber.
     * @return true if number was set.
     */
    public void setProjectNumber(String number) {
		projectNumber = new String(number);
    }



    /**
     * Get the project number.
     *
     * 
     * @return the project number.
     */
    public String getProjectNumber() {
  		return projectNumber;
    }



    /**
     * Set the language.
     *
     * @param laguage the language.
     * @return true if language was set.
     */
    public void setLanguage(String language) {
		this.language = new String(language);
    }



    /**
     * Get the language.
     *
     * 
     * @return the language.
     */
    public String getLanguage() {
  		return language;
    }



    /**
     * Set the date.
     *
     * @param date the date.
     * @exception if date was illegal.
     */
    public void setDate(String date) throws Exception {
		Datum tmpDatum;
		try {
			tmpDatum = new Datum(date);
		}
		catch(Exception e) {
			throw e;
		}		
		datum = tmpDatum;
    }



    /**
     * Get the date.
     *
     * 
     * @return the date.
     */
    public String getDate() {
		return datum.dateToString();
    }



    /**
     * Compare if dates are the same.
     *
     * @param date the date to compare to this.
     * @return true if they are the same
     * @exception if illegal date.
     */
    private boolean compareDate(String date) throws Exception {
		Datum tmpDatum;
		try {
			tmpDatum = new Datum(date);
		}
		catch(Exception e) {
			throw e;
		}
		if (datum.dateToString().compareTo(tmpDatum.dateToString()) != 0) {
			return false;
		}
		else {
			return true;
		}
    }



    /**
     * Set project data
     *
     * @param userName the user name
     * @param projectName the project name
     * @param projectNumber the project number
     * @param language the used language
     * @param level the PSP level
     * @param date the date
	 * @return true if data was set
     * @exception if date was illegal.
     */
    public void setData(String userName
			 , String projectName
			 , String projectNumber
			 , String language
			 , String level
			 , String date)
		   throws Exception {
		try {
			if (!(this.userName.compareTo(userName) == 0)) {
				this.userName = new String(userName);
			}
			if (!(this.projectName.compareTo(projectName) == 0)) {
				this.projectName = new String(projectName);
			}
			if (!(this.projectNumber.compareTo(projectNumber) == 0)) {
				this.projectNumber = new String(projectNumber);
			}
			if (!(this.language.compareTo(language) == 0)) {
				this.language = new String(language);
			}

			if (!(this.level.compareTo(level) == 0)) {
				this.level = new String(level);
			}
			if (!compareDate(date)) {
				setDate(date);
			}
		}
		catch(Exception e) {
			throw e;
		}
    }



    /**
     * Set the changed flag.
     *
     * @param value the boolean value.
     */
    public void setChanged(boolean value) {
		hasChanged = value;
    }



    /**
     * Checks if any values have been changed.
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
		return new String("NAME\t" + userName + "\n"
				+ "PNAME\t" + projectName + "\n"
				+ "PNUM\t" + projectNumber + "\n"
				+ "PSP\t" + level + "\n"
				+ "LANG\t" + language + "\n"
				+ "DATE\t" + datum.dateToString() + "\n");
    }
}
