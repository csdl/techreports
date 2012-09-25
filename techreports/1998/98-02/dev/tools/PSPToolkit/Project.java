import java.io.*;
import java.util.*;


/**
 *
 * Project.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   The user doesn't need to provide extension for filenames.
 *   Removed Object inheritance.
 *   Merged PSPFile in Project.java.
 *   Fixed defect when adding history items with whitespace characters
 *     in Windows 95/NT.
 *   Added comments to methods.
 *   Replaced DatumException with Exception.
 *   Replaced PSPFileException with Exception.
 *	 Replaced isOpen and isNew with isSavedBefore method.
 *   Removed redundant datecheck from method.
 *
 */
public class Project {
	private boolean savedBefore;			// has project been saved at least once?
    
	private String fileName;				// file name
    private String path;					// path
	private String userName;
    private String projectName;
    private String projectNumber;
    private String language;
    private String level;
    private String date;

	private ProjectData projectData;		// project data
    private TimeLog timeLog;				// timelog
    private DefectLog defectLog;			// defectlog
    private Summary summary;				// summary
	private SummaryList summaryList;
	
	private File file;
    private RandomAccessFile accessFile;


    /**
     * Create a new project.
     *
     * 
     * 
     */
    public Project() {
		savedBefore = false;
		
		fileName = new String("");			// file name
		path = new String(".");				// path
	    projectName = new String("");
	    projectNumber = new String("");
		language = new String("");
		level = new String("");
		date = new String("");

		projectData = new ProjectData();
		timeLog = new TimeLog();
		defectLog = new DefectLog();
		summary = new Summary();

		summary.setTimeList(timeLog.getTimeList());
		summary.setDefectList(defectLog.getDefectList());
		summaryList = summary.getSummaryList();

		setChanged(false);
	}



    /**
     * Set current file name.
     *
     * @param fileName the file name.
     */
	public void setFileName(String f) {
		if (!f.endsWith(".psp")) {
			fileName = new String(f+".psp");
		}
		else {
			fileName = new String(f);
		}
    }



    /**
     * Get current file name.
     *
     * 
     * @return the file name:
     */
    public String getFileName() {
		return fileName;
    }



    /**
     * Set current path.
     *
     * @param p the path.
     */
    public void setPath(String p) {
		path = new String(p);
    }



    /**
     * Get current path.
     *
     * 
     * @return current path.
     */
    public String getPath() {
		return path;
    }



    /**
     * Get the project data.
     *
     * 
     * @return the project data.
     */
    public ProjectData getProjectData() {
	  	return projectData;
    }



    /**
     * Gets the time log.
     *
     *
     * @return the time log.
     */
    public TimeLog getTimeLog() {
		return timeLog;
    }



    /**
     * Gets the defect log.
     *
     *
     * @return the defect log.
     */
    public DefectLog getDefectLog() {
		return defectLog;
    }



    /**
     * Get the summary.
     *
     *
     * @return the summary.
     */
    public Summary getSummary() {
		return summary;
    }



	/**
	 *  Return if the project has been saved at least once before.
	 *
	 */
	public boolean isSavedBefore() {
		return savedBefore;
	}



	/**
	 * Used to force the change flag when closing a project.
	 *
	 */
	public void setChanged(boolean b) {
		projectData.setChanged(b);
		timeLog.setChanged(b);
		defectLog.setChanged(b);
		summary.setChanged(b);
	}



	/**
     * Save the project.
     *
	 *
     * @exception if the project was not saved.
     */
    public void save() throws Exception {
		if ((projectData != null)
		&& (summary != null)
		&& (timeLog != null)
		&& (defectLog != null)) {
			try {
				writeFile("VER\t0\n" + projectData.toString()
			       + "END\n" + "PLN" + summary.planToString()
			       + "END\n" + "ACT" + summary.actualToString()
			       + "END\n" + "TIM\n" + timeLog.toString()
			       + "END\n" + "DEF\n" + defectLog.toString()
			       + "END\n" + "SUM\n" + summaryList.toString()
			       + "END\n");
			}
			catch (Exception e) {
				throw e;
			}
			savedBefore = true;
			summary.update();
			setChanged(false);
		}
    }

	 /**
     * Write the .psp file.
     *
     * @param string the contents of the file.
     * @exception if the project failed to save.
     */
    private void writeFile(String string) throws Exception {
		file = new File(path, fileName);
		File tmpFile = new File(path, fileName + ".tmp");
		try {
			// write tmp file
			accessFile = new RandomAccessFile(tmpFile, "rw");
			accessFile.writeBytes(string);
			accessFile.close();

			if (file.exists() == true) {		// write new file
				file.delete();
			}
			accessFile = new RandomAccessFile(file, "rw");
			accessFile.writeBytes(string);
			accessFile.close();
			tmpFile.delete();					// delete tmp file
		}
		catch(IOException e) {
			throw new Exception("Can't write to file.");
		}
    }


	/**
     * Open a project.
     *
     *
     * @return true if the project was opened.
     * @exception if the file was corrupt or did not exist.
     */
    public void open() throws Exception {
		file = new File(path, fileName);
		if (file.exists() == true) {
			try {
				accessFile = new RandomAccessFile(file, "r");
/* version */	String tmpString = new String(accessFile.readLine());
				if (checkVersion(tmpString).compareTo("0") != 0) {
					accessFile.close();
					throw new Exception("Not a valid version.");
				}
/* project */	tmpString = new String(accessFile.readLine());
				while (tmpString.startsWith("END") == false) {
					if ((tmpString == null)
					|| (setProjectData(tmpString) == false)) {
						throw new Exception("Corrupt file, (0).");
					}
					tmpString = new String(accessFile.readLine());
				}
/* plan */		tmpString = new String(accessFile.readLine());
				if (tmpString.startsWith("PLN") == true) {
					while (tmpString.startsWith("END") == false) {
						if ((tmpString == null)
						|| (setPlan(tmpString) == false)) {
							throw new Exception("Corrupt file, (1).");
						}
						tmpString = new String(accessFile.readLine());
					}
				}
/* actual */	tmpString = new String(accessFile.readLine());
				if (tmpString.startsWith("ACT") == true) {
					while (tmpString.startsWith("END") == false) {
						if ((tmpString == null)
						|| (setActual(tmpString) == false)) {
							throw new Exception("Corrupt file, (2).");
						}
						tmpString = new String(accessFile.readLine());
					}
				}
/* timelog */	tmpString = new String(accessFile.readLine());
				if (tmpString.startsWith("TIM") == true) {
					tmpString = new String(accessFile.readLine());
					while (tmpString.startsWith("END") == false) {
						if ((tmpString == null)
						|| (setTime(tmpString) == false)) {
							throw new Exception("Corrupt file, (3).");
						}
						tmpString = new String(accessFile.readLine());
					}
				}
/* deflog */	tmpString = new String(accessFile.readLine());
				if (tmpString.startsWith("DEF") == true) {
					tmpString = new String(accessFile.readLine());
					while (tmpString.startsWith("END") == false) {
						if ((tmpString == null)
						|| (setDefect(tmpString) == false)) {
							throw new Exception("Corrupt file, (4).");
						}
						tmpString = new String(accessFile.readLine());
					}
				}
/* history */	tmpString = new String(accessFile.readLine());
				if (tmpString.startsWith("SUM") == true) {
					tmpString = new String(accessFile.readLine());
					while (tmpString.startsWith("END") == false) {
						if ((tmpString == null)
						|| (setSummary(tmpString) == false)) {
							throw new Exception("Corrupt file, (5).");
						}
						tmpString = new String(accessFile.readLine());
					}
				}
				try {
					projectData.setData(userName
										, projectName
										, projectNumber
										, language
										, level
										, date);
				}
				catch(Exception e) {
					throw new Exception("Corrupt file, (7).");
				}
				accessFile.close();
			}
			catch(IOException e) {
				throw new Exception("Can not read file, (0).");
			}
			savedBefore = true;
			summary.update();
			setChanged(false);
		}
		else throw new Exception("Could not find file, (1).");
    }


    /**
     * Check for version number.
     *
     * @param string the string to check.
     * @return the string representation of the found version number.
     */
    private String checkVersion(String string) {
		if (string.startsWith("VER") == true) {
			return string.substring(string.indexOf('\t') + 1);
		}
		return "";
    }

    /**
     * Set up project data.
     *
     * @param string the string to read the project data from.
     * @return true if there was valid data.
     */
    private boolean setProjectData(String string) {
		if (string.startsWith("NAME") == true) {
			userName = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		if (string.startsWith("PNAME") == true) {
			projectName = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		if (string.startsWith("PNUM") == true) {
			projectNumber = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		if (string.startsWith("PSP") == true) {
			level = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		if (string.startsWith("LANG") == true) {
			language = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		if (string.startsWith("DATE") == true) {
			date = new String(string.substring(string.indexOf('\t') + 1));
			return true;
		}
		return false;
    }


    /**
     * Set timelog data.
     *
     * @param timeLogString the time log string.
     * @return true if all logs OK.
     */
    public boolean setTime(String timeLogString) {
		StringTokenizer tokenizer = new StringTokenizer(timeLogString,"\t");
		int i = tokenizer.countTokens();
		if ((i < 3) || (i > 4)) {
			return false;
		}
		String start = new String(tokenizer.nextToken());
		String stop = new String(tokenizer.nextToken());
		String phase = new String(tokenizer.nextToken());
		String desc;
		if (i == 4) 
			desc = new String(tokenizer.nextToken());
		else
			desc = new String("");
		TimeData timeData = new TimeData();
		try {
			timeData.setData(start,stop,phase,desc);
			timeLog.add(timeData);
		} catch (Exception e) {
			return false;
		}
		return true;
    }

    /**
     * Set the planned values.
     *
     * @param the plan string.
     * @return true if the plan was set.
     */
    private boolean setPlan(String plan) {
		StringTokenizer st = new StringTokenizer(plan ,"\t");
		if ((st.countTokens() != 45)
		|| (st.nextToken().compareTo("PLN") != 0))
			return false;
		SummaryData summaryData = new SummaryData();
		int i;
		for (i = 0; i < 10; i++)
			summaryData.setSize(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 13; i++)
			summaryData.setTimeInPhase(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 10; i++)
			summaryData.setInjected(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 11; i++)
			summaryData.setRemoved(i, new Integer(st.nextToken()).intValue());
		summary.setPlan(summaryData);
		return true;
    }



    /**
     * Set the actual values.
     *
     * @param the actual string
     * @return true if the values were set.
     */
    private boolean setActual(String actual) {
		StringTokenizer st = new StringTokenizer(actual ,"\t");
		if ((st.countTokens() != 45)
		|| (st.nextToken().compareTo("ACT") != 0))
			return false;
		SummaryData summaryData = new SummaryData();
		int i;
		for (i = 0; i < 10; i++)
			summaryData.setSize(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 13; i++)
			summaryData.setTimeInPhase(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 10; i++)
			summaryData.setInjected(i, new Integer(st.nextToken()).intValue());
		for (i = 0; i < 11; i++)
			summaryData.setRemoved(i, new Integer(st.nextToken()).intValue());
		summary.setActual(summaryData);
		return true;
    }



    /**
     * Set the defects.
     *
     * @param defect the defect string.
     * @return true if the defects were set.
     */
    private boolean setDefect(String defect) {
		StringTokenizer st = new StringTokenizer(defect, ",");
		if (st.countTokens() >= 6) {
			DefectData dd = new DefectData();
			String foundDate = new String(st.nextToken());
			String injectedPhase = new String(st.nextToken());
			String fixDate = new String(st.nextToken());
			String removedPhase = new String(st.nextToken());
			String defectType = new String(st.nextToken());
			StringBuffer descriptionArea = new StringBuffer();
			descriptionArea.append(st.nextToken());
			while (st.hasMoreTokens() == true) {
				descriptionArea.append(",");
				descriptionArea.append(st.nextToken());
			}
			try {
			dd.setData(foundDate.trim()
				 , injectedPhase.trim()
				 , fixDate.trim()
				 , removedPhase.trim()
				 , defectType.trim()
				 , descriptionArea.toString().trim());
			} catch (Exception e) { return false; }
			return defectLog.add(dd);
		}
		return false;
    }



    /**
     * Set the summary path.
     *
     * @param path the path.
     * @return true if the path was set.
     */
    private boolean setSummary(String path) {
		StringTokenizer st = new StringTokenizer(path,"\t");
		if (st.countTokens() != 2)
			return false;
		String p = new String(st.nextToken());
		String f = new String(st.nextToken());
		summaryList.addPathFile(p, f);
		summaryList.setChanged(false);
		return true;
    }




    /**
     * Check if any values have been changed.
     *
     *
     * @return true if changes have been made.
     */
    public boolean hasChanged() {
		return (projectData.hasChanged()
			|| timeLog.hasChanged()
			|| defectLog.hasChanged()
			|| summary.hasChanged());
    }




    /**
     * Create a string that represents this object.
     *
     * 
     * @return a string representation of this object.
     */
    public String toString() {
		return new String("File: " + fileName + "\n"
				+ "Directory: " + path + "\n"
				+ "Data: " + projectData + "\n"
				+ "Time: " + timeLog + "\n"
				+ "Summary: " + summary);
    }
}
