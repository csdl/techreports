import java.util.*;


/**
 * 
 * TimeData.java
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Exchanged DatumException for Exception.
 *   Removed Object inheritance.
 *
 */
public class TimeData {
    private Datum startDate;				// start date
    private Datum stopDate;				// stop date
    private String phase;				// phase
	private String desc;



    /**
     * Creates a new time data.
     *
     *
     *
     */
    public TimeData() {
    }



    /**
     * Sets time data.
     *
     * @param start the start date.
     * @param stop the stop date.
     * @param phase the phase.
     * @return true if data was set.
     * @exception if any date was illegal.
     */
    public boolean setData(String start,String stop,String phase,String desc) throws Exception {
		Datum tmpStartDate = null;
		Datum tmpStopDate = null;
		try {
			tmpStartDate = new Datum(start);
			if (stop.compareTo("") != 0) {
				tmpStopDate = new Datum(stop);
				if (tmpStopDate.getTime() < tmpStartDate.getTime()) {
					throw new Exception("Start time must be before Stop time");
				}
			}
//			if (checkPhase(phase) == false) {
//			  return false;
//			}
		}
		catch (Exception e) {
			throw e;
		}
		startDate = tmpStartDate;
		stopDate = tmpStopDate;
		this.phase = new String(phase);
		this.desc = makeDesc(desc);
//		this.desc = desc;
		return true;
    }



    /**
     * Check if it is a valid phase.
     *
     * @param phase the phase to check.
     * @return true if phase was OK.
     */
//    private boolean checkPhase(String phase) {
//		if ((phase.compareTo("Planning") == 0)
//		|| (phase.compareTo("High Level Design") == 0)
//		|| (phase.compareTo("High Level Design Review") == 0)
//		|| (phase.compareTo("Design") == 0)
//		|| (phase.compareTo("Design Review") == 0)
//		|| (phase.compareTo("Code") == 0)
//		|| (phase.compareTo("Code Review") == 0)
//		|| (phase.compareTo("Compile") == 0)
//		|| (phase.compareTo("Test") == 0)
//		|| (phase.compareTo("Post Mortem") == 0))
//			return true;
//		else return false;
  //  }


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
     * Create a string representation of TimeData.
     *
     *
     * @return the string representation of TimeData.
     */
    public String toString() {
		return new String(startDate + "\t"
				+ stopDate + "\t"
				+ phase + "\t"
				+ makeLine(desc));
    }



    /**
     * Get the start date.
     *
     *
     * @return the start date.
     */
    public Datum getStartDate() {
		return startDate;
    }



    /**
     * Gets the stop date.
     *
     *
     * @return the stop date.
     */
    public Datum getStopDate() {
		return stopDate;
    }



    /**
     * Gets the phase.
     *
     *
     * @return the phase.
     */
    public String getPhase() {
		return phase;
    }

	public String getDesc() {
		return desc;
	}


    /**
     * Check if the TimeDatas are overlapping.
     *
     * @param timeData the TimeData to compare to.
     * @return true if the dates were overlapping.
     */
//    public boolean checkOverlap(TimeData timeData) {
//		Datum foreignStartDate = timeData.getStartDate();
//		Datum foreignStopDate = timeData.getStopDate();
//		int i = 0;
//		if (startDate != null)
//			i = i + 1;
//		if (stopDate != null)
//			i = i + 2;
//		if (foreignStartDate != null)
//			i = i + 4;
//		if (foreignStopDate != null)
//			i = i + 8;
//		if (i == 7)
//			return inTimeFrame(startDate, foreignStartDate, stopDate);
//		if (i == 11)
//			return inTimeFrame(startDate, foreignStopDate, stopDate);
//		if (i == 13)
//			return inTimeFrame(foreignStartDate, startDate, foreignStopDate);
//		if (i == 14)
//			return inTimeFrame(foreignStartDate, stopDate, foreignStopDate);
//		if (i == 15) {
//			if (((startDate.getTime() <= foreignStartDate.getTime())
//			&& (stopDate.getTime() <= foreignStartDate.getTime()))
//			|| ((startDate.getTime() >= foreignStopDate.getTime())
//			&& (stopDate.getTime() >= foreignStopDate.getTime())))
//				return false;
//			else
//				return true;
//		}
//		return true;
//    }



    /**
     * Check if date is in a time frame.
     *
     * @param date_1 the first date.
     * @param date_2 the second date.
     * @param date_3 the third date.
     * @return true if date_1 < date_2 < date_3.
     */
    private boolean inTimeFrame(Datum date_1, Datum date_2, Datum date_3) {
		if ((date_1.getTime() < date_2.getTime())
		&& (date_2.getTime() < date_3.getTime()))
			return true;
		else return false;
    }



    /**
     * Check if a date this is before an other TimeData
     *
     * @param timeData the other TimeData.
     * @return true if this <= timeData.
     */
    public boolean before(TimeData timeData) {
		if ((startDate == null)
		|| (stopDate == null)
		|| (timeData.getStartDate() == null)
		|| (timeData.getStopDate() == null)
		|| (stopDate.getTime() > timeData.getStartDate().getTime()))
			return false;
		else return true;
    }



    /**
     * Check if a date this is after an other TimeData
     *
     * @param timeData the other TimeData.
     * @return true if timeData <= this.
     */
    public boolean after(TimeData timeData) {
		if ((startDate == null)
		|| (stopDate == null)
		|| (timeData.getStartDate() == null)
		|| (timeData.getStopDate() == null)
		|| (startDate.getTime() < timeData.getStopDate().getTime()))
			return false;
		else return true;
    }
}
