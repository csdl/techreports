import java.util.*;


/**
 *
 * Datum.java
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Changed DatumExcpetion to Exception.
 *
 */
public class Datum {
    private Date date;					// date
    private int year = 1998;				// year
    private int month = 3;				// month
    private int day = 10;				// day
    private int hour = 10;				// hour
    private int minute = 50;				// minute
    private int second = 20;				// second
    private boolean timeSet = false;
    private boolean dateSet = false;



    /**
     * Creates new datum
     *
     *
     *
     */
    public Datum() {
		date = new Date();
		year = (1900 + date.getYear());			// year
		month = (1 + date.getMonth());			// month
		day = date.getDate();				// day
		hour = date.getHours();				// hour
		minute = date.getMinutes();			// minute
		second = date.getSeconds();			// second
    }



    /**
     * Creates new datum
     *
     * @param date date on the form "year/month/day hour:minute:second".
     *
     * @exception if illegal date.
     */
    public Datum(String date) throws Exception {
		this.date = new Date();
		year = (1900 + this.date.getYear());		// year
		month = (1 + this.date.getMonth());		// month
		day = this.date.getDate();			// day
		hour = this.date.getHours();			// hour
		minute = this.date.getMinutes();		// minute
		second = this.date.getSeconds();		// second
		try {
			splitString(date);				// split string
			this.date = new Date((year - 1900)
					, (month - 1)
					, day
					, hour
					, minute
					, second);		// create new date
		}
		catch(NumberFormatException e) {
			throw new Exception("Not an integer!");
		}
		catch(IllegalArgumentException e) {
			throw new Exception("Date out of range!");
		}
	}



    /**
     * Splits a Datum string into time and date
     *
     * @param string the string representing a date and time.
     * @return true if the string was successfully split.
     * @exception DatumException if the string had to many arguments.
     */
    private boolean splitString(String string) throws Exception {
		String firstPart;
		String secondPart;
		StringTokenizer st;

		st = new StringTokenizer(string, " ");
		if (st.countTokens() == 2) {
			firstPart = new String(st.nextToken());
			secondPart = new String(st.nextToken());
			splitDateTime(firstPart);
			splitDateTime(secondPart);
		}
		else if (st.countTokens() == 1) {
			firstPart = new String(st.nextToken());
			splitDate(firstPart);
		}
		else
			throw new Exception("Too many arguments");
		return true;
	}



    /**
     * Checks if it is a time or a date string.
     *
     * @param dateTimeString a string representing time or date.
     * @return true if the string was valid.
     * @exception DatumException if the string was not valid.
     */
    private boolean splitDateTime(String dateTimeString) throws Exception {
		if (dateTimeString.indexOf('/') != -1)
			splitDate(dateTimeString);
		else if (dateTimeString.indexOf(':') != -1)
			splitTime(dateTimeString);
		else
			throw new Exception("Not a valid date/time string");
		return true;
    }



    /**
     * Splits a date string.
     *
     * @param dateString the date string.
     * @return true if string was split.
     * @exception DatumException if the string had to many or to few arguments.
     */
    private boolean splitDate(String dateString) throws Exception {
		if (dateSet)
			throw new Exception("Double date " + dateString);
		StringTokenizer st = new StringTokenizer(dateString, "/");
		if (st.countTokens() == 3) {
			year = Integer.valueOf(st.nextToken()).intValue();
			month = Integer.valueOf(st.nextToken()).intValue();
			day = Integer.valueOf(st.nextToken()).intValue();
			if (year < 1900)
				year = year + 1900;
		}
		else
			throw new Exception("Too few or too many argument " + dateString);
		if ((day < 1)
		|| (day > 31)
		|| (month <1)
		|| (month > 12)) {
			throw new Exception("Not a valid date " + dateString);
}
		date = new Date((year - 1900)
			      , (month - 1)
				  , day
				  , hour
				  , minute
			      , second);
		dateSet = true;
		return true;
    }



    /**
     * Split a time string.
     *
     * @param timeString the time string.
     * @return true if string was split.
     * @exception DatumException if the string had too many or too few arguments.
     */
    private boolean splitTime(String timeString) throws Exception {
		if (timeSet)
			throw new Exception("Double time " + timeString);
		StringTokenizer st = new StringTokenizer(timeString, ":");
		if (st.countTokens() == 3) {
			hour = Integer.valueOf(st.nextToken()).intValue();
			minute = Integer.valueOf(st.nextToken()).intValue();
			second = Integer.valueOf(st.nextToken()).intValue();
		}
		else if (st.countTokens() == 2) {
			hour = Integer.valueOf(st.nextToken()).intValue();
			minute = Integer.valueOf(st.nextToken()).intValue();
		}
		else
			throw new Exception("Wrong number of arguments" + timeString);
		if ((hour < 0)
		|| (hour > 23)
		|| (minute < 0)
		|| (minute > 59)
		|| (second < 0)
		|| (second > 59)) {
			throw new Exception("Not a valid time " + timeString);
		}
		hour = hour % 24;
		minute = minute % 60;
		second = second % 60;
		date = new Date((year - 1900)
			      , (month - 1)
				  , day
			      , hour
				  , minute
				  , second);
		timeSet = true;
		return true;
    }



    /**
     * Return the number of milliseconds since 1970/01/01 00:00:00 GMT.
     *
     *
     * @return the number of milliseconds since 1970/01/01 00:00:00 GMT.
     */
    public long getTime() {
		return date.getTime();
    }



    /**
     * Calculate the difference (this - datum) i minutes.
     *
     * @param datum the time to subtract from this.
     * @return the difference in minutes.
     */
    public long timeDifference(Datum datum) {
		return (date.getTime() - datum.getTime()) / 60000;
    }



    /**
     * Create a string representation of this date.
     *
     *
     * @return the string representation of this date.
     */
    public String dateToString() {
		return new String(year + "/"
				+ month + "/"
				+ day);
    }



    /**
     * Create a string representation of this time.
     *
     *
     * @return the string representation of this time.
     */
    public String timeToString() {
		String start = "";
		String hmSep = ":";
		String msSep = ":";
		if (hour < 10)
			start = new String("0");
		if (minute < 10)
			hmSep = new String(":0");
		if (second < 10)
			msSep = new String(":0");
		return new String(start
				+ hour + hmSep
				+ minute + msSep
				+ second);
	}



    /**
     * Creates a string representation of Datum.
     *
     *
     * @return the string representation of Datum.
     */
    public String toString() {
		return new String(dateToString() + " "
				+ timeToString());
    }
}
