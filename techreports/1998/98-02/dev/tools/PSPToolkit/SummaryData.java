


/**
 * 
 * SummaryData.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed Object inheritance.
 *
 */
public class SummaryData {
    private int[] summary = new int[12];
    private int[] size = new int[10];
    private int[] phase = new int[13];
    private int[] injected = new int[10];
    private int[] removed = new int[11];
    private float[] efficiency = new float[7];



    /**
     * Create a new SummaryData.
     *
     *
     *
     */
    public SummaryData() {
		int i;
		for (i = 0; i < 12; i++) {
			summary[i] = 0;
		}
		for (i = 0; i < 10; i++) {
			size[i] = 0;
		}
		for (i = 0; i < 13; i++) {
			phase[i] = 0;
		}
		for	(i = 0; i < 10; i++) {
			injected[i] = 0;
		}
		for (i = 0; i < 11; i++) {
			removed[i] = 0;
		}
		for (i = 0; i < 7; i++) {
			efficiency[i] = 0;
		}
    }



    /**
     * Get Summary value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public int getSummary(int i) {
		if ((i < 0)
		|| (i > 11))
			return -1;
		else return summary[i];
    }



    /**
     * Set Summary value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setSummary(int i, int value) {
		if ((i < 0)
		|| (i > 11))
			return false;
		else {
			summary[i] = value;
			return true;
		}
    }



    /**
     * Get Size value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public int getSize(int i) {
		if ((i < 0)
		|| (i > 9))
			return -1;
		else return size[i];
    }



    /**
     * Set Size value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setSize(int i, int value) {
		if ((i < 0)
		|| (i > 9))
			return false;
		else { 
			size[i] = value;
			return true;
		}
    }



    /**
     * Get Time in Phase value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public int getTimeInPhase(int i) {
		if ((i < 0)
		|| (i > 12))
			return -1;
		else return phase[i];
    }



    /**
     * Set Time in Phase value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setTimeInPhase(int i, int value) {
		if ((i < 0)
		|| (i > 12))
			return false;
		else {
			phase[i] = value;
			return true;
		}
    }



    /**
     * Get Defects Injected value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public int getInjected(int i) {
		if ((i < 0)
		|| (i > 9))
			return -1;
		else return injected[i];
    }



    /**
     * Set Defects Injected value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setInjected(int i, int value) {
		if ((i < 0)
		|| (i > 9))
			return false;
		else {
			injected[i] = value;
			return true;
		}
    }



    /**
     * Get Defects Removed value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public int getRemoved(int i) {
		if ((i < 0)
		|| (i > 10))
			return -1;
		else return removed[i];
    }



    /**
     * Set Defects Removed value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setRemoved(int i, int value) {
		if ((i < 0)
		|| (i > 10))
			return false;
		else {
			removed[i] = value;
			return true;
		}
    }



    /**
     * Get Defects Removal Efficiency Value value.
     *
     * @param i index of the value.
     * @return the value, if error -1 is returned.
     */
    public float getEfficiency(int i) {
		if ((i < 0)
		|| (i > 6))
			return -1;
		else return efficiency[i];
    }



    /**
     * Set Defects Removal Efficiency value.
     *
     * @param i index of the value.
     * @param value the value.
     * @return true if the value was set.
     */
    public boolean setEfficiency(int i, float value) {
		if ((i < 0)
		|| (i > 6))
			return false;
		else {
			efficiency[i] = value;
			return true;
		}
    }




    /**
     * Create a string representation of SummaryData.
     *
     *
     * @return the string representation of SummaryData.
     */
    public String toString() {
		StringBuffer sb = new StringBuffer();
		int i;
		for (i = 0; i < 10; i++) {
			sb.append("\t");
			sb.append(size[i]);
		}
		for (i = 0; i < 13; i++) {
			sb.append("\t");
			sb.append(phase[i]);
		}
		for (i = 0; i < 10; i++) {
			sb.append("\t");
			sb.append(injected[i]);
		}
		for (i = 0; i < 11; i++) {
			sb.append("\t");
			sb.append(removed[i]);
		}
		return new String(sb.toString());
    }



    /**
     * Update the values that can be calculated.
     *
     *
     *
     */
    public void update() {
		phase[10] = phase[0] + phase[1] + phase[2] + phase[3] + phase[4]
				+ phase[5] + phase[6] + phase[7] + phase[8] + phase[9];
		injected[9] = injected[0] + injected[1] + injected[2]
			    + injected[3] + injected[4] + injected[5]
				+ injected[6] + injected[7] + injected[8];
		removed[9] = removed[0]
				+ removed[1]
				+ removed[2]
				+ removed[3]
				+ removed[4]
				+ removed[5]
				+ removed[6]
				+ removed[7]
				+ removed[8];
		if (phase[10] == 0)
			summary[0] = 0;
		else
			summary[0] = (size[5] * 60) / phase[10];
		summary[1] = phase[10];
		summary[2] = phase[10];
		if (summary[2] == 0)
			summary[3] = 100;
		else
			summary[3] = (summary[1] * 100) / summary[2];
		if (size[6] == 0)
			summary[4] = 100;
		else
			summary[4] = (size[4] * 100) / size[6];
		if (size[5] == 0) {
			summary[5] = 0;
			summary[6] = 0;
			summary[7] = 0;
		}
		else {
			summary[5] = (size[7] * 100) / size[5];
			summary[6] = (removed[8] * 1000) / size[5];
			summary[7] = (removed[9] * 1000) / size[5];
		}
		if (removed[9] == 0)
			summary[8] = 0;
		else
			summary[8] = ((removed[0]
					+ removed[1]
					+ removed[2]
					+ removed[3]
					+ removed[4]
					+ removed[5]
					+ removed[6])
					* 100) / removed[9];
		if (phase[10] == 0) {
			summary[9] = 100;
			summary[10] = 100;
		}
		else {
			summary[9] = ((phase[4] + phase[6]) * 100) / phase[10];
			summary[10] = ((phase[7] + phase[8]) * 100) / phase[10];
		}
		if (summary[10] == 0)
			summary[11] = 100;
		else
			summary[11] = (summary[9] * 100) / summary[10];
		if (phase[4] == 0)
			efficiency[0] = 0;
		else
			efficiency[0] = new Float(removed[4] * 60).floatValue()
				  / new Float(phase[4]).floatValue();
		if (phase[6] == 0)
			efficiency[1] = 0;
		else
			efficiency[1] = new Float(removed[6] * 60).floatValue()
				  / new Float(phase[6]).floatValue();
		if (phase[7] == 0)
			efficiency[2] = 0;
		else
			efficiency[2] = new Float(removed[7] * 60).floatValue()
				  / new Float(phase[7]).floatValue();
		if (phase[8] == 0)
			efficiency[3] = 0;
		else
			efficiency[3] = new Float(removed[8] * 60).floatValue()
				  / new Float(phase[8]).floatValue();
		if (efficiency[3] == 0) {
			efficiency[4] = 0;
			efficiency[5] = 0;
			efficiency[6] = 0;
		}
		else {
			efficiency[4] = efficiency[0] / efficiency[3];
			efficiency[5] = efficiency[1] / efficiency[3];
			efficiency[6] = efficiency[2] / efficiency[3];
		}
    }
}
