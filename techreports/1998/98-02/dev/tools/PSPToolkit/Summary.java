


/**
 *
 * Summary.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed Object inheritance.
 *
 */
public class Summary {
    private boolean hasChanged;				// has changed
    private SummaryData plan;
    private SummaryData actual;
    private SummaryData toDate;
    private SummaryData toDateProc;
    private SummaryList summaryList;
    private TimeList timeList;
    private DefectList defectList;



    /**
     * Create a new Summary.
     *
     *
     *
     */
    public Summary() {
		hasChanged = false;
		summaryList = new SummaryList();
		plan = new SummaryData();
		actual = new SummaryData();
		toDate = new SummaryData();
		toDateProc = new SummaryData();
    }



    /**
     * Set the TimeList association.
     *
     * @param timeList the time list.
     */
    public void setTimeList(TimeList timeList) {
		this.timeList = timeList;
    }



    /**
     * Set the DefectList association.
     *
     * @param defectList the time list.
     */
    public void setDefectList(DefectList defectList) {
		this.defectList = defectList;
    }



    /**
     * Get the plan summary.
     *
     *
     * @return the plan summary.
     */
    public SummaryData getPlan() {
		return plan;
    }



    /**
     * Get the actual summary.
     *
     *
     * @return the actual summary.
     */
    public SummaryData getActual() {
		return actual;
    }



    /**
     * Get the to date summary.
     *
     *
     * @return the to date summary.
     */
    public SummaryData getToDate() {
		return toDate;
    }



    /**
     * Get the to date % summary.
     *
     *
     * @return the to date % summary.
     */
    public SummaryData getToDateProc() {
		return toDateProc;
    }



    /**
     * Get the summary list.
     *
     *
     * @return the summary list.
     */
    public SummaryList getSummaryList() {
		return summaryList;
    }



    /**
     * Check if any values have been changed.
     *
     *
     * @return true if changes have been made.
     */
    public boolean hasChanged() {
		if (summaryList.hasChanged() == true)
			hasChanged = true;
		return hasChanged;
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
     * Set the planned values.
     *
     * @param plan the new plan.
     */
    public void setPlan(SummaryData plan) {
		boolean c = false;
		int i = 0;
		for (i = 0; i < 10; i++)
			if (this.plan.getSize(i) != plan.getSize(i))
				c = true;
		for (i = 0; i < 13; i++)
			if (this.plan.getTimeInPhase(i) != plan.getTimeInPhase(i))
				c = true;
		for (i = 0; i < 10; i++)
			if (this.plan.getInjected(i) != plan.getInjected(i))
				c = true;
		for (i = 0; i < 11; i++)
			if (this.plan.getRemoved(i) != plan.getRemoved(i))
				c = true;
		if (c) {
			hasChanged = true;
			this.plan = plan;
		}
    }



    /**
     * Set the actual values.
     *
     * @param actual the new actual values.
     */
    public void setActual(SummaryData actual) {
		boolean c = false;
		int i = 0;
		for (i = 0; i < 10; i++)
			if (this.actual.getSize(i) != actual.getSize(i))
				c = true;
		for (i = 0; i < 13; i++)
			if (this.actual.getTimeInPhase(i) != actual.getTimeInPhase(i))
				c = true;
		for (i = 0; i < 10; i++)
			if (this.actual.getInjected(i) != actual.getInjected(i))
				c = true;
		for (i = 0; i < 11; i++)
			if (this.actual.getRemoved(i) != actual.getRemoved(i))
				c = true;
		if (c == true) {
			hasChanged = true;
			this.actual = actual;
		}
    }



    /**
     * Update the values.
     *
     */
    public void update() {
		updateProgramSize();
		timeList.calculateTimes();
		updateTimeInPhase();
		defectList.count();
		updateDefectsInjected();
		updateDefectsRemoved();
		plan.update();
		actual.update();
		toDate.update();
		setToDate();
		setToDateProc();
    }



    /**
     * Update the program size values.
     *
     */
    private void updateProgramSize() {
		plan.setSize(3, plan.getSize(5)
		      - plan.getSize(2));
		plan.setSize(6, plan.getSize(5)
		      + plan.getSize(0)
		      - plan.getSize(2)
		      - plan.getSize(1)
		      + plan.getSize(4));
		actual.setSize(3, actual.getSize(6)
			- actual.getSize(0)
			+ actual.getSize(1)
			- actual.getSize(4));
		actual.setSize(5, actual.getSize(3)
			+ actual.getSize(2));
    }



    /**
     * Update the time in phase values.
     *
     */
    private void updateTimeInPhase(){
		actual.setTimeInPhase(0, timeList.planTime);
		actual.setTimeInPhase(1, timeList.hLDesignTime);
		actual.setTimeInPhase(2, timeList.hLDesignReviewTime);
		actual.setTimeInPhase(3, timeList.designTime);
		actual.setTimeInPhase(4, timeList.designReviewTime);
		actual.setTimeInPhase(5, timeList.codeTime);
		actual.setTimeInPhase(6, timeList.codeReviewTime);
		actual.setTimeInPhase(7, timeList.compileTime);
		actual.setTimeInPhase(8, timeList.testTime);
		actual.setTimeInPhase(9, timeList.postMortemTime);
    }



    /**
     * Update the defects injected values.
     *
     */
    private void updateDefectsInjected() {
		actual.setInjected(0, defectList.injectedPlanning);
		actual.setInjected(1, defectList.injectedHighLevelDesign);
		actual.setInjected(2, defectList.injectedHighLevelDesignReview);
		actual.setInjected(3, defectList.injectedDesign);
		actual.setInjected(4, defectList.injectedDesignReview);
		actual.setInjected(5, defectList.injectedCode);
		actual.setInjected(6, defectList.injectedCodeReview);
		actual.setInjected(7, defectList.injectedCompile);
		actual.setInjected(8, defectList.injectedTest);
		toDateProc.setInjected(9, 100);
    }



    /**
     * Update the defects removed values.
     *
     */
    private void updateDefectsRemoved() {
		actual.setRemoved(0, defectList.removedPlanning);
		actual.setRemoved(1, defectList.removedHighLevelDesign);
		actual.setRemoved(2, defectList.removedHighLevelDesignReview);
		actual.setRemoved(3, defectList.removedDesign);
		actual.setRemoved(4, defectList.removedDesignReview);
		actual.setRemoved(5, defectList.removedCode);
		actual.setRemoved(6, defectList.removedCodeReview);
		actual.setRemoved(7, defectList.removedCompile);
		actual.setRemoved(8, defectList.removedTest);
    }



    /**
     * Sets the to date values.
     *
     *
     * @return true if the vales were set.
     */
    private boolean setToDate() {
		toDate = new SummaryData();
		int i = 0;
		for (i = 0; i < 10; i++)
			toDate.setSize(i, actual.getSize(i));
		for (i = 0; i < 13; i++)
			toDate.setTimeInPhase(i, actual.getTimeInPhase(i));
		for (i = 0; i < 10; i++)
			toDate.setInjected(i, actual.getInjected(i));
		for (i = 0; i < 11; i++)
			toDate.setRemoved(i, actual.getRemoved(i));
		int siz = summaryList.size();
		int	j = 0;
		SummaryData tmpSummaryData = new SummaryData();
		summaryList.read();
		for (j = 0; j < siz; j++) {
			tmpSummaryData = summaryList.getActual(j);
			for (i = 0; i < 10; i++)
				toDate.setSize(i, toDate.getSize(i)
							+ tmpSummaryData.getSize(i));
			for (i = 0; i < 13; i++)
				toDate.setTimeInPhase(i, toDate.getTimeInPhase(i)
							+ tmpSummaryData.getTimeInPhase(i));
			for (i = 0; i < 10; i++)
				toDate.setInjected(i, toDate.getInjected(i)
						    + tmpSummaryData.getInjected(i));
			for (i = 0; i < 11; i++)
				toDate.setRemoved(i, toDate.getRemoved(i)
						    + tmpSummaryData.getRemoved(i));
		}
		toDate.update();
		for (j = 0; j < siz; j++) {
			tmpSummaryData = summaryList.getPlan(j);
			toDate.setSummary(1, toDate.getSummary(1)
			       + tmpSummaryData.getTimeInPhase(10));
		}
		if (toDate.getSummary(2) == 0)
			toDate.setSummary(3, 100);
		else
			toDate.setSummary(3, (toDate.getSummary(1) * 100)
					/ toDate.getSummary(2));
		return true;
    }



    /**
     * Sets the to date values.
     *
     *
     * @return true if the vales were set.
     */
    private boolean setToDateProc() {
		int tmpInt = 0;
		tmpInt = toDate.getTimeInPhase(10);
		if (tmpInt == 0)
			for (int i = 0; i< 10; i++)
				toDateProc.setTimeInPhase(i, 0);
		else {
			toDateProc.setTimeInPhase(0, (toDate.getTimeInPhase(0) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(1, (toDate.getTimeInPhase(1) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(2, (toDate.getTimeInPhase(2) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(3, (toDate.getTimeInPhase(3) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(4, (toDate.getTimeInPhase(4) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(5, (toDate.getTimeInPhase(5) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(6, (toDate.getTimeInPhase(6) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(7, (toDate.getTimeInPhase(7) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(8, (toDate.getTimeInPhase(8) * 100)
					       / tmpInt);
			toDateProc.setTimeInPhase(9, (toDate.getTimeInPhase(9) * 100)
					       / tmpInt);
		}
		toDateProc.setTimeInPhase(10, 100);
		tmpInt = 0;
		tmpInt = toDate.getInjected(9);
		if (tmpInt == 0)
			for (int i = 0; i< 9; i++)
				toDateProc.setInjected(i, 0);
		else {
			toDateProc.setInjected(0, (toDate.getInjected(0) * 100) / tmpInt);
			toDateProc.setInjected(1, (toDate.getInjected(1) * 100) / tmpInt);
			toDateProc.setInjected(2, (toDate.getInjected(2) * 100) / tmpInt);
			toDateProc.setInjected(3, (toDate.getInjected(3) * 100) / tmpInt);
			toDateProc.setInjected(4, (toDate.getInjected(4) * 100) / tmpInt);
			toDateProc.setInjected(5, (toDate.getInjected(5) * 100) / tmpInt);
			toDateProc.setInjected(6, (toDate.getInjected(6) * 100) / tmpInt);
			toDateProc.setInjected(7, (toDate.getInjected(7) * 100) / tmpInt);
			toDateProc.setInjected(8, (toDate.getInjected(8) * 100) / tmpInt);
		}
		toDateProc.setInjected(9, 100);
		tmpInt = 0;
		tmpInt = toDate.getRemoved(9);
		if (tmpInt == 0)
			for (int i = 0; i< 9; i++)
				toDateProc.setRemoved(i, 0);
		else {
			toDateProc.setRemoved(0, (toDate.getRemoved(0) * 100) / tmpInt);
			toDateProc.setRemoved(1, (toDate.getRemoved(1) * 100) / tmpInt);
			toDateProc.setRemoved(2, (toDate.getRemoved(2) * 100) / tmpInt);
			toDateProc.setRemoved(3, (toDate.getRemoved(3) * 100) / tmpInt);
			toDateProc.setRemoved(4, (toDate.getRemoved(4) * 100) / tmpInt);
			toDateProc.setRemoved(5, (toDate.getRemoved(5) * 100) / tmpInt);
			toDateProc.setRemoved(6, (toDate.getRemoved(6) * 100) / tmpInt);
			toDateProc.setRemoved(7, (toDate.getRemoved(7) * 100) / tmpInt);
			toDateProc.setRemoved(8, (toDate.getRemoved(8) * 100) / tmpInt);
		}
		toDateProc.setRemoved(9, 100);
		return true;
    }



    /**
     * Create a string representation of the plan.
     *
     *
     * @return the string representation of the plan.
     */
    public String planToString() {
		return new String(plan.toString() + "\n");
    }



    /**
     * Create a string representation of the actual.
     *
     *
     * @return the string representation of the actual.
     */
    public String actualToString() {
		return new String(actual.toString() + "\n");
    }



    /**
     * Create a string representation of Summary.
     *
     *
     * @return the string representation of Summary.
     */
    public String toString() {
		return new String("Plan\n" + plan.toString() + "\n"
				+ "Actual\n" + actual.toString() + "\n"
				+ "ToDate\n" + toDate.toString() + "\n"
				+ "ToDate%\n" + toDateProc.toString());
    }
}
