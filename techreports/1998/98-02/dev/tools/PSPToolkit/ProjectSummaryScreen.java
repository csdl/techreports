import java.awt.*;
import java.awt.event.*;



/**
 * 
 * ProjectSummaryScreen.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Updated all deprecated API.
 *   Merged PSScreen into ProjectSummaryScreen.java.
 *   Now subclass to ScreenBase instead of Panel.
 *   Removed setGridBagConstraints.
 *   Layout tasks moved to ScreenBase.
 *   Error methods moved to ScreenBase.
 *
 */
public class ProjectSummaryScreen extends ScreenBase implements ActionListener {
    private Summary summary;

	private Button summaryButton;
    private Button sizeButton;
    private Button phaseButton;
    private Button injectedButton;
    private Button removedButton;
    private Button efficiencyButton;
    private Button updateButton;
    private DefectsRemovalEfficiencyScreen DREScreen;
    private DefectsRemovedScreen DRScreen;
    private DefectsInjectedScreen DIScreen;
    private TimeInPhaseScreen TIPScreen;
    private ProgramSizeScreen PrSScreen;
    private SummaryScreen SScreen;
    private Panel summaryPanel;
	private Panel buttonPanel;
	private Panel buttonPanel2;



    /**
     * Create new project summary screen.
     *
     *
     *
     */
    public ProjectSummaryScreen() {
		makeButtonPanel();
		makeSummaryPanel();
		gridBagLayout = new GridBagLayout();
		setLayout(gridBagLayout);
		setGridBagConstraints();
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(buttonPanel, gridBagConstraints);
		addRow(this, buttonPanel);
		addRow(this, buttonPanel2);
		errorLabel = new Label();
		errorLabel.setForeground(new Color(255, 0, 0));
		addRow(this, errorLabel);
		addRow(this, summaryPanel);
		Label fillLabel = new Label();
		addLastRow(fillLabel);
    }


    /**
     * Create the button panel.
     *
     */
    private void makeButtonPanel() {
		gridBagLayout = new GridBagLayout();
		buttonPanel = new Panel();
		buttonPanel.setLayout(gridBagLayout);
		buttonPanel2 = new Panel();
		buttonPanel2.setLayout(gridBagLayout);
		summaryButton = new Button("Summary");
		summaryButton.setActionCommand("summary");
		summaryButton.addActionListener(this);
		sizeButton = new Button("Program Size");
		sizeButton.setActionCommand("size");
		sizeButton.addActionListener(this);
		phaseButton = new Button("Time In Phase");
		phaseButton.setActionCommand("phase");
		phaseButton.addActionListener(this);
		injectedButton = new Button("Defects Injected");
		injectedButton.setActionCommand("injected");
		injectedButton.addActionListener(this);
		removedButton = new Button("Defects Removed");
		removedButton.setActionCommand("removed");
		removedButton.addActionListener(this);
		efficiencyButton = new Button("Defects Removal Efficiency");
		efficiencyButton.setActionCommand("efficiency");
		efficiencyButton.addActionListener(this);
		updateButton = new Button("Update");
		updateButton.setActionCommand("update");
		updateButton.addActionListener(this);
		updateButton.setBackground(Color.gray);
		setGridBagConstraints();
		addRow(buttonPanel, phaseButton, sizeButton, injectedButton);
		addRow(buttonPanel2, removedButton, summaryButton, efficiencyButton, updateButton);
    }



    /**
     * Create the summary panel.
     *
     */
    private void makeSummaryPanel() {
		CardLayout cardLayout = new CardLayout();
		summaryPanel = new Panel();
		summaryPanel.setLayout(cardLayout);

		TIPScreen = new TimeInPhaseScreen();
		summaryPanel.add("TIM", TIPScreen);
		DREScreen = new DefectsRemovalEfficiencyScreen();
		summaryPanel.add("DRES", DREScreen);
		DRScreen = new DefectsRemovedScreen();
		summaryPanel.add("DRS", DRScreen);
		DIScreen = new DefectsInjectedScreen();
		summaryPanel.add("INJ", DIScreen);
		PrSScreen = new ProgramSizeScreen();
		summaryPanel.add("SIZ", PrSScreen);
		SScreen = new SummaryScreen();
		summaryPanel.add("SUM", SScreen);
    }
  

    /**
     * Create the gridbag layout.
     *
     */
    private void setGridBagLayout() {
		gridBagLayout = new GridBagLayout();
		summaryPanel.setLayout(gridBagLayout);
    }


    /**
     * Set the summary association.
     *
     * @param summary the time log.
     */
    public void setSummary(Summary summary) {
        this.summary = summary;
    }



    /**
     * Save the values to the summary.
     *
     *
     * @return true if values were saved.
     */
    public boolean save() {
		errorLabel.setText("");
		SummaryData tmpData = new SummaryData();
		int i;
		int	tmpInt;
		for (i = 0; i < 10; i++) {
			if (PrSScreen.planField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(PrSScreen.planField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
						      + PrSScreen.planField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "SIZ");
					return false;
				}
			}
			tmpData.setSize(i, tmpInt);
			if (DIScreen.planField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(DIScreen.planField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + DIScreen.planField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DEF");
					return false;
				}
			}
			tmpData.setInjected(i, tmpInt);
		}
		for (i = 0; i < 13; i++) {
			if (TIPScreen.planField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(TIPScreen.planField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + TIPScreen.planField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "TIM");
					return false;
				}
			}
			tmpData.setTimeInPhase(i, tmpInt);
		}
		for (i = 0; i < 11; i++) {
			if (DRScreen.planField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(DRScreen.planField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + DRScreen.planField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DRS");
					return false;
				}
			}
			tmpData.setRemoved(i, tmpInt);
		}
		summary.setPlan(tmpData);
		tmpData = new SummaryData();
		for (i = 0; i < 10; i++) {
			if (PrSScreen.actualField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(PrSScreen.actualField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + PrSScreen.actualField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "SIZ");
					return false;
				}
			}
			tmpData.setSize(i, tmpInt);
			if (DIScreen.actualField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(DIScreen.actualField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + DIScreen.actualField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DEF");
					return false;
				}
			}
			tmpData.setInjected(i, tmpInt);
		}
		for (i = 0; i < 13; i++) {
			if (TIPScreen.actualField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(TIPScreen.actualField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							  + TIPScreen.actualField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "TIM");
					return false;
				}
			}
			tmpData.setTimeInPhase(i, tmpInt);
		}
		for (i = 0; i < 11; i++) {
			if (DRScreen.actualField[i].getText().compareTo("") == 0) {
				tmpInt = 0;
			}
			else {
				try {
					tmpInt = new Integer(DRScreen.actualField[i].getText()).intValue();
				}
				catch (NumberFormatException e) {
					errorLabel.setText("Not a number: "
							+ DRScreen.actualField[i].getText());
					((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DRS");
					return false;
				}
			}
			tmpData.setRemoved(i, tmpInt);
		}
		summary.setActual(tmpData);
		summary.update();
		return true;
    }



    /**
     * Read the summary.
     *
     *
     * @return true if the summary was read.
     */
    public boolean read() {
		SummaryData plan = summary.getPlan();
		SummaryData actual = summary.getActual();
		SummaryData toDate = summary.getToDate();
		SummaryData toDateProc = summary.getToDateProc();
		int i;
		for (i = 0; i < 12; i++) {
			SScreen.planField[i].setText(Integer.toString(plan.getSummary(i)));
			SScreen.actualField[i].setText(Integer.toString(actual.getSummary(i)));
			SScreen.toDateField[i].setText(Integer.toString(toDate.getSummary(i)));
		}
		for (i = 0; i < 10; i++) {
			PrSScreen.planField[i].setText(Integer.toString(plan.getSize(i)));
			PrSScreen.actualField[i].setText(Integer.toString(actual.getSize(i)));
			PrSScreen.toDateField[i].setText(Integer.toString(toDate.getSize(i)));
			DIScreen.planField[i].setText(Integer.toString(plan.getInjected(i)));
			DIScreen.actualField[i].setText(Integer.toString(actual.getInjected(i)));
			DIScreen.toDateField[i].setText(Integer.toString(toDate.getInjected(i)));
			DIScreen.toDateProcField[i].setText(Integer.toString(toDateProc.getInjected(i)));
		}
		for (i = 0; i < 13; i++) {
			TIPScreen.planField[i].setText(Integer.toString(plan.getTimeInPhase(i)));
			TIPScreen.actualField[i].setText(Integer.toString(actual.getTimeInPhase(i)));
			TIPScreen.toDateField[i].setText(Integer.toString(toDate.getTimeInPhase(i)));
			TIPScreen.toDateProcField[i].setText(Integer.toString(toDateProc.getTimeInPhase(i)));
		}
		for (i = 0; i < 11; i++) {
			DRScreen.planField[i].setText(Integer.toString(plan.getRemoved(i)));
			DRScreen.actualField[i].setText(Integer.toString(actual.getRemoved(i)));
			DRScreen.toDateField[i].setText(Integer.toString(toDate.getRemoved(i)));
			DRScreen.toDateProcField[i].setText(Integer.toString(toDateProc.getRemoved(i)));
		}
/*
	for (i = 0; i < 7; i++) {
	    DREScreen.planField[i].setText(Integer.toString(plan.getEfficiency(i)));
	    DREScreen.actualField[i].setText(Integer.toString(actual.getEfficiency(i)));
	    DREScreen.toDateField[i].setText(Integer.toString(toDate.getEfficiency(i)));
	}
*/
		for (i = 0; i < 7; i++) {
			DREScreen.planField[i].setText(Float.toString(plan.getEfficiency(i)));
			DREScreen.actualField[i].setText(Float.toString(actual.getEfficiency(i)));
			DREScreen.toDateField[i].setText(Float.toString(toDate.getEfficiency(i)));
		}
        return true;
    }



    public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equals("summary")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "SUM");
		}
		if (command.equals("size")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "SIZ");
		}
		if (command.equals("phase")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "TIM");
		}
		if (command.equals("injected")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "INJ");
		}
		if (command.equals("removed")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DRS");
		}
		if (command.equals("efficiency")) {
			((CardLayout)summaryPanel.getLayout()).show(summaryPanel, "DRES");
		}
		if (command.equals("update")) {
			if (save())
				read();
		}
//		validate();
    }
}


