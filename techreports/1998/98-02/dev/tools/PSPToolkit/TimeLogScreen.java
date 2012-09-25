import java.awt.*;
import java.awt.event.*;


/**
 * 
 * TimeLogScreen.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Updated deprecated API.)
 *   Exchanged DatumException for Exception.
 *   Changed comments.
 *   Now subclass to ScreenBase instead of Panel.
 *   Merged TLScreen into TimeLogScreen.java.
 *   Moved setGridBagConstraints to ScreenBase.
 *   Method setError moved to ScreenBase.
 *   Editted items are moved (not copied) to the
 *     current window.
 *   Added description box.
 * 
 */
public class TimeLogScreen extends ScreenBase implements ActionListener {
    private TimeData timeData;
    private TimeData tmpTimeData;
    private TimeLog timeLog;

	private Button addButton;				// add
    private Button startButton;				// start
    private Button stopButton;				// stop
    private Button clearButton;				// clear
    private Button editButton;				// edit
    private Button deleteButton;			// delete
    private Choice phaseChoice;				// project phase
    private Label startLabel;				// start
    private Label stopLabel;				// stop
    private Label phaseLabel;				// phase
	private Label descriptionLabel;
    private List historyList;				// history
    private TextField startField;			// start time
    private TextField stopField;			// stop time
	private TextArea descriptionArea;
	private Panel buttonPanel1;
	private Panel buttonPanel2;


    /**
     * Create a time log screen.
     *
     *
     *
     */
    public TimeLogScreen() {
		timeData = null;
		tmpTimeData = null;
		makeButtons();
		makeChoices();
		makeLabels();
		makeLists();
		makeTextFields();
		setGridBagConstraints();
		setGridBagLayout();
    }



    /**
     * Set the time log association.
     *
     * @param timeLog the time log.
     */
    public void setTimeLog(TimeLog timeLog) {
		this.timeLog = timeLog;
    }



    /**
     * Update the history list.
     *
     */
    public void read() {
		historyList.removeAll();
		int i = timeLog.size();
		int j = 0;
		while (i > j) {
			historyList.add(timeLog.getElementString(j));
			j++;
		}
    }



    /**
     * Add a time log.
     *
     *
     * @return true if the log was added.
     */
    private boolean add() {
		tmpTimeData = new TimeData();
		if (getStartTime().compareTo("") == 0) {
			setError(new String("Enter a start time!"));
			return false;
		}
		String tmpStr = new Datum().toString();
		if (getStopTime().compareTo("") == 0)
			setStopTime(tmpStr);
		try {
			tmpTimeData.setData(startField.getText(),stopField.getText(),getPhase(),descriptionArea.getText());
			timeLog.add(tmpTimeData);
		}
		catch (Exception e) {
			setError(new String(e.getMessage()));
			return false;
		}
		clear();
		startField.setText(tmpStr);
		read();
		return true;
    }



    /**
     * Clear the screen.
     *
     */
    public void clear() {
		setStartTime("");
		setStopTime("");
		setError("");
		descriptionArea.setText("");
		timeData = null;
		tmpTimeData = null;
    }



    /**
     * Edit a defect log.
     *
     *
     * @return true if the selected log was brought.
     */
    private boolean edit() {
		int index = historyList.getSelectedIndex();
		if (index < 0)
			return false;
		timeData = timeLog.getElement(index);
		if (timeData != null) {
			historyList.remove(index);
			setStartTime(timeData.getStartDate().toString());
			setStopTime(timeData.getStopDate().toString());
			setPhase(timeData.getPhase());
			descriptionArea.setText(timeData.getDesc());
		}
		return true;
    }



    /**
     * Delete a defect from the log.
     *
     *
     * @return true if the log was deleted.
     */
    private boolean delete() {
		int index = historyList.getSelectedIndex();
		if (index < 0)
			return false;
		timeLog.removeElement(index);
		read();
		return true;
    }



	/**
     * Create the buttons.
     *
     */
    private void makeButtons() {
		buttonPanel1 = new Panel();
		buttonPanel1.setLayout(gridBagLayout);
		buttonPanel2 = new Panel();
		buttonPanel2.setLayout(gridBagLayout);
		addButton = new Button("Add");
		addButton.setActionCommand("add");
		addButton.addActionListener(this);
		startButton = new Button("Start Time");
		startButton.setActionCommand("start");
		startButton.addActionListener(this);
		stopButton = new Button("Stop Time");
		stopButton.setActionCommand("stop");
		stopButton.addActionListener(this);
		clearButton = new Button("Clear");
		clearButton.setActionCommand("clear");
		clearButton.addActionListener(this);
		editButton = new Button("Edit");
		editButton.setActionCommand("edit");
		editButton.addActionListener(this);
		deleteButton = new Button("Delete");
		deleteButton.setActionCommand("delete");
		deleteButton.addActionListener(this);
		setGridBagConstraints();
		addRow(buttonPanel1,addButton,startButton,stopButton,clearButton);
		addRow(buttonPanel2,editButton,deleteButton);
    }


	/**
     * Create the choices.
     *
     */
    private void makeChoices() {
		phaseChoice = new Choice();
		addPhaseChoices(phaseChoice);
    }



    /**
     * Add PSP choices.
     *
     * @param choice The choice to add phase items to.
     */
    private void addPhaseChoices(Choice choice) {
		choice.addItem("Planning");
		choice.addItem("High Level Design");
		choice.addItem("High Level Design Review");
		choice.addItem("Design");
		choice.addItem("Design Review");
		choice.addItem("Code");
		choice.addItem("Code Review");
		choice.addItem("Compile");
		choice.addItem("Test");
		choice.addItem("Post Mortem");
    }



    /**
     * Create the labels.
     *
     */
    private void makeLabels() {
		startLabel = new Label("Start Time:");
		stopLabel = new Label("Stop Time:");
		phaseLabel = new Label("Phase:");
		descriptionLabel = new Label("Description:");
		errorLabel = new Label("");
		errorLabel.setForeground(new Color(255, 0, 0));
    }



    /**
     * Create the lists.
     *
     */
    private void makeLists() {
		historyList = new List();
		historyList.addActionListener(this);
    }



    /**
     * Create the text fields.
     *
     */
    private void makeTextFields() {
		startField = new TextField();
		stopField = new TextField();
		descriptionArea = new TextArea(3, 2);
    }



    /**
     * Create the gridbag layout.
     *
     */
    private void setGridBagLayout() {
		gridBagLayout = new GridBagLayout();
		setLayout(gridBagLayout);
		addRow(this, buttonPanel1);
		addRow(this, startLabel, startField);
		addRow(this, stopLabel, stopField);
		addRow(this, phaseLabel, phaseChoice);
		addRow(this, descriptionLabel);
		addRow(this, descriptionArea);
//		addRow(this, new Label());
		addRow(this, errorLabel);
		addRow(this, buttonPanel2);
		addLastRow(historyList);
    }





    /**
     * Set the start time
     *
     * @param start the start time.
     */
    protected void setStartTime(String start) {
		startField.setText(start);
    }



    /**
     * Get the start time.
     *
     * 
     * @return the start time.
     */
    protected String getStartTime() {
		return startField.getText();
    }



    /**
     * Set the stop time.
     *
     * @param stop the stop time.
     * @return true if succeeded.
     */
    protected void setStopTime(String stop) {
		stopField.setText(stop);
    }



    /**
     * Get the stop time.
     *
     *
     * @return the stop time.
     */
    protected String getStopTime() {
		return stopField.getText();
    }



    /**
     * Set current phase.
     *
     * @param phase the phase.
     * @return true if phase was set.
     */
    protected void setPhase(String phase) {
		phaseChoice.select(phase);
    }



    /**
     * Get current phase.
     *
     *
     * @return surrent phase.
     */
    protected String getPhase() {
		return new String(phaseChoice.getSelectedItem());
    }


    /**
     *
     *
     *
     *
     */
    public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if (command.equals("add")) {
		    add();
		}
		if (command.equals("start")) {
			setStartTime(new Datum().toString());
		}
		if (command.equals("stop")) {
			setStopTime(new Datum().toString());
		}
		if (command.equals("clear")) {
			clear();
		}
		if (command.equals("edit")) {
			edit();
		}
		if (e.getSource() == historyList) {
			edit();
		}
		if (command.equals("delete")) {
			delete();
		}
    }
}
