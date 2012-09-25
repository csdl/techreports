import java.awt.*;
import java.awt.event.*;


/**
 *
 * DefectLogScreen.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modfied Stefan Olofsson 1999
 *   Updated all deprecated API.
 *   Exchanged DefectDataException for Exception.
 *   Edited items are always (temporarily) removed 
 *     from historyList.
 *   Now displays all kinds of error messages (some
 *	   were never reached).
 *   The fields are cleared when a project is closed.
 *   Merged DLScreen into DefectLogScreen.java.
 *   Changed "Emvironment" to "Environment".
 *   Changed "Build, Package" to "Build/Package" since "," is used as
 *     a field separator by the project file loader (corrupted files).
 *   Removed setGridBagConstraints method (exists in ScreenBase).
 *
 */
public class DefectLogScreen extends ScreenBase implements ActionListener {
    private DefectData defectData;
    private DefectData tmpDefectData;
    private DefectLog defectLog;
	private Button addButton;
    private Button clearButton;
    private Button foundDateButton;
    private Button fixDateButton;
    private Button editButton;
    private Button deleteButton;
    private Choice removedPhaseChoice;
    private Choice injectedPhaseChoice;
    private Choice defectTypeChoice;
    private Label foundDateLabel;
    private Label fixDateLabel;
    private Label removedPhaseLabel;
    private Label injectedPhaseLabel;
    private Label defectTypeLabel;
    private Label descriptionLabel;
    private List historyList;
    private Panel buttonPanel;
    private Panel editPanel;
    private TextArea descriptionArea;
    private TextField foundDateField;
    private TextField fixDateField;



    /**
     * Create a new defect log screen.
     *
     * 
     *
     */
    public DefectLogScreen() {
   		makeButtonPanel();
		makeEditPanel();
		setLayout(gridBagLayout);
		makeRows();
		addComponents();
	}



    /**
     * Create the button panel.
     *
     */
    private void makeButtonPanel() {
		gridBagLayout = new GridBagLayout();
		buttonPanel = new Panel();
		buttonPanel.setLayout(gridBagLayout);
		foundDateButton = new Button("Found Date");
		foundDateButton.setActionCommand("found");
		foundDateButton.addActionListener(this);
		fixDateButton = new Button("Fix Date");
		fixDateButton.setActionCommand("fix");
		fixDateButton.addActionListener(this);
		addButton = new Button("Add");
		addButton.setActionCommand("add");
		addButton.addActionListener(this);
		clearButton = new Button("Clear");
		clearButton.setActionCommand("clear");
		clearButton.addActionListener(this);
		setGridBagConstraints();
		addRow(buttonPanel, addButton, foundDateButton, fixDateButton, clearButton);
    }



    /**
     * Create the edit panel.
     *
     */
    private void makeEditPanel() {
		gridBagLayout = new GridBagLayout();
		editPanel = new Panel();
		editPanel.setLayout(gridBagLayout);
		editButton = new Button("Edit");
		editButton.setActionCommand("edit");
		editButton.addActionListener(this);
		deleteButton = new Button("Delete");
		deleteButton.setActionCommand("delete");
		deleteButton.addActionListener(this);
		setGridBagConstraints();
		addRow(editPanel, editButton, deleteButton);
    }



    /**
     * Add the components.
     *
     */
    private void addComponents() {
		setGridBagConstraints();
		addRow(this, buttonPanel);
		addRow(this, foundDateLabel, foundDateField);
		addRow(this, injectedPhaseLabel, injectedPhaseChoice);
		addRow(this, new Label());
		addRow(this, fixDateLabel, fixDateField);
		addRow(this, removedPhaseLabel, removedPhaseChoice);
		addRow(this, defectTypeLabel, defectTypeChoice);
		addRow(this, descriptionLabel);
		addRow(this, descriptionArea);
		addRow(this, errorLabel);
		addRow(this, editPanel);
		addLastRow(historyList);
    }



    /**
     * Create the rows.
     *
     */
    private void makeRows() {
		removedPhaseChoice = new Choice();
		removedPhaseChoice.addItem("");
		removedPhaseChoice.addItem("Planning");
		removedPhaseChoice.addItem("High Level Design");
		removedPhaseChoice.addItem("High Level Design Review");
		removedPhaseChoice.addItem("Design");
		removedPhaseChoice.addItem("Design Review");
		removedPhaseChoice.addItem("Code");
		removedPhaseChoice.addItem("Code Review");
		removedPhaseChoice.addItem("Compile");
		removedPhaseChoice.addItem("Test");
		injectedPhaseChoice = new Choice();
		injectedPhaseChoice.addItem("");
		injectedPhaseChoice.addItem("Planning");
		injectedPhaseChoice.addItem("High Level Design");
		injectedPhaseChoice.addItem("High Level Design Review");
		injectedPhaseChoice.addItem("Design");
		injectedPhaseChoice.addItem("Design Review");
		injectedPhaseChoice.addItem("Code");
		injectedPhaseChoice.addItem("Code Review");
		injectedPhaseChoice.addItem("Compile");
		injectedPhaseChoice.addItem("Test");
		defectTypeChoice = new Choice();
		defectTypeChoice.addItem("");
		defectTypeChoice.addItem("Documentation");
		defectTypeChoice.addItem("Syntax");
		defectTypeChoice.addItem("Build/Package");
		defectTypeChoice.addItem("Assignment");
		defectTypeChoice.addItem("Interface");
		defectTypeChoice.addItem("Checking");
		defectTypeChoice.addItem("Data");
		defectTypeChoice.addItem("Function");
		defectTypeChoice.addItem("System");
		defectTypeChoice.addItem("Environment");
		foundDateLabel = new Label("Found Date:");
		fixDateLabel = new Label("Fix Date:");
		removedPhaseLabel = new Label("Removal Phase:");
		injectedPhaseLabel = new Label("Injection Phase:");
		defectTypeLabel = new Label("Defect Type:");
		descriptionLabel = new Label("Description:");
		errorLabel = new Label("");
		errorLabel.setForeground(new Color(255, 0, 0));
		historyList = new List();
		historyList.addActionListener(this);
		descriptionArea = new TextArea(3, 2);
		foundDateField = new TextField();
		fixDateField = new TextField();
    }
    



    /**
     * Set the defect log association.
     *
     * @param defectLog the defect log.
     */
    public void setDefectLog(DefectLog defectLog) {
        this.defectLog = defectLog;
    }



    /**
     * Clear the screen.
     *
     */
    public void clear() {
		foundDateField.setText("");
		removedPhaseChoice.select("");
		fixDateField.setText("");
		injectedPhaseChoice.select("");
		defectTypeChoice.select("");
		descriptionArea.setText("");
    }



    /**
     * Read values.
     *
     */
    public void read() {
		historyList.removeAll();
		int size = defectLog.size();
		if (size > 0)
			for (int i = 0; i < size; i++)
				historyList.add(i+1+" "+defectLog.getElementString(i));
    }



    /**
     * Add a defect log.
     *
     *
     * @return true if the log was added.
     */
    private boolean add() {
		setError("");
		tmpDefectData = new DefectData();
		try {
			tmpDefectData.setData(foundDateField.getText()
				    , injectedPhaseChoice.getSelectedItem()
					, fixDateField.getText()
				    , removedPhaseChoice.getSelectedItem()
					, defectTypeChoice.getSelectedItem()
					, descriptionArea.getText());
		}
		catch (Exception e) {
			if (e.getMessage().compareTo("No data!") == 0)
				return true;
			setError(e.getMessage());
			return false;
		}
		defectLog.add(tmpDefectData);
		tmpDefectData = null;
		defectData = null;
		clear();
		read();
		return true;
    }


    /**
     * Edit a defect log.
     *
     *
     * @return true if the selected log was brought.
     */
    public boolean edit() {
		int index = historyList.getSelectedIndex();
		if (index < 0)
			return false;
//		if (add() == false)
//			return false;
		defectData = defectLog.getElement(index);
		if (defectData != null) {
			historyList.remove(index);
			foundDateField.setText(defectData.getFoundDate().toString());
			injectedPhaseChoice.select(defectData.getInjectedPhase());
			if (defectData.getDefectType() != null)
				defectTypeChoice.select(defectData.getDefectType());
			if (defectData.getFixDate() != null) {
				fixDateField.setText(defectData.getFixDate().toString());
				removedPhaseChoice.select(defectData.getRemovedPhase());
			}
			descriptionArea.setText(defectData.getDesc());
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
		defectLog.removeElement(index);
		read();
		return true;
    }



    public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
        if (command.equals("add")) {
			add();
		}
        else if (command.equals("clear")) {
			clear();
		}
        else if (command.equals("found")) {
			foundDateField.setText(new Datum().toString());
		}
        else if (command.equals("fix")) {
			fixDateField.setText(new Datum().toString());
		}
        else if (command.equals("edit")) {
			edit();
		}
        else if (e.getSource() == historyList) {
			edit();
		}
        else if (command.equals("delete")) {
		    delete();
		}
    }
}
