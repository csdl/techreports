import java.awt.*;
import java.awt.event.*;


/**
 *
 * ProjectScreen.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Exchanged DatumException for Exception.
 *   Removed all deprecated API.
 *   Changed/extended comments for methods.
 *   Merged PScreen into ProjectScreen.java.
 *   Extends ScreenBase instead of Panel.
 *   Removed all internal methods inherited from ScreenBase.
 *   Listens for changes in the textfields to set the changed flag.
 *
 */
public class ProjectScreen extends ScreenBase 
						   implements KeyListener {
    private ProjectData projectData;				// project
	private Choice pspChoice;						// PSP level
    private Label nameLabel;						// user name
    private Label projectNameLabel;					// project name
    private Label projectNumberLabel;				// project number
    private Label pspLabel;							// PSP level
    private Label languageLabel;					// language
    private Label dateLabel;						// date
    private TextField nameField;					// user name
    private TextField projectNameField;				// project name
    private TextField projectNumberField;			// project number
    private TextField languageField;				// language
    private TextField dateField;					// date


    /**
     * Create a new project screen.
     *
     *
     *
     */
    public ProjectScreen() {
		makeChoices();
		makeLabels();
		makeTextFields();
		setGridBagConstraints();
		setGridBagLayout();
    }


	/**
     * Create the choices.
     *
     *
     * @return true if succeeded.
     */
    private void makeChoices() {
		pspChoice = new Choice();
		pspChoice.addItem("PSP 0");
		pspChoice.addItem("PSP 0.1");
		pspChoice.addItem("PSP 1");
		pspChoice.addItem("PSP 1.1");
		pspChoice.addItem("PSP 2");
		pspChoice.addItem("PSP 2.1");
		pspChoice.addItem("PSP 3");
//		pspChoice.addItemListener(this);
    }



    /**
     * Create the labels.
     *
     *
     * @return true if succeeded.
     */
    private void makeLabels() {
		nameLabel = new Label("Name:");					 // user name
		projectNameLabel = new Label("Project Name:");	 // projectname						
		projectNumberLabel = new Label("Project Number:"); // projectnumber
		languageLabel = new Label("Programming Language:");// language
		dateLabel = new Label("Date:");					 // date
		pspLabel = new Label("PSP level:");				 // PSP level
		errorLabel = new Label("");						 // error
		errorLabel.setForeground(new Color(255, 0, 0));
    }


    /**
     * Create the text fields.
     *
     *
     * @return true if succeeded.
     */
    private void makeTextFields() {
		nameField = new TextField();			// user name
		projectNameField = new TextField();		// project name
		projectNumberField = new TextField();	// project number
		languageField = new TextField();		// language
		dateField = new TextField();			// date

		nameField.addKeyListener(this);
		projectNameField.addKeyListener(this);
		projectNumberField.addKeyListener(this);
		languageField.addKeyListener(this);
		dateField.addKeyListener(this);
    }



    /**
     * Create the gridbag layout.
     *
     *
     * @return true if succeeded.
     */
    private void setGridBagLayout() {
		gridBagLayout = new GridBagLayout();
		setLayout(gridBagLayout);
		addRow(this, nameLabel, nameField);					 // name
		addRow(this, projectNameLabel, projectNameField);	 // project name
		addRow(this, projectNumberLabel, projectNumberField);// project number
		addRow(this, languageLabel, languageField);			 // language
		addRow(this, dateLabel, dateField);					 // date
		addRow(this, pspLabel, pspChoice);
		addRow(this, errorLabel);
		addLastRow(new Label());
    }



    /**
     * Set user name
     *
     * @param name the user name.
     * @return true if succeeded.
     */
    protected void setUserName(String name) {
		nameField.setText(name);
    }



    /**
     * Get user name.
     *
     * 
     * @return the user name.
     */
    protected String getUserName() {
		return nameField.getText();
    }



    /**
     * Set project name.
     *
     * @param projectName the project name.
     * @return true if succeeded.
     */
    protected void setProjectName(String projectName) {
		projectNameField.setText(projectName);
    }



    /**
     * Get project name.
     *
     *
     * @return the project name.
     */
    protected String getProjectName() {
		return projectNameField.getText();
    }



    /**
     * Set project number.
     *
     * @param projectNumber the project number.
     * @return true if succeeded.
     */
    protected void setProjectNumber(String projectNumber) {
		projectNumberField.setText(projectNumber);
    }



    /**
     * Get project number.
     *
     *
     * @return the project number.
     */
    protected String getProjectNumber() {
		return projectNumberField.getText();
    }



    /**
     * Set language.
     *
     * @param language the language.
     * @return true if succeeded.
     */
    protected void setLanguage(String language) {
		languageField.setText(language);
    }



    /**
     * Get language.
     *
     *
     * @return the language.
     */
    protected String getLanguage() {
		return languageField.getText();
    }



    /**
     * Set level.
     *
     * @param level the level.
     * @return true if succeeded.
     */
    protected void setLevel(String level) {
		pspChoice.select(level);
    }



    /**
     * Get level.
     *
     *
     * @return the level.
     */
    protected String getLevel() {
		return pspChoice.getSelectedItem();
    }



    /**
     * Set date.
     *
     * @param date the date.
     * @return true if succeeded.
     */
    protected void setDate(String date) {
		dateField.setText(date);
    }



    /**
     * Get date.
     *
     *
     * @return the date.
     */
    protected String getDate() {
		return dateField.getText();
    }

    /**
     * Set the project association.
     *
     * @param project the project.
     * @return true if association was set.
     */
    public void setProjectData(ProjectData projectData) {
		this.projectData = projectData;
    }



    /**
     * Read values from project.
     *
     *
     * @return true if values were read.
     */
    public boolean read() {
		if (projectData != null) {
			setError("");
			setUserName(projectData.getUserName());
			setProjectName(projectData.getProjectName());
			setProjectNumber(projectData.getProjectNumber());
			setLanguage(projectData.getLanguage());
			setLevel(projectData.getLevel());
			setDate(projectData.getDate());
			return true;
		}
		else
			return false;
    }



    /**
     * Save the values to project.
     *
     *
     * @return true if values was saved.
     */
    public boolean save() {
		if (projectData != null) {
			try {
				setError("");
				projectData.setData(getUserName()
					, getProjectName()
					, getProjectNumber()
					, getLanguage()
					, getLevel()
					, getDate());
			}
			catch(Exception e) {
				setError("Illegal date format: " + e.getMessage() + " (yy/mm/dd)");
				return false;
			}
			read();
			return true;
		}
		else
			return false;
	}


	/** 
	 * Listen for and indicate changes made to the textfields.
	 *
	 */
	public void keyTyped(KeyEvent e) {
		if (projectData != null) {
			if (!projectData.hasChanged()) {
				projectData.setChanged(true);
			}
		}
	}


	/**
	 * Must be implemented (KeyListener interface method).
	 *
	 */
	public void keyPressed(KeyEvent e) {;}


	/**
	 * Must be implemented (KeyListener interface method).
	 *
	 */
	public void keyReleased(KeyEvent e) {;}
}
