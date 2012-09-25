import java.awt.*;


/**
 *
 * ScreenBase.java
 *
 * 
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Changed comments/beautified code.
 *
 */
public class ScreenBase extends Panel {
    protected GridBagLayout gridBagLayout;
    protected GridBagConstraints gridBagConstraints;
	protected Label errorLabel;


    /**
     * Create a new ScreenBase.
     *
     *
     *
     */
    public ScreenBase() {
		setGridBagConstraints();
		gridBagLayout = new GridBagLayout();
    }



    /**
     * Set the gridbag constraints.
     *
     */
    protected void setGridBagConstraints() {
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = GridBagConstraints.RELATIVE;
		gridBagConstraints.gridy = GridBagConstraints.RELATIVE;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		gridBagConstraints.ipadx = 0;
		gridBagConstraints.ipady = 0;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.gridheight = 1;
    }



    /**
     * Add a row with one component to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     */
    protected void addRow(Panel destination
				, Component component_1) {
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
    }



    /**
     * Add a row with two components to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     * @param component_2 the second component in the row.
     */
    protected void addRow(Panel destination
			   , Component component_1
			   , Component component_2) {
		gridBagConstraints.gridwidth = 1;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_2, gridBagConstraints);
		destination.add(component_2);
    }


    /**
     * Add a row with three components to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     * @param component_2 the second component in the row.
     * @param component_3 the third component in the row.
     */
    protected void addRow(Panel destination
			   , Component component_1
			   , Component component_2
			   , Component component_3) {
		gridBagConstraints.gridwidth = 1;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
		gridBagLayout.setConstraints(component_2, gridBagConstraints);
		destination.add(component_2);
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_3, gridBagConstraints);
		destination.add(component_3);
    }



    /**
     * Add a row with four components to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     * @param component_2 the second component in the row.
     * @param component_3 the third component in the row.
     * @param component_4 the fourth component in the row.
     */
    protected void addRow(Panel destination
			   , Component component_1
			   , Component component_2
			   , Component component_3
			   , Component component_4) {
		gridBagConstraints.gridwidth = 1;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
		gridBagLayout.setConstraints(component_2, gridBagConstraints);
		destination.add(component_2);
		gridBagLayout.setConstraints(component_3, gridBagConstraints);
		destination.add(component_3);
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_4, gridBagConstraints);
		destination.add(component_4);
    }



    /**
     * Add a row with five components to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     * @param component_2 the second component in the row.
     * @param component_3 the third component in the row.
     * @param component_4 the fourth component in the row.
     * @param component_5 the fifth component in the row.
     */
    protected void addRow(Panel destination
			   , Component component_1
			   , Component component_2
			   , Component component_3
			   , Component component_4
			   , Component component_5) {
		gridBagConstraints.gridwidth = 1;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
		gridBagLayout.setConstraints(component_2, gridBagConstraints);
		destination.add(component_2);
		gridBagLayout.setConstraints(component_3, gridBagConstraints);
		destination.add(component_3);
		gridBagLayout.setConstraints(component_4, gridBagConstraints);
		destination.add(component_4);
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_5, gridBagConstraints);
		destination.add(component_5);
    }



    /**
     * Add a row with six components to the grid bag.
     *
     * @param destination of the components.
     * @param component_1 the first component in the row.
     * @param component_2 the second component in the row.
     * @param component_3 the third component in the row.
     * @param component_4 the fourth component in the row.
     * @param component_5 the fifth component in the row.
     * @param component_6 the sixth component in the row.
     */
    protected void addRow(Panel destination
			   , Component component_1
			   , Component component_2
			   , Component component_3
			   , Component component_4
			   , Component component_5
			   , Component component_6) {
		gridBagConstraints.gridwidth = 1;
		gridBagLayout.setConstraints(component_1, gridBagConstraints);
		destination.add(component_1);
		gridBagLayout.setConstraints(component_2, gridBagConstraints);
		destination.add(component_2);
		gridBagLayout.setConstraints(component_3, gridBagConstraints);
		destination.add(component_3);
		gridBagLayout.setConstraints(component_4, gridBagConstraints);
		destination.add(component_4);
		gridBagLayout.setConstraints(component_5, gridBagConstraints);
		destination.add(component_5);
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagLayout.setConstraints(component_6, gridBagConstraints);
		destination.add(component_6);
    }


	 /**
     * Add component that fills the rest.
     *
     * @param component_1 the component to add.
     */
    protected void addLastRow(Component component_1) {
        gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
        gridBagConstraints.gridheight = GridBagConstraints.REMAINDER;
        gridBagConstraints.weighty = 1.2;
        gridBagLayout.setConstraints(component_1, gridBagConstraints);
        add(component_1);
    }


	/**
     * Set error message.
     *
     * @param message the message.
     */
    protected void setError(String message) {
		errorLabel.setText(message);
    }



    /**
     * Clear/hide error message.
     *
     */
//    protected void clearError() {
//		errorLabel.setVisible(false);
//		validate();
//  }
}
