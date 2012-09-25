import java.awt.*;


/**
 *
 * ProgramSizeScreen.java
 *
 * 
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Edit boxes are both disabled (for Windows users)
 *     and non-editable (for UNIX users).
 *
 */
public class ProgramSizeScreen extends ScreenBase {
    private Label columnLabel1;
    private Label columnLabel2;
    private Label columnLabel3;
    private Label columnLabel4;
    private Label[] rowLabel = new Label[10];
    private Label[] rowFill = new Label[10];
    protected TextField[] planField = new TextField[10];
    protected TextField[] actualField = new TextField[10];
    protected TextField[] toDateField = new TextField[10];



    /**
     * Create a new Program Size Screen.
     *
     *
     *
     */
    public ProgramSizeScreen() {
		setLayout(gridBagLayout);
		makeRows();
		addComponents();
    }



    /**
     * Add the components.
     *
	 */
    private void addComponents() {
		addRow(this, columnLabel1, columnLabel2
			 , columnLabel3, columnLabel4, new Label());
		addRow(this, rowLabel[0], planField[0]
			 , actualField[0], rowFill[0]);
		addRow(this, rowLabel[1], planField[1]
			 , actualField[1], rowFill[1]);
		addRow(this, rowLabel[2], planField[2]
			 , actualField[2], rowFill[2]);
		addRow(this, rowLabel[3], planField[3]
			 , actualField[3], rowFill[3]);
		addRow(this, rowLabel[4], planField[4]
			 , actualField[4], toDateField[4], rowFill[4]);
		addRow(this, rowLabel[5], planField[5]
			 , actualField[5], toDateField[5], rowFill[5]);
		addRow(this, rowLabel[6], planField[6]
			 , actualField[6], toDateField[6], rowFill[6]);
		addRow(this, rowLabel[7], planField[7]
			 , actualField[7], toDateField[7], rowFill[7]);
		addRow(this, rowLabel[8], planField[8]
			 , rowFill[8]);
		addRow(this, rowLabel[9], planField[9]
			 , rowFill[9]);
		addLastRow(new Label());
    }



    /**
     * Create the rows.
     *
     */
    private void makeRows() {
		columnLabel1 = new Label("Program Size (LOC)");
		columnLabel2 = new Label("Plan");
		columnLabel3 = new Label("Actual");
		columnLabel4 = new Label("To Date");
		rowLabel[0] = new Label("Base");
		rowLabel[1] = new Label("Deleted");
		rowLabel[2] = new Label("Modified");
		rowLabel[3] = new Label("Added");
		rowLabel[4] = new Label("Reused");
		rowLabel[5] = new Label("Total New And Changed");
		rowLabel[6] = new Label("Total LOC");
		rowLabel[7] = new Label("Total New Reused");
		rowLabel[8] = new Label("UPI");
		rowLabel[9] = new Label("LPI");
		for (int i = 0; i < 10; i++) {
			rowFill[i] = new Label();
			planField[i] = new TextField("0");
			actualField[i] = new TextField("0");
			toDateField[i] = new TextField("0");
			toDateField[i].setEnabled(false);
			toDateField[i].setEditable(false);
		}
		planField[3].setEnabled(false);
		planField[3].setEditable(false);
		planField[5].setBackground(Color.yellow);
		planField[6].setEnabled(false);
		planField[6].setEditable(false);
		planField[6].setBackground(new Color(210,210,255));
		actualField[3].setEnabled(false);
		actualField[3].setEditable(false);
		actualField[5].setBackground(new Color(210,210,255));
		actualField[5].setEnabled(false);
		actualField[5].setEditable(false);
		actualField[6].setBackground(Color.yellow);
		toDateField[5].setBackground(new Color(210,210,255));
		toDateField[6].setBackground(new Color(210,210,255));
    }
}
