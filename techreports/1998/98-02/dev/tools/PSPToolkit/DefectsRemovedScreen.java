import java.awt.*;


/**
 *
 * DefectsRemovedScreen.java
 *
 * 
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Edit boxes are both disabled (for Windows users)
 *     and non-editable (for UNIX users).
 *
 */
public class DefectsRemovedScreen extends ScreenBase {
    private Label columnLabel1;
    private Label columnLabel2;
    private Label columnLabel3;
    private Label columnLabel4;
    private Label columnLabel5;
    private Label[] rowLabel = new Label[11];
    private Label[] rowFill = new Label[11];
    protected TextField[] planField = new TextField[11];
    protected TextField[] actualField = new TextField[11];
    protected TextField[] toDateField = new TextField[11];
    protected TextField[] toDateProcField = new TextField[11];



    /**
     * Create a new Defects Removed Screen.
     *
     *
     *
     */
    public DefectsRemovedScreen() {
		setLayout(gridBagLayout);
		makeRows();
		addComponents();
    }



    /**
     * Add the components.
     *
     *
     * @return true if succeded.
     */
    private boolean addComponents() {
		addRow(this, columnLabel1, columnLabel2
			 , columnLabel3, columnLabel4, columnLabel5
			 , new Label());
		for (int i = 0; i < 10; i++) {
			addRow(this, rowLabel[i], planField[i]
				, actualField[i], toDateField[i], toDateProcField[i]
				, rowFill[i]);
		}
		addRow(this, rowLabel[10], planField[10]
			 , actualField[10], toDateField[10], new Label()
			 , rowFill[10]);
		addLastRow(new Label());
		return true;
    }



    /**
     * Create the rows.
     *
     */
    private void makeRows() {
		columnLabel1 = new Label("Defects Removed");
		columnLabel2 = new Label("Plan");
		columnLabel3 = new Label("Actual");
		columnLabel4 = new Label("To Date");
		columnLabel5 = new Label("To Date %");
		rowLabel[0] = new Label("Planning");
		rowLabel[1] = new Label("HL Design");
		rowLabel[2] = new Label("HL Design Review");
		rowLabel[3] = new Label("Design");
		rowLabel[4] = new Label("Design Review");
		rowLabel[5] = new Label("Code");
		rowLabel[6] = new Label("Code Review");
		rowLabel[7] = new Label("Compile");
		rowLabel[8] = new Label("Test");
		rowLabel[9] = new Label("Total Developement");
		rowLabel[10] = new Label("After Developement");
		for (int i = 0; i < 11; i++) {
			rowFill[i] = new Label();
			planField[i] = new TextField("0");
			if (i < 9) planField[i].setBackground(Color.yellow);
			actualField[i] = new TextField("0");
			actualField[i].setEnabled(false);
			actualField[i].setEditable(false);
			if (i < 10) actualField[i].setBackground(new Color(210,210,255));
			toDateField[i] = new TextField("0");
			toDateField[i].setEnabled(false);
			toDateField[i].setEditable(false);
			if (i < 10) toDateField[i].setBackground(new Color(210,210,255));
			toDateProcField[i] = new TextField("0");
			toDateProcField[i].setEnabled(false);
			toDateProcField[i].setEditable(false);
			if (i < 10) toDateProcField[i].setBackground(new Color(210,210,255));
		}
		actualField[10].setEditable(true);
		planField[9].setEnabled(false);
		planField[9].setEditable(false);
		planField[9].setBackground(new Color(210,210,255));
    }
}
