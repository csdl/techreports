import java.awt.*;


/**
 *
 * TimeInPhaseScreen.java
 *
 * 
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Edit boxes are both disabled (for Windows users)
 *     and non-editable (for UNIX users).
 *   Added units to labels.
 *
 */
public class TimeInPhaseScreen extends ScreenBase {
    private Label columnLabel1;
    private Label columnLabel2;
    private Label columnLabel3;
    private Label columnLabel4;
    private Label columnLabel5;
    private Label[] rowLabel = new Label[13];
    private Label[] rowFill = new Label[13];
    protected TextField[] planField = new TextField[13];
    protected TextField[] actualField = new TextField[13];
    protected TextField[] toDateField = new TextField[13];
    protected TextField[] toDateProcField = new TextField[13];



    /**
     * Creates a new Time In Phase.
     *
     *
     *
     */
    public TimeInPhaseScreen() {
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
			, columnLabel3, columnLabel4, columnLabel5
			, new Label());
		for (int i = 0; i < 11; i++) {
			addRow(this, rowLabel[i], planField[i]
				, actualField[i], toDateField[i], toDateProcField[i]
				, rowFill[i]);
		}
		addRow(this, rowLabel[11], planField[11], rowFill[11]);
		addRow(this, rowLabel[12], planField[12], rowFill[12]);
		addLastRow(new Label());
    }



    /**
     * Create the rows.
     *
     */
    private void makeRows() {
		columnLabel1 = new Label("Time In Phase (m)");
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
		rowLabel[9] = new Label("Post-Mortem");
		rowLabel[10] = new Label("Total");
		rowLabel[11] = new Label("Total UPI");
		rowLabel[12] = new Label("Total LPI");
		for (int i = 0; i < 13; i++) {
			rowFill[i] = new Label();
			planField[i] = new TextField("0");
			if (i < 10) planField[i].setBackground(Color.yellow);
			actualField[i] = new TextField("0");
			actualField[i].setEnabled(false);
			actualField[i].setEditable(false);
			if (i < 11) actualField[i].setBackground(new Color(210,210,255));
			toDateField[i] = new TextField("0");
			toDateField[i].setEnabled(false);
			toDateField[i].setEditable(false);
			if (i < 11) toDateField[i].setBackground(new Color(210,210,255));
			toDateProcField[i] = new TextField("0");
			toDateProcField[i].setEnabled(false);
			toDateProcField[i].setEditable(false);
			if (i < 11) toDateProcField[i].setBackground(new Color(210,210,255));
		}
		planField[10].setEnabled(false);
		planField[10].setEditable(false);
		planField[10].setBackground(new Color(210,210,255));
//		actualField[10].setEnabled(false);
//		actualField[10].setEditable(false);
    }
}
