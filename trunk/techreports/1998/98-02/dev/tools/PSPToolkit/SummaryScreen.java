import java.awt.*;


/**
 *
 * SummaryScreen.java
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
public class SummaryScreen extends ScreenBase {
    private Label columnLabel1;
    private Label columnLabel2;
    private Label columnLabel3;
    private Label columnLabel4;
    private Label[] rowLabel = new Label[12];
    private Label[] rowFill = new Label[12];
    protected TextField[] planField = new TextField[12];
    protected TextField[] actualField = new TextField[12];
    protected TextField[] toDateField = new TextField[12];



    /**
     * Create a new Summary Screen
     *
     *
     *
     */
    public SummaryScreen() {
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
			 , actualField[0], toDateField[0], rowFill[0]);
		addRow(this, rowLabel[1], planField[1]
			 , new Label(), toDateField[1], rowFill[1]);
		addRow(this, rowLabel[2], new Label()
			 , actualField[2], toDateField[2], rowFill[2]);
		addRow(this, rowLabel[3], new Label()
			 , new Label(), toDateField[3], rowFill[3]);
		for (int i = 4; i < 12; i++) {
			addRow(this, rowLabel[i], planField[i]
				, actualField[i], toDateField[i], rowFill[i]);
		}
		addLastRow(new Label());
    }



    /**
     * Create the rows.
     *
     */
    private void makeRows() {
		columnLabel1 = new Label("Summary");
		columnLabel2 = new Label("Plan");
		columnLabel3 = new Label("Actual");
		columnLabel4 = new Label("To Date");
		rowLabel[0] = new Label("LOC/Hour");
		rowLabel[1] = new Label("Planned Time (m)");
		rowLabel[2] = new Label("Actual Time (m)");
		rowLabel[3] = new Label("CPI");
		rowLabel[4] = new Label("% Reused (LOC)");
		rowLabel[5] = new Label("% New Reused (LOC)");
		rowLabel[6] = new Label("Test Defects/KLOC");
		rowLabel[7] = new Label("Total Defects/KLOC");
		rowLabel[8] = new Label("Yield %");
		rowLabel[9] = new Label("% Appraisal COQ");
		rowLabel[10] = new Label("% Failure COQ");
		rowLabel[11] = new Label("COQ A/F Ratio");
		for (int i = 0; i < 12; i++) {
			rowFill[i] = new Label();
			planField[i] = new TextField("0");
			planField[i].setEnabled(false);
			planField[i].setEditable(false);
			actualField[i] = new TextField("0");
			actualField[i].setEnabled(false);
			actualField[i].setEditable(false);
			toDateField[i] = new TextField("0");
			toDateField[i].setEnabled(false);
			toDateField[i].setEditable(false);
		}
		planField[0].setBackground(new Color(210,210,255));
		planField[1].setBackground(new Color(210,210,255));
		planField[2].setBackground(new Color(210,210,255));
		planField[6].setBackground(new Color(210,210,255));
		planField[7].setBackground(new Color(210,210,255));
		actualField[0].setBackground(new Color(210,210,255));
		actualField[1].setBackground(new Color(210,210,255));
		actualField[2].setBackground(new Color(210,210,255));
		actualField[6].setBackground(new Color(210,210,255));
		actualField[7].setBackground(new Color(210,210,255));
		toDateField[0].setBackground(new Color(210,210,255));
		toDateField[1].setBackground(new Color(210,210,255));
		toDateField[2].setBackground(new Color(210,210,255));
		toDateField[6].setBackground(new Color(210,210,255));
		toDateField[7].setBackground(new Color(210,210,255));
    }
}
