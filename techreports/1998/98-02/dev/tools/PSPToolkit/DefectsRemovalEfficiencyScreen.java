import java.awt.*;



/**
 *
 * DefectsRemovalEfficiencyScreen.java
 *
 * 
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Edit boxes are both disabled (for Windows users)
 *     and non-editable (for UNIX users).
 *
 */
public class DefectsRemovalEfficiencyScreen extends ScreenBase {
    private Label columnLabel1;
    private Label columnLabel2;
    private Label columnLabel3;
    private Label columnLabel4;
    private Label[] rowLabel = new Label[7];
    private Label[] rowFill = new Label[7];
    protected TextField[] planField = new TextField[7];     // why protected??
    protected TextField[] actualField = new TextField[7];
    protected TextField[] toDateField = new TextField[7];



    /**
     * Create a new DefectsRemovalEfficiencyScreen.
     *
     *
     *
     */
    public DefectsRemovalEfficiencyScreen() {
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
		for (int i = 0; i < 7; i++) {
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
		columnLabel1 = new Label("Defects Removal Efficiency");
		columnLabel2 = new Label("Plan");
		columnLabel3 = new Label("Actual");
		columnLabel4 = new Label("To Date");
		rowLabel[0] = new Label("DHDR");
		rowLabel[1] = new Label("DHCR");
		rowLabel[2] = new Label("DHC");
		rowLabel[3] = new Label("DHT");
		rowLabel[4] = new Label("DRL(DR/UT)");
		rowLabel[5] = new Label("DRL(CR/UT)");
		rowLabel[6] = new Label("DRL(C/UT)");	  
		for (int i = 0; i < 7; i++) {
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
    }
}
