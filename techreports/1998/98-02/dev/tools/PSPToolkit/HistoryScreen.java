import java.awt.*;
import java.awt.event.*;


/**
 *
 * HistoryScreen.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed all deprecated API.
 *   Removed setGridBagConstraints method (exists in ScreenBase).
 *
 */
public class HistoryScreen extends ScreenBase implements ActionListener {
    private Button addButton;
    private Button deleteButton;
    private List historyList;
    private Summary summary;
    private SummaryList summaryList;
	FileDialog fileDialog;



    /**
     * Create a new history screen.
     *
     *
     *
     */
    public HistoryScreen() {
		addButton = new Button("Add");
		addButton.setActionCommand("add");
		deleteButton = new Button("Delete");
		deleteButton.setActionCommand("delete");
		addButton.addActionListener(this);
		deleteButton.addActionListener(this);
		errorLabel = new Label();
		historyList = new List();
		gridBagLayout = new GridBagLayout();
		setLayout(gridBagLayout);
		setGridBagConstraints();
		addRow(this, addButton, deleteButton);
		addRow(this, errorLabel);
		addLastRow(historyList);

		fileDialog = new FileDialog(new Frame()
									, "Open"
									, FileDialog.LOAD);
    }



    /**
     * Set the summary association.
     *
     * @param summary the summary.
     */
    public void setSummary(Summary summary) {
		this.summary = summary;
		this.summaryList = summary.getSummaryList();
    }



    /**
     * Read the history.
     *
     */
    public void read() {
		historyList.removeAll();
		for (int index = 0; index < summaryList.size(); index++)
		    historyList.add(summaryList.getPathFile(index));
		summaryList.read();
    }



    /**
     * Add an old project.
     *
     */
    private void add() {
		fileDialog.setVisible(true);
		if (fileDialog.getFile() != null) {
			historyList.add(fileDialog.getDirectory()
				      + fileDialog.getFile());
			summaryList.addPathFile(fileDialog.getDirectory()
					  , fileDialog.getFile());
		}
		fileDialog.setVisible(false);
    }



    /**
     * Remove an old project.
     *
     */
    private void delete() {
		int index = historyList.getSelectedIndex();
		if (index >= 0) {
			summaryList.removeElementAt(index);
			historyList.remove(index);
		}
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
		if (command.equals("delete")) {
			delete();
		}
    }
}
