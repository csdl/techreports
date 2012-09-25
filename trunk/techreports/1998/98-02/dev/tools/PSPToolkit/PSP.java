import java.awt.*;
import java.awt.event.*;
import java.io.*;

/**
 * 
 * PSP.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Added menu constraints (menus disabled when unusable).
 *   Added shortcuts to menu items.
 *   Separated menus.
 *   Removed all deprecated API.
 *   Removed defect: Opening an erroneous file and then New would yield a 
 *					 fatal error.
 *   PSPFileException exchanged for Exception.
 *   The user is now prompted to save a changed project
 *     before a program exit can be accomplished.
 *   Only one filedialog is kept in memory so the pathways
 *     are kept between loading project files (also speeds up execution).
 *	 To indicate whether a project exists in memory or not, projectExists is
 *     used instead of isOpen and isNew in Project.java.
 *
 */
public class PSP extends Frame implements ActionListener {
    private boolean inApplet = true;
    private Project project;				// project
	private boolean projectExists;			// is there a project in memory?
	private Dimension windowSize;

	private FileDialog fileDialog;

    private MenuBar menuBar;				// menubar

    private Menu fileMenu;					// file
    private MenuItem newMenuItem;			// new
    private MenuItem openMenuItem;			// open
    private MenuItem closeMenuItem;			// close
    private MenuItem saveMenuItem;			// save
    private MenuItem saveAsMenuItem;		// save as
    private MenuItem projectInfoMenuItem;	// project info
    private MenuItem historyMenuItem;		// history
    private MenuItem exitMenuItem;			// exit

    private Menu toolsMenu;							// tools
    private MenuItem timeLogMenuItem;				// time log
    private MenuItem defectLogMenuItem;				// defect log
    private MenuItem summaryMenuItem;				// summary
	private MenuItem LOCMenuItem;					// LOC counter

    private Menu graphMenu;							// graphs
    private MenuItem timeInPhaseGraphMenuItem;		// time in phase
    private MenuItem defectsInjectedGraphMenuItem;	// defects injected
    private MenuItem defectsRemovedGraphMenuItem;	// defects removed
    private MenuItem locPerHourGraphMenuItem;		// LOC/hour
    private MenuItem testDefectsGraphMenuItem;
    private MenuItem totalDefectsGraphMenuItem;
    private MenuItem yieldGraphMenuItem;
    private MenuItem appraisalGraphMenuItem;
    private MenuItem failureGraphMenuItem;
    private MenuItem coqGraphMenuItem;
    private MenuItem dhdrGraphMenuItem;
    private MenuItem dhcrGraphMenuItem;
    private MenuItem dhcGraphMenuItem;
    private MenuItem dhtGraphMenuItem;
    private MenuItem drutGraphMenuItem;
    private MenuItem crutGraphMenuItem;
    private MenuItem cutGraphMenuItem;

    private Panel cards;							// cards for layout
    private ProjectScreen projectScreen;			// project screen
    private TimeLogScreen timeLogScreen;			// time screen
    private DefectLogScreen defectLogScreen;		// defect screen
    private ProjectSummaryScreen projectSummaryScreen;// summary screen
    private HistoryScreen historyScreen;			// history screen
    private SummaryGraphs summaryGraphs;			// summary graphs



    /**
     * Create a new PSP Toolkit.
     *
     * 
     *
     */
    public PSP() {
		projectExists = false;

		fileDialog = new FileDialog(new Frame());

		this.enableEvents(AWTEvent.COMPONENT_EVENT_MASK);

		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				if (projectExists) {
					close();
					if ((!project.hasChanged()) && !projectExists) {
						System.exit(0);
					}
				}
				else {
					System.exit(0);
				}
			}
		});

		makeMenu();
		cards = new Panel();
		cards.setLayout(new CardLayout());
		cards.add("Empty", new Panel());

		projectScreen = new ProjectScreen();
		cards.add("Info", projectScreen);

		timeLogScreen = new TimeLogScreen();
		cards.add("Time", timeLogScreen);

		defectLogScreen = new DefectLogScreen();
		cards.add("Def", defectLogScreen);

		projectSummaryScreen = new ProjectSummaryScreen();
		cards.add("Sum", projectSummaryScreen);

		historyScreen = new HistoryScreen();
		cards.add("Hist", historyScreen);

		setLayout(new BorderLayout());
		add("Center", cards);

		summaryGraphs = new SummaryGraphs();
    }



	/**
	 * When the PSP window is resized, this method takes care of
	 * keeping the components from getting squeezed or invisible.
	 *
	 */
	public void processComponentEvent(ComponentEvent e) {
		if (e.getID() == ComponentEvent.COMPONENT_RESIZED) {
			Dimension d = new Dimension(this.getSize());
			windowSize = new Dimension(this.getPreferredSize());
			if (d.height > windowSize.height)
				windowSize.height = d.height;
			if (d.width > windowSize.width)
				windowSize.width = d.width;
			this.setSize(windowSize);
		}
	}



    /**
     * Create main menu.
     *
     * 
     * @return true if creation OK
     */
    private void makeMenu() {
		menuBar = new MenuBar();
		this.setMenuBar(menuBar);

		fileMenu = new Menu("File");
		newMenuItem = new MenuItem("New", new MenuShortcut(KeyEvent.VK_N));
		openMenuItem = new MenuItem("Open...", new MenuShortcut(KeyEvent.VK_O));
		closeMenuItem = new MenuItem("Close");
		saveMenuItem = new MenuItem("Save", new MenuShortcut(KeyEvent.VK_S));
		saveAsMenuItem = new MenuItem("Save as...");
		projectInfoMenuItem = new MenuItem("Project Info", new MenuShortcut(KeyEvent.VK_I));
		historyMenuItem = new MenuItem("History");
		exitMenuItem = new MenuItem("Exit");

		fileMenu.add(newMenuItem);
		fileMenu.add(openMenuItem);
		fileMenu.add(closeMenuItem);
		fileMenu.addSeparator();
		fileMenu.add(saveMenuItem);
		fileMenu.add(saveAsMenuItem);
		fileMenu.addSeparator();
		fileMenu.add(projectInfoMenuItem);
		fileMenu.add(historyMenuItem);
		fileMenu.addSeparator();
		fileMenu.add(exitMenuItem);
 
		closeMenuItem.setEnabled(false);
		saveMenuItem.setEnabled(false);	  
		saveAsMenuItem.setEnabled(false);
		projectInfoMenuItem.setEnabled(false);
		historyMenuItem.setEnabled(false);

		newMenuItem.addActionListener(this);
		openMenuItem.addActionListener(this);
		closeMenuItem.addActionListener(this);
		saveMenuItem.addActionListener(this);
		saveAsMenuItem.addActionListener(this);
		projectInfoMenuItem.addActionListener(this);
		historyMenuItem.addActionListener(this);
		exitMenuItem.addActionListener(this);
	  	  
		toolsMenu = new Menu("Tools");
		timeLogMenuItem = new MenuItem("Time Log");
		defectLogMenuItem = new MenuItem("Defect Log");
		summaryMenuItem = new MenuItem("Project Plan Summary (PPS)");
		LOCMenuItem = new MenuItem("LOC Counter");

		toolsMenu.add(timeLogMenuItem);
		toolsMenu.add(defectLogMenuItem);
		toolsMenu.add(summaryMenuItem);
		toolsMenu.add(LOCMenuItem);

		toolsMenu.setEnabled(false);
	  
		timeLogMenuItem.addActionListener(this);
		defectLogMenuItem.addActionListener(this);
		summaryMenuItem.addActionListener(this);
		LOCMenuItem.addActionListener(this);
  	  
		graphMenu = new Menu("Graphs");
		timeInPhaseGraphMenuItem = new MenuItem("Time In Phase");
		defectsInjectedGraphMenuItem = new MenuItem("Defects Injected");
		defectsRemovedGraphMenuItem = new MenuItem("Defects Removed");
		locPerHourGraphMenuItem = new MenuItem("LOC / Hour");
		testDefectsGraphMenuItem = new MenuItem("Test Defects / KLOC");
		totalDefectsGraphMenuItem = new MenuItem("Total Defects / KLOC");
		yieldGraphMenuItem = new MenuItem("Yield");
		appraisalGraphMenuItem = new MenuItem("Appraisal COQ");
		failureGraphMenuItem = new MenuItem("Failure COQ");
		coqGraphMenuItem = new MenuItem("COQ A/F Ratio");
		dhdrGraphMenuItem = new MenuItem("DHDR");
		dhcrGraphMenuItem = new MenuItem("DHCR");
		dhcGraphMenuItem = new MenuItem("DHC");
		dhtGraphMenuItem = new MenuItem("DHT");
		drutGraphMenuItem = new MenuItem("DRL(DR/UT)");
		crutGraphMenuItem = new MenuItem("DRL(CR/UT)");
		cutGraphMenuItem = new MenuItem("DRL(C/UT)");
	  
		graphMenu.setEnabled(false);
	  
		graphMenu.add(timeInPhaseGraphMenuItem);
		graphMenu.add(defectsInjectedGraphMenuItem);
		graphMenu.add(defectsRemovedGraphMenuItem);
		graphMenu.add(locPerHourGraphMenuItem);
		graphMenu.add(testDefectsGraphMenuItem);
		graphMenu.add(totalDefectsGraphMenuItem);
		graphMenu.add(yieldGraphMenuItem);
		graphMenu.add(appraisalGraphMenuItem);
		graphMenu.add(failureGraphMenuItem);
		graphMenu.add(coqGraphMenuItem);
		graphMenu.add(dhdrGraphMenuItem);
		graphMenu.add(dhcrGraphMenuItem);
		graphMenu.add(dhcGraphMenuItem);
		graphMenu.add(dhtGraphMenuItem);
		graphMenu.add(drutGraphMenuItem);
		graphMenu.add(crutGraphMenuItem);
		graphMenu.add(cutGraphMenuItem);

		timeInPhaseGraphMenuItem.addActionListener(this);
		defectsInjectedGraphMenuItem.addActionListener(this);
		defectsRemovedGraphMenuItem.addActionListener(this);
		locPerHourGraphMenuItem.addActionListener(this);
		testDefectsGraphMenuItem.addActionListener(this);
		totalDefectsGraphMenuItem.addActionListener(this);
		yieldGraphMenuItem.addActionListener(this);
		appraisalGraphMenuItem.addActionListener(this);
		failureGraphMenuItem.addActionListener(this);
		coqGraphMenuItem.addActionListener(this);
		dhdrGraphMenuItem.addActionListener(this);
		dhcrGraphMenuItem.addActionListener(this);
		dhcGraphMenuItem.addActionListener(this);
		dhtGraphMenuItem.addActionListener(this);
		drutGraphMenuItem.addActionListener(this);
		crutGraphMenuItem.addActionListener(this);
		cutGraphMenuItem.addActionListener(this);

		menuBar.add(fileMenu);
		menuBar.add(toolsMenu);
		menuBar.add(graphMenu);
	}


    /**
     * Set the associations.
     *
     *
     * @return true if the associations were set.
     */
    private void setAssociations() {
		projectScreen.setProjectData(project.getProjectData());
		timeLogScreen.setTimeLog(project.getTimeLog());
		defectLogScreen.setDefectLog(project.getDefectLog());
		projectSummaryScreen.setSummary(project.getSummary());
		historyScreen.setSummary(project.getSummary());
		summaryGraphs.setSummary(project.getSummary());
	}



    /**
     * Create a new project.
     *
     */
	public void newProject() {
		if (projectExists) {
			close();
			if (!project.hasChanged()) {
				project = new Project();
				setAssociations();
				projectExists = true;
			}
		}
		else {
			project = new Project();
			setAssociations();
			projectExists = true;
		}
    }



    /**
     * Save the project as.
     *
     *
     * @return true if the project was saved.
     */
    private boolean saveAs() {
		if (projectExists) {
			projectScreen.save();
			fileDialog.setMode(FileDialog.SAVE);
			fileDialog.setTitle("Save project");
			fileDialog.setVisible(true);
			if (fileDialog.getFile() != null) {
				project.setFileName(fileDialog.getFile());
				project.setPath(fileDialog.getDirectory());
				fileDialog.setVisible(false);
				fileDialog.setFile("");
			}
			else
				return false;
			try {
				project.save();
				showMenus();
			}
			catch (Exception e) {
				CommonDialog error = new CommonDialog(this,"Save error",e.getMessage(),"OK",null,null);
				error.show();
				return false;
			}
		}
		return true;
	}


    /**
     * Save the project.
     *
     *
     * @return true if the project was saved.
     */
    private boolean save() {
		if (projectExists) {
			if (!project.isSavedBefore()) {
				saveAs();
			}
			else if (project.hasChanged()) {
				projectScreen.save();
				try {
					project.save();
					showMenus();
				}
				catch (Exception e) {
					CommonDialog error = new CommonDialog(this,"Save error",e.getMessage(),"OK",null,null);
					error.show();
					return false;
				}
			}
		}
		return true;
	}


    /**
     * Close the project.
     *
     *
     * @return true if project was closed.
     */
    private void close() {
		if (projectExists) {
			if (project.hasChanged()) {
				projectScreen.save();
				CommonDialog d = new CommonDialog(this,"Close project","There are unsaved changes. Save now?","Yes","No","Cancel");
				d.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						if (e.getActionCommand().equals("yes")) {
							if (save()) {
								project.setChanged(false);
								projectExists = false;
								((CardLayout)cards.getLayout()).show(cards, "Empty");
								hideMenus();
								timeLogScreen.clear();
								defectLogScreen.clear();
							}
						}
						else if (e.getActionCommand().equals("no")) {
							project.setChanged(false);
							projectExists = false;
							((CardLayout)cards.getLayout()).show(cards, "Empty");
							hideMenus();
							timeLogScreen.clear();
							defectLogScreen.clear();
						}
					}
				});
				d.show();
			}
			else {
				projectExists = false;
				((CardLayout)cards.getLayout()).show(cards, "Empty");
				hideMenus();
				timeLogScreen.clear();
				defectLogScreen.clear();
			}
		}
    }



    /**
     * Open a project.
     *
     *
     * @return true if a project was opened.
     */
	public boolean open() {
		newProject();
		if (!project.hasChanged()) {
			fileDialog.setMode(FileDialog.LOAD);
			fileDialog.setTitle("Open project");
			fileDialog.setVisible(true);
			if (fileDialog.getFile() != null) {
				project.setFileName(fileDialog.getFile());
				project.setPath(fileDialog.getDirectory());
				fileDialog.setVisible(false);
				fileDialog.setFile("");
			}
			else {
				return false;
			}
			try {
				project.open();
			}
			catch (Exception e) {
				projectExists = false;
				CommonDialog error = new CommonDialog(this,"Error",e.getMessage(),"OK",null,null);
				error.show();
				return false;
			}
			return true;
		}
		else
			return false;
	}


	public void hideMenus() {
		closeMenuItem.setEnabled(false);
		saveMenuItem.setEnabled(false);
		saveAsMenuItem.setEnabled(false);
		toolsMenu.setEnabled(false);
		graphMenu.setEnabled(false);
		projectInfoMenuItem.setEnabled(false);
		historyMenuItem.setEnabled(false);
		super.setTitle("PSP: ");
	}

	public void showMenus() {
		closeMenuItem.setEnabled(true);
		saveMenuItem.setEnabled(true);
		saveAsMenuItem.setEnabled(true);
		toolsMenu.setEnabled(true);
		graphMenu.setEnabled(true);
		projectInfoMenuItem.setEnabled(true);
		historyMenuItem.setEnabled(true);
		super.setTitle("PSP: " + project.getFileName());
	}


    /**
     * 
	 * Handle action events from the main menu.
     *
     *  
     * 
     */
    public void actionPerformed(ActionEvent e) {
		if (e.getSource() == newMenuItem) {			// new
			newProject();
			projectScreen.read();
			timeLogScreen.read();
			defectLogScreen.read();
			projectSummaryScreen.read();
			historyScreen.read();
//			closeMenuItem.setEnabled(true);
//			saveMenuItem.setEnabled(true);
//			saveAsMenuItem.setEnabled(true);
//			projectInfoMenuItem.setEnabled(false);
//			historyMenuItem.setEnabled(false);
//			toolsMenu.setEnabled(false);
//			graphMenu.setEnabled(false);
			showMenus();
			((CardLayout)cards.getLayout()).show(cards, "Info");
			super.setTitle("PSP: ");
		}
		else if (e.getSource() == openMenuItem) {			// open
			if (open() == true) {
				projectScreen.read();
				timeLogScreen.read();
				defectLogScreen.read();
				projectSummaryScreen.read();
				historyScreen.read();
				showMenus();
				((CardLayout)cards.getLayout()).show(cards, "Info");
			}
		}
		else if (e.getSource() == closeMenuItem) {   // close
			close();
		}
		else if (e.getSource() == saveMenuItem) {			// save
			save();
		}
		else if (e.getSource() == saveAsMenuItem) {			// save as
			saveAs();
		}
		else if (e.getSource() == projectInfoMenuItem) {		// project info
			//projectScreen.read();
			((CardLayout)cards.getLayout()).show(cards, "Info");
		}
		else if (e.getSource() == historyMenuItem) {
			((CardLayout)cards.getLayout()).show(cards, "Hist");
		}
		else if (e.getSource() == exitMenuItem) {				// exit
			if (projectExists) {
				close();
				if ((!project.hasChanged()) && !projectExists) {
					System.exit(0);
				}
			}
			else {
				System.exit(0);
			}
		}
		else if (e.getSource() == timeLogMenuItem) {
//			projectSummaryScreen.save();
			((CardLayout)cards.getLayout()).show(cards, "Time");
		}
		else if (e.getSource() == defectLogMenuItem) {
//			projectSummaryScreen.save();
			((CardLayout)cards.getLayout()).show(cards, "Def");
		}
		else if (e.getSource() == summaryMenuItem)
			((CardLayout)cards.getLayout()).show(cards, "Sum");
		else if (e.getSource() == LOCMenuItem) {
			LOCCounter locCounter = new LOCCounter(true);
			locCounter.show();
		}
		else if (e.getSource() == timeInPhaseGraphMenuItem)
		    summaryGraphs.timeInPhaseGraph();
		else if (e.getSource() == defectsInjectedGraphMenuItem)
			summaryGraphs.defectsInjectedGraph();
		else if (e.getSource() == defectsRemovedGraphMenuItem)
		    summaryGraphs.defectsFoundGraph();
		else if (e.getSource() == locPerHourGraphMenuItem)
			summaryGraphs.locPerHourGraph();
		else if (e.getSource() == testDefectsGraphMenuItem)
		    summaryGraphs.testDefectsGraph();
		else if (e.getSource() == totalDefectsGraphMenuItem)
		    summaryGraphs.totalDefectsGraph();
		else if (e.getSource() == yieldGraphMenuItem)
			summaryGraphs.yieldGraph();
		else if (e.getSource() == appraisalGraphMenuItem)
		    summaryGraphs.appraisalGraph();
		else if (e.getSource() == failureGraphMenuItem)
			summaryGraphs.failureGraph();
		else if (e.getSource() == coqGraphMenuItem)
			summaryGraphs.coqGraph();
		else if (e.getSource() == dhdrGraphMenuItem)
			summaryGraphs.dhdrGraph();
		else if (e.getSource() == dhcrGraphMenuItem)
			summaryGraphs.dhcrGraph();
		else if (e.getSource() == dhcGraphMenuItem)
			summaryGraphs.dhcGraph();
		else if (e.getSource() == dhtGraphMenuItem)
			summaryGraphs.dhtGraph();
		else if (e.getSource() == drutGraphMenuItem)
			summaryGraphs.drutGraph();
		else if (e.getSource() == crutGraphMenuItem)
			summaryGraphs.crutGraph();
		else if (e.getSource() == cutGraphMenuItem)
			summaryGraphs.cutGraph();
    }


    /**
     * The main program.
     *
     *  
     * 
     */
    public static void main(String args[]) {		// run as an application
		PSP window = new PSP();
		window.inApplet = false; 					// not in applet
		Insets insets = window.getInsets();
		window.setTitle("PSP");
//		windowSize = new Dimension(500 + insets.left + insets.right,
//			      450 + insets.top + insets.bottom);
		window.pack();
		window.setSize(window.getPreferredSize());
		window.show();
    }
}
