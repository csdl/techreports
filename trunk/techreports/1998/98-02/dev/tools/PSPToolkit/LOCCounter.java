import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 * 
 * LOCCounter.java
 *
 * Modeless or standalone LOC counter. Counts ";" in source files.
 *
 *
 * @author Stefan Olofsson 1999
 * @version 0.0
 *
 */

public class LOCCounter extends Frame implements ActionListener {
	private int LOCs = 0;
	boolean modeless = false;
	Label filenameLabel;
	Label locLabel;
	TextField filenameField;
	TextField locField;
	Button openButton;

	/**
     * Create a new LOC Counter.
     *
     * A modeless initializing implies that the parent application
		 * remains open when the LOCCounter is disposed.
     *
     */
	public LOCCounter(boolean modeless) {
		super("LOC Counter");
		this.modeless = modeless;
//		this.setResizable(false);

		Insets insets = this.getInsets();
		this.setSize(350 + insets.left + insets.right,
					 130 + insets.top + insets.bottom);
	
		MenuItem exit = new MenuItem("Exit");

		if (modeless) {
			exit.setLabel("Close");
			this.addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent e) {
					dispose();
				}
			});
		}
		else {
			this.addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent e) {
					System.exit(0);
				}
			});
		}

		MenuBar menubar = new MenuBar();
		this.setMenuBar(menubar);

		Menu file = new Menu("File");
		menubar.add(file);

		MenuItem open = new MenuItem("Browse files...", new MenuShortcut(KeyEvent.VK_O));
		open.setActionCommand("open");
		open.addActionListener(this);
		file.add(open);

		exit.setActionCommand("exit");
		exit.addActionListener(this);
		file.add(exit);

		openButton = new Button("Browse files...");
		openButton.addActionListener(this);
		openButton.setActionCommand("open");
		openButton.setSize(40,60);

		filenameField = new TextField(30);
		locField = new TextField(30);

		filenameField.setEnabled(false);
		filenameField.setEditable(false);
		locField.setEnabled(false);
		locField.setEditable(false);
		
		filenameLabel = new Label("Filename: ");
		locLabel = new Label("LOC: ");

		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(5,5,5,5);

		c.gridx = 0; c.gridy = 0; c.gridwidth = 3;
		c.gridheight = 1; c.weightx = c.weighty = 1.0;
		this.add(openButton, c);

		c.gridx = 0; c.gridy = 2; c.gridwidth = 1;
		this.add(filenameLabel, c);

		c.gridx = 2; c.gridwidth = 1;
		this.add(filenameField, c);

		c.gridx = 0; c.gridy = 3;
		this.add(locLabel, c);

		c.gridx = 2;
		this.add(locField, c);
	}

	/**
	*
	* Open and scan a file for ;'s and return the number found.
	*
	*/
	public int scanFile(String filename) throws IOException {
		int nr_of_lines = 0;
		File input_file = new File(filename);

		if (!input_file.exists()) {
			CommonDialog error = new CommonDialog(this,"Error","File doesn't exist: "+input_file,"OK",null,null);
		    error.show();
		    return -1;
		}
		
		if (!input_file.isFile()) {
			CommonDialog error = new CommonDialog(this,"Error","Not a file: "+input_file,"OK",null,null);
		    error.show();
		    return -1;
		}

		if (!input_file.canRead()) {
			CommonDialog error = new CommonDialog(this,"Error","Can't read file: "+input_file,"OK",null,null);
		    error.show();
		    return -1;
		}


		FileInputStream input = null;

		try {
			input = new FileInputStream(input_file);
			byte[] buffer = new byte[4096];
			int bytes_read;
			StringTokenizer st;
			while((bytes_read = input.read(buffer)) != -1) {
				st = new StringTokenizer(new String(buffer),";",false);
				nr_of_lines += st.countTokens();
			}
			return nr_of_lines-1;
		}
		finally {
			if (input != null) try { input.close(); } catch (IOException e) {}
		}
	}

	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		if ((command.equals("exit")) && (modeless)) {
			this.dispose();
		}
		else if (command.equals("exit")) {
			System.exit(0);
		}
		else if (command.equals("open")) {
			FileDialog d = new FileDialog(this, "Open File", FileDialog.LOAD);
			d.show();
			if (!(d.getFile() == null)) {
				try { 
					int tmpInt = scanFile(d.getDirectory()+d.getFile());
					if (tmpInt != -1) {
						filenameField.setText(d.getFile());
						locField.setText(tmpInt+"");
					}
				} 
				catch (IOException ex) {}
			}
			d.dispose();
		}
	}

	public static void main(String args[]) {
		LOCCounter wLOC = new LOCCounter(false);
		wLOC.show();
	}
}