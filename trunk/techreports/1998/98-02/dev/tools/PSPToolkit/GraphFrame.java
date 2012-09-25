import java.awt.*;
import java.awt.event.*;
import java.util.*;



/**
 * 
 * GraphFrame.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Updated all deprecated API.
 *
 */
public class GraphFrame extends Frame implements ActionListener {
    private boolean inApplet = true;
    private Button closeButton;
    private Panel buttonPanel;
    private Graph graph;


    /**
     *
     * Create new GraphFrame.
     * 
     *
     */
    public GraphFrame() {

		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) { dispose(); }
		});

		buttonPanel = new Panel();
		closeButton = new Button("Close");
		buttonPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
		buttonPanel.add(closeButton);
		closeButton.addActionListener(this);
		graph = new Graph();
		setLayout(new BorderLayout());
		add("North", buttonPanel);
		add("Center", graph);

		Insets insets = super.getInsets();
		setTitle("PSP");
		setSize(500 + insets.left + insets.right,
				300 + insets.top + insets.bottom);

	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == closeButton) {
			this.dispose();
		}
	}

    public void addSeries(GraphData graphData) {
		graph.addSeries(graphData);
	}

    public void setBaseText(String text) {
		graph.setBaseText(text);
    }

	public void addXUnitLabel(String xst) {
		graph.addXUnitLabel(xst);
	}

	public void addYUnitLabel(String yst) {
		graph.addYUnitLabel(yst);
	}
    
	public void addXLabels(Vector gl) {
		graph.addXLabels(gl);
	}

    public static void main(String[] args) {
		GraphFrame gf = new GraphFrame();
		gf.inApplet = false;
		gf.show();
    }
}
