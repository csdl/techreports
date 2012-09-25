import java.awt.*;
import java.util.*;

/**
 * 
 * SummaryGraphs.java
 *
 *
 * @author Johan Forsman
 * @version 0.0
 * @modified Stefan Olofsson 1999
 *   Removed Object inheritance.
 *
 */
public class SummaryGraphs {
    Summary summary;



    /**
     *
     *
     * 
     *
     */
    SummaryGraphs() {
    }



    public void setSummary(Summary summary) {
		this.summary = summary;
    }



    /**
     * Paint LOC/Hour graph.
     *
     *
     *
     */
    public void locPerHourGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addXUnitLabel("Project #");
		gf.addSeries(getSummarySerie(0));
		gf.setBaseText("LOC / Hour");
		gf.show();
    }



    /**
     * Paint Test Defects / KLOC graph.
     *
     *
     *
     */
    public void testDefectsGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(6));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("Test Defects / KLOC");
		gf.show();
    }



    /**
     * Paint Total Defects / KLOC graph.
     *
     *
     *
     */
    public void totalDefectsGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(7));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("Total Defects / KLOC");
		gf.show();
    }



    /**
     * Paint Yield % graph.
     *
     *
     *
     */
    public void yieldGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(8));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("Yield %");
		gf.show();
    }



    /**
     * Paint % Appraisal COQ graph.
     *
     *
     *
     */
    public void appraisalGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(9));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("% Appraisal COQ");
		gf.show();
    }



    /**
     * Paint % Failure COQ graph.
     *
     *
     *
     */
    public void failureGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(10));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("% Failure COQ");
		gf.show();
    }



    /**
     * Paint COQ A/F Ratio graph.
     *
     *
     *
     */
    public void coqGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getSummarySerie(11));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("COQ A/F Ratio");
		gf.show();
    }



    /**
     * Paint DHDR graph.
     *
     *
     *
     */
    public void dhdrGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(0));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DHDR");
		gf.show();
    }



    /**
     * Paint DHCR graph.
     *
     *
     *
     */
    public void dhcrGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(1));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DHCR");
		gf.show();
    }



    /**
     * Paint DHC graph.
     *
     *
     *
     */
    public void dhcGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(2));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DHC");
		gf.show();
	}



    /**
     * Paint DHT graph.
     *
     *
     *
     */
    public void dhtGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(3));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DHT");
		gf.show();
    }



    /**
     * Paint DRL(DR/UT) graph.
     *
     *
     *
     */
    public void drutGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(4));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DRL(DR/UT)");
		gf.show();
    }



    /**
     * Paint DRL(CR/UT) graph.
     *
     *
     *
     */
    public void crutGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(5));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DRL(CR/UT)");
		gf.show();
    }



    /**
     * Paints DRL(C/UT) graph.
     *
     *
     *
     */
    public void cutGraph() {
		GraphFrame gf = new GraphFrame();
		gf.addSeries(getEfficiencySerie(6));
		gf.addXUnitLabel("Project #");
		gf.setBaseText("DRL(C/UT)");
		gf.show();
    }



    /**
     * Get a summary serie.
     *
     *
     *
     */
    private GraphData getSummarySerie(int index) {
		SummaryData actual = summary.getActual();
		SummaryList summaryList = summary.getSummaryList();
		GraphData serie = new GraphData();
		serie.type = serie.LINE;
		serie.color(new Color(200, 0, 0));
		Point tmpPoint;
		int i = 0;
		for (i = 0; i < summaryList.size(); i++) {
			tmpPoint = new Point(i, (summaryList.getActual(i)).getSummary(index));
			serie.addElement(tmpPoint);
		}
		tmpPoint = new Point(i, actual.getSummary(index));
		serie.addElement(tmpPoint);
		return serie;
    }



    /**
     * Get an efficiency serie.
     *
     *
     *
     */
    private GraphData getEfficiencySerie(int index) {
		SummaryData actual = summary.getActual();
		SummaryList summaryList = summary.getSummaryList();
		GraphData serie = new GraphData();
		serie.type = serie.LINE;
		serie.color(new Color(200, 0, 0));
		Point tmpPoint;
		int i = 0;
		for	(i = 0; i < summaryList.size(); i++) {
			tmpPoint = new Point(i, new Float((summaryList.getActual(i)).getEfficiency(index) * 100).intValue());
			serie.addElement(tmpPoint);
		}
		tmpPoint = new Point(i, new Float((actual.getEfficiency(i)) * 100).intValue());
		serie.addElement(tmpPoint);
		return serie;
    }



    /**
     * Paint time in phase graph.
     *
     *
     *
     */
    public void timeInPhaseGraph() {
		Vector v = new Vector();
		v.addElement(new GraphLabel("Plan", 0));
		v.addElement(new GraphLabel("HLdes", 1));
		v.addElement(new GraphLabel("HLdesR", 2));
		v.addElement(new GraphLabel("Des", 3));
		v.addElement(new GraphLabel("DesR", 4));
		v.addElement(new GraphLabel("Code", 5));
		v.addElement(new GraphLabel("CodeR", 6));
		v.addElement(new GraphLabel("Comp", 7));
		v.addElement(new GraphLabel("Test", 8));
		v.addElement(new GraphLabel("PM", 9));
		
		GraphFrame gf = new GraphFrame();
		SummaryData actual = summary.getActual();
		GraphData serie = new GraphData();

		serie.type = serie.BAR;
		serie.color(new Color(0, 100, 0));
		Point tmpPoint;
		for (int i = 0; i < 10; i++) {
			tmpPoint = new Point(i, actual.getTimeInPhase(i));
			serie.addElement(tmpPoint);
		}
		gf.addSeries(serie);
		gf.addXLabels(v);
		gf.addYUnitLabel("(m)");
		gf.setBaseText("Time In Phase");
		gf.show();
    }



    /**
     * Paint defects injected graph.
     *
     *
     *
     */
    public void defectsInjectedGraph() {
		Vector v = new Vector();
		v.addElement(new GraphLabel("Plan", 0));
		v.addElement(new GraphLabel("HLdes", 1));
		v.addElement(new GraphLabel("HLdesR", 2));
		v.addElement(new GraphLabel("Des", 3));
		v.addElement(new GraphLabel("DesR", 4));
		v.addElement(new GraphLabel("Code", 5));
		v.addElement(new GraphLabel("CodeR", 6));
		v.addElement(new GraphLabel("Comp", 7));
		v.addElement(new GraphLabel("Test", 8));
		v.addElement(new GraphLabel("PM", 9));

		GraphFrame gf = new GraphFrame();
		SummaryData actual = summary.getActual();
		GraphData serie = new GraphData();
		serie.type = serie.BAR;
		serie.color(new Color(0, 100, 0));
		Point tmpPoint;
		for (int i = 0; i < 9; i++) {
			tmpPoint = new Point(i, actual.getInjected(i));
			serie.addElement(tmpPoint);
		}
		gf.addSeries(serie);
		gf.addXLabels(v);
		gf.setBaseText("# Injected Defects");
		gf.show();
    }



    /**
     * Paint defects found graph.
     *
     *
     *
     */
    public void defectsFoundGraph() {
		Vector v = new Vector();
		v.addElement(new GraphLabel("Plan", 0));
		v.addElement(new GraphLabel("HLdes", 1));
		v.addElement(new GraphLabel("HLdesR", 2));
		v.addElement(new GraphLabel("Des", 3));
		v.addElement(new GraphLabel("DesR", 4));
		v.addElement(new GraphLabel("Code", 5));
		v.addElement(new GraphLabel("CodeR", 6));
		v.addElement(new GraphLabel("Comp", 7));
		v.addElement(new GraphLabel("Test", 8));
		v.addElement(new GraphLabel("PM", 9));

		GraphFrame gf = new GraphFrame();
		SummaryData actual = summary.getActual();
		GraphData serie = new GraphData();
		serie.type = serie.BAR;
		serie.color(new Color(0, 100, 0));
		Point tmpPoint;
		for (int i = 0; i < 9; i++) {
			tmpPoint = new Point(i, actual.getRemoved(i));
			serie.addElement(tmpPoint);
		}
		gf.addSeries(serie);
		gf.addXLabels(v);
		gf.setBaseText("# Found Defects");
		gf.show();
    }
}

