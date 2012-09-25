import java.awt.*;
import java.awt.event.*;


/**
 * 
 * CommonDialog.java
 *
 *
 * @author Stefan Olofsson 1999
 * @version 0.0
 *    
 */
class CommonDialog extends Dialog {
    
    /**
     * Creates a new Yes/No Dialog.
     *
     * @param parent the parent of the dialog.
     * @param message the message.
     *
     */
    public CommonDialog(Frame parent, String title, String message,
				String yes_label, String no_label, String cancel_label) {
		
		super(parent, title, true);
		
		this.setLayout(new BorderLayout(15,15));

		Panel msgbox = new Panel();
		Label label = new Label(message);
		msgbox.add(label);
		this.add("Center", msgbox);

		ActionListener listener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				CommonDialog.this.dispose();
				if (listeners != null)
					listeners.actionPerformed(new ActionEvent(CommonDialog.this,e.getID(),e.getActionCommand()));
			}
		};

		Panel buttonbox = new Panel();
		buttonbox.setLayout(new FlowLayout(FlowLayout.CENTER,25,15));
		this.add("South",buttonbox);

		if (yes_label != null) {
			Button yes = new Button(yes_label);
			yes.setActionCommand("yes");
			yes.addActionListener(listener);
			buttonbox.add(yes);
		}

		if (no_label != null) {
			Button no = new Button(no_label);
			no.setActionCommand("no");
			no.addActionListener(listener);
			buttonbox.add(no);
		}

		if (cancel_label != null) {
			Button cancel = new Button(cancel_label);
			cancel.setActionCommand("cancel");
			cancel.addActionListener(listener);
			buttonbox.add(cancel);
		}
		
		this.pack();
    }

	
	protected ActionListener listeners = null;

	/**
	 *  Include a listener in the list of registered ActionListeners.
	 */
	public void addActionListener(ActionListener l) {
		listeners = AWTEventMulticaster.add(listeners,l);
	}

	/**
	 * Remove a listener from the list of registered ActionListeners.
	 */
	public void removeActionListener(ActionListener l) {
		listeners = AWTEventMulticaster.remove(listeners,l);
	}
}


