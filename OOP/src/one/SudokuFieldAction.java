package one;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JOptionPane;

/**
 * Action listener to handle button press events for
 * buttons associated with Sudoku fields.
 */
public class SudokuFieldAction implements ActionListener {

	private final GameGrid game;
	private final int column;	
	private final int row;

    /**
     * Create a new action listener.
     * @param game The Sudoku game grid associated with this window.
     * @param col the column coordinate of this button
     * @param row the row coordinate of this button
     */    
	public SudokuFieldAction(GameGrid game, int col, int row) {
		this.game = game;
		this.column = col;
		this.row = row;
	}
	
    /**
     * Invoked when an action occurs.
     * @param event The corresponding JButton object.
     */
	@Override
	public void actionPerformed(ActionEvent event) {
		if (event.getSource() instanceof JButton) {
			JButton button = (JButton) event.getSource();

			// Message introduced in Sudoku World 1.8
			String input = JOptionPane.showInputDialog("Provide new number: \n" +
					"1 = cow, 2 = football, 3 = house\n" +
					"4 = keanu, 5 = rain, 6 = rudolph\n" +
					"7 = sun, 8 = superwoman, 9 = tree\n" +
					"0 to reset");
			if (input == null) return;

			// try-catch introduced in Sudoku World 1.8
			try {
				int value = Integer.parseInt(input);
				if (value == GameGrid.EMPTY_VAL || 
					(value >= GameGrid.MIN_VAL && value <= GameGrid.MAX_VAL )) {
					processValue(value, button);
				} else {
					JOptionPane.showMessageDialog(null, "Invalid input: " + input);
				}
			} catch(NumberFormatException exception) {
				JOptionPane.showMessageDialog(null, "Invalid input: " + input);
			}
		}
	}

    /**
     * Update the button text according to the given value.
     * @param value the new value for the button between 0 and 9
     * @param button the button to be updated
     */
	private void processValue(int value, JButton button) {
		if (value == 0) {
			game.clearField(column, row);
			button.setIcon(null);
		} else if (game.setField(column, row, value)) {
			button.setIcon(FieldImage.icons[value]);
		} else {
			JOptionPane.showMessageDialog(null, "Number " + value + " not allowed here.");
		}
	}

}
