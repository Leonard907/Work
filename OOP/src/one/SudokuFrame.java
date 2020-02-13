package one;
import java.awt.*;
import java.util.Objects;

import javax.swing.*;
import javax.swing.border.LineBorder;

/**
 * Sudoku window frame container.
 */
public class SudokuFrame extends JFrame {
	
	private static final int WINDOW_DIM = 500;
	
	private final GameGrid game;
	
    /**
     * Create a new Sudoku window frame.
     * @param gameFile path to a Sudoku game file
     */
	public SudokuFrame(String gameName, int[][] gameData) {
        Objects.requireNonNull(gameName);

		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		setLayout(new GridLayout(GameGrid.GRID_DIM,GameGrid.GRID_DIM));
		setSize(WINDOW_DIM, WINDOW_DIM);

		setTitle(gameName);
		game = new GameGrid(gameData);

        createButtonGrid();
   	}

    /**
     * Create a button for each field in the Sudoku game grid
     * and set the corresponding action handler.
     */
	private void createButtonGrid() {
		for(int row = 0; row < GameGrid.GRID_DIM; row++) {
            for(int column = 0; column < GameGrid.GRID_DIM; column++) {
				int value = game.getField(column, row);
            	JButton button = new JButton();

            	// Introduced in Sudoku World 1.9
            	int topBorderAmount = 1;
            	int leftBorderAmount = 1;
            	if (row % GameGrid.SUBGRID_DIM == 0)
            		topBorderAmount = 5;
            	if (column % GameGrid.SUBGRID_DIM == 0)
            		leftBorderAmount = 5;
				button.setBorder(BorderFactory.createMatteBorder(topBorderAmount, leftBorderAmount, 1, 1, Color.ORANGE));

            	if (value != GameGrid.EMPTY_VAL) {
            		button.setEnabled(false);
					button.setIcon(FieldImage.icons[value-1]);
					button.setDisabledIcon(FieldImage.icons[value-1]);
            	}
            	button.addActionListener(new SudokuFieldAction(game, column, row));
            	add(button);	
            }
        }
	}
}
