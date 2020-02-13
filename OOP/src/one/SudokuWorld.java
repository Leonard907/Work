package one;
public class SudokuWorld {
    /**
     * Start the Sudoku game gui with sample grid data
     *
     * @param args - unused
     */
	public static void main(String[] args) {
		String gameName = "SudokuWorld";
		int[][] gameData = SampleGameData.sample5;

		SudokuFrame window = new SudokuFrame(gameName, gameData);
		window.setVisible(true);
	}
}
