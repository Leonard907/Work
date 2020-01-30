import java.util.Scanner;
import java.util.Objects;

/**
 * A utility class for the fox hound program.
 * 
 * It contains helper functions for all user interface related
 * functionality such as printing menus and displaying the game board.
 */
public class FoxHoundUI {

    /** Number of main menu entries. */
    private static final int MENU_ENTRIES = 2;
    /** Main menu display string. */
    private static final String MAIN_MENU =
        "\n1. Move\n2. Exit\n\nEnter 1 - 2:";

    /** Menu entry to select a move action. */
    public static final int MENU_MOVE = 1;
    /** Menu entry to terminate the program. */
    public static final int MENU_EXIT = 2;

    public static String[][] initialiseBoard(String[] players, int dimension) {
        String[][] board = new String[dimension][dimension];

        for (int i = 0; i < dimension; i++) {
            for (int j = 0; j < dimension; j++) {
                board[i][j] = "   |";
            }
        }
        for (int i = 0; i < players.length; i++) {
            int[] intPosition = parsePosition(players[i]);
            if (i == players.length - 1) {
                // Fox position
                board[intPosition[0]][intPosition[1]] = " F |";
            }
            else {
                // Hound position
                board[intPosition[0]][intPosition[1]] = " H |";
            }
        }
        return board;
    }

    public static void displayBoard(String[] players, int dimension) {
        // TODO implement me
        String[][] board = initialiseBoard(players, dimension);

        /* Print the board
           1. Print row coordinates: ABCD...
           2. Initialise the separation line.
           3. Print board & Player positions.
         */
        // Initialise row coordinates
        StringBuilder rowCoordinates = new StringBuilder();
        for (int i = 0; i < (int) (Math.log10(dimension) + 4); i++) {
            rowCoordinates.append(" ");
        }
        for (int i = 0; i < dimension; i++) {
            rowCoordinates.append((char) (65 + i)).append("   ");
        }
        System.out.println(rowCoordinates);
        // Initialise the separation line
        StringBuilder separation = new StringBuilder();
        for (int i = 0; i < (int) (Math.log10(dimension) + 2); i++) {separation.append(" "); }

        separation.append("|");

        for (int i = 0; i < dimension; i++) { separation.append("===|"); }
        // Print the board & Player position
        for (int i = 0; i < dimension; i++) {
            System.out.println(separation);
            StringBuilder row = new StringBuilder();
            StringBuilder prettyDigit = new StringBuilder(); // Add zeros to number while necessary
            for (int zeros = 0; zeros < ((int) Math.log10(dimension) - (int) Math.log10(i + 1)); zeros++) {
                // Add zeros to the vertical coordinates
                prettyDigit.append(0);
            }
            prettyDigit.append(i+1);
            row.append(prettyDigit).append(" |");
            for (int j = 0; j < dimension; j++) {
                // Print the state of each row
                row.append(board[i][j]);
            }
            row.append(" ").append(prettyDigit);
            System.out.println(row);
        }
        System.out.println(separation);
        System.out.println(rowCoordinates);
    }
    public static int[] parsePosition(String position) {
        int column = position.charAt(0) - 65; // Coordinates in "ABCD..."
        int row = Integer.parseInt(position.substring(1)) - 1;
        return new int[] {row, column};
    }

    /**
     * Print the main menu and query the user for an entry selection.
     * 
     * @param figureToMove the figure type that has the next move
     * @param stdin a Scanner object to read user input from
     * @return a number representing the menu entry selected by the user
     * @throws IllegalArgumentException if the given figure type is invalid
     * @throws NullPointerException if the given Scanner is null
     */
    public static int mainMenuQuery(char figureToMove, Scanner stdin) {
        Objects.requireNonNull(stdin, "Given Scanner must not be null");
        if (figureToMove != FoxHoundUtils.FOX_FIELD 
         && figureToMove != FoxHoundUtils.HOUND_FIELD) {
            throw new IllegalArgumentException("Given figure field invalid: " + figureToMove);
        }

        String nextFigure = 
            figureToMove == FoxHoundUtils.FOX_FIELD ? "Fox" : "Hounds";

        int input = -1;
        while (input == -1) {
            System.out.println(nextFigure + " to move");
            System.out.println(MAIN_MENU);

            boolean validInput = false;
            if (stdin.hasNextInt()) {
                input = stdin.nextInt();
                validInput = input > 0 && input <= MENU_ENTRIES;
            }

            if (!validInput) {
                System.out.println("Please enter valid number.");
                input = -1; // reset input variable
            }

            stdin.nextLine(); // throw away the rest of the line
        }

        return input;
    }
}
