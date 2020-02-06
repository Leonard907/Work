import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Scanner;
import java.util.Objects;
import java.nio.file.Path;

/**
 * A utility class for the fox hound program.
 * 
 * It contains helper functions for all user interface related
 * functionality such as printing menus and displaying the game board.
 */
public class FoxHoundUI {

    /** Number of main menu entries. */
    private static final int MENU_ENTRIES = 4;
    /** Main menu display string. */
    private static final String MAIN_MENU =
        "\n1. Move\n2. Save\n3. Load\n4. Exit\n\nEnter 1 - 4:";

    /** Menu entry to select a move action. */
    public static final int MENU_MOVE = 1;
    /** Menu entry to save the game */
    public static final int MENU_SAVE = 2;
    /** Menu entry to save the game */
    public static final int MENU_LOAD = 3;
    /** Menu entry to terminate the program. */
    public static final int MENU_EXIT = 4;
    /** ASCII code of the beginning column coordinate (A) */
    public static final int ASC_A = 65;

    /**
     * Create a 2D array that stores the state of each grid,
     * @param players The positions of fox and hounds
     * @param dimension The dimension of the board
     * @return a 2D array representing
     */
    public static String[][] initialiseBoard(String[] players, int dimension) {
        String[][] board = new String[dimension][dimension];

        for (int i = 0; i < dimension; i++) {
            for (int j = 0; j < dimension; j++) {
                board[i][j] = ".";
            }
        }
        for (int i = 0; i < players.length; i++) {
            int[] intPosition = parsePosition(players[i]);
            if (i == players.length - 1) {
                // Fox position
                board[intPosition[0]][intPosition[1]] = "F";
            }
            else {
                // Hound position
                board[intPosition[0]][intPosition[1]] = "H";
            }
        }
        return board;
    }

    public static void displayBoard(String[] players, int dimension) {
        /* Print the board
           1. Print row coordinates: ABCD...
           2. Print board & Player positions.
         */
        String[][] board = initialiseBoard(players, dimension);
        // Initialise row coordinates
        StringBuilder rowCoordinates = new StringBuilder();
        StringBuilder spaces = new StringBuilder("  "); // The leading spaces in first row
        if (dimension >= 10) {
            // Extra one space needed
            spaces.append(" ");
        }
        rowCoordinates.append(spaces);
        for (int i = 0; i < dimension; i++) {
            rowCoordinates.append((char) (ASC_A + i));
        }
        rowCoordinates.append(spaces);
        System.out.println(rowCoordinates);
        System.out.println();

        // Print the board & Player position
        for (int i = 0; i < dimension; i++) {
            StringBuilder row = new StringBuilder();
            StringBuilder prettyDigit = new StringBuilder(); // Add zeros to number while necessary
            if (dimension >= 10) {
                // Add zeros to the vertical coordinates
                prettyDigit.append(0);
            }
            prettyDigit.append(i + 1);
            row.append(prettyDigit).append(" ");
            for (int j = 0; j < dimension; j++) {
                // Print the state of each row
                row.append(board[i][j]);
            }
            row.append(" ").append(prettyDigit);
            System.out.println(row);
        }
        System.out.println();
        System.out.println(rowCoordinates);
    }

    public static String[][] initialiseBoardPretty(String[] players, int dimension) {
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

    public static void displayBoardFancy(String[] players, int dimension) {
        /* Print the board
           1. Print row coordinates: ABCD...
           2. Initialise the separation line.
           3. Print board & Player positions.
         */
        String[][] board = initialiseBoardPretty(players, dimension);
        // Initialise row coordinates
        StringBuilder rowCoordinates = new StringBuilder();
        StringBuilder spaces = new StringBuilder("    "); // Leading spaces in first row
        if (dimension >= 10) {
            spaces.append(" ");
        }
        rowCoordinates.append(spaces);
        for (int i = 0; i < dimension; i++) {
            rowCoordinates.append((char) (ASC_A + i)).append("   ");
        }
        System.out.println(rowCoordinates);
        // Initialise the separation line
        StringBuilder separation = new StringBuilder("  "); // leading 2 spaces

        if (dimension >= 10) {
            separation.append(" ");
        }

        separation.append("|");

        for (int i = 0; i < dimension; i++) {
            separation.append("===|");
        }

        // Print the board & Player position
        for (int i = 0; i < dimension; i++) {
            System.out.println(separation);
            StringBuilder row = new StringBuilder();
            StringBuilder prettyDigit = new StringBuilder(); // Add zeros to number while necessary
            if (dimension >= 10 && i < 9) {
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
        try {
            int column = position.charAt(0) - 65; // Coordinates in "ABCD..."
            int row = Integer.parseInt(position.substring(1)) - 1;
            return new int[] {row, column};
        }
        catch (Exception ex) {
            throw new IllegalArgumentException("ERROR: Please enter valid coordinate pair separated by space.");
        }
    }

    public static void updatePosition(String origin, String destination, String[] players) {
        for (int i = 0; i < players.length; i++) {
            if (Arrays.equals(FoxHoundUI.parsePosition(players[i]),
                              FoxHoundUI.parsePosition(origin))) {
                players[i] = destination;
                break;
            }
        }
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

    public static String[] positionQuery(int dim, Scanner stdIn) {
        // Prompt input from console
        while (true) {
            System.out.println("Provide origin and destination coordinates.");
            System.out.printf("Enter two positions between A1-%c%d:", (char) (65 + dim - 1), dim);
            System.out.println();
            String[] input = stdIn.nextLine().split(" ");
            if (FoxHoundUtils.isValidPosition(dim, input[0]) &&
                    FoxHoundUtils.isValidPosition(dim, input[1]) &&
                    input.length == 2) {
                return input;
            }
            else {
                System.err.println("ERROR: Please enter valid coordinate pair separated by space.");
                System.out.println();
            }
        }
    }

    public static Path fileQuery(Scanner stdIn) {
        System.out.println("Enter file path:");
        String fileName = stdIn.nextLine();
        Path filePath = Paths.get(fileName);
        return filePath;
    }
}







