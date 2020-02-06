import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.InputMismatchException;
import java.util.Scanner;

/** 
 * The Main class of the fox hound program.
 * 
 * It contains the main game loop where main menu interactions
 * are processed and handler functions are called.
  */
public class FoxHoundGame {

    /** 
     * This scanner can be used by the program to read from
     * the standard input. 
     * 
     * Every scanner should be closed after its use, however, if you do
     * that for StdIn, it will close the underlying input stream as well
     * which makes it difficult to read from StdIn again later during the
     * program.
     * 
     * Therefore, it is advisable to create only one Scanner for StdIn 
     * over the course of a program and only close it when the program
     * exits. Additionally, it reduces complexity. 
     */
    private static final Scanner STDIN_SCAN = new Scanner(System.in);

    /**
     * Swap between fox and hounds to determine the next
     * figure to move.
     * 
     * @param currentTurn last figure to be moved
     * @return next figure to be moved
     */
    private static char swapPlayers(char currentTurn) {
        if (currentTurn == FoxHoundUtils.FOX_FIELD) {
            return FoxHoundUtils.HOUND_FIELD;
        } else {
            return FoxHoundUtils.FOX_FIELD;
        }
    }

    /**
     * The main loop of the game. Interactions with the main
     * menu are interpreted and executed here.
     * 
     * @param dim the dimension of the game board
     * @param players current position of all figures on the board in board coordinates
     */
    private static void gameLoop(int dim, String[] players) throws IOException {

        // start each game with the Fox
        char turn = FoxHoundUtils.FOX_FIELD;
        boolean exit = false;
        while(!exit) {
            System.out.println("\n#################################");
            FoxHoundUI.displayBoardFancy(players, dim);

            int choice = FoxHoundUI.mainMenuQuery(turn, STDIN_SCAN);
            // handle menu choice
            switch(choice) {
                case FoxHoundUI.MENU_MOVE:
                    String[] input = {};
                    boolean validInput = false;
                    while (!validInput) {
                        input = FoxHoundUI.positionQuery(dim, STDIN_SCAN);
                        if (FoxHoundUtils.isValidMove(dim, players, 'H', input[0], input[1]) ||
                            FoxHoundUtils.isValidMove(dim, players, 'F', input[0], input[1])) {
                            System.out.println(1);
                            validInput = true;
                        }
                        else {
                            System.out.println("ERROR: Invalid move, try again");
                            System.out.println();
                        }
                    }
                    FoxHoundUI.updatePosition(input[0], input[1], players);
                    if (FoxHoundUtils.isHoundWin(players, dim)) {
                        FoxHoundUI.displayBoardFancy(players, dim);
                        System.out.println("The hounds win!");
                        System.exit(0);
                    }
                    else if (FoxHoundUtils.isFoxWin(players[players.length - 1])) {
                        FoxHoundUI.displayBoardFancy(players, dim);
                        System.out.println("The fox wins!");
                        System.exit(0);
                    }
                    turn = swapPlayers(turn);
                    break;
                 // Save file
                case FoxHoundUI.MENU_SAVE:
                    while (true) {
                        System.out.println("Enter file name:");
                        Path file = Paths.get(STDIN_SCAN.nextLine());
                        try {
                            boolean successful = FoxHoundIO.saveGame(players, turn, file);
                            if (successful)
                                System.exit(0);
                            else
                                System.out.println("ERROR: Saving file failed");
                        }
                        catch (Exception ex) {
                            System.out.println("ERROR: Saving file failed.");
                        }
                    }
                case FoxHoundUI.MENU_LOAD:
                    while (true) {
                        System.out.println("Enter file you want to load");
                        Path file = Paths.get(STDIN_SCAN.nextLine());
                        if (FoxHoundIO.loadGame(players, file) == '#') {
                            System.out.println("ERROR: Loading from file failed.");
                            System.out.println();
                        }
                        else {
                            turn = FoxHoundIO.loadGame(players, file);
                            break;
                        }
                    }
                    break;
                case FoxHoundUI.MENU_EXIT:
                    exit = true;
                    break;
                default:
                    System.err.println("ERROR: invalid menu choice: " + choice);
            }
        }
    }

    /**
     * Entry method for the Fox and Hound game. 
     * 
     * The dimensions of the game board can be passed in as
     * optional command line argument.
     * 
     * If no argument is passed, a default dimension of 
     * {@value FoxHoundUtils#DEFAULT_DIM} is used. 
     * 
     * Dimensions must be between {@value FoxHoundUtils#MIN_DIM} and 
     * {@value FoxHoundUtils#MAX_DIM}.
     * 
     * @param args contain the command line arguments where the first can be
     * board dimensions.
     */
    public static void main(String[] args) throws IOException {
        while (true) {
            System.out.println("Enter the dimension of board: ");
            try {
                int dimension = Integer.parseInt(STDIN_SCAN.nextLine());
                FoxHoundUtils.DIM = dimension;
                String[] players = FoxHoundUtils.initialisePositions(dimension);
                gameLoop(dimension, players);
                break;
            }
            catch (IllegalArgumentException e) {
                System.out.println("Not in range [4-26]");
                System.out.println();
            }
        }
        // Close the scanner reading the standard input stream
        STDIN_SCAN.close();
    }
}
