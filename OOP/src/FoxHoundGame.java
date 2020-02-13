import java.io.IOException;
import java.nio.file.Files;
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
                        if (FoxHoundUtils.isValidMove(dim, players, turn, input[0], input[1])) {
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
                // Defense against fox AI
                case FoxHoundUI.MENU_AI:
                    if (turn == FoxHoundUtils.HOUND_FIELD) {
                        System.out.println("Hound AI is still under construction.\n" +
                                "Estimated completing time: 2020.2.30");
                        break;
                    }
                    boolean successMove = FoxAI.makeMove(players, dim);
                    if (successMove) {
                        turn = FoxHoundUtils.HOUND_FIELD;
                    }
                    else {
                        FoxHoundUI.displayBoardFancy(players, dim);
                        System.out.println("The hound wins!");
                        System.exit(0);
                    }
                    if (FoxHoundUtils.isFoxWin(players[players.length - 1])) {
                        FoxHoundUI.displayBoardFancy(players, dim);
                        System.out.println("The fox wins!");
                        System.exit(0);
                    }
                    break;
                // Save file
                case FoxHoundUI.MENU_SAVE:
                    boolean saveSuccessful = false;
                    while (!saveSuccessful) {
                        Path file = FoxHoundUI.fileQuery(STDIN_SCAN);
                        if (Files.exists(file)) {
                            System.err.println("WARNING: File already exists.\n" +
                                "Do you want to overwrite its contents? y/n");
                            String overwrite = STDIN_SCAN.nextLine();
                            if (overwrite.equals("n")) {
                                System.out.println("Trying again :-)");
                                System.out.println();
                                continue;
                            }
                            else if (!overwrite.equals("y")) {
                                System.err.println("ERROR: Invalid Symbol, y/n only.");
                                System.out.println();
                            }
                        }
                        try {
                            boolean successful = FoxHoundIO.saveGame(players, turn, file);
                            if (successful) {
                                saveSuccessful = true;
                            } else {
                                System.out.println("ERROR: Saving file failed");
                            }
                        }
                        catch (Exception ex) {
                            System.out.println("ERROR: Saving file failed.");
                        }
                    }
                    break;
                case FoxHoundUI.MENU_LOAD:
                    while (true) {
                        Path file = FoxHoundUI.fileQuery(STDIN_SCAN);
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
                FoxHoundUtils.setDim(dimension);
                String[] players = FoxHoundUtils.initialisePositions(dimension);
                gameLoop(dimension, players);
                break;
            }
            catch (IllegalArgumentException e) {
                System.err.println("Not in range [4-26]");
                System.out.println();
            }
        }
        // Close the scanner reading the standard input stream
        STDIN_SCAN.close();
    }

}

/**
 * Utility class that generates a simple Fox AI. Players can call this in main menu.
 *
 * It contains a makeMove function that calculates the next move of the fox based
 * on the board layout.
 */
class FoxAI {
    // Below is the Fox AI implementation
    /**
     * Make a move.
     * Algorithm:
     * 1. Whether a move forward is possible (both directions)
     *    Yes: If two moves (left & right) are possible, calculate left & right spaces.
     *         The one that has the most spaces indicates the next move's direction.
     *         If only one move is possible, do that.
     *    No: If two backward moves (left & right) are possible, calculate left & right spaces.
     *        The one that has the most spaces indicates the next move's direction.
     *        If only one move is possible, do that.
     * 2. If no moves are possible, game is over. return false.
     * @param players figures' positions
     * @param dim     dimension of the board
     * @return the move
     */
    public static boolean makeMove(String[] players, int dim) {
        int[] foxPosition = FoxHoundUI.parsePosition(players[players.length - 1]);
        // Moves: top left, top right, bottom left, bottom right
        int[][] possibleMoves = {{foxPosition[0] - 1, foxPosition[1] - 1},
                {foxPosition[0] - 1, foxPosition[1] + 1},
                {foxPosition[0] + 1, foxPosition[1] - 1},
                {foxPosition[0] + 1, foxPosition[1] + 1}};
        boolean optimal = false;
        if (!breakThrough(players, dim, foxPosition[1]).equals("no match")) {
            if (breakThrough(players, dim, foxPosition[1]).equals("left")) {
                if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
                    optimal = true;
                }
                else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
                    optimal = true;
                }
            }
            else if (breakThrough(players, dim, foxPosition[1]).equals("right")) {
                if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
                    optimal = true;
                }
                else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
                    optimal = true;
                }
            }
        }
        if (!optimal) {
            if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0]))
                    && FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1]))) {
                int[] spaceCount = leftRightSpace(players, dim, foxPosition[1]);
                if (spaceCount[0] >= spaceCount[1])
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
                else
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0]))) {
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1]))) {
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
                // No forward moves possible
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2]))
                    && FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3]))) {
                int[] spaceCount = leftRightSpace(players, dim, foxPosition[1]);
                if (spaceCount[0] >= spaceCount[1])
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
                else
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2]))) {
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3]))) {
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
            } else
                return false;
        }
        return true;
    }

    /**
     * Calculate the number of spaces on the left/right of the fox
     * Throws IllegalArgumentException when input coordinates is invalid
     * @param players The positions of the figures
     * @param dim Dimension of the board
     * @param fox Column position of the fox
     * @return An array that contains left & right spaces
     */
    private static int[] leftRightSpace(String[] players, int dim, int fox) {
        int[] playersRow = new int[players.length - 1];
        int[] playersCol = new int[players.length - 1];
        int leftCount = 0;
        int rightCount = 0;
        for (int i = 0; i < players.length - 1; i++) {
            playersRow[i] = FoxHoundUI.parsePosition(players[i])[0];
            playersCol[i] = FoxHoundUI.parsePosition(players[i])[1];
        }
        for (int i = 0; i < dim; i++) {
            for (int j = 0; j < dim; j++) {
                if (!contains(i, filterLeft(playersCol, playersRow, fox)) && j < fox) {
                    leftCount += 1;
                } if (!contains(i, filterRight(playersCol, playersRow, fox)) && j > fox) {
                    rightCount += 1;
                }
            }
        }
        // Calculate left & right difference
        for (int col: playersCol) {
            if (col < fox) {
                leftCount -= 5;
            } else if (col > fox) {
                rightCount -= 5;
            }
        }
        return new int[] {leftCount, rightCount};
    }

    private static boolean contains(int element, int[] array) {
        for (int target: array) {
            if (element == target) {
                return true;
            }
        }
        return false;
    }

    private static int[] filterLeft(int[] nums, int[] rows, int fox) {
        int count = 0;
        for (int num: nums) {
            if (num < fox) {
                count++;
            }
        }
        int[] filter = new int[count];
        int index = 0;
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] < fox) {
                filter[index] = rows[i];
                index++;
            }
        }
        return filter;
    }

    private static int[] filterRight(int[] nums, int[] rows, int fox) {
        int count = 0;
        for (int num: nums) {
            if (num > fox) {
                count++;
            }
        }
        int[] filter = new int[count];
        int index = 0;
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] > fox) {
                filter[index] = rows[i];
                index++;
            }
        }
        return filter;
    }

    private static String breakThrough(String[] players, int dim, int fox) {
        int[] playersCol = new int[players.length - 1];
        for (int i = 0; i < players.length - 1; i++) {
            playersCol[i] = FoxHoundUI.parsePosition(players[i])[1];
        }
        for (int i = 0; i < fox - 1; i++) {
            if (!contains(i, playersCol) && !(contains(i+1, playersCol))) {
                return "left";
            }
        }
        for (int i = fox + 1; i < dim - 1; i++) {
            if (!contains(i, playersCol) && !(contains(i+1, playersCol))) {
                return "right";
            }
        }
        return "no match";
    }
}