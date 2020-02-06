import java.util.Arrays;

/**
 * A utility class for the fox hound program.
 * 
 * It contains helper functions to check the state of the game
 * board and validate board coordinates and figure positions.
 */
public class FoxHoundUtils {

    // ATTENTION: Changing the following given constants can 
    // negatively affect the outcome of the auto grading!

    /** Default dimension of the game board in case none is specified. */
    public static final int DEFAULT_DIM = 8;
    /** Minimum possible dimension of the game board. */
    public static final int MIN_DIM = 4;
    /** Maximum possible dimension of the game board. */
    public static final int MAX_DIM = 26;

    /** Symbol to represent a hound figure. */
    public static final char HOUND_FIELD = 'H';
    /** Symbol to represent the fox figure. */
    public static final char FOX_FIELD = 'F';

    // HINT Write your own constants here to improve code readability ...
    public static int DIM;

    public static String[] initialisePositions(int dimension) {
        // Check the dimension is within range
        if (dimension >= MIN_DIM && dimension <= MAX_DIM)
            return insert(initialiseFoxPosition(dimension), initialiseHoundPositions(dimension));
        else
            throw new IllegalArgumentException("Not in range [4-26]");
    }

    public static String[] insert(String element, String[] list) {
        // Insert an element to a list
        String[] concat = new String[list.length + 1];
        for (int i = 0; i < list.length; i++) {
            concat[i] = list[i];
        }
        concat[list.length] = element;
        return concat;
    }

    private static String initialiseFoxPosition(int dimension) {
        // Initialise positions of fox
        int start = 65; // The ASCII value of 'A'
        if (dimension % 2 == 0) {
            // Last row starts with a black square
            if ((dimension / 2) % 2 == 0) {
                // Central position is black
                return Character.toString((char) (start + dimension / 2)) + Integer.toString(dimension);
            }
            else {
                // Central position is white, fox is on the right
                return Character.toString((char) (start + dimension / 2 + 1)) + Integer.toString(dimension);
            }
        }
        else {
            // Last row starts with a white square
            if ((dimension / 2) % 2 == 0) {
                // Central position is white, fox is on the right
                return Character.toString((char) (start + dimension / 2 + 1)) + Integer.toString(dimension);
            }
            else {
                // Central position is black
                return Character.toString((char) (start + dimension / 2)) + Integer.toString(dimension);
            }
        }
    }

    private static String[] initialiseHoundPositions(int dimension) {
        // Initialise positions of hounds
        int start = 65;
        String[] houndPositions = new String[dimension / 2];
        for (int i = 0; i < dimension; i++) {
            if (i % 2 == 1) {
                // black square, place a hound
                houndPositions[i / 2] = Character.toString((char) (start + i)) + "1";
            }
        }
        return houndPositions;
    }

    private static String toPosition(int[] coordinates) {
        int start = 65;
        StringBuilder position = new StringBuilder();
        position.append((char) (start + coordinates[0]));
        position.append(coordinates[1]);
        return position.toString();
    }

    public static boolean isValidMove(int dim, String[] players, char figure,
                                      String origin, String destination) {
        if (isValidInput(dim, players, figure, origin, destination)) {
            // Test whether destination is occupied
            if (!contains(destination, players)) {
                if (figure == 'H') {
                    // Check hound
                    boolean isHound = false;
                    for (int i = 0; i < players.length - 1; i++) {
                        if (Arrays.equals(FoxHoundUI.parsePosition(players[i]),
                                          FoxHoundUI.parsePosition(origin))) {
                            isHound = true;
                            break;
                        }
                    }
                    if (isHound) {
                        // Check move is in range
                        int[] originCoordinates = FoxHoundUI.parsePosition(origin);
                        int[] destinationCoordinates = FoxHoundUI.parsePosition(destination);
                        return originCoordinates[0] - destinationCoordinates[0] == -1 &&
                                Math.abs(originCoordinates[1] - destinationCoordinates[1]) == 1;
                    } else
                        return false;
                }
                else {
                    // Check fox
                    if (Arrays.equals(FoxHoundUI.parsePosition(players[players.length - 1]),
                                      FoxHoundUI.parsePosition(origin))) {
                        // Check move in range
                        int[] originCoordinates = FoxHoundUI.parsePosition(origin);
                        int[] destinationCoordinates = FoxHoundUI.parsePosition(destination);
                        return Math.abs(originCoordinates[0] - destinationCoordinates[0]) == 1 &&
                                Math.abs(originCoordinates[1] - destinationCoordinates[1]) == 1;
                    } else {
                        return false;
                    }
                }
            }
            else
                return false;
        }
        else
            return false;
    }

    private static boolean isValidInput(int dim, String[] players, char figure,
                                        String origin, String destination) {
        /* Determine whether the input is valid or not from:
           1.players not null
           2.figure not 'F', 'H'
           3.origin, destination valid & in range
        */
        return isValidPosition(dim, origin) && isValidPosition(dim, destination) &&
                (figure == 'F' || figure == 'H') && players != null;
    }

    public static boolean isValidPosition(int dim, String position) {
        // Check input is in correct format
        try {
            int[] coordinates = FoxHoundUI.parsePosition(position);
            // Check validity of coordinates
            return coordinates[0] < dim && coordinates[1] < dim
                    && coordinates[0] >= 0 && coordinates[1] >= 0;
        }
        catch (IllegalArgumentException ex) {
            return false;
        }
    }

    private static boolean contains(String element, String[] array) {
        for (String target: array) {
            if (element.equals(target))
                return true;
        }
        return false;
    }

    public static boolean isFoxWin(String foxPosition) {
        // Determine whether the fox reaches the other side
        if (foxPosition == null)
            throw new NullPointerException();
        return FoxHoundUI.parsePosition(foxPosition)[0] == 0;
    }

    public static boolean isHoundWin(String[] players, int dim) {
        // Get fox's position and its possible moves
        if (dim < MIN_DIM || dim > MAX_DIM)
            throw new IllegalArgumentException();
        int[] foxPosition = FoxHoundUI.parsePosition(players[players.length - 1]);
        int[][] possibleMoves = {{foxPosition[0] - 1, foxPosition[1] - 1},
                                 {foxPosition[0] - 1, foxPosition[1] + 1},
                                 {foxPosition[0] + 1, foxPosition[1] - 1},
                                 {foxPosition[0] + 1, foxPosition[1] + 1}};
        boolean[] blockedByHounds = new boolean[4];
        /* Check whether the move is possible
           -> Move is invalid
           -> Blocked by hounds
        */
        for (int i = 0; i < possibleMoves.length; i++) {
            if (isValidPosition(dim, toPosition(possibleMoves[i]))) {
                for (int j = 0; j < players.length - 1; j++) {
                    if (Arrays.equals(possibleMoves[i], FoxHoundUI.parsePosition(players[j])))
                        blockedByHounds[i] = true;
                }
            }
            else {
                blockedByHounds[i] = true;
            }
        }
        for (boolean blocked: blockedByHounds) {
            if (!blocked)
                return false;
        }
        return true;
    }
}
