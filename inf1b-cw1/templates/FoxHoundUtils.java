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

    public static String[] initialisePositions(int dimension) {
        // Initialise the positions of Fox & Hounds
        return insert(initialiseFoxPosition(dimension), initialiseHoundPositions(dimension));
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
}
