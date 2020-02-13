import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
/**
 * A utility class for the fox hound program.
 * 
 * It contains helper functions for all file input / output
 * related operations such as saving and loading a game.
 */
public class FoxHoundIO {

    /**
     * Save the game in .txt format that stores the current state of the board as follows:
     * 1. The first letter is the figure to move next.
     * 2. The rest are positions of the figures. The last one is the fox's position.
     * @param players Array of position of players
     * @param figure The figure to move next
     * @param file The file path you wish to save.
     * @return A boolean indicates whether you saved the game successfully.
     */
    public static boolean saveGame(String[] players, char figure, Path file) {
        /*  Create a path and save the contents.
            Write contents of players and figure to file.
         */
        if (file == null)
            throw new NullPointerException();
        // Whether the players array is valid
        validPlayers(players);
        // Store the contents
        StringBuilder contents = new StringBuilder();
        contents.append(figure).append(" ");
        for (String position: players)
            contents.append(position).append(" ");
        String[] gameData = {contents.toString()};
        try {
            Files.write(file, Arrays.asList(gameData), StandardCharsets.UTF_8);
            if (Files.readAllLines(file).equals(Arrays.asList(gameData))) {
                return true;
            } else {
                return false;
            }
        }
        catch (IOException ex) {
            return false;
        }
    }

    /**
     * Load a saved game to the current board. Details:
     * 1. The first letter is the figure to be moved next.
     * 2. The rest are positions of the players to be put on the board.
     * @param players The original positions of the players.
     * @param loadFile The file path you wish to load
     * @return A character indicating whether the game is loaded successfully.
     */
    public static char loadGame(String[] players, Path loadFile) {
        try {
            String[] figureAndPosition = Files.readAllLines(loadFile).get(0).split(" ");
            validPlayers(players);
            if (figureAndPosition.length - 1 != players.length) {
                return '#';
            }
            // Check dimensions are correct
            if (!validFile(figureAndPosition, loadFile)) {
                System.out.println();
                return '#';
            }
            for (int i = 1; i < figureAndPosition.length; i++) {
                players[i - 1] = figureAndPosition[i];
            }
            return figureAndPosition[0].charAt(0);
        } catch (IOException ex) {
            return '#'; // Indicates in valid file
        }
    }

    /**
     * Check whether the file is valid from several aspects:
     * 1. The length of the file is 1 line only.
     * 2. All positions of the figures are valid.
     * @param players The input array of figures
     * @param file The file path you enter
     * @return A boolean indicating whether the file and its contents are valid.
     */
    public static boolean validFile(String[] players, Path file) {
        /* Check file is valid
        * Input is correct
        * No multiple rows
        * Positions are correct
        * Figure is correct: 1 character, F or H.
        * Input is in default dimension
        * */
        boolean correctFigure = players[0].equals("H") || players[0].equals("F");
        boolean singleRow;
        try {
            singleRow = Files.readAllLines(file).size() == 1;
        }
        catch (IOException ex) {
            return false;
        }
        for (int i = 1; i < players.length; i++) {
            if (!FoxHoundUtils.isValidPosition(FoxHoundUtils.DEFAULT_DIM, players[i]))
                return false;
        }
        return correctFigure && singleRow;
    }

    /**
     * Whether the original players array is valid by determining validity of
     * their positions.
     * @param players Array that stores the positions of the players
     * @return A boolean indicating whether the players array is valid.
     */
    public static boolean validPlayers(String[] players) {
        for (String position: players) {
            if (!FoxHoundUtils.isValidPosition(FoxHoundUtils.DEFAULT_DIM, position)) {
                throw new IllegalArgumentException("ERROR: Players array not valid.");
            }
        }
        return true;
    }
}
