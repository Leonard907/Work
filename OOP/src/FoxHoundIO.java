import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
/**
 * A utility class for the fox hound program.
 * 
 * It contains helper functions for all file input / output
 * related operations such as saving and loading a game.
 */
public class FoxHoundIO {

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

    public static boolean validPlayers(String[] players) {
        for (String position: players) {
            if (!FoxHoundUtils.isValidPosition(FoxHoundUtils.DEFAULT_DIM, position))
                throw new IllegalArgumentException();
        }
        return true;
    }
}
