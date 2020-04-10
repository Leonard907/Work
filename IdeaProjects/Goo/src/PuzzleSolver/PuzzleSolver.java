package PuzzleSolver;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.*;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.util.LinkedList;


public class PuzzleSolver extends Application {

    private Board input;
    private Text noSuchFile = new Text("File does not exist");
    private GridPane grid = new GridPane();
    private int moves = 0;
    private VBox left = new VBox(20);
    private LinkedList<Board> solution;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BorderPane pane = new BorderPane();
        HBox bottom = new HBox(10);
        Button next = new Button("Next move");
        Button puzzleText = new Button("Enter puzzle");
        TextField puzzle = new TextField();
        puzzle.setOnAction(e -> {
            try {
                bottom.getChildren().remove(noSuchFile);
            } catch (Exception ignored) {

            }
            try {
                input = Solver.solverFromFile(puzzle.getText());
                Solver solve = new Solver(input);
                solution = solve.solution();
                setup(input);
                Text move = new Text("Initial\nboard");
                move.setFont(Font.font("JetBrains Mono", 20));
                left.getChildren().add(move);
                left.getChildren().add(next);
            } catch (IllegalArgumentException ex) {
                bottom.getChildren().add(noSuchFile);
            }
        });
        puzzleText.setOnAction(e -> {
            try {
                bottom.getChildren().add(puzzle);
            } catch (IllegalArgumentException ignored) {
            }
        });
        next.setOnAction(e -> {
            if (solution == null) {
                left.getChildren().clear();
                Text noSolution = new Text("Unsolvable");
                noSolution.setFont(Font.font("JetBrains Mono", 20));
                left.getChildren().add(noSolution);
            } else {
                if (!solution.isEmpty()) {
                    next();
                    if (!solution.isEmpty()) {
                        Text currentMoves = new Text(String.format("Moves: \n  %d", moves++));
                        currentMoves.setFont(Font.font("JetBrains Mono", 20));
                        left.getChildren().addAll(currentMoves, next);
                    } else {
                        Text currentMoves = new Text(String.format("Total Moves: \n  %d", moves++));
                        currentMoves.setFont(Font.font("JetBrains Mono", 20));
                        left.getChildren().addAll(currentMoves, next);
                    }
                }
            }
        });
        bottom.getChildren().add(puzzleText);
        pane.setLeft(left);
        pane.setCenter(grid);
        pane.setBottom(bottom);

        Scene scene = new Scene(pane, 500,500);
        primaryStage.setTitle("8 Puzzle");
        primaryStage.setScene(scene);
        primaryStage.show(); // display scene
    }

    public void setup(Board input) {
        grid.getChildren().clear(); left.getChildren().clear();
        grid.setHgap(300.0 / input.dimension());
        grid.setVgap(300.0 / input.dimension());
        left.setPrefWidth(100);
        for (int i = 0; i < input.dimension(); i++) {
            for (int j = 0; j < input.dimension(); j++) {
                Text n = new Text(Integer.toString(input.tiles[i][j]));
                n.setFont(Font.font("Helvetica", 30));
                grid.add(n, j, i);
            }
        }
    }

    public void next() {
        Board next = solution.remove(0);
        setup(next);
    }
}
