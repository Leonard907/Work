package one;

import javafx.application.Application;
import javafx.scene.shape.Arc;
import javafx.scene.shape.Line;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.scene.*;
import javafx.scene.layout.*;
import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class HangMan extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BorderPane pane = new BorderPane();
        HangManPane game = new HangManPane();
        HBox operations = new HBox(10);

        TextField guessArea = new TextField();
        Label input = new Label("Enter your guess: ", guessArea);
        input.setContentDisplay(ContentDisplay.RIGHT);
        input.setFont(Font.font("Helvetica",FontWeight.BLACK,
                FontPosture.REGULAR,14));
        guessArea.setOnAction(e -> game.processGuess(guessArea.getText()));

        Button next = new Button("->");
        next.setOnAction(e -> game.nextWord());
        Label nextWord = new Label("Click here for another word",next);
        nextWord.setContentDisplay(ContentDisplay.RIGHT);

        operations.getChildren().addAll(input,nextWord);

        pane.setCenter(game);
        pane.setBottom(operations);

        Scene scene = new Scene(pane, 600, 400);
        primaryStage.setTitle("HangMan");
        primaryStage.setScene(scene);
        primaryStage.show(); // display scene
    }


}

class HangManPane extends Pane {
    private String[] dictionary = {"adolescence","tolerance","delicious","jigsaw"};
    private List<String> word = new ArrayList<>();
    private String[] display;
    private Text text = new Text(300,350,"");
    private int wrongs = 0;
    private Circle head = new Circle(200,125,50);
    private Line body = new Line(200,175,200,300);
    private Line left_arm = new Line(200,225,125,225);
    private Line right_arm = new Line(200,225,275,225);
    private Line left_leg = new Line(200,300,125,350);
    private Line right_leg = new Line(200,300,275,350);
    private Arc base = new Arc(50,350,50,50,45,90);
    private Line support1 = new Line(50,300,50,50);
    private Line support2 = new Line(50,50,200,50);
    private Line support3 = new Line(200,50,200,75);
    private List<Node> pieces = new ArrayList<>();
    private Button next = new Button("->");
    private String missed = "";
    private int index = 0;

    HangManPane() {
        setup(0);
    }

    private void setup(int reference) {
        word = Arrays.asList(separateString(dictionary[reference]));
        display = new String[word.size()];
        head.setFill(Color.TRANSPARENT);
        head.setStroke(Color.BLACK);
        addFigure();
        getChildren().addAll(base,support1,support2,support3);
        for (int i = 0; i < word.size(); i++) {
            display[i] = ("*");
        }
        text.setText(concatChars(display));
        getChildren().add(text);
    }

    void nextWord() {
        index += 1;
        try {
            getChildren().clear();
            missed = "";
            wrongs = 0;
            pieces.clear();
            addFigure();
            setup(index);
        }
        catch (IndexOutOfBoundsException e) {
            Text endAlert = new Text(300,100,"Words exhausted");
            setup(0);
            getChildren().remove(text);
            getChildren().add(endAlert);
        }
    }

    private void addFigure() {
        pieces.add(head);
        pieces.add(body);
        pieces.add(left_arm);
        pieces.add(right_arm);
        pieces.add(left_leg);
        pieces.add(right_leg);
    }

    private boolean endGame() {
        return wrongs == 6;
    }

    private boolean wonGame() {
        boolean won = true;
        for (String i: display) {
            if (i.equals("*"))
                won = false;
        }
        return won;
    }

    void processGuess(String oneChar) {
        getChildren().remove(text);
        int length = word.size();
        ArrayList<Integer> indices = new ArrayList<>();
        try {
            char test = oneChar.charAt(1);
            Text moreThanOneWord = new Text(300,200,
                    "Warning: You should enter one letter only");
            getChildren().addAll(text, moreThanOneWord);
        }
        catch (IndexOutOfBoundsException e) {
            if (!word.contains(oneChar)) {
                getChildren().add(pieces.get(0));
                pieces.remove(0);
                wrongs += 1;
                missed += oneChar;
                Text missedLetters = new Text(400, 350, String.format("Missed letters: %s", missed));
                getChildren().addAll(text, missedLetters);
            }
            else {
                for (int i = 0; i < length; i++) {
                    String ch = word.get(i);
                    if (ch.equals(oneChar)) {
                        indices.add(i);
                    }
                }
                for (int i = 0; i < length; i++) {
                    if (indices.contains(i)) {
                        display[i] = oneChar;
                    }
                }
                text.setText(concatChars(display));
                getChildren().add(text);
            }
            if (endGame()) {
                Text end = new Text(300, 300, "You lose");
                getChildren().add(end);
            }
            else if (wonGame()) {
                Text won = new Text(300, 300, "You won");
                getChildren().add(won);
            }
        }
    }

    private String[] separateString(String line) {
        String[] chars = new String[line.length()];
        for (int i = 0; i < line.length(); i++) {
            chars[i] = Character.toString(line.charAt(i));
        }
        return chars;
    }

    private String concatChars(String[] line) {
        String string = "";
        for (int i = 0; i < line.length; i++) {
            string += line[i];
        }
        return string;
    }
}