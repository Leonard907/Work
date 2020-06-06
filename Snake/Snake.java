import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.util.Duration;

import java.awt.*;
import java.util.Random;

public class Snake extends Application {

    /* Constants */
    private static final int PROPORTION = 10;
    private static final int GAME_WINDOW_PROPORTION = 7;
    private static final int DEFAULT_DIMENSION = 20;
    private static final double CIRCLE_RADIUS_PROPORTION =
            // *2 for radius
            (double) GAME_WINDOW_PROPORTION / (DEFAULT_DIMENSION * PROPORTION * 2);
    private static final int START_LENGTH = 3;
    private static final Button startNewGame = new Button("Start a new game");
    private static final Button resumeGame = new Button("Resume");
    private static final int FRAME_RATE = 100;
    public static final String LEFT = "left";
    public static final String RIGHT = "right";
    public static final String UP = "up";
    public static final String DOWN = "down";

    /* Fields */
    private BorderPane main = new BorderPane();
    private Pane gamePane = new Pane();
    private int score;
    private SnakeBody snake;
    private Point food;
    private Timeline snakeAnimation;
    private boolean gameStart;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        Scene scene = new Scene(main, 500, 500);
        scene.getStylesheets().add("SnakeStyle.css");
        primaryStage.setTitle("Snake");
        primaryStage.setScene(scene);
        primaryStage.setResizable(false);
        primaryStage.show();
        interfaceSetup();

        startNewGame.setOnAction(e -> {
            mainLoop();
        });

        resumeGame.setOnAction(e -> {
            snakeAnimation.play();
        });
    }

    /* ------------------- UI Methods -------------------- */

    private void mainLoop() {
        score = 0;
        gameStart = true;
        food = null;
        snake = new SnakeBody(DEFAULT_DIMENSION, START_LENGTH, Snake.LEFT);
        setNewTimeline();
        snakeAnimation.play();
    }

    /**
     * Setup the interface
     */
    private void interfaceSetup() {
        main.getChildren().clear();
        gamePane.getChildren().clear();

        // start game
        if (!gameStart) {
            Text start = new Text(main.getWidth() / 4, main.getHeight() / 5,
                    "Press the button to start!");
            start.setFont(Font.font(
                    "Palatino", FontPosture.REGULAR, 20
            ));
            gamePane.getChildren().add(start);
        }
        addBorder();

        // Game status
        HBox gameButtons = new HBox(15);
        setButtonStyle(startNewGame);
        setButtonStyle(resumeGame);

        gameButtons.getChildren().addAll(startNewGame, resumeGame);
        gameButtons.setAlignment(Pos.CENTER);

        // Score
        VBox gameScore = new VBox(10);
        Text scoreDisplay = new Text("Food:  \n\n" + score);
        scoreDisplay.setFont(Font.font(
                "Palatino", FontWeight.BOLD, FontPosture.REGULAR, 20
        ));

        gameScore.getChildren().add(scoreDisplay);
        gameScore.setAlignment(Pos.CENTER);

        main.setCenter(gamePane);
        main.setBottom(gameButtons);
        main.setRight(gameScore);
    }

    /**
     * Set a new animation. With key events detected
     */
    private void setNewTimeline() {
        snakeAnimation = new Timeline(new KeyFrame(Duration.millis(FRAME_RATE),
                e -> {
                    main.setOnKeyPressed(this::handle);
                    main.requestFocus();
                    interfaceSetup();
                    drawSnake();
                    drawFood();
                    moveJudge();
                    if (snake.isLoss()) {
                        snakeAnimation.stop();
                    }
                }));
        snakeAnimation.setCycleCount(Timeline.INDEFINITE);
    }

    /**
     * Make a move, if it hits a food, increase snake's length
     */
    private void moveJudge() {
        snake.move();
        if (snake.getFood(food)) {
            score++;
            snake.acquireFood();
            food = null;
        }
    }

    /**
     * Draw the snake body.
     */
    private void drawSnake() {
        double radius = main.getWidth() * CIRCLE_RADIUS_PROPORTION;
        addBorder();
        for (Point p: snake.getBody()) {
            Circle bodySegment = new Circle(
                    main.getWidth() / PROPORTION + (2 * p.x - 1) * radius,
                    main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * p.y - 1) * radius,
                    radius
            );
            bodySegment.setId("body");
            gamePane.getChildren().add(bodySegment);
        }
        Circle eye1, eye2;
        if (snake.getNext().equals(Snake.RIGHT) || snake.getNext().equals(Snake.LEFT)) {
            eye1 = new Circle(
                    main.getWidth() / PROPORTION + (2 * snake.getBody().get(0).x - 1) * radius,
                    main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * snake.getBody().get(0).y - 0.5) * radius,
                    0.3 * radius
            );
            eye2 = new Circle(
                    main.getWidth() / PROPORTION + (2 * snake.getBody().get(0).x - 1) * radius,
                    main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * snake.getBody().get(0).y - 1.5) * radius,
                    0.3 * radius
            );
        } else {
            eye1 = new Circle(
                    main.getWidth() / PROPORTION + (2 * snake.getBody().get(0).x - 0.5) * radius,
                    main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * snake.getBody().get(0).y - 1) * radius,
                    0.3 * radius
            );
            eye2 = new Circle(
                    main.getWidth() / PROPORTION + (2 * snake.getBody().get(0).x - 1.5) * radius,
                    main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * snake.getBody().get(0).y - 1) * radius,
                    0.3 * radius
            );
        }
        eye1.setId("eye");
        eye2.setId("eye");
        gamePane.getChildren().addAll(eye1, eye2);
    }

    /**
     * Add the border of the game display area
     */
    private void addBorder() {
        // Game window
        Rectangle gameDisplay = new Rectangle(
                main.getWidth() / PROPORTION,
                main.getHeight() / PROPORTION,
                main.getWidth() * GAME_WINDOW_PROPORTION / PROPORTION,
                main.getHeight() * GAME_WINDOW_PROPORTION / PROPORTION
        );
        gameDisplay.setFill(Color.TRANSPARENT);
        gameDisplay.setStroke(Color.BLACK);

        gamePane.getChildren().add(gameDisplay);
    }

    /**
     * draw a food source
     */
    private void drawFood() {
        double radius = main.getWidth() * CIRCLE_RADIUS_PROPORTION;
        if (food == null) {
            int x = new Random().nextInt(20) + 1;
            int y = new Random().nextInt(20) + 1;
            food = new Point(x, y);
            while (snake.getBody().contains(food)) {
                x = new Random().nextInt(20) + 1;
                y = new Random().nextInt(20) + 1;
                food = new Point(x, y);
            }
        }
        Circle foodCircle = new Circle(
                main.getWidth() / PROPORTION + (2 * food.x - 1) * radius,
                main.getHeight() * (GAME_WINDOW_PROPORTION + 1) / PROPORTION - (2 * food.y - 1) * radius,
                radius * 0.75
        );
        gamePane.getChildren().add(foodCircle);
    }

    /**
     * Set the button style, color、border etc.
     * @param buttons the buttons to be set a style
     */
    private void setButtonStyle(Button... buttons) {
        for (Button b: buttons)
            b.setId("button");
    }

    private void handle(KeyEvent key) {
        switch (key.getCode()) {
            case UP:
                if (!snake.getNext().equals(DOWN)) {
                    snake.setNext(Snake.UP);
                    snake.returnToPreviousState();
                    moveJudge();
                }
                break;
            case DOWN:
                if (!snake.getNext().equals(UP)) {
                    snake.setNext(Snake.DOWN);
                    snake.returnToPreviousState();
                    moveJudge();
                }
                break;
            case LEFT:
                if (!snake.getNext().equals(RIGHT)) {
                    snake.setNext(Snake.LEFT);
                    snake.returnToPreviousState();
                    moveJudge();
                }
                break;
            case RIGHT:
                if (!snake.getNext().equals(LEFT)) {
                    snake.setNext(Snake.RIGHT);
                    snake.returnToPreviousState();
                    moveJudge();
                }
                break;
            case P:
                snake.returnToPreviousState();
                snakeAnimation.stop();
                break;
        }
    }
}