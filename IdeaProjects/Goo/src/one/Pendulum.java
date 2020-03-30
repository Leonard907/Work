package one;

import javafx.application.Application;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.*;
import javafx.scene.input.KeyCode;
import javafx.animation.*;
import javafx.util.Duration;

public class Pendulum extends Application{

    public void start(Stage stage) {
        Swing pendulum = new Swing(); // create new canvas
        pendulum.setOnMousePressed(e -> pendulum.stop1());
        pendulum.setOnMouseReleased(e -> pendulum.play1());
        /* press mouse to stop the pendulum */

        pendulum.setOnKeyPressed(e -> {
            if (e.getCode() == KeyCode.UP)
                pendulum.increase();
            else if (e.getCode() == KeyCode.DOWN)
                pendulum.decrease();
        });
        /* in/decrease the speed of movement */

        Scene scene = new Scene(pendulum, 400, 400);
        stage.setTitle("Pendulum");
        stage.setScene(scene);
        stage.show(); // display scene

        pendulum.requestFocus();
    }

    public static void main(String[] args) {
        // TODO Auto-generated method stub
        Application.launch(args);
    }

}

class Swing extends Pane {
    private Timeline linemovement;
    private Timeline massmovement;
    private double x = 200 - 200 * Math.sin(Math.PI / 4),
            y = 50 + 200 * Math.sin(Math.PI / 4);
    private Line thread = new Line(200, 50, x, y); // line
    private Circle mass = new Circle(); // pendulum
    private double startAngle = Math.PI / 3; // range of movement
    private double changeAngle = Math.PI / 6000;

    Swing() {
        Circle pivot = new Circle(200, 50, 15); // pivot point
        Arc trackup = new Arc(200, 50, 200, 200, 225, 90);
        mass.setRadius(50);
        trackup.setType(ArcType.OPEN);
        trackup.setFill(Color.TRANSPARENT);
        mass.setFill(Color.CORNFLOWERBLUE);
        pivot.setFill(Color.FIREBRICK);
        getChildren().addAll(thread, mass, pivot, trackup);
        massmovement = new Timeline(new KeyFrame(Duration.millis(0.25),
                e -> moveBallandLine()));
        massmovement.setCycleCount(Timeline.INDEFINITE);
        massmovement.setAutoReverse(true);
        massmovement.play();
        linemovement = new Timeline(new KeyFrame(Duration.millis(0.25),
                e -> moveBallandLine()));
        linemovement.setCycleCount(Timeline.INDEFINITE);
        linemovement.setAutoReverse(true);
        linemovement.play();

    }

    void play1() {
        massmovement.play();
        linemovement.play();
    }

    void stop1() {
        massmovement.pause();
        linemovement.pause();
    }

    void increase() {
        massmovement.setRate(massmovement.getRate() + 1);
        linemovement.setRate(linemovement.getRate() + 1);
    }

    void decrease() {
        massmovement.setRate(massmovement.getRate() > 0 ?
                massmovement.getRate() - 1 : 0);
        linemovement.setRate(linemovement.getRate() > 0 ?
                linemovement.getRate() - 1 : 0);
    }

    private void moveBallandLine() {

        startAngle += changeAngle; // indicate the change in angle

        if (startAngle > (Math.PI * 2 / 3)) {
            changeAngle *= -1;
        }
        if (startAngle < (Math.PI / 3)) {
            changeAngle *= -1;
        }
        /* Direction changes whenever one of the boundaries is reached */

        if (startAngle < (Math.PI / 2)) {
            x = 200 - 200 * Math.cos(startAngle);
            y = 50 + 200 * Math.sin(startAngle);
            /* Get the position of next frame when pendulum is on the left of the pivot */
        }
        else {
            x = 200 + 200 * Math.sin(startAngle - (Math.PI / 2));
            y = 50 + 200 * Math.sin(Math.PI - startAngle);
            /* Get position on the right of the pivot */

        }
        thread.setEndX(x);
        thread.setEndY(y);
        mass.setCenterX(x);
        mass.setCenterY(y); // Set the positions






    }




}

