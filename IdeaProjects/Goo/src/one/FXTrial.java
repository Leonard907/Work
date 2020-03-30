package one;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Line;
import javafx.stage.Stage;

public class FXTrial extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        Line line = new Line(30,30,30,300);

        Pane pane = new Pane();
        pane.getChildren().add(line);

        Scene scene = new Scene(pane, 400,400);
        primaryStage.setTitle("Calendar");
        primaryStage.setScene(scene);
        primaryStage.show(); // display scene
    }
}
