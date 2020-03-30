package one;

import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.scene.layout.*;
import javafx.scene.Scene;
import javafx.scene.shape.*;
import javafx.scene.control.*;

public class H_Tree extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        HPane tree = new HPane();
        TextField Order = new TextField();
        Order.setOnAction(e -> tree.setOrder(Integer.parseInt(Order.getText())));
        Order.setPrefColumnCount(4);
        Order.setAlignment(Pos.CENTER);
        HBox label = new HBox(10);
        label.getChildren().addAll(new Label("Order: "), Order);
        label.setAlignment(Pos.CENTER);
        BorderPane pane = new BorderPane();
        pane.setCenter(tree);
        pane.setBottom(label);



        Scene scene = new Scene(pane, 1000, 1000);
        primaryStage.setTitle("odd");
        primaryStage.setScene(scene);
        primaryStage.show();


    }



}

class HPane extends Pane {
    private int order = 3;
    private double baseWidth = 500;
    private double baseHeight = 500;

    HPane() {

    }

    void setOrder(int order){
        this.order = order;
        paint();

    }

    void paint(){
        getChildren().clear();
        displayH(order, 500, 500);

    }

    void displayH(int order, double xPos, double yPos) {
        double p = 4 * Math.pow(2, this.order + 1 - order);

        if (order == 0) {
            Line v1 = new Line(xPos - baseWidth / p, yPos - baseHeight / p, xPos - baseWidth / p, yPos + baseHeight / p);
            Line v2 = new Line(xPos + baseWidth / p, yPos - baseHeight / p, xPos + baseWidth / p, yPos + baseHeight / p);
            Line h = new Line(xPos - baseWidth / p, yPos, xPos + baseWidth / p, yPos);
            v1.setStroke(Color.BLACK);
            v2.setStroke(Color.BLACK);
            h.setStroke(Color.BLACK);
            getChildren().addAll(v1, v2, h);


        }
        else {

            Line v1 = new Line(xPos - baseWidth / p, yPos - baseHeight / p, xPos - baseWidth / p, yPos + baseHeight / p);
            Line v2 = new Line(xPos + baseWidth / p, yPos - baseHeight / p, xPos + baseWidth / p, yPos + baseHeight / p);
            Line h = new Line(xPos - baseWidth / p, yPos, xPos + baseWidth / p, yPos);
            v1.setStroke(Color.BLACK);
            v2.setStroke(Color.BLACK);
            h.setStroke(Color.BLACK);
            getChildren().addAll(v1, v2, h);
            displayH(order - 1, xPos - baseWidth / p, yPos - baseHeight / p);
            displayH(order - 1, xPos - baseWidth / p, yPos + baseHeight / p);
            displayH(order - 1, xPos + baseWidth / p, yPos - baseHeight / p);
            displayH(order - 1, xPos + baseWidth / p, yPos + baseHeight / p);



        }

    }


}
