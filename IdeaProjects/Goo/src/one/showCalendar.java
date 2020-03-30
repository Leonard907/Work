package one;

import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.layout.*;
import javafx.scene.control.*;
import javafx.scene.text.*;


import java.util.GregorianCalendar;

public class showCalendar extends Application {
    private Button previous = new Button("<---");
    private Button next = new Button("--->");
    private int year = 2019;
    private int month = 7;
    private BorderPane pane = new BorderPane();
    private Calendar Month = new Calendar();

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        BorderPane buttons = new BorderPane();
        buttons.setLeft(previous);
        TextField month = new TextField();
        month.setOnAction(e -> Month.setDay(Month.parseDate(month.getText())));
        Label input = new Label("Input format: MM/YYYY", month);
        input.setContentDisplay(ContentDisplay.RIGHT);
        input.setFont(Font.font("Helvetica",FontWeight.BLACK,
                FontPosture.REGULAR,14));
        buttons.setCenter(input);
        buttons.setRight(next);
        Label Title = Month.getLabel();
        Title.setFont(Font.font("Helvetica", FontWeight.BOLD, FontPosture.ITALIC, 30));
        Title.setAlignment(Pos.TOP_CENTER);
        pane.setTop(Title);
        pane.setCenter(Month);
        pane.setBottom(buttons);

        Scene scene = new Scene(pane, 690, 300);
        primaryStage.setTitle("Calendar");
        primaryStage.setScene(scene);
        primaryStage.show(); // display scene
    }


    class Calendar extends GridPane {
        private String[] months = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October",
                "November", "December"};
        private Label[] weeks = {new Label(" Sunday "), new Label(" Monday "), new Label(" Tuesday "),
                new Label(" Wednesday "), new Label(" Thursday "),
                new Label(" Friday "), new Label(" Saturday ")};
        private Label label = new Label(String.format("  %s, %d", months[month - 1], year));

        private int[] daysInMonth = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

        Calendar() {
            addDay();
            buttonAction(previous,next);
        }

        int[] parseDate(String date) {
            String month = date.substring(0,2);
            String year = date.substring(3);
            int[] input = new int[2];
            input[0] = Integer.parseInt(month); input[1] = Integer.parseInt(year);
            return input;
        }

        void setDay(int[] pair) {
            month = pair[0];
            year = pair[1];
            label.setText(String.format("  %s, %d", months[month - 1], year));
            addDay();
        }

        void buttonAction(Button a, Button b) {
            a.setOnAction(e -> {
                month -= 1;
                if (month == 0) {
                    month = 12;
                    year -= 1;
                }
                label.setText(String.format("  %s, %d", months[month - 1], year));
                addDay();
            });

            b.setOnAction(e -> {
                month += 1;
                if (month == 13){
                    month = 1;
                    year += 1;
                }
                label.setText(String.format("  %s, %d", months[month - 1], year));
                addDay();
            });
        }

        Label getLabel() {
            return label;
        }

        Boolean isLeap(int Year) {
            if (Year % 100 == 0)
                return (Year % 400 == 0);
            else
                return (Year % 4 == 0);

        }

        void addWeek() {
            for (int i = 0; i < 7; i++){
                weeks[i].setFont(Font.font("Times New Roman", FontWeight.BLACK, FontPosture.REGULAR, 20));
                add(weeks[i], i, 0);
            }
        }

        void addDay() {
            getChildren().clear();
            addWeek();
            if (isLeap(year))
                daysInMonth[1] = 29;

            int rowIndex = 1;

            for (int i = 1; i <= daysInMonth[month - 1]; i++) {
                java.util.Calendar date = new GregorianCalendar(year, month - 1, i);
                int getDay = date.get(java.util.Calendar.DAY_OF_WEEK);
                Label Day = new Label(String.format("      %d", i));
                Day.setFont(Font.font("Futura", FontWeight.BLACK, FontPosture.REGULAR, 14));
                add(Day, (getDay - 1),  rowIndex);
                if (getDay == 7)
                    rowIndex += 1;

            }

        }



    }



}



