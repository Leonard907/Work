package week6;

public class Dalek1 {

    private double batteryCharge = 5.0; // instance variable

    public void batteryReCharge(double c) {
        batteryCharge += c;
        System.out.println("Battery charge is: " + batteryCharge);
    }

    public void move(int distance) {
        StringBuilder movement = new StringBuilder();
        for (int i = 1; i <= distance; i++) {
            if (batteryCharge >= 0.5) {
                batteryCharge -= 0.5;
                System.out.print(String.format("[%d] ", i));
            }
            else {
                System.out.println("Out of power!");
                break;
            }
        }
    }

    public static void main(String[] args) {
        Dalek1 d = new Dalek1(); // start off with a well-charged battery
        d.move(11);              // move around and use up the charge
        d.batteryReCharge(2.5);  // get a new charge
        d.batteryReCharge(0.5);  // add a bit more
        d.move(5);               // move some more
    }

}
