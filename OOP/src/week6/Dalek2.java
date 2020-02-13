package week6;

public class Dalek2 {

    private String[] sayings = {};

    public void speak() {
        if (sayings.length == 0)
            System.out.println("No utterances installed!");
        else if (sayings.length == 1)
            System.out.println(sayings[0]);
        else {
            int choice = (int) (Math.random() * sayings.length);
            System.out.println(sayings[choice]);
        }
    }

    public void setSayings(String[] sayings) {
        this.sayings = sayings;
    }

    public static void main(String[] args) {

        Dalek2 d1 = new Dalek2();
        String[] u1 = { "Exterminate, Exterminate!", "I obey!",
                "Exterminate, annihilate, DESTROY!", "You cannot escape.",
                "Daleks do not feel fear.", "The Daleks must survive!" };
        d1.setSayings(u1);

        System.out.println("\nDalek d1 says: ");
        for (int i = 0; i < 10; i++) {
            d1.speak();
        }

        System.out.println("\nDalek d2 says: ");
        Dalek2 d2 = new Dalek2();
        String[] u2 = { "I obey!" };
        d2.setSayings(u2);

        for (int i = 0; i < 10; i++) {
            d2.speak();
        }

    }

}
