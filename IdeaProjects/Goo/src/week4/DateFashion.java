package week4;

public class DateFashion {
    public static void main(String[] args) {
        System.out.println(dateFashion(Integer.parseInt(args[0]), Integer.parseInt(args[1])));
    }
    public static int dateFashion(int you, int date) {
        if (you <= 2 || date <= 2)
            return 0;
        else if (you >= 8 || date >= 8)
            return 2;
        else
            return 1;
    }
}
