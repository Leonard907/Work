package week4;

import java.util.Scanner;

public class IsTriangular {
    public static boolean isTri(double a, double b, double c) {
        return (a + b > c) && (a + c > b) && (b + c > a);
    }
}
