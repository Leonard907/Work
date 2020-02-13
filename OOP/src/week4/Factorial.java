package week4;

public class Factorial {
    public static int factorial(int N) {
        // ADD CODE HERE
        if (N == 0) {
            return 1;
        }
        else {
            return N * factorial(N - 1);
        }
    }
}
