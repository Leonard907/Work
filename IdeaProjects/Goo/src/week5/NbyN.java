package week5;

public class NbyN {
    public static int[][] nbyn(int N) {
        int[][] matrix = new int[N][N];
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                if (i == j) {
                    matrix[i][j] = i;
                }
                else {
                    matrix[i][j] = 0;
                }
            }
        }
        return matrix;
    }

    public static void main(String[] args) {
        int[][] test =nbyn(10);
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j ++) {
                System.out.print(test[i][j] + " ");
            }
            System.out.println();
        }
    }
}
