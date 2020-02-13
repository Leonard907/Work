package one;

import java.util.*;

public class AlmostSorted {
    // Complete the almostSorted function below.
    private static void almostSorted(Integer[] arr) {
        int start = arr[0];
        int end = arr[arr.length - 1];
        int front = 0;
        int back = arr.length - 1;
        Integer[] original = arr.clone();
        for (int i = 0; i < arr.length - 1; i++) {
            if (start < arr[i + 1]) {
                front = i + 1;
            }
            else {
                break;
            }
            start = arr[i + 1];
        }

        for (int j = arr.length - 1; j > 0; j--) {
            if (end > arr[j - 1])
                back = j - 1;
            else
                break;
            end = arr[j - 1];
        }

        arr[front] = original[back];
        arr[back] = original[front];
        if (isAscending(arr)) {
            System.out.println("yes");
            System.out.println("swap " + (front + 1) + " " + (back + 1));
            System.exit(0);
        }
        arr = original.clone();
        int temp = arr[front];
        original[front] = arr[back];
        for (int i = front; i < back; i++) {
            original[i + 1] = arr[back + front - i - 1];
            if (temp < arr[i + 1]) {
                System.out.println("no");
                System.exit(0);
            }
            temp = arr[i + 1];
        }
        if (isAscending(original)) {
            System.out.println("yes");
            System.out.println("reverse " + (front + 1) + " " + (back + 1));
        }
        else {
            System.out.println("no");
        }
    }

    private static boolean isAscending(Integer[] arr) {
        int temp = arr[0];
        boolean isAscending = true;
        for (int i = 0; i < arr.length - 1; i++) {
            if (temp >= arr[i + 1]) {
                isAscending = false;
                break;
            }
            temp = arr[i + 1];
        }
        return isAscending;
    }

    private static final Scanner scanner = new Scanner(System.in);

    public static void main(String[] args) {
        int n = scanner.nextInt();
        scanner.skip("(\r\n|[\n\r\u2028\u2029\u0085])?");

        Integer[] arr = new Integer[n];

        String[] arrItems = scanner.nextLine().split(" ");

        for (int i = 0; i < n; i++) {
            int arrItem = Integer.parseInt(arrItems[i]);
            arr[i] = arrItem;
        }

        almostSorted(arr);

        scanner.close();


    }
}
