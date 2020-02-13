package Sort_Algorithm;

/*
Working Mechanism:
1. Given the maximum element, construct a counting array that stores the appearance
   of each number
2. Sum the array from left to right to obtain the index each number should appear in
   the output
3. For elements in input array, output them with reference to counting array

Complexity: O(n + k), k is the value of the maximum element
*/
public class Counting {
    public static void main(String[] args) {
        int[] list = {2,3,2,5,6,1,4,3,14,12};
        Counting_Sort sort_machine = new Counting_Sort();
        sort_machine.sort(list);
        for (int i: list) {
            System.out.print(i + " ");
        }
    }
}

class Counting_Sort implements Sort {
    private static int k = 15; // Represents the maximum value in the list
    @Override
    public void sort(int[] list) {
        int[] counting_array = new int[k + 1];
        // Initialize counting array
        for (int i = 0; i < k + 1; i++) {
            counting_array[i] = 0;
        }
        // Count appearances of each item
        for (int num: list) {
            counting_array[num] += 1;
        }
        for (int i = 1; i < k + 1; i++) {
            counting_array[i] += counting_array[i - 1];
        }
        // Output results
        int[] list_copy = list.clone();
        for (int num: list_copy) {
            list[counting_array[num] - 1] = num;
            counting_array[num] -= 1;
        }
    }
}
