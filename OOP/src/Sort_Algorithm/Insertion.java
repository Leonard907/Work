package Sort_Algorithm;

/*
Working Mechanism:
1. Starting on index 1 (2nd number)
2. Insert this number to the previous sorted sublist
3. Repeat process 2 for each element from start to the end of the list

Complexity: O(n^2)
*/

public class Insertion {
    public static void main(String[] args) {
        int[] list = {2,3,2,5,6,1,-2,3,14,12};
        Insertion_Sort sort_machine = new Insertion_Sort();
        sort_machine.sort(list);
        for (int i: list) {
            System.out.print(i + " ");
        }
    }
}

class Insertion_Sort implements Sort {
    @Override
    public void sort(int[] list) {
        for (int i = 1; i < list.length; i++) {
            int temp = list[i];
            int j;
            for (j = i - 1; j >= 0; j--) {
                if (list[j] > temp) {
                    list[j + 1] = list[j]; // Move items to the right
                }
                else {
                    break;
                }
            }
            list[j + 1] = temp; // Insert item at the right location
        }
    }
}