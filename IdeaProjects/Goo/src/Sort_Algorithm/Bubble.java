package Sort_Algorithm;

/*
Working Mechanism:
1. Starting on first item
2. Compare it with the next item
3. If it is larger, swap
4. Repeat 2,3 for items afterwards
   -> After 1~4 the max element in the list should be placed at the end of the list
5. Repeat 1~4, ignoring the elements which are put to the end of the list each time
   -> In each process, finish at the index which is 1 smaller than the previous
      finishing index
6. If 1~4 does not perform a single swap, list is sorted.

Complexity: O(n^2)
*/

public class Bubble {
    public static void main(String[] args) {
        int[] list = {2,3,2,5,6,1,-2,3,14,12};
        Bubble_Sort sort_machine = new Bubble_Sort();
        sort_machine.sort(list);
        for (int i: list) {
            System.out.print(i + " ");
        }
    }
}

class Bubble_Sort implements Sort {
    @Override
    public void sort(int[] list) {
        for (int i = 0; i < list.length; i++) {
            boolean swapped = false;
            for (int j = 0; j < list.length - i - 1; j++) {
                if (list[j] > list[j + 1]) {
                    int temp = list[j];
                    list[j] = list[j + 1];
                    list[j + 1] = temp;
                    swapped = true;
                }
            }
            if (!swapped) {
                break;
            }
        }
    }
}