package Sort_Algorithm;

/*
Working Mechanism:
1. Start at the first item (pivot)
2. From left and right simultaneously.
      If on left an item greater than pivot found
   && If on right an item smaller than pivot found
   -> Swap two items
3. Repeat 2 until searches coincide (low > high)
4 (Important). Decrease high to find the first item smaller than pivot,
               then swap it with the pivot
        This is to avoid cases such as length 2 lists sorting
5. Continue 1~4 for sublist left and right (divided by the swapped pivot)

Complexity: O(n^2) -> Worst case
*/

public class Quick {
    public static void main(String[] args) {
        int[] list = {2,3,2,5,6,1,-2,3,14,12};
        Quick_Sort sort_machine = new Quick_Sort();
        sort_machine.sort(list);
        for (int i: list) {
            System.out.print(i + " ");
        }
    }
}

class Quick_Sort implements Sort {
    @Override
    public void sort(int[] list) {
        quick_sort(list, 0, list.length - 1);
    }

    private void quick_sort(int[] list, int first, int end) {
        int pivot = list[first];
        int low = first + 1;
        int high = end;

        while (high > low) {
            while (list[low] <= pivot)
                low++;

            while (list[high] > pivot)
                high--;

            if (high > low) {
                int temp = list[low];
                list[low] = list[high];
                list[high] = temp;
            }
        }

        // Avoid cases where low == high. E.g Performing operation on a length 2 list
        while (list[high] >= pivot && high > first) {
            high--;
        }
        list[first] = list[high];
        list[high] = pivot;

        if (first < high - 1) {
            quick_sort(list, first, high - 1);
        }
        if (low < end) {
            quick_sort(list, low, end);
        }
    }
}