package Sort_Algorithm;

/*
Working Mechanism:
1. Separate whole list into two equal size parts
2. Repeat until each sublist has size 1
3. Merge backwards, combining sublist in order to make them sorted

Complexity: O(n * log(n))
*/

public class Merge {
    public static void main(String[] args) {
        int[] list = {2,3,2,5,6,1,-2,3,14,12};
        Merge_Sort sort_machine = new Merge_Sort();
        sort_machine.sort(list);
        for (int i: list) {
            System.out.print(i + " ");
        }
    }
}

class Merge_Sort implements Sort {
    @Override
    public void sort(int[] list) {
        if (list.length > 1) {
            int pivot = list.length / 2;
            int[] first_half = new int[pivot];
            System.arraycopy(list, 0, first_half, 0, pivot);
            sort(first_half);
            int[] second_half = new int[list.length - pivot];
            System.arraycopy(list, pivot, second_half, 0, second_half.length);
            sort(second_half);
            merge(first_half, second_half, list);
        }
    }

    private void merge(int[] first, int[] second, int[] list) {
        int index_first = 0;
        int index_second = 0;
        int index_final = 0;

        while (index_first < first.length && index_second < second.length) {
            if (first[index_first] < second[index_second]) {
                list[index_final++] = first[index_first++];
            }
            else {
                list[index_final++] = second[index_second++];
            }
        }

        while (index_first < first.length) {
            list[index_final++] = first[index_first++];
        }

        while (index_second < second.length) {
            list[index_final++] = second[index_second++];
        }
    }
}