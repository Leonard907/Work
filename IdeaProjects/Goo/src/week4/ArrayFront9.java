package week4;

public class ArrayFront9 {
    public static void main(String[] args) {
        int N = args.length;
        int[] nums = new int[N];
        for (int i = 0; i < N; i++) {
            nums[i] = Integer.parseInt(args[i]);
        }
        System.out.println(arrayFront9(nums));
    }
    public static boolean arrayFront9(int[] nums) {
        boolean has9 = false;
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] == 9 && i < 4) {
                has9 = true;
                break;
            }
            else if (i == 4) {
                break;
            }
        }
        return has9;
    }
}
