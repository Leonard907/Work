package week4;

public class ErrorHandlingException {
    public static String lowerAndTrim(String string) {
        if (string == null)
            throw new NullPointerException();
        return string.trim().toLowerCase();
    }
    public static void formatText(String[] strings) {
        int count = 0;
        for (String string: strings) {
            try {
                String format = lowerAndTrim(string);
                System.out.println(format);
            }
            catch (RuntimeException ex) {
                count += 1;
            }
        }
        System.out.println(count);
    }
}
