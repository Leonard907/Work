package week5;

public class SplitPathName {
    public static String[] splitPath(String s) {
        int lastSlash = s.lastIndexOf('/');
        int lastPoint = s.lastIndexOf('.');
        String[] result = new String[4];
        result[0] = s.substring(0, lastSlash + 1);
        result[1] = s.substring(lastSlash + 1);
        if (lastPoint == -1) {
            lastPoint = s.length();
        }
        result[2] = s.substring(lastSlash + 1, lastPoint);
        result[3] = s.substring(lastPoint);
        return result;
    }

    public static void main(String[] args) {
        for (String path: args) {
            String[] sep = splitPath(path);
            System.out.println("File: " + sep[1] + " Type: " + sep[3] + " [" + sep[0] + "]");
        }
    }
}
