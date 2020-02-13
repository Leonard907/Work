package week5;

public class AbsolutePath {
    public static String ensureAbsolute(String path) {
        if (path.startsWith("/"))
            return path;
        else
            return System.getProperty("user.dir") + "/" + path;
    }

    public static String[] absoluteSplitPath(String s) {
        String[] splitPath = SplitPathName.splitPath(s);
        splitPath[0] = ensureAbsolute(splitPath[0]);
        return splitPath;
    }
}
