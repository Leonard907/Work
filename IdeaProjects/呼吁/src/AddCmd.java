import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

public class AddCmd extends LibraryCommand {

    /** Delimiter for separating a path name for a file. */
    public static final String PATH_DELIMITER = "/";
    /** Suffix of valid file name. */
    public static final String VALID_SUFFIX = ".csv";

    private Path dataPath;

    public AddCmd(String argumentInput) {
        super(CommandType.ADD, argumentInput);
    }

    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        data.loadData(dataPath);
    }

    @Override
    protected boolean parseArguments(String argumentInput) {
        Objects.requireNonNull(argumentInput, "Given input argument must not be null.");

        String[] separatedPaths = argumentInput.split(PATH_DELIMITER);
        String fileName = getFileNameFromPath(separatedPaths);
        if (fileName.endsWith(VALID_SUFFIX)) {
            dataPath = Paths.get(argumentInput);
            return true;
        } else {
            return false;
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                                    //
//                                             PRIVATE METHODS SECTION                                                //
//                                                                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Extract the file name from an array of separated paths.
     * @param paths Paths separated by {@value PATH_DELIMITER}.
     * @return A string indicating the file name.
     */
    private static String getFileNameFromPath(String[] paths) {
        int indexOfFileName = paths.length - 1;
        return paths[indexOfFileName];
    }
}
