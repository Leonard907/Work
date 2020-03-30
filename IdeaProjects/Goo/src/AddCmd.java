import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

public class AddCmd extends LibraryCommand {

    /** Delimiter for separating a path name for a file. */
    private static final String PATH_DELIMITER = "/";
    /** Suffix of valid file name. */
    private static final String VALID_SUFFIX = ".csv";

    /** The path of the data parsed from {@link AddCmd#parseArguments(String argumentInput)}. */
    private Path dataPath;

    /**
     * Create an ADD Command
     * @param argumentInput argument input followed by the Command
     */
    public AddCmd(String argumentInput) {
        super(CommandType.ADD, argumentInput);
    }

    /**
     * Load the given data to the library.
     * @param data book data to be considered for command execution.
     */
    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        data.loadData(dataPath);
    }

    /**
     * Check whether input is in format {@value VALID_SUFFIX}.
     * @param argumentInput argument input for this command
     * @return boolean of whether input is valid or not.
     */
    @Override
    protected boolean parseArguments(String argumentInput) {
        // Remove spaces
        argumentInput = argumentInput.trim();
        System.out.println(argumentInput);

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
