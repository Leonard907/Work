import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** 
 * Class responsible for loading
 * book data from file.
 */
public class LibraryFileLoader {

    /** Delimiter for contents in .csv file. */
    public static final String DELIMITER_CSV = ",";
    /** Delimiter for authors string in .csv file. */
    public static final String DELIMITER_AUTHORS = "-";

    /** Index of title of the book in .csv file. */
    public static final int INDEX_TITLE = 0;
    /** Index of authors of the book in .csv file. */
    public static final int INDEX_AUTHORS = 1;
    /** Index of rating of the book in .csv file. */
    public static final int INDEX_RATING = 2;
    /** Index of ISBN code of the book in .csv file. */
    public static final int INDEX_ISBN = 3;
    /** Index of No. of pages of the book in .csv file. */
    public static final int INDEX_PAGES = 4;

    /**
     * Contains all lines read from a book data file using
     * the loadFileContent method.
     * 
     * This field can be null if loadFileContent was not called
     * for a valid Path yet.
     * 
     * NOTE: Individual line entries do not include line breaks at the 
     * end of each line.
     */


    /** Create a new loader. No file content has been loaded yet. */
    public LibraryFileLoader() { 
        fileContent = null;
    }

    /**
     * Load all lines from the specified book data file and
     * save them for later parsing with the parseFileContent method.
     * 
     * This method has to be called before the parseFileContent method
     * can be executed successfully.
     * 
     * @param fileName file path with book data
     * @return true if book data could be loaded successfully, false otherwise
     * @throws NullPointerException if the given file name is null
     */
    public boolean loadFileContent(Path fileName) {
        Objects.requireNonNull(fileName, "Given filename must not be null.");
        boolean success = false;

        try {
            fileContent = Files.readAllLines(fileName);
            success = true;
        } catch (IOException | SecurityException e) {
            System.err.println("ERROR: Reading file content failed: " + e);
        }

        return success;
    }

    /**
     * Has file content been loaded already?
     * @return true if file content has been loaded already.
     */
    public boolean contentLoaded() {
        return fileContent != null;
    }

    /**
     * Parse file content loaded previously with the loadFileContent method.
     * 
     * @return books parsed from the previously loaded book data or an empty list
     * if no book data has been loaded yet.
     */
    public List<BookEntry> parseFileContent() {
        // Check contents are loaded or not.
        if (!contentLoaded()) {
            System.err.println("ERROR: No content loaded before parsing.");
            return new ArrayList<>();
        }

        // Remove headers
        fileContent = fileContent.subList(1, fileContent.size());

        ArrayList<BookEntry> bookEntriesFromFileContents = new ArrayList<>();
        // Iterate through fileContent and add items
        fileContent.forEach(book -> {
            String[] bookInformation = book.split(DELIMITER_CSV);

            // Extract information from the book
            String title = bookInformation[INDEX_TITLE];
            String[] authors = bookInformation[INDEX_AUTHORS].split(DELIMITER_AUTHORS);
            float rating = Float.parseFloat(bookInformation[INDEX_RATING]);
            String ISBN = bookInformation[INDEX_ISBN];
            int pages = Integer.parseInt(bookInformation[INDEX_PAGES]);

            bookEntriesFromFileContents.add(
                    new BookEntry(title, authors, rating, ISBN, pages)
            );
        });
        return bookEntriesFromFileContents;
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                                    //
//                                             PRIVATE METHODS SECTION                                                //
//                                                                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private void checkNullContents() {
        fileContent.forEach(bookInformation -> {
            Objects.requireNonNull(bookInformation, "Contents in the array cannot be null");
        });
    }
}
