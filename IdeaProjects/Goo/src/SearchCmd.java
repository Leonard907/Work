import java.util.List;
import java.util.Objects;

public class SearchCmd extends LibraryCommand {

    /** Delimiter between words in title. */
    private static final String DELIMITER_WORD = " ";

    /** Target search item. */
    private String searchKey;

    /**
     * Create a SEARCH command
     * @param argumentInput argument input followed by the Command
     */
    public SearchCmd(String argumentInput) {
        super(CommandType.SEARCH, argumentInput);
    }

    /**
     * Search for occurrence of the specified key. Works for title only.
     * @param data book data to be considered for command execution.
     */
    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");
        List<BookEntry> books = data.getBookData();
        // Case is not considered here.
        String searchKeyIgnoreCase = searchKey.toLowerCase();
        boolean foundMatchingBook = false;

        for (BookEntry book: books) {
            String lowerCaseBookTitle = book.getTitle().toLowerCase();

            if (lowerCaseBookTitle.contains(searchKeyIgnoreCase)) {
                System.out.println(book.getTitle());
                foundMatchingBook = true;
            }
        }

        if (!foundMatchingBook) {
            System.out.println("No hits found for search term: " + searchKey);
        }
    }

    /**
     * Check whether argument is valid or not.
     * @param argumentInput argument input for this command
     * @return whether argument is valid or not.
     */
    @Override
    protected boolean parseArguments(String argumentInput) {
        // Remove spaces
        argumentInput = argumentInput.trim();

        boolean isSingleWord = !argumentInput.contains(DELIMITER_WORD);
        boolean notBlank = !argumentInput.isBlank();

        // Ignore case
        this.searchKey = argumentInput;

        return isSingleWord && notBlank;
    }
}
