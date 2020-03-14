import java.util.List;
import java.util.Objects;

public class SearchCmd extends LibraryCommand {

    /** Delimiter between words in title. */
    public static final String DELIMITER_WORD = " ";

    private String searchKey;
    private boolean foundMatchingBook;

    public SearchCmd(String argumentInput) {
        super(CommandType.SEARCH, argumentInput);
    }

    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");
        List<BookEntry> books = data.getBookData();
        String searchKeyIgnoreCase = searchKey.toLowerCase();
        foundMatchingBook = false;

        books.forEach(book -> {
            String lowerCaseBookTitle = book.getTitle().toLowerCase();
            if (lowerCaseBookTitle.contains(searchKeyIgnoreCase)) {
                System.out.println(book.getTitle());
                foundMatchingBook = true;
            }
        });

        if (!foundMatchingBook) {
            System.out.println("No hits found for search term: " + searchKey);
        }
    }

    @Override
    protected boolean parseArguments(String argumentInput) {
        boolean isSingleWord = !argumentInput.contains(DELIMITER_WORD);
        boolean notBlank = !argumentInput.isBlank();

        // Ignore case
        this.searchKey = argumentInput;

        return isSingleWord && notBlank;
    }
}
