import java.util.List;
import java.util.Objects;

public class ListCmd extends LibraryCommand {

    /** Argument indicating short message display. */
    private static final String DISPLAY_SHORT = "short";
    /** Argument indicating long message display. */
    private static final String DISPLAY_LONG = "long";
    /** Argument indicating blank message. */
    private static final String DISPLAY_BLANK = "";
    /** Argument indicating a new line. */
    private static final String NEW_LINE = "\n";

    /** String storing the listing way. */
    private String listingType;

    /**
     * Create a LIST command
     * @param argumentInput argument input followed by the Command
     */
    public ListCmd(String argumentInput) {
        super(CommandType.LIST, argumentInput);
    }

    /**
     * Display book contents.
     * @param data book data to be considered for command execution.
     */
    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        List<BookEntry> books = data.getBookData();
        // Check whether the library is empty or not.
        if (books.isEmpty()) {
            System.out.println("The library has no book entries.");
            return;
        }
        System.out.println(books.size() + " books in library:");
        switch (listingType) {
            case DISPLAY_BLANK:
                // Default case, follow the "short" case
            case DISPLAY_SHORT:
                // Print only the title
                for (BookEntry book: books) {
                    System.out.println(book.getTitle());
                }
                break;
            case DISPLAY_LONG:
                for (BookEntry book: books) {
                    System.out.println(book + NEW_LINE);
                }
                break;
        }
    }

    /**
     * Determine whether the input argument is valid or not.
     * Valid arguments are: long, short.
     * Blank argument is also accepted and interpreted as "short"
     *
     * @param argumentInput argument input for this command
     * @return whether the input argument is valid or not.
     */
    @Override
    protected boolean parseArguments(String argumentInput) {
        // Remove spaces
        argumentInput = argumentInput.trim();

        boolean isShort = argumentInput.equals(DISPLAY_SHORT);
        boolean isLong = argumentInput.equals(DISPLAY_LONG);
        boolean isBlank = argumentInput.isBlank();

        this.listingType = argumentInput;

        return isShort || isLong || isBlank;
    }
}
