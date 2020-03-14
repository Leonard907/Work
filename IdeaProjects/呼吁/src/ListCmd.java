import java.util.List;
import java.util.Objects;

public class ListCmd extends LibraryCommand {

    /** Argument indicating short message display. */
    public static final String DISPLAY_SHORT = "short";
    /** Argument indicating long message display. */
    public static final String DISPLAY_LONG = "long";
    /** Argument indicating blank message. */
    public static final String DISPLAY_BLANK = "";

    private String listingType;

    public ListCmd(String argumentInput) {
        super(CommandType.LIST, argumentInput);
    }

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
                books.forEach(book -> System.out.println(book.getTitle()));
                break;
            case DISPLAY_LONG:
                books.forEach(book -> System.out.println(book + BookEntry.NEW_LINE));
                break;
        }
    }



    @Override
    protected boolean parseArguments(String argumentInput) {
        boolean isShort = argumentInput.equals(DISPLAY_SHORT);
        boolean isLong = argumentInput.equals(DISPLAY_LONG);
        boolean isBlank = argumentInput.isBlank();

        this.listingType = argumentInput;

        return isShort || isLong || isBlank;
    }
}
