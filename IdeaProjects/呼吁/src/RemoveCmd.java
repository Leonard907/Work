import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class RemoveCmd extends LibraryCommand {

    /** Separator between <b>Title / Author</b> and <b>Query name</b>. */
    public static final String SEPARATOR_ARGUMENT = " ";
    /** Removing titles. */
    public static final String TITLE = "TITLE";
    /** Removing authors. */
    public static final String AUTHOR = "AUTHOR";

    private String removeType;
    private String removeKey;
    private boolean removeSuccessful = false;
    private int removedBooks = 0;
    private ArrayList<BookEntry> booksToBeRemoved = new ArrayList<>();

    public RemoveCmd(String argumentInput) {
        super(CommandType.REMOVE, argumentInput);
    }

    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        List<BookEntry> books = data.getBookData();

        switch (removeType) {
            case TITLE:
                books.forEach(book -> {
                    if (book.getTitle().equals(removeKey)) {
                        removeBook(book);
                        System.out.println(book.getTitle() + ": removed successfully.");
                    }
                });

                books.removeAll(booksToBeRemoved);

                if (!removeSuccessful) {
                    System.out.println(removeKey + ": not found.");
                }
                break;
            case AUTHOR:
                books.forEach(book -> {
                    List<String> authors = Arrays.asList(book.getAuthors());
                    if (authors.contains(removeKey)) {
                        removeBook(book);
                        removedBooks++;
                    }
                });

                books.removeAll(booksToBeRemoved);
                System.out.println(removedBooks + " books removed for author: " + removeKey);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    protected boolean parseArguments(String argumentInput) {
        try {
            parseTitle_Author(argumentInput);

            boolean isTitle = removeType.equals(TITLE);
            boolean isAuthor = removeType.equals(AUTHOR);
            boolean keyNotBlank = !removeKey.isBlank();

            return (isTitle || isAuthor) && keyNotBlank;
        } catch (IllegalArgumentException ex) {
            return false;
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                                    //
//                                             PRIVATE METHODS SECTION                                                //
//                                                                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private void parseTitle_Author(String argumentInput) {
        int indexOfSeparator = argumentInput.indexOf(SEPARATOR_ARGUMENT);

        if (indexOfSeparator != -1) {
            String removeType = argumentInput.substring(0, indexOfSeparator);
            String removeKey = argumentInput.substring(indexOfSeparator + 1);

            this.removeType = removeType;
            this.removeKey = removeKey;
            return;
        }

        // No separator detected.
        throw new IllegalArgumentException("Invalid argument input");
    }

    private void removeBook(BookEntry book) {
        booksToBeRemoved.add(book);
        removeSuccessful = true;
    }
}
