import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class RemoveCmd extends LibraryCommand {

    /** Separator between <b>Title / Author</b> and <b>Query name</b>. */
    private static final String SEPARATOR_ARGUMENT = " ";
    /** Removing titles. */
    private static final String TITLE = "TITLE";
    /** Removing authors. */
    private static final String AUTHOR = "AUTHOR";

    /** Title or author to be removed. */
    private String removeType;
    /** Removed key word. */
    private String removeKey;
    /** For title only, whether some book is removed. */
    private boolean removeSuccessful = false;
    /** For author only, how many books are removed. */
    private int removedBooks = 0;
    /** Books to be removed. */
    private ArrayList<BookEntry> booksToBeRemoved = new ArrayList<>();

    /**
     * Create an REMOVE Command
     * @param argumentInput argument input followed by the Command
     */
    public RemoveCmd(String argumentInput) {
        super(CommandType.REMOVE, argumentInput);
    }

    /**
     * Remove a title or an author from the library.
     * @param data book data to be considered for command execution.
     */
    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        List<BookEntry> books = data.getBookData();

        switch (removeType) {
            case TITLE:
                for (BookEntry book: books) {
                    if (book.getTitle().equals(removeKey)) {
                        removeBook(book);
                        System.out.println(book.getTitle() + ": removed successfully.");
                    }
                }

                books.removeAll(booksToBeRemoved);

                if (!removeSuccessful) {
                    System.out.println(removeKey + ": not found.");
                }
                break;
            case AUTHOR:
                for (BookEntry book: books) {
                    List<String> authors = Arrays.asList(book.getAuthors());
                    if (authors.contains(removeKey)) {
                        removeBook(book);
                        removedBooks++;
                    }
                }

                books.removeAll(booksToBeRemoved);
                System.out.println(removedBooks + " books removed for author: " + removeKey);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    /**
     * Check the input argument is valid or not. Valid input consists of two parts:
     * 1. Title or Author to be removed.
     * 2. The key word of title or author.
     * @param argumentInput argument input for this command
     * @return whether given argument is valid or not.
     */
    @Override
    protected boolean parseArguments(String argumentInput) {
        // Remove spaces
        argumentInput = argumentInput.trim();

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
