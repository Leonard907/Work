import java.util.Arrays;
import java.util.Objects;

/**
 * Immutable class encapsulating data for a single book entry.
 */
public class BookEntry {
    /** Title of the book. */
    private final String title;
    /** Authors of the book. */
    private final String[] authors;
    /** Rating of the book. */
    private final float rating;
    /** ISBN code of the book. */
    private final String ISBN;
    /** No. of pages of the book. */
    private final int pages;

    /** Exception message for book name null. */
    private static final String NULL_NAME = "Book name cannot be null.";
    /** Exception message for authors array null. */
    private static final String NULL_AUTHORS = "Author array cannot be null.";
    /** Exception message for author name null. */
    private static final String NULL_AUTHOR = "Author name cannot be null";
    /** Exception message for invalid book rating. */
    private static final String INVALID_RATING = "Rating must be in range 0~5.";
    /** Exception message for ISBN code null. */
    private static final String NULL_ISBN = "ISBN code cannot be null.";
    /** Exception message for invalid page number. */
    private static final String INVALID_PAGES = "Pages cannot be negative.";

    /** String for starting a new line. */
    private static final String NEW_LINE = "\n";
    /** Delimiter between author names. */
    private static final String AUTHOR_NAME_DELIMITER = ", ";

    /**
     * Set the title, authors, rating, ISBN code and No. of pages from the parameters passed.
     * All fields are not changeable after the initialisation.
     * <p>
     * All objects are not supposed to be null. Rating is between 0 and 5, No. of pages are
     * not negative.
     * </p>
     * @param title Title of the book.
     * @param authors Authors of the book.
     * @param rating Rating of the book.
     * @param ISBN ISBN code of the book.
     * @param pages No. of pages of the book.
     * @throws NullPointerException Whenever {@code title}, {@code authors}, {@code ISBN} are null
     * @throws IllegalArgumentException Whenever {@code rating} is not between 0 and 5, or
     * {@code pages} is negative.
     */
    public BookEntry(String title, String[] authors, float rating, String ISBN, int pages) {
        // Check null objects
        Objects.requireNonNull(title, NULL_NAME);
        Objects.requireNonNull(authors, NULL_AUTHORS);
        checkNullAuthors(authors);
        Objects.requireNonNull(ISBN, NULL_ISBN);
        // Check validity of parameters
        if (rating < 0 || rating > 5) {
            throw new IllegalArgumentException(INVALID_RATING);
        }

        if (pages < 0) {
            throw new IllegalArgumentException(INVALID_PAGES);
        }
        // Set fields
        this.title = title;
        this.authors = authors;
        this.rating = Float.parseFloat(ratingTwoDecimalPlaces(rating));
        this.ISBN = ISBN;
        this.pages = pages;
    }

    /**
     * Return the title of the book.
     * @return A string indicating the title.
     */
    public String getTitle() {
        return title;
    }

    /**
     * Return an array of authors of the book.
     * @return A string array of the authors.
     */
    public String[] getAuthors() {
        return authors;
    }

    /**
     * Return the rating of the book.
     * @return Rating for the book.
     */
    public float getRating() {
        return rating;
    }

    /**
     * Return the ISBN code of the book.
     * @return ISBN code of the book.
     */
    public String getISBN() {
        return ISBN;
    }

    /**
     * Return the number of pages of the book.
     * @return Number of pages of the book.
     */
    public int getPages() {
        return pages;
    }

    /**
     * Description of the book. Format:
     * <li>Title</li>
     * <li>Authors</li>
     * <li>Rating</li>
     * <li>ISBN code</li>
     * <li>No. of pages</li>
     * @return A string indicating the description.
     */
    @Override
    public String toString() {
        return title + NEW_LINE +
                "by " + authorToString() + NEW_LINE +
                "Rating: " + ratingTwoDecimalPlaces(rating) + NEW_LINE +
                "ISBN: " + ISBN + NEW_LINE +
                pages + " pages"
                ;
    }

    /**
     * Test whether two instances of {@link BookEntry} is equal in terms of:
     * <li>Title</li>
     * <li>Authors</li>
     * <li>Rating</li>
     * <li>ISBN code</li>
     * <li>No. of pages</li>
     * @param o The instance that the current instance is compared to.
     * @return Whether two instances are equal to each other by the criteria above.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BookEntry bookEntry = (BookEntry) o;
        return Float.compare(bookEntry.rating, rating) == 0 &&
                pages == bookEntry.pages &&
                title.equals(bookEntry.title) &&
                Arrays.equals(authors, bookEntry.authors) &&
                ISBN.equals(bookEntry.ISBN);
    }

    /**
     * Return the hash code of this instance.
     * @return Integer representing the hash code of the instance.
     */
    @Override
    public int hashCode() {
        int result = Objects.hash(title, rating, ISBN, pages);
        result = 31 * result + Arrays.hashCode(authors);
        return result;
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                                    //
//                                             PRIVATE METHODS SECTION                                                //
//                                                                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Convert the author array to a string.
     * @return A string that contains all authors.
     */
    private String authorToString() {
        return String.join(AUTHOR_NAME_DELIMITER, authors);
    }

    /**
     * Keep 2 d.p. of the rating of the book.
     * @return A string indicating the rating to 2 d.p.
     */
    private String ratingTwoDecimalPlaces(float rating) {
        return String.format("%.2f", rating);
    }

    /**
     * Check whether each author name in {@link BookEntry#authors} is null.
     * @param authors A string array of the authors.
     * @throws NullPointerException If one of the element in the array is null.
     */
    private void checkNullAuthors(String[] authors) {
        for (String author: authors) {
            Objects.requireNonNull(author, NULL_AUTHOR);
        }
    }
}



