import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.TreeMap;

public class GroupCmd extends LibraryCommand{

    /** Grouping titles. */
    public static final String TITLE = "TITLE";
    /** Grouping authors. */
    public static final String AUTHOR = "AUTHOR";
    /** Indent Block for displaying contents of each group. */
    public static final String INDENT_GROUP = "    ";

    private String groupKey;
    private TreeMap<String, ArrayList<String>> groups = new TreeMap<>();

    public GroupCmd(String argumentInput) {
        super(CommandType.GROUP, argumentInput);
    }

    @Override
    public void execute(LibraryData data) {
        Objects.requireNonNull(data, "Data loaded cannot be null.");

        List<BookEntry> books = data.getBookData();
        // Check empty
        if (books.isEmpty()) {
            System.out.println("The library has no book entries.");
            return;
        }

        switch (groupKey) {
            case TITLE:
                books.forEach(book -> {
                    String firstLetter = retrieveFirstLetter(book.getTitle());

                    if (!groups.containsKey(firstLetter)) {
                        groups.put(firstLetter, new ArrayList<>());
                    }
                    groups.get(firstLetter).add(book.getTitle());
                });
                break;
            case AUTHOR:
                books.forEach(book -> {
                    String[] authors = book.getAuthors();
                    for (String author: authors) {
                        if (!groups.containsKey(author)) {
                            groups.put(author, new ArrayList<>());
                        }
                        groups.get(author).add(book.getTitle());
                    }
                });
                break;
        }
        displayGroups();
    }

    @Override
    protected boolean parseArguments(String argumentInput) {
        boolean isTitle = argumentInput.equals(TITLE);
        boolean isAuthor = argumentInput.equals(AUTHOR);

        this.groupKey = argumentInput;
        return isTitle || isAuthor;
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                                                    //
//                                             PRIVATE METHODS SECTION                                                //
//                                                                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    private String retrieveFirstLetter(String title) {
        char firstLetter = Character.toUpperCase(title.charAt(0));
        if (Character.isDigit(firstLetter)) {
            return "[0-9]";
        } else {
            return Character.toString(firstLetter);
        }
    }

    private void displayGroups() {
        if (groupKey.equals(TITLE)) {
            System.out.println("Grouped data by TITLE");
        } else if (groupKey.equals(AUTHOR)) {
            System.out.println("Grouped data by AUTHOR");
        } else {
            throw new IllegalArgumentException();
        }
        groups.forEach((group, contents) -> {
            System.out.println("## " + group);
            contents.forEach(content ->
                    System.out.println(INDENT_GROUP + content));
        });
    }
}
