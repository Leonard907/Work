import org.w3c.dom.ls.LSOutput;

import java.util.*;

public class GroupCmd extends LibraryCommand{

    /** Grouping titles. */
    private static final String TITLE = "TITLE";
    /** Grouping authors. */
    private static final String AUTHOR = "AUTHOR";
    /** Indent Block for displaying contents of each group. */
    private static final String INDENT_GROUP = "    ";

    /** Title or author group. */
    private String groupKey;
    /** Map containing entries for each group. */
    private TreeMap<String, ArrayList<String>> groups = new TreeMap<>();

    /**
     * Create a GROUP Command
     * @param argumentInput argument input followed by the Command
     */
    public GroupCmd(String argumentInput) {
        super(CommandType.GROUP, argumentInput);
    }

    /**
     * Group the data by title or author.
     * @param data book data to be considered for command execution.
     */
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
                for (BookEntry book: books) {
                    String firstLetter = retrieveFirstLetter(book.getTitle());

                    if (!groups.containsKey(firstLetter)) {
                        groups.put(firstLetter, new ArrayList<>());
                    }
                    groups.get(firstLetter).add(book.getTitle());
                }
                break;
            case AUTHOR:
                for (BookEntry book: books) {
                    String[] authors = book.getAuthors();
                    for (String author: authors) {
                        if (!groups.containsKey(author)) {
                            groups.put(author, new ArrayList<>());
                        }
                        groups.get(author).add(book.getTitle());
                    }
                }
                break;
        }
        displayGroups();
    }

    /**
     * Check whether argument is valid or not. 2 arguments are accepted:
     * 1. TITLE
     * 2. AUTHOR
     * @param argumentInput argument input for this command
     * @return
     */
    @Override
    protected boolean parseArguments(String argumentInput) {
        // Remove spaces
        argumentInput = argumentInput.trim();

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

    /**
     * Determine the group for a title. If start letter is a number, consider it under
     * [0-9].
     * @param title title of the book
     * @return group of the book
     */
    private String retrieveFirstLetter(String title) {
        char firstLetter = Character.toUpperCase(title.charAt(0));
        if (Character.isDigit(firstLetter)) {
            return "[0-9]";
        } else {
            return Character.toString(firstLetter);
        }
    }

    /**
     * Display all groups. Order is already fixed by TreeMap.
     */
    private void displayGroups() {
        if (groupKey.equals(TITLE)) {
            System.out.println("Grouped data by TITLE");
        } else if (groupKey.equals(AUTHOR)) {
            System.out.println("Grouped data by AUTHOR");
        } else {
            throw new IllegalArgumentException();
        }
        for (Map.Entry<String, ArrayList<String>> group: groups.entrySet()) {
            ArrayList<String> contents = group.getValue();
            System.out.println("## " + group.getKey());
            for (String content: contents) {
                System.out.println(INDENT_GROUP + content);
            }
        }
    }
}
