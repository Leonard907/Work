import org.junit.Test;

import java.lang.reflect.Modifier;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class BookEntryAdvancedTest extends BookEntryTest {
    @Test
    public void checkDecimalPlaces() {
        String title = "Do it";
        String[] authors = new String[] {"Qennys"};
        float rating = 4.5902f;
        String ISBN = "300";
        int pages = 3;
        BookEntry a = new BookEntry(title, authors, rating, ISBN, pages);
        String expected = "Do it\n" + "by Qennys\n" + "Rating: 4.59\n" + "ISBN: 300\n" + "3 pages";
        assertEquals(expected, a.toString());
    }

    @Test
    public void checkFieldsValidity() {
    }
}
