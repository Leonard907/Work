package week6;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


public class FavouritesTest {

    private PrintStream originalOut;
    private ByteArrayOutputStream capturedOut;

    /**
     * Temporarily captures the output to the standard output stream.
     * This method is run before every testcase.
     */
    @Before
    public void captureOutput() {
        capturedOut = new ByteArrayOutputStream();
        originalOut = System.out;
        System.setOut(new PrintStream(capturedOut));
    }

    /**
     * Restores the standard output stream.
     * This method is run after every testcase.
     */
    @After
    public void restoreOutput() {
        System.setOut(originalOut);
    }

    @Test
    public void testShowFavourites_emptyList() {
        Favourites favourites = new Favourites();
        favourites.showFavourites();
        assertEquals("", capturedOut.toString().trim());
    }

    /**
     * Convert Windows format linefeeds to Unix/Linux/OSX format for string comparison
     * @param input input string containing newlines in '\n', '\r', '\r\n' format
     * @return string containing only '\n' newlines
     */
    private String convertToUnix(String input) {
        if (input == null) {
            return null;
        }
        return input.replaceAll("\r\n", "\n").replaceAll("\r", "\n");
    }

    @Test
    public void testShowFavourites_partialList() {
        Favourites favourites = new Favourites();
        favourites.addTrack("Fun", "Some Nights");
        favourites.addTrack("Oliver Tank", "Help You Breathe");
        favourites.addTrack("Horse Feathers", "Fit Against the Country");
        favourites.showFavourites();

        String expectedOutput = "Some Nights | Fun\n" +
                "Help You Breathe | Oliver Tank\n" +
                "Fit Against the Country | Horse Feathers";
        assertEquals(expectedOutput, convertToUnix(capturedOut.toString().trim()));
    }

    @Test
    public void testShowFavourites_fullList() {
        Favourites favourites = new Favourites();
        favourites.addTrack("Fun", "Some Nights");
        favourites.addTrack("Oliver Tank", "Help You Breathe");
        favourites.addTrack("Horse Feathers", "Fit Against the Country");
        favourites.addTrack("Emile Sande", "Country House");
        favourites.addTrack("Fun", "Walking the Dog");
        favourites.showFavourites();

        String expectedOutput = "Some Nights | Fun\n" +
                "Help You Breathe | Oliver Tank\n" +
                "Fit Against the Country | Horse Feathers\n" +
                "Country House | Emile Sande\n" +
                "Walking the Dog | Fun";
        assertEquals(expectedOutput, convertToUnix(capturedOut.toString().trim()));
    }

    @Test
    public void testShowFavourites_tooManyItems() {
        Favourites favourites = new Favourites();
        favourites.addTrack("Fun", "Some Nights");
        favourites.addTrack("Oliver Tank", "Help You Breathe");
        favourites.addTrack("Horse Feathers", "Fit Against the Country");
        favourites.addTrack("Emile Sande", "Country House");
        favourites.addTrack("Fun", "Walking the Dog");
        favourites.addTrack("Porcelain Raft", "Put Me To Sleep");
        favourites.showFavourites();

        String expectedOutput = "Sorry, can't add: Put Me To Sleep | Porcelain Raft\n" +
                "\n" +
                "Some Nights | Fun\n" +
                "Help You Breathe | Oliver Tank\n" +
                "Fit Against the Country | Horse Feathers\n" +
                "Country House | Emile Sande\n" +
                "Walking the Dog | Fun";
        assertEquals(expectedOutput, convertToUnix(capturedOut.toString().trim()));
    }

}
