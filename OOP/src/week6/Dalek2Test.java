package week6;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class Dalek2Test {

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
    public void testSpeak_noSayings() {
        Dalek2 dalek2 = new Dalek2();
        dalek2.speak();
        String result = capturedOut.toString().trim();
        assertEquals("No utterances installed!", result);
    }

    @Test
    public void testSpeak_withSayings() {
        Dalek2 dalek2 = new Dalek2();
        dalek2.setSayings(new String[] { "ELEVATE!"});
        dalek2.speak();
        String result = capturedOut.toString().trim();
        assertEquals("ELEVATE!", result);
    }

    @Test
    public void testCallingMain() {
        // Call Dalek2.main() and check the output is as expected.
        String[] args = { "0.0", "1", "1" };
        Dalek2.main(args);

        String result = capturedOut.toString().trim();
        String[] resultLines = result.split("[\r\n]+");

        // Due to the output of the first dalek being random, the assertions in
        // this test case are a bit more complicated than usual.
        Set<String> quotes = new HashSet<>();
        quotes.add("Exterminate, Exterminate!");
        quotes.add("I obey!");
        quotes.add("Exterminate, annihilate, DESTROY!");
        quotes.add("You cannot escape.");
        quotes.add("Daleks do not feel fear.");
        quotes.add("The Daleks must survive!");

        assertEquals(22, resultLines.length);
        assertEquals("Dalek d1 says: ", resultLines[0]);
        for (int i = 1; i < 11; i++) {
            assertTrue(quotes.contains(resultLines[i]));
        }
        assertEquals("Dalek d2 says: ", resultLines[11]);
        for (int i = 12; i < 22; i++) {
            assertTrue(quotes.contains(resultLines[i]));
        }
    }


}