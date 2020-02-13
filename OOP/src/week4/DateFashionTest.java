package week4;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class DateFashionTest {

    /**
     * Temporarily captures the output to the standard output stream, then
     * restores the standard output stream once complete.
     *
     * Note: the implementation details of this function are advanced. If you
     * don't understand it just now, don't worry.
     *
     * @param args
     *            arguments to pass to main function of class to be tested
     * @return output result of calling main function of class to be tested
     */
    private String captureOutputOfMain(String args[]) {
        OutputStream outputStream = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;
        System.setOut(new PrintStream(outputStream));
        try {
            DateFashion.main(args);
        }
        finally {
            System.setOut(originalOut);
        }
        return outputStream.toString().trim();
    }

    @Test
    public void testCallingMain() {
        // Call DateFashion.main() and check the output is as expected.
        String[] args = { "5", "10" };
        assertEquals("2", captureOutputOfMain(args));

        args = new String[] { "5", "2" };
        assertEquals("0", captureOutputOfMain(args));

        args = new String[] { "5", "5" };
        assertEquals("1", captureOutputOfMain(args));
    }

    @Test
    public void testDateFashion() {
        // Basic cases
        assertEquals(2, DateFashion.dateFashion(5, 10));
        assertEquals(0, DateFashion.dateFashion(5, 0));
        assertEquals(1, DateFashion.dateFashion(5, 5));

        assertEquals(2, DateFashion.dateFashion(10, 5));
        assertEquals(0, DateFashion.dateFashion(0, 5));

        // Boundary cases - boundaries are at 8-7 and 3-2
        assertEquals(2, DateFashion.dateFashion(8, 8));
        assertEquals(2, DateFashion.dateFashion(8, 7));
        assertEquals(2, DateFashion.dateFashion(8, 3));
        assertEquals(0, DateFashion.dateFashion(8, 2));

        assertEquals(2, DateFashion.dateFashion(7, 8));
        assertEquals(1, DateFashion.dateFashion(7, 7));
        assertEquals(1, DateFashion.dateFashion(7, 3));
        assertEquals(0, DateFashion.dateFashion(7, 2));

        assertEquals(2, DateFashion.dateFashion(3, 8));
        assertEquals(1, DateFashion.dateFashion(3, 7));
        assertEquals(1, DateFashion.dateFashion(3, 3));
        assertEquals(0, DateFashion.dateFashion(3, 2));

        assertEquals(0, DateFashion.dateFashion(2, 8));
        assertEquals(0, DateFashion.dateFashion(2, 7));
        assertEquals(0, DateFashion.dateFashion(2, 3));
        assertEquals(0, DateFashion.dateFashion(2, 2));
    }

}
