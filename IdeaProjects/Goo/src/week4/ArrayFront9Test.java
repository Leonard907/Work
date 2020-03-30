package week4;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class ArrayFront9Test {

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
            ArrayFront9.main(args);
        }
        finally {
            System.setOut(originalOut);
        }
        return outputStream.toString().trim();
    }

    @Test
    public void testCallingMain() {
        // Call ArrayFront9.main() and check the output is as expected.
        String[] args = { "1", "2", "9", "3", "4" };
        assertEquals("true", captureOutputOfMain(args));

        args = new String[] { "1", "2", "3", "4", "5" };
        assertEquals("false", captureOutputOfMain(args));
    }

    @Test
    public void testArrayFront9() {

        // Test empty array
        assertFalse(ArrayFront9.arrayFront9(new int[] {}));

        // Test single value
        assertFalse(ArrayFront9.arrayFront9(new int[] { 1 }));
        assertTrue(ArrayFront9.arrayFront9(new int[] { 9 }));

        // Test multiple values
        assertTrue(ArrayFront9.arrayFront9(new int[] { 1, 9 }));
        assertTrue(ArrayFront9.arrayFront9(new int[] { 1, 2, 9 }));
        assertTrue(ArrayFront9.arrayFront9(new int[] { 1, 2, 9, 4 }));
        assertTrue(ArrayFront9.arrayFront9(new int[] { 1, 2, 9, 3, 4 }));
        assertFalse(ArrayFront9.arrayFront9(new int[] { 1, 2, 3, 4 }));
        assertFalse(ArrayFront9.arrayFront9(new int[] { 1, 2, 3, 4, 9 }));
        assertFalse(ArrayFront9.arrayFront9(new int[] { 1, 2, 3, 4, 5 }));
    }

}
