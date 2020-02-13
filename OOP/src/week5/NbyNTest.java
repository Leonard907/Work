package week5;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class NbyNTest {

    @Test
    public void testNbyn() {
        int[][] expectedResult = new int[][] { { 0 }, };
        compareArrays(expectedResult, NbyN.nbyn(1));

        expectedResult = new int[][] { { 0, 0, 0 }, { 0, 1, 0 }, { 0, 0, 2 }, };
        compareArrays(expectedResult, NbyN.nbyn(3));

        expectedResult = new int[][] { { 0, 0, 0, 0 }, { 0, 1, 0, 0 }, { 0, 0, 2, 0 }, { 0, 0, 0, 3 }, };
        compareArrays(expectedResult, NbyN.nbyn(4));
    }

    /**
     * A helper function to compare two 2-dimensional arrays of ints.
     *
     * @param expected
     * @param actual
     */
    private void compareArrays(int[][] expected, int[][] actual) {
        assertEquals("Row counts different", expected.length, actual.length);
        for (int i = 0; i < expected.length; i++) {
            assertArrayEquals("Array row " + i, expected[i], actual[i]);
        }
    }

    @Test
    public void testMain() {

        // Call NbyN.main() with argument 1 and check the output is as expected.
        String result = captureOutputOfMain(null);

        // Split the result into individual lines
        String[] resultLines = result.split("[\r\n]+");

        // Check we have exactly 10 result lines
        assertEquals("Number of result lines", 10, resultLines.length);

        // Check the values are correct
        assertEquals("0 0 0 0 0 0 0 0 0 0", resultLines[0].trim());
        assertEquals("0 1 0 0 0 0 0 0 0 0", resultLines[1].trim());
        assertEquals("0 0 2 0 0 0 0 0 0 0", resultLines[2].trim());
        assertEquals("0 0 0 3 0 0 0 0 0 0", resultLines[3].trim());
        assertEquals("0 0 0 0 4 0 0 0 0 0", resultLines[4].trim());
        assertEquals("0 0 0 0 0 5 0 0 0 0", resultLines[5].trim());
        assertEquals("0 0 0 0 0 0 6 0 0 0", resultLines[6].trim());
        assertEquals("0 0 0 0 0 0 0 7 0 0", resultLines[7].trim());
        assertEquals("0 0 0 0 0 0 0 0 8 0", resultLines[8].trim());
        assertEquals("0 0 0 0 0 0 0 0 0 9", resultLines[9].trim());
    }

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
            NbyN.main(args);
        }
        finally {
            System.setOut(originalOut);
        }
        return outputStream.toString().trim();
    }

}
