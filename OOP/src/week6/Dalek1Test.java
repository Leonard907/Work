package week6;

import static org.junit.Assert.assertArrayEquals;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class Dalek1Test {

    @Test
    public void testCallingMain() {
        // Call Dalek1.main() and check the output is as expected.
        String[] args = { "0.0", "1", "1" };
        String result = captureOutputOfMain(args);

        String[] resultLines = result.split("[\r\n]+");
        String[] expectedResult = {
                "[1] [2] [3] [4] [5] [6] [7] [8] [9] [10] Out of power!",
                "Battery charge is: 2.5",
                "Battery charge is: 3.0",
                "[1] [2] [3] [4] [5]",
        };
        assertArrayEquals(expectedResult, resultLines);
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
            Dalek1.main(args);
        }
        finally {
            System.setOut(originalOut);
        }
        return outputStream.toString().trim();
    }

}
