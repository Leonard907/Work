import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class BinaryConverterTest {
    @Test
    public void testPositive1() {
        int input = 1;
        int expectedOutput = 1;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    @Test
    public void testPositive2() {
        // TODO: for input "10", the expected output should be "2"
        int input = 10;
        int expectedOutput = 2;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    @Test
    public void testPositive4() {
        // TODO: fill this in
        // what input would you expect to result in 4?
        int input = 100;
        int expectedOutput = 4;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    @Test
    public void testPositive127() {
        // TODO: fill this in
        int input = 1111111;
        int expectedOutput = 127;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    @Test
    public void testPositive128() {
        // TODO: fill this in
        int input = 10000000;
        int expectedOutput = 128;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    @Test (timeout = 1000)
    public void testZero() {
        assertEquals(0, BinaryConverter.binaryToDecimal(0));
    }

    @Test
    public void test56() {
        int input = 111000;
        int expectedOutput = 56;
        assertEquals(expectedOutput, BinaryConverter.binaryToDecimal(input));
    }

    // TODO: can you think of any more tests?
    // add them here!
}
