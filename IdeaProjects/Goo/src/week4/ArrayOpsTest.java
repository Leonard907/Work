package week4;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ArrayOpsTest {

    // Error tolerance in comparing floating point numbers
    private static final double TOLERANCE = 0.0001;

    @Test
    public void testFindMax() {
        assertEquals(1.0, ArrayOps.findMax(new double[] { 1.0 }), TOLERANCE);
        assertEquals(2.1, ArrayOps.findMax(new double[] { 2.1, 2.1 }), TOLERANCE);
        assertEquals(1.0, ArrayOps.findMax(new double[] { 1.0, 0.0, 0.0 }), TOLERANCE);
        assertEquals(1.0, ArrayOps.findMax(new double[] { 0.0, 1.0, 0.0 }), TOLERANCE);
        assertEquals(1.0, ArrayOps.findMax(new double[] { 0.0, 0.0, 1.0 }), TOLERANCE);
        assertEquals(-1.5, ArrayOps.findMax(new double[] { -1.5, -10.0, -10.0 }), TOLERANCE);
        assertEquals(-1.5, ArrayOps.findMax(new double[] { -10.0, -1.5, -10.0 }), TOLERANCE);
        assertEquals(-1.5, ArrayOps.findMax(new double[] { -10.0, -10.0, -1.5 }), TOLERANCE);
    }

    @Test
    public void testSwap() {
        // Test basic case
        double[] input1 = { 1.0, 2.0, 3.0, 4.0 };
        double[] input2 = { 5.0, 4.0, 3.0, 2.0 };

        ArrayOps.swap(input1, input2);

        // Check contents of the arrays
        assertArrayEquals(new double[] { 5.0, 4.0, 3.0, 2.0 }, input1, TOLERANCE);
        assertArrayEquals(new double[] { 1.0, 2.0, 3.0, 4.0 }, input2, TOLERANCE);

        // Test single value array case
        input1 = new double[] { 1.0 };
        input2 = new double[] { 5.0 };

        ArrayOps.swap(input1, input2);

        // Check contents of the arrays
        assertArrayEquals(new double[] { 5.0 }, input1, TOLERANCE);
        assertArrayEquals(new double[] { 1.0 }, input2, TOLERANCE);

        // Test empty array case
        input1 = new double[] {};
        input2 = new double[] {};

        ArrayOps.swap(input1, input2);

        // Check contents of the arrays
        assertArrayEquals(new double[] {}, input1, TOLERANCE);
        assertArrayEquals(new double[] {}, input2, TOLERANCE);
    }

    @Test
    public void testNormaliseInPlace() {
        double[] input = { 11.0, 11.0, 11.0, 11.0, 11.0 };
        ArrayOps.normaliseInPlace(input);
        assertArrayEquals(new double[] { 0.2, 0.2, 0.2, 0.2, 0.2 }, input, TOLERANCE);

        input = new double[] { 2.2, 6.6, 8.8, 4.4 };
        ArrayOps.normaliseInPlace(input);
        assertArrayEquals(new double[] { 0.1, 0.3, 0.4, 0.2 }, input, TOLERANCE);

        input = new double[] { 5.5 };
        ArrayOps.normaliseInPlace(input);
        assertArrayEquals(new double[] { 1.0 }, input, TOLERANCE);
    }

    @Test
    public void testNormalise() {
        double[] input = { 11.0, 11.0, 11.0, 11.0, 11.0 };
        double[] result = ArrayOps.normalise(input);
        assertArrayEquals(new double[] { 11.0, 11.0, 11.0, 11.0, 11.0 }, input, TOLERANCE);
        assertArrayEquals(new double[] { 0.2, 0.2, 0.2, 0.2, 0.2 }, result, TOLERANCE);

        input = new double[] { 2.2, 6.6, 8.8, 4.4 };
        result = ArrayOps.normalise(input);
        assertArrayEquals(new double[] { 2.2, 6.6, 8.8, 4.4 }, input, TOLERANCE);
        assertArrayEquals(new double[] { 0.1, 0.3, 0.4, 0.2 }, result, TOLERANCE);

        input = new double[] { 5.5 };
        result = ArrayOps.normalise(input);
        assertArrayEquals(new double[] { 5.5 }, input, TOLERANCE);
        assertArrayEquals(new double[] { 1.0 }, result, TOLERANCE);
    }

    @Test
    public void testReverse() {
        // Test empty array
        double[] input = { };
        double[] result = ArrayOps.reverse(input);
        assertArrayEquals(new double[] {  }, input, TOLERANCE);
        assertArrayEquals(new double[] { }, result, TOLERANCE);

        // Test single element
        input = new double[] { 2.2 };
        result = ArrayOps.reverse(input);
        assertArrayEquals(new double[] { 2.2 }, input, TOLERANCE);
        assertArrayEquals(new double[] { 2.2 }, result, TOLERANCE);

        // Test multiple elements
        input = new double[] { 2.2, 6.6, 8.8, 4.4 };
        result = ArrayOps.reverse(input);
        assertArrayEquals(new double[] { 2.2, 6.6, 8.8, 4.4 }, input, TOLERANCE);
        assertArrayEquals(new double[] { 4.4, 8.8, 6.6, 2.2 }, result, TOLERANCE);
    }

    @Test
    public void testReverseInPlace() {
        // Test empty array
        double[] input = { };
        ArrayOps.reverseInPlace(input);
        assertArrayEquals(new double[] { }, input, TOLERANCE);

        // Test single element
        input = new double[] { 2.2 };
        ArrayOps.reverseInPlace(input);
        assertArrayEquals(new double[] { 2.2 }, input, TOLERANCE);

        // Test multiple elements
        input = new double[] { 2.2, 6.6, 8.8, 4.4 };
        ArrayOps.reverseInPlace(input);
        assertArrayEquals(new double[] { 4.4, 8.8, 6.6, 2.2 }, input, TOLERANCE);
    }

}
