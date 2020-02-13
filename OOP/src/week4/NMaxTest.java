package week4;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class NMaxTest {

    @Test
    public void testMax_double() {
        assertEquals(2.5, NMax.max(2.5, 0.0, 0.0), 0.001);
        assertEquals(2.5, NMax.max(0.0, 2.5, 0.0), 0.001);
        assertEquals(2.5, NMax.max(0.0, 0.0, 2.5), 0.001);

        assertEquals(-2.0, NMax.max(-2.0, -4.0, -5.0), 0.001);
        assertEquals(-2.0, NMax.max(-5.0, -2.0, -4.0), 0.001);
        assertEquals(-2.0, NMax.max(-4.0, -4.0, -2.0), 0.001);

        assertEquals(20.0, NMax.max(20.0, -20.0, 0.0), 0.001);
        assertEquals(20.0, NMax.max(-20.0, 20.0, 0.0), 0.001);
        assertEquals(20.0, NMax.max(-20.0, 20.0, -20.0), 0.001);
        assertEquals(20.0, NMax.max(-20.0, -20.0, 20.0), 0.001);
    }

    @Test
    public void testMax_int() {
        assertEquals(2, NMax.max(2, 0, 0));
        assertEquals(2, NMax.max(0, 2, 0));
        assertEquals(2, NMax.max(0, 0, 2));

        assertEquals(-2, NMax.max(-2, -4, -5));
        assertEquals(-2, NMax.max(-5, -2, -4));
        assertEquals(-2, NMax.max(-3, -2, -2));

        assertEquals(20, NMax.max(20, -20, 0));
        assertEquals(20, NMax.max(-20, 20, 0));
        assertEquals(20, NMax.max(-20, 20, -20));
        assertEquals(20, NMax.max(-20, -20, 20));
    }

}
