package week4;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class FactorialTest {

    @Test
    public void testFactorial() {
        assertEquals(1, Factorial.factorial(1));
        assertEquals(2, Factorial.factorial(2));
        assertEquals(6, Factorial.factorial(3));
        assertEquals(24, Factorial.factorial(4));
        assertEquals(120, Factorial.factorial(5));
        assertEquals(720, Factorial.factorial(6));
        assertEquals(5040, Factorial.factorial(7));
    }

}
