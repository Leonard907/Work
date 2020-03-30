package week5;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class Has271Test {

    @Test
    public void testHas271(){
        assertTrue(Has271.has271(new int[] {1, 2, 7, 1}));
        assertTrue(Has271.has271(new int[] {2, 7, 1}));
        assertTrue(Has271.has271(new int[] {4, 9, 3}));
        assertTrue(Has271.has271(new int[] {1, 3, 8, 2, 5}));

        assertFalse(Has271.has271(new int[] {1, 2, 8, 1}));
    }
}
