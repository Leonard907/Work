package week5;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class NoTriplesTest {

    @Test
    public void testNoTriples() {
        // Too short
        assertTrue(NoTriples.noTriples(new int[] { 5 }));
        assertTrue(NoTriples.noTriples(new int[] { 5, 5 }));

        // Contains triples
        assertFalse(NoTriples.noTriples(new int[] { 5, 5, 5 }));
        assertFalse(NoTriples.noTriples(new int[] { 1, 1, 2, 2, 2, 1 }));
        assertFalse(NoTriples.noTriples(new int[] { 1, 1, 1, 2, 2, 2, 1 }));

        // Does not contain triples
        assertTrue(NoTriples.noTriples(new int[] { 1, 1, 2, 2, 1 }));
    }

}