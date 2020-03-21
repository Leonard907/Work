package proj1b;

import org.junit.Test;
import static org.junit.Assert.*;

public class TestOffByOne {
    // You must use this CharacterComparator and not instantiate
    // new ones, or the autograder might be upset.
    static CharacterComparator offByOne = new OffByOne();

    // Your tests go here.
    @Test
    public void testEqualChars() {
        // True
        char true1 = '"'; char true2 = '#';
        char true3 = 'Z'; char true4 = '[';
        assertTrue(offByOne.equalChars(true1, true2));
        assertTrue(offByOne.equalChars(true3, true4));
        // False
        char false1 = '?'; char false2 = '!';
        char false3 = 'h';
        assertFalse(offByOne.equalChars(false1, false2));
        assertFalse(offByOne.equalChars(false3, false3));
    }
}