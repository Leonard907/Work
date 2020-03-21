package proj1b;

import org.junit.Test;
import static org.junit.Assert.*;
import proj1a.Deque;

public class TestPalindrome {
    // You must use this palindrome, and not instantiate
    // new Palindromes, or the autograder might be upset.
    static Palindrome palindrome = new Palindrome();

    @Test
    public void testWordToDeque() {
        Deque d = palindrome.wordToDeque("persiflage");
        String actual = "";
        for (int i = 0; i < "persiflage".length(); i++) {
            actual += d.removeFirst();
        }
        assertEquals("persiflage", actual);
    }

    @Test
    public void testIsPalindrome() {
        String target1 = "racecar";
        String gibberish1 = "Wdnmd";
        assertTrue(palindrome.isPalindrome(target1));
        assertFalse(palindrome.isPalindrome(gibberish1));
        // Using a comparator
        String target2 = "adefcb";
        String gibberish2 = "DoIt on ytt";
        assertTrue(palindrome.isPalindrome(target2, new OffByOne()));
        assertFalse(palindrome.isPalindrome(gibberish2, new OffByOne()));
    }

}