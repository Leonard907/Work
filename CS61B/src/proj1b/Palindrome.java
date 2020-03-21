package proj1b;

import proj1a.ArrayDeque;
import proj1a.Deque;
import proj1a.LinkedListDeque;

public class Palindrome {
    public Deque<Character> wordToDeque(String word) {
        LinkedListDeque<Character> letters = new LinkedListDeque<>();
        for (char letter: word.toCharArray()) {
            letters.addLast(letter);
        }
        return letters;
    }

    public boolean isPalindrome(String word) {
        for (int i = 0; i < word.length() / 2;i++) {
            if (word.charAt(i) != word.charAt(word.length() - 1 - i)) return false;
        }
        return true;
    }

    public boolean isPalindrome(String word, CharacterComparator cc) {
        for (int i = 0; i < word.length() / 2; i++) {
            if (!cc.equalChars(word.charAt(i), word.charAt(word.length() - 1 - i))) return false;
        }
        return true;
    }

}
