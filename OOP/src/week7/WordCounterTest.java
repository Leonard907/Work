package week7;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;

import org.junit.BeforeClass;
import org.junit.Test;

public class WordCounterTest {

    private static final String INPUT_TEXT =
            "Sixteen years had Miss Taylor been in Mr. Woodhouse's family, less as a "
                    + "governess than a friend, very fond of both daughters, but particularly "
                    + "of Emma. Between _them_ it was more the intimacy of sisters. Even before "
                    + "Miss Taylor had ceased to hold the nominal office of governess, the";

    private static String[] TOKENS;

    private static final int[] EXPECTED_LENGTH_COUNTS = { 0, 3, 9, 7, 12, 1, 8, 4, 1, 4, 0, 0, 1 };
    private static final int EXPECTED_MAXIMUM_VALUE = 12;
    private static final double[] EXPECTED_PERCENTAGES = { 0, 25.0, 75.0, 58.3, 100, 8.3, 66.7, 33.3, 8.3, 33.3, 0, 0, 8.3 };


    @BeforeClass
    public static void setUp() throws Exception {
        // The tokens are set up once for all testcases in this text class
        Tokenizer tokenizer = new Tokenizer();
        tokenizer.tokenize(INPUT_TEXT);
        TOKENS = tokenizer.getTokens();
    }

    @Test
    public void testGetFreqDist() {
        // Test empty counter
        WordCounter wordCounter = new WordCounter(new String[] {});
        assertEquals(0, wordCounter.getFreqDist().size());

        // Test populated counter
        wordCounter = new WordCounter(TOKENS);
        HashMap<Integer, Integer> expectedMap = new HashMap<>();
        for (int i = 0; i < EXPECTED_LENGTH_COUNTS.length; i++) {
            if (EXPECTED_LENGTH_COUNTS[i] > 0) {
                expectedMap.put(i, EXPECTED_LENGTH_COUNTS[i]);
            }
        }
        assertEquals(expectedMap, wordCounter.getFreqDist());

        // Test changing counter with new tokens
        wordCounter.wordLengthFreq(new String[] {"other"});
        System.out.println(wordCounter.getFreqDist());
        assertEquals(1, wordCounter.getFreqDist().size());
        assertEquals((Integer) 1, wordCounter.getFreqDist().get(5));
    }

    @Test
    public void testMaxVal() {
        // Test empty counter
        WordCounter wordCounter = new WordCounter(new String[] {});
        assertEquals(0, wordCounter.getFreqDist().size());

        // Test populated counter
        wordCounter = new WordCounter(TOKENS);
        assertEquals(EXPECTED_MAXIMUM_VALUE, wordCounter.maxVal());

        // Test changing counter with new tokens
        wordCounter.wordLengthFreq(new String[] {"other"});
        assertEquals(1, wordCounter.maxVal());
    }

    @Test
    public void testMap2Array() {
        // Test empty counter
        WordCounter wordCounter = new WordCounter(new String[] {});
        assertEquals(0, wordCounter.map2array().length);

        // Test populated counter
        wordCounter = new WordCounter(TOKENS);
        assertArrayEquals(EXPECTED_PERCENTAGES, wordCounter.map2array(), 0.1);

        // Test changing counter with new tokens
        wordCounter.wordLengthFreq(new String[] {"other"});
        assertArrayEquals(new double[] {0, 0, 0, 0, 0, 100}, wordCounter.map2array(), 0.1);
    }

}
