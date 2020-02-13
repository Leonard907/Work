package week7;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

import org.junit.Test;

public class TokenizerTest {

    private static final String INPUT_TEXT1 = "this is some text\n and--here\tis more; And 'more' text";
    private static final String INPUT_TEXT2 = "Together we can change the world";
    private static final String[] TOKENS1 = new String[] { "this", "is", "some", "text", "and", "here", "is", "more", "And", "more", "text" };
    private static final String[] TOKENS2 = new String[] { "Together", "we", "can", "change", "the", "world" };

    @Test
    public void testGetTokens() {
        Tokenizer tokenizer = new Tokenizer();

        // Initial setting before tokenising
        assertArrayEquals(new String[] {}, tokenizer.getTokens());

        // Tokenise
        tokenizer.tokenize(INPUT_TEXT2);
        assertArrayEquals(TOKENS2, tokenizer.getTokens());
        tokenizer.tokenize(INPUT_TEXT1);
        assertArrayEquals(TOKENS1, tokenizer.getTokens());
    }

    @Test
    public void testConstructor_noArguments() {
        Tokenizer tokenizer = new Tokenizer();
        assertArrayEquals(new String[] {}, tokenizer.getTokens());
    }

    @Test
    public void testTokensFromFile() throws IOException {
        // Create temporary input data file
        File temporaryInputFile = File.createTempFile("tempInput", ".txt");
        try {
            //
            Writer fileWriter = new FileWriter(temporaryInputFile);
            fileWriter.write(INPUT_TEXT1);
            fileWriter.close();

            // Read tokens from file
            Tokenizer tokenizer = new Tokenizer();
            tokenizer.tokensFromFile(temporaryInputFile.getPath());
            String[] tokens = tokenizer.getTokens();
            assertArrayEquals(TOKENS1, tokens);

            // Check token list can be overwritten
            tokenizer.tokenize(INPUT_TEXT2);
            assertArrayEquals(TOKENS2, tokenizer.getTokens());
        }
        finally {
            // Clean up temporary file
            temporaryInputFile.delete();
        }
    }

    @Test
    public void testConstructor_fileArgument() throws IOException {
        // Create temporary input data file
        File temporaryInputFile = File.createTempFile("tempInput", ".txt");
        try {
            //
            Writer fileWriter = new FileWriter(temporaryInputFile);
            fileWriter.write(INPUT_TEXT1);
            fileWriter.close();

            // Read tokens from file
            Tokenizer tokenizer = new Tokenizer(temporaryInputFile.getPath());
            String[] tokens = tokenizer.getTokens();
            assertArrayEquals(TOKENS1, tokens);

            // Check token list can be overwritten
            tokenizer.tokenize(INPUT_TEXT2);
            assertArrayEquals(TOKENS2, tokenizer.getTokens());
        }
        finally {
            // Clean up temporary file
            temporaryInputFile.delete();
        }
    }

    @Test
    public void testGetNumberTokens() {
        Tokenizer tokenizer = new Tokenizer();

        // Initial setting before tokenising
        assertEquals(0, tokenizer.getNumberTokens());

        // Tokenise
        tokenizer.tokenize(INPUT_TEXT2);
        assertEquals(TOKENS2.length, tokenizer.getNumberTokens());
        tokenizer.tokenize(INPUT_TEXT1);
        assertEquals(TOKENS1.length, tokenizer.getNumberTokens());
    }

}