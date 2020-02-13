package week6;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class DNAStrandTest {

    private DNAStrand agtc = new DNAStrand("AGTC");
    private DNAStrand agtcagtc = new DNAStrand("AGTCAGTC");

    @Test
    public void testIsValidDNA() {
        assertTrue(agtc.isValidDNA());
        assertTrue(agtcagtc.isValidDNA());

        DNAStrand invalid1 = new DNAStrand("AGTCAGTB");
        DNAStrand invalid2 = new DNAStrand("AgTCAGT");
        DNAStrand invalid3 = new DNAStrand("");

        assertFalse(invalid1.isValidDNA());
        assertFalse(invalid2.isValidDNA());
        assertFalse(invalid3.isValidDNA());
    }

    @Test
    public void testComplementWC() {
        assertEquals("TCAG", agtc.complementWC());
        assertEquals("TCAGTCAG", agtcagtc.complementWC());
    }

    @Test
    public void testPalindromeWC() {
        assertEquals("GACT", agtc.palindromeWC());
        assertEquals("GACTGACT", agtcagtc.palindromeWC());
    }

    @Test
    public void testToString() {
        assertEquals("AGTC", agtc.toString());
        assertEquals("AGTCAGTC", agtcagtc.toString());
    }

    @Test
    public void testContainsSequence() {
        assertTrue(agtc.containsSequence("AG"));
        assertTrue(agtc.containsSequence("TC"));
        assertTrue(agtcagtc.containsSequence("CAG"));

        assertFalse(agtc.containsSequence("AT"));
        assertFalse(agtc.containsSequence("AC"));
        assertFalse(agtcagtc.containsSequence("TA"));
    }

}
