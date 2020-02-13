package week6;

public class DNAStrand {
    String dna;

    public DNAStrand(String dna) {
        this.dna = dna;
    }

    public boolean isValidDNA() {
        boolean hasA = false;
        boolean hasT = false;
        boolean hasC = false;
        boolean hasG = false;
        for (int i = 0; i < dna.length(); i++) {
            switch(dna.charAt(i)) {
                case 'A':
                    hasA = true;
                    break;
                case 'T':
                    hasT = true;
                    break;
                case 'G':
                    hasG = true;
                    break;
                case 'C':
                    hasC = true;
                    break;
                default:
                    return false;
            }
        }
        return hasA && hasC && hasG && hasT;
    }

    public String complementWC() {
        String complement = "";
        for (int i = 0; i < dna.length(); i++) {
            switch(dna.charAt(i)) {
                case 'A':
                    complement += 'T';
                    break;
                case 'T':
                    complement += 'A';
                    break;
                case 'G':
                    complement += 'C';
                    break;
                case 'C':
                    complement += 'G';
                    break;
                default:
                    break;
            }
        }
        return complement;
    }

    public String palindromeWC() {
        String palindrome = "";
        String origin = complementWC();
        for (int i = 0; i < dna.length(); i++)
            palindrome += origin.charAt(origin.length() - 1 - i);
        return palindrome;
    }

    public boolean containsSequence(String seq) {
        for (int i = 0; i <= dna.length() - seq.length(); i++) {
            if (dna.substring(i, i + seq.length()).equals(seq))
                return true;
        }
        return false;
    }

    public String toString() {
        return dna;
    }

}
