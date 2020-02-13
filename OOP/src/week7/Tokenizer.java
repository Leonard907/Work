package week7;

public class Tokenizer {

    String[] tokens = {};

    public Tokenizer() {

    }

    public Tokenizer(String fname) {
        tokensFromFile(fname);
    }

    public void tokensFromFile(String fname) {
        In file = new In(fname);
        tokenize(file.readAll());
    }

    public void tokenize(String str) {
        String regex = "\\W+";
        tokens = str.split(regex);
    }

    public String[] getTokens() {
        return tokens;
    }

    public int getNumberTokens() {
        return tokens.length;
    }
}
