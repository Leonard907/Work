package week4;

public class OneTimePadEncipher {

    public static int charToInt(char l) {
        // ADD CODE HERE
        // Should convert a character to an integer, for example 'a' -> 0, 'b' -> 1
        if (l >= 97)
            return l - 97;
        else
            return l - 65;
    }

    public static char intToChar(int i) {
        // ADD CODE HERE
        // Should convert an integer to a character, for example 0 -> 'a', b -> '1'
        // it should always return lower case char
        return (char) (i + 97);
    }

    public static boolean isAlpha(char c) {
        // You do not need to implement this method, but you may find it useful.
        if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) {
            return true;
        }
        return false;
    }

    public static String encipher(String original, String onetimepad) {
        // ADD CODE HERE
        int[] encipher = new int[original.length()];
        String code = "";
        for (int i = 0; i < original.length(); i++) {
            if (isAlpha(original.charAt(i))) {
                encipher[i] = (charToInt(original.charAt(i)) + charToInt(onetimepad.charAt(i))) % 26;
            }
            else {
                encipher[i] = -1; // A space detected
            }
        }
        for (int chr: encipher) {
            if (chr == -1)
                code += " ";
            else
                code += Character.toString((char) (chr + 97));
        }
        return code;
    }


    public static void main(String[] args) {
        String ciphertext = encipher("HELLO","XMCKL");
        System.out.print(ciphertext);
    }

}
