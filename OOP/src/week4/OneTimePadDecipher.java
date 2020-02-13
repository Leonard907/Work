package week4;

public class OneTimePadDecipher {
    public static String decipher(String encipheredText, String onetimepad) {
        int[] decipher = new int[encipheredText.length()];
        String code = "";
        for (int i = 0; i < encipheredText.length(); i++) {
            if (OneTimePadEncipher.isAlpha(encipheredText.charAt(i))) {
                int dif = (OneTimePadEncipher.charToInt(encipheredText.charAt(i)) - OneTimePadEncipher.charToInt(onetimepad.charAt(i))) + 26;
                decipher[i] = dif % 26;
            }
            else {
                decipher[i] = -1;
            }
        }
        for (int chr: decipher) {
            if (chr == -1)
                code += " ";
            else
                code += Character.toString((char) (chr + 65));
        }
        return code;
    }
}
