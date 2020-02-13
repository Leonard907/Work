package week6;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Calendar;

import org.junit.Before;
import org.junit.Test;

public class CreditCardTest {

    private CreditCard creditCardExpired;
    private CreditCard creditCardThisMonth;
    private CreditCard creditCardNextMonth;
    private CreditCard creditCardNextYear;

    String dateThisMonth;
    String dateNextMonth;
    String dateNextYear;

    /**
     * Helper method to create expiry date strings
     * @param month
     * @param year
     * @return expiry date string
     */
    private String toStringExpiryDate(int month, int year) {
        String shortExpiryYear = Integer.toString(year).substring(2, 4);
        return String.format("%s/%s", month, shortExpiryYear);
    }

    @Before
    public void setUp() {
        // Setup test data.
        // This code is more complex than other test cases as the expiry date
        // changes over time.

        Calendar now = Calendar.getInstance();
        int nowYear = now.get(Calendar.YEAR);
        int nowMonth = now.get(Calendar.MONTH) + 1;
        int nextMonth = nowMonth + 1;
        int nextYear = nowYear + 1;

        creditCardExpired = new CreditCard(10, 2010, "Bob", "Jones", "1234567890123456"); // expired!
        creditCardThisMonth = new CreditCard(nowMonth, nowYear, "John", "Smith", "2345123412341234"); // expired!
        creditCardNextMonth = new CreditCard(nextMonth, nowYear, "Fred", "Kerr", "2345123412341234"); // valid!
        creditCardNextYear = new CreditCard(1, nextYear, "John", "Smith", "2345123412341234"); // valid!

        dateThisMonth = toStringExpiryDate(nowMonth, nowYear);
        dateNextMonth = toStringExpiryDate(nextMonth, nowYear);
        dateNextYear = toStringExpiryDate(1, nextYear);
    }

    @Test
    public void testConstructor() {
        assertNotNull(creditCardExpired);
        assertNotNull(creditCardNextYear);
    }

    @Test
    public void testFormatExpiryDate() {
        assertEquals("10/10", creditCardExpired.formatExpiryDate());
        assertEquals(dateThisMonth, creditCardThisMonth.formatExpiryDate());
        assertEquals(dateNextMonth, creditCardNextMonth.formatExpiryDate());
        assertEquals(dateNextYear, creditCardNextYear.formatExpiryDate());
    }

    @Test
    public void testFormatFullName() {
        assertEquals("Bob Jones", creditCardExpired.formatFullName());
        assertEquals("John Smith", creditCardThisMonth.formatFullName());
        assertEquals("Fred Kerr", creditCardNextMonth.formatFullName());
    }

    @Test
    public void testFormatCCNumber() {
        assertEquals("1234 5678 9012 3456", creditCardExpired.formatCCNumber());
        assertEquals("2345 1234 1234 1234", creditCardNextMonth.formatCCNumber());
    }

    @Test
    public void testIsValid() {
        assertFalse(creditCardExpired.isValid());
        assertFalse(creditCardThisMonth.isValid());
        assertTrue(creditCardNextMonth.isValid());
        assertTrue(creditCardNextYear.isValid());
    }

    /**
     * Convert Windows format linefeeds to Unix/Linux/OSX format for string comparison
     * @param input input string containing newlines in '\n', '\r', '\r\n' format
     * @return string containing only '\n' newlines
     */
    private String convertToUnix(String input) {
        if (input == null) {
            return null;
        }
        return input.replaceAll("\r\n", "\n").replaceAll("\r", "\n");
    }


    @Test
    public void testToString() {
        String expected =
                "Number: 1234 5678 9012 3456\n" +
                        "Expiration date: 10/10\n" +
                        "Account holder: Bob Jones\n" +
                        "Is valid: false";
        assertEquals(expected, convertToUnix(creditCardExpired.toString()));

        Calendar now = Calendar.getInstance();
        int nowYear = now.get(Calendar.YEAR);
        int nextYear = nowYear + 1;

        expected =
                "Number: 2345 1234 1234 1234\n" +
                        String.format("Expiration date: 1/%d\n",nextYear-2000) +
                        "Account holder: John Smith\n" +
                        "Is valid: true";
        //Was setup for next year.
        assertEquals(expected, convertToUnix(creditCardNextYear.toString()));
    }

}
