package week6;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class CreditCard {
    private int expiryMonth;
    private int expiryYear;
    private String firstName;
    private String lastName;
    private String ccNumber;

    public CreditCard(int expiryMonth, int expiryYear, String firstName, String lastName, String ccNumber) {
        this.expiryMonth = expiryMonth;
        this.expiryYear = expiryYear;
        this.firstName = firstName;
        this.lastName = lastName;
        this.ccNumber = ccNumber;
    }

    public String formatExpiryDate() {
        return Integer.toString(expiryMonth) + "/" + Integer.toString(expiryYear - 2000);
    }

    public String formatFullName() {
        return firstName + " " + lastName;
    }

    public String formatCCNumber() {
        String format = "";
        for (int i = 1; i <= ccNumber.length(); i++) {
            if (i == ccNumber.length())
                format += ccNumber.charAt(i - 1);
            else if (i % 4 == 0) {
                format += ccNumber.charAt(i - 1);
                format += " ";
            }
            else
                format += ccNumber.charAt(i - 1);
        }
        return format;
    }

    public boolean isValid() {
        Calendar today = Calendar.getInstance();
        int month = today.get(Calendar.MONTH);
        int year = today.get(Calendar.YEAR);
        if (year < expiryYear)
            return true;
        else if (year == expiryYear) {
            if (month + 1 < expiryMonth)
                return true;
            else
                return false;
        }
        else {
            return false;
        }
    }

    public String toString() {
        return "Number: " + formatCCNumber() + "\n"
                + "Expiration date: " + formatExpiryDate() + "\n"
                + "Account holder: " + formatFullName() + "\n"
                + "Is valid: " + isValid();
    }
}

