package week4;

public class CoordinateConverter {

    public static double convertXYtoR(double x, double y) {
        return Math.sqrt(x*x + y*y);
    }

    public static double convertXYtoT(double x, double y) {
        if (x < 0)
            return Math.PI - Math.atan(y / -x);
        else
            return Math.atan(y / x);
    }

    public static double convertRTtoX(double r, double theta) {
        return r * Math.cos(theta);
    }

    public static double convertRTtoY(double r, double theta) {
        return r * Math.sin(theta);
    }

    public static double convertDegToRad(double deg) {
        return deg * Math.PI / 180;
    }

    public static double convertRadToDeg(double rad) {
        return rad * 180 / Math.PI;
    }

}
