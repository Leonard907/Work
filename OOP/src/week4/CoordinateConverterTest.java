package week4;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class CoordinateConverterTest {

    // Error tolerance in comparing floating point numbers
    private static final double TOLERANCE = 0.0001;

    @Test
    public void testConvertXYtoR() {
        assertEquals(CoordinateConverter.convertXYtoR(0.0, 0.0), 0, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoR(10.0, 0.0), 10.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoR(0.0, 10.0), 10.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoR(10.0, 10.0), 14.1421, TOLERANCE);
    }

    @Test
    public void testConvertXYtoT() {
        assertEquals(CoordinateConverter.convertXYtoT(1.0, 0.0), 0, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(0.0, 1.0), Math.PI / 2, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(-1.0, 0.0), Math.PI, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(0.0, -1.0), -Math.PI / 2, TOLERANCE);

        assertEquals(CoordinateConverter.convertXYtoT(10.0, 0.0), 0, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(0.0, 10.0), Math.PI / 2, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(-10.0, 0.0), Math.PI, TOLERANCE);
        assertEquals(CoordinateConverter.convertXYtoT(0.0, -10.0), -Math.PI / 2, TOLERANCE);
    }

    @Test
    public void testConvertRTtoX() {
        assertEquals(CoordinateConverter.convertRTtoX(10.0, 0.0), 10.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(10.0, Math.PI / 4), 10 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(10.0, Math.PI / 2), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(10.0, 3 * Math.PI / 4), -10 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(10.0, Math.PI), -10.0, TOLERANCE);

        assertEquals(CoordinateConverter.convertRTtoX(1.0, 0.0), 1.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(1.0, Math.PI / 4), 1 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(1.0, Math.PI / 2), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(1.0, 3.0 * Math.PI / 4), -1 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoX(1.0, Math.PI), -1.0, TOLERANCE);
    }

    @Test
    public void testConvertRTtoY() {
        assertEquals(CoordinateConverter.convertRTtoY(1.0, 0.0), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(1.0, Math.PI / 4), 1 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(1.0, Math.PI / 2), 1.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(1.0, 3.0 * Math.PI / 4), 1 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(1.0, -1.0 * Math.PI / 4), -1 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(1.0, Math.PI), 0.0, TOLERANCE);

        assertEquals(CoordinateConverter.convertRTtoY(10.0, 0.0), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(10.0, Math.PI / 4), 10 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(10.0, Math.PI / 2), 10.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(10.0, 3 * Math.PI / 4), 10 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(10.0, -1 * Math.PI / 4), -10 * 0.707106, TOLERANCE);
        assertEquals(CoordinateConverter.convertRTtoY(10.0, Math.PI), 0.0, TOLERANCE);
    }

    @Test
    public void testConvertDegToRad() {
        assertEquals(CoordinateConverter.convertDegToRad(0.0), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertDegToRad(90.0), Math.PI / 2, TOLERANCE);
        assertEquals(CoordinateConverter.convertDegToRad(-180.0), -Math.PI, TOLERANCE);
        assertEquals(CoordinateConverter.convertDegToRad(270.0), Math.PI * 3 / 2, TOLERANCE);
        assertEquals(CoordinateConverter.convertDegToRad(360.0), Math.PI * 2, TOLERANCE);
    }

    @Test
    public void testConvertRadToDeg() {
        assertEquals(CoordinateConverter.convertRadToDeg(0.0), 0.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRadToDeg(Math.PI / 2), 90.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRadToDeg(-Math.PI), -180.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRadToDeg(Math.PI * 3 / 2), 270.0, TOLERANCE);
        assertEquals(CoordinateConverter.convertRadToDeg(Math.PI * 2), 360.0, TOLERANCE);
    }

}
