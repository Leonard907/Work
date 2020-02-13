package week6;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class Vector3DTest {

    // Error tolerance in comparing floating point numbers
    private static final double TOLERANCE = 0.0001;

    @Test
    public void testGetR() {
        assertEquals(10.0, new Vector3D(-10.0, 0.0, 0.0).getR(), TOLERANCE);
        assertEquals(10.0, new Vector3D(0.0, -10.0, 0.0).getR(), TOLERANCE);
        assertEquals(10.0, new Vector3D(0.0, 0.0, -10.0).getR(), TOLERANCE);

        assertEquals(Math.sqrt(50), new Vector3D(3.0, 4.0, 5.0).getR(), TOLERANCE);
    }

    @Test
    public void testGetPhi() {
        assertEquals(0.0, new Vector3D(1.0, 0.0, 0.0).getPhi(), TOLERANCE);
        assertEquals(0.0, new Vector3D(10.0, 0.0, 0.0).getPhi(), TOLERANCE);

        assertEquals(Math.PI, new Vector3D(-1.0, 0.0, 0.0).getPhi(), TOLERANCE);
        assertEquals(Math.PI, new Vector3D(-10.0, 0.0, 0.0).getPhi(), TOLERANCE);

        assertEquals(Math.PI / 2., new Vector3D(0.0, 1.0, 0.0).getPhi(), TOLERANCE);
        assertEquals(Math.PI / 2., new Vector3D(0.0, 10.0, 0.0).getPhi(), TOLERANCE);

        assertEquals(-1.0 * Math.PI / 2., new Vector3D(0.0, -1.0, 0.0).getPhi(), TOLERANCE);
        assertEquals(-1.0 * Math.PI / 2., new Vector3D(0.0, -10.0, 0.0).getPhi(), TOLERANCE);
    }

    @Test
    public void testGetTheta() {
        assertEquals(Math.PI / 2., new Vector3D(1.0, 0.0, 0.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 2., new Vector3D(10.0, 0.0, 0.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 2., new Vector3D(-1.0, 0.0, 0.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 2., new Vector3D(-10.0, 0.0, 0.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 2., new Vector3D(0.0, 1.0, 0.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 2., new Vector3D(0.0, 10.0, 0.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 2., new Vector3D(0.0, -1.0, 0.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 2., new Vector3D(0.0, -10.0, 0.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4., new Vector3D(1.0, 0.0, 1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4., new Vector3D(10.0, 0.0, 10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4., new Vector3D(-1.0, 0.0, 1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4., new Vector3D(-10.0, 0.0, 10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4., new Vector3D(0.0, 1.0, 1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4., new Vector3D(0.0, 10.0, 10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4., new Vector3D(0.0, -1.0, 1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4., new Vector3D(0.0, -10.0, 10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4. * 3., new Vector3D(1.0, 0.0, -1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4. * 3., new Vector3D(10.0, 0.0, -10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4. * 3., new Vector3D(-1.0, 0.0, -1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4. * 3., new Vector3D(-10.0, 0.0, -10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4. * 3., new Vector3D(0.0, 1.0, -1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4. * 3., new Vector3D(0.0, 10.0, -10.0).getTheta(), TOLERANCE);

        assertEquals(Math.PI / 4. * 3., new Vector3D(0.0, -1.0, -1.0).getTheta(), TOLERANCE);
        assertEquals(Math.PI / 4. * 3., new Vector3D(0.0, -10.0, -10.0).getTheta(), TOLERANCE);

    }

    private void checkVectorsEqual(Vector3D lhs, Vector3D rhs) {
        assertEquals(lhs.getR(), rhs.getR(), TOLERANCE);
        assertEquals(lhs.getPhi(), rhs.getPhi(), TOLERANCE);
        assertEquals(lhs.getTheta(), rhs.getTheta(), TOLERANCE);
    }

    @Test
    public void testAdd() {
        checkVectorsEqual(new Vector3D(2, 2, 2), Vector3D.add(new Vector3D(1, 1, 1), new Vector3D(1, 1, 1)));
        checkVectorsEqual(new Vector3D(5, 7, 9), Vector3D.add(new Vector3D(1, 2, 3), new Vector3D(4, 5, 6)));
    }

    @Test
    public void testSubtract() {
        checkVectorsEqual(new Vector3D(1, 1, 1), Vector3D.subtract(new Vector3D(2, 2, 2), new Vector3D(1, 1, 1)));
        checkVectorsEqual(new Vector3D(-3, -3, -3), Vector3D.subtract(new Vector3D(1, 2, 3), new Vector3D(4, 5, 6)));

        checkVectorsEqual(new Vector3D(33, 28, 25), Vector3D.subtract(new Vector3D(34, 30, 28), new Vector3D(1, 2, 3)));
    }

    @Test
    public void testScale() {
        checkVectorsEqual(new Vector3D(34, 30, 28), Vector3D.scale(new Vector3D(34, 30, 28), 1.0));
        checkVectorsEqual(new Vector3D(-34, -30, -28), Vector3D.scale(new Vector3D(34, 30, 28), -1.0));
        checkVectorsEqual(new Vector3D(68, 60, 56), Vector3D.scale(new Vector3D(34, 30, 28), 2.0));
        checkVectorsEqual(new Vector3D(17, 15, 14), Vector3D.scale(new Vector3D(34, 30, 28), 0.5));
    }
}
