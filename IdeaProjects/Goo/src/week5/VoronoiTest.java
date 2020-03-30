package week5;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class VoronoiTest {

    // Error tolerance in comparing floating point numbers
    private static final double TOLERANCE = 0.0001;

    @Test
    public void testPointDist() {
        // Points identical
        assertEquals(0.0, Voronoi.pointDist(new Point2D(0, 0), new Point2D(0, 0)), TOLERANCE);
        assertEquals(0.0, Voronoi.pointDist(new Point2D(1, 0), new Point2D(1, 0)), TOLERANCE);
        assertEquals(0.0, Voronoi.pointDist(new Point2D(0, -10), new Point2D(0, -10)), TOLERANCE);

        // Points separate on one axis
        assertEquals(2.0, Voronoi.pointDist(new Point2D(0, 0), new Point2D(0, 2)), TOLERANCE);
        assertEquals(3.0, Voronoi.pointDist(new Point2D(0, 0), new Point2D(-3, 0)), TOLERANCE);

        // General case
        assertEquals(5.0, Voronoi.pointDist(new Point2D(1, 2), new Point2D(-2, 6)), TOLERANCE);
        assertEquals(Math.sqrt(2), Voronoi.pointDist(new Point2D(0, 0), new Point2D(1, 1)), TOLERANCE);
    }


    private Point2D[] getTestPoints() {
        Point2D[] points = new Point2D[4];
        points[0] = new Point2D(5, 5);
        points[1] = new Point2D(0, 5);
        points[2] = new Point2D(5, 0);
        points[3] = new Point2D(0, 0);
        return points;
    }

    @Test
    public void testFindClosestPoint() {
        Point2D[] points = getTestPoints();

        assertEquals(3, Voronoi.findClosestPoint(new Point2D(0, 0), points));
        assertEquals(3, Voronoi.findClosestPoint(new Point2D(-100, 0), points));
        assertEquals(1, Voronoi.findClosestPoint(new Point2D(-100, 3), points));

        // Boundaries between Voronoi cells
        assertEquals(3, Voronoi.findClosestPoint(new Point2D(2, 2), points));
        assertEquals(1, Voronoi.findClosestPoint(new Point2D(2, 3), points));
        assertEquals(2, Voronoi.findClosestPoint(new Point2D(3, 2), points));
        assertEquals(0, Voronoi.findClosestPoint(new Point2D(3, 3), points));
    }

    @Test
    public void testBuildVeronoiMap() {
        // Test single Voronoi cell
        Point2D[] points = { new Point2D(1, 1) };
        int[][] indexmap = Voronoi.buildVoronoiMap(points, 4, 3);

        // Note - because map is indexed as expectedMap[x][y], it looks sideways here
        int[][] expectedMap = {
                {0, 0, 0},
                {0, 0, 0},
                {0, 0, 0},
                {0, 0, 0},
        };
        compareArrays(expectedMap, indexmap);

        // Test multiple Voronoi cells
        points = new Point2D[] { new Point2D(2, 3), new Point2D(0, 0) };
        indexmap = Voronoi.buildVoronoiMap(points, 3, 4);

        // Note - because map is indexed as expectedMap[x][y], it looks sideways here
        expectedMap = new int[][] {
                {1, 1, 1, 0},
                {1, 1, 0, 0},
                {1, 0, 0, 0},
        };
        compareArrays(expectedMap, indexmap);
    }

    /**
     * A helper function to compare two 2-dimensional arrays of ints.
     *
     * @param expected
     * @param actual
     */
    private void compareArrays(int[][] expected, int[][] actual) {
        assertEquals("Row counts different", expected.length, actual.length);
        for (int i = 0; i < expected.length; i++) {
            assertArrayEquals("Array row " + i, expected[i], actual[i]);
        }
    }
}
