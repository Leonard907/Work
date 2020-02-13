package week5;

import java.awt.Color;

public class Voronoi {

    public static double pointDist(Point2D point1, Point2D point2) {
        // ADD CODE HERE
        return Math.sqrt(Math.pow((point1.getX() - point2.getX()), 2) + Math.pow((point1.getY() - point2.getY()), 2));
    }

    public static int findClosestPoint(Point2D point, Point2D[] sites) {
        // ADD CODE HERE
        double minDistance = pointDist(point, sites[0]);
        int index = 0;
        for (int i = 1; i < sites.length; i++) {
            if (pointDist(point, sites[i]) < minDistance) {
                minDistance = pointDist(point, sites[i]);
                index = i;
            }
        }
        return index;
    }

    public static int[][] buildVoronoiMap(Point2D[] sites, int width, int height) {
        // ADD CODE HERE
        int[][] indexMap = new int[width][height];
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                indexMap[i][j] = findClosestPoint(new Point2D(i, j), sites);
            }
        }
        return indexMap;
    }

    public static void plotVoronoiMap(Point2D[] sites, Color[] colors, int[][] indexMap) {
        // ADD CODE HERE
        int width = indexMap.length;
        int height = indexMap[0].length;

        StdDraw.setCanvasSize(width, height);
        StdDraw.setXscale(0, width);
        StdDraw.setYscale(0, height);

        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                StdDraw.setPenColor(colors[indexMap[i][j]]);
                StdDraw.point(i, j);
            }
        }

        StdDraw.setPenColor(Color.BLACK);
        for (Point2D point: sites) {
            StdDraw.filledCircle(point.getX(), point.getY(), 3);
        }


    }

    public static void main(String[] args) {
        int width = 200;
        int height = 200;

        int nSites = 5;
        Point2D[] sites = new Point2D[nSites];
        sites[0] = new Point2D(50, 50);
        sites[1] = new Point2D(100, 50);
        sites[2] = new Point2D(50, 100);
        sites[3] = new Point2D(125, 50);
        sites[4] = new Point2D(100, 175);

        Color[] colors = new Color[nSites];
        colors[0] = Color.BLUE;
        colors[1] = Color.GREEN;
        colors[2] = Color.YELLOW;
        colors[3] = Color.ORANGE;
        colors[4] = Color.MAGENTA;

        int[][] indexmap = buildVoronoiMap(sites, width, height);
        plotVoronoiMap(sites, colors, indexmap);

    }

}
