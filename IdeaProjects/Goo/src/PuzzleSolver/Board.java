package PuzzleSolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import edu.princeton.cs.algs4.StdRandom;

public class Board {

    final int[][] tiles;
    private int row;
    private int col;
    private int count = 0;
    private int total = 0;

    // create a board from an n-by-n array of tiles,
    // where tiles[row][col] = tile at (row, col)
    public Board(int[][] tiles) {
        this.tiles = tiles;
        row = StdRandom.uniform(dimension());
        col = StdRandom.uniform(dimension());
        while (tiles[row][col] == 0) {
            row = StdRandom.uniform(dimension());
            col = StdRandom.uniform(dimension());
        }
        for (int i = 1; i < Math.pow(dimension(), 2); i++) {
            if (tiles[(i-1) / dimension()][(i-1) % dimension()] != i) count++;
        }
        for (int i = 0; i < dimension(); i++) {
            for (int j = 0; j < dimension(); j++) {
                int current = tiles[i][j];
                if (current != 0) {
                    int h = Math.abs((current-1) % dimension() - j);
                    int v = Math.abs((current-1) / dimension() - i);
                    total += (h + v);
                }
            }
        }
    }

    // string representation of this board
    public String toString() {
        StringBuilder board = new StringBuilder();
        // Add size
        board.append(dimension());
        for (int i = 0; i < dimension(); i++) {
            board.append("\n").append(" ");
            for (int j = 0; j < dimension(); j++) {
                board.append(tiles[i][j]).append("  ");
            }
        }
        return board.toString();
    }

    // board dimension n
    public int dimension() {
        return tiles.length;
    }

    // number of tiles out of place
    public int hamming() {
        return count;
    }

    // sum of Manhattan distances between tiles and goal
    public int manhattan() {
        return total;
    }

    // is this board the goal board?
    public boolean isGoal() {
        return hamming() == 0;
    }

    // does this board equal y?
    public boolean equals(Object y) {
        if (y == this) return true;
        if (y == null) return false;
        if (y.getClass() != this.getClass()) return false;
        Board o = (Board) y;
        // Dimension
        if (o.dimension() != dimension()) return false;
        // Contents
        for (int i = 0; i < dimension(); i++) {
            if (!Arrays.equals(tiles[i], o.tiles[i])) return false;
        }
        return true;
    }

    // all neighboring boards
    public Iterable<Board> neighbors() {
        List<Board> neighbours = new ArrayList<>();
        int i = 0; int j = 0;
        for (int k = 0; k < dimension(); k++) {
            for (int l = 0; l < dimension(); l++) {
                if (tiles[k][l] == 0) {i = k; j = l; break;}
            }
        }
        int[] dr = {1, 0, -1, 0};
        int[] dc = {0, 1, 0, -1};
        for (int k = 0; k < 4; k++) {
            if (i + dr[k] >= 0 && i + dr[k] < dimension() && j + dc[k] >= 0 && j + dc[k] < dimension()) {
                int[][] b = initialize();
                b[i][j] = b[i + dr[k]][j + dc[k]];
                b[i + dr[k]][j + dc[k]] = 0;
                neighbours.add(new Board(b));
            }
        }
        return neighbours;
    }

    // a board that is obtained by exchanging any pair of tiles
    public Board twin() {
        int[] dr = {1, 0, -1, 0};
        int[] dc = {0, 1, 0, -1};
        int[][] neighbour = initialize();
        for (int k = 0; k < 4; k++) {
            if (row + dr[k] >= 0 && row + dr[k] < dimension() && col + dc[k] >= 0 && col + dc[k] < dimension()
                && tiles[row + dr[k]][col + dc[k]] != 0) {
                int temp = neighbour[row][col];
                neighbour[row][col] = neighbour[row+dr[k]][col+dc[k]];
                neighbour[row+dr[k]][col+dc[k]] = temp;
                break;
            }
        }
        return new Board(neighbour);
    }

    private int[][] initialize() {
        int[][] board = new int[dimension()][dimension()];
        for (int i = 0; i < dimension(); i++) {
            for (int j = 0; j < dimension(); j++) {
                board[i][j] = tiles[i][j];
            }
        }
        return board;
    }

    // unit testing (not graded)
    public static void main(String[] args) {
        int[][] board = {{8,1,3},{4,0,2},{7,6,5}};
        Board b = new Board(board);
        System.out.println(b.twin());
    }
}
