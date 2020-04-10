package PuzzleSolver;
import edu.princeton.cs.algs4.MinPQ;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdOut;

import java.util.Comparator;
import java.util.LinkedList;

public class Solver {

    private boolean solvable = false;
    private boolean twinSolvable = false;
    private final Board initial;
    private final Board twin;
    private Node last;

    // find a solution to the initial board (using the A* algorithm)
    public Solver(Board initial) {
        if (initial == null) throw new IllegalArgumentException();
        this.initial = initial;
        this.twin = initial.twin();
        aStar();
    }

    public static Board solverFromFile(String path) {
        In in = new In(path);
        int n = in.readInt();
        int[][] tiles = new int[n][n];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                tiles[i][j] = in.readInt();
        return new Board(tiles);
    }

    // is the initial board solvable? (see below)
    public boolean isSolvable() {
        return solvable;
    }

    // min number of moves to solve initial board
    public int moves() {
        if (last == null) return -1;
        return last.moves;
    }

    // sequence of boards in a shortest solution
    public LinkedList<Board> solution() {
        if (last == null) return null;
        LinkedList<Board> solution = new LinkedList<>();
        solution(solution, last);
        return solution;
    }

    private void solution(LinkedList<Board> solution, Node last) {
        if (last == null) return;
        solution.addFirst(last.current);
        solution(solution, last.previous);
    }

    private void aStar() {
        // Set up
        if (initial.isGoal()) {
            solvable = true;
            last = new Node(initial);
            return;
        }
        MinPQ<Node> queue = new MinPQ<>(new Node(initial));
        queue.insert(new Node(initial));
        MinPQ<Node> twinQueue = new MinPQ<>(new Node(twin));
        twinQueue.insert(new Node(twin));
        // Solve
        while (true) {
            Node current = queue.delMin();
            Node twinCurrent = twinQueue.delMin();
            Iterable<Board> neighbors = current.current.neighbors();
            int moves = current.moves;
            Iterable<Board> twinNeighbors = twinCurrent.current.neighbors();
            int twinMoves = twinCurrent.moves;
            for (Board neighbor: neighbors) {
                if (neighbor.isGoal()) {
                    last = new Node(neighbor, current, moves+1);
                    solvable = true; break;
                }
                if (current.previous == null || !current.previous.current.equals(neighbor)) {
                    Node next = new Node(neighbor, current, moves+1);
                    queue.insert(next);
                }
            }
            for (Board neighbor: twinNeighbors) {
                if (neighbor.isGoal()) {
                    twinSolvable = true; break;
                }
                if (twinCurrent.previous == null || !twinCurrent.previous.current.equals(neighbor)) {
                    Node next = new Node(neighbor, twinCurrent, twinMoves+1);
                    twinQueue.insert(next);
                }
            }
            if (solvable || twinSolvable) break;
        }
    }

    private class Node implements Comparator<Node> {
        private int moves;
        private final Board current;
        private Node previous;
        private int manhatten;
        public Node(Board current) {
            moves = 0;
            this.current = current;
            this.manhatten = current.manhattan();
        }
        public Node(Board current, Node previous, int moves) {
            this.current = current;
            this.previous = previous;
            this.moves = moves;
            this.manhatten = current.manhattan();
        }
        @Override
        public int compare(Node node, Node t1) {
            return node.moves + node.manhatten - t1.moves - t1.manhatten;
        }
    }
}
