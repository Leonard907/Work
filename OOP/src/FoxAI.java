/**
 * AI for fox
 */
public class FoxAI {

    /**
     * Make a move.
     * Algorithm:
     * 1. Whether a move forward is possible (both directions)
     *    Yes: If two moves (left & right) are possible, calculate left & right spaces.
     *         The one that has the most spaces indicates the next move's direction.
     *         If only one move is possible, do that.
     *    No: If two backward moves (left & right) are possible, calculate left & right spaces.
     *        The one that has the most spaces indicates the next move's direction.
     *        If only one move is possible, do that.
     * 2. If no moves are possible, game is over. return false.
     * @param players figures' positions
     * @param dim     dimension of the board
     * @return the move
     */
    public static boolean makeMove(String[] players, int dim) {
        int[] foxPosition = FoxHoundUI.parsePosition(players[players.length - 1]);
        // Moves: top left, top right, bottom left, bottom right
        int[][] possibleMoves = {{foxPosition[0] - 1, foxPosition[1] - 1},
                {foxPosition[0] - 1, foxPosition[1] + 1},
                {foxPosition[0] + 1, foxPosition[1] - 1},
                {foxPosition[0] + 1, foxPosition[1] + 1}};
        boolean optimal = false;
        if (!breakThrough(players, dim, foxPosition[1]).equals("no match")) {
            if (breakThrough(players, dim, foxPosition[1]).equals("left")) {
                if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
                    optimal = true;
                }
                else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
                    optimal = true;
                }
            }
            else if (breakThrough(players, dim, foxPosition[1]).equals("right")) {
                if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
                    optimal = true;
                }
                else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                        players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3]))) {
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
                    optimal = true;
                }
            }
        }
        if (!optimal) {
            if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0]))
                    && FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1]))) {
                int[] spaceCount = leftRightSpace(players, dim, foxPosition[1]);
                if (spaceCount[0] >= spaceCount[1])
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
                else
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[0])))
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[0]);
            else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[1])))
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[1]);
                // No forward moves possible
            else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2]))
                    && FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3]))) {
                int[] spaceCount = leftRightSpace(players, dim, foxPosition[1]);
                if (spaceCount[0] >= spaceCount[1])
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
                else
                    players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
            } else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[2])))
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[2]);
            else if (FoxHoundUtils.isValidMove(dim, players, 'F',
                    players[players.length - 1], FoxHoundUtils.toPosition(possibleMoves[3])))
                players[players.length - 1] = FoxHoundUtils.toPosition(possibleMoves[3]);
            else
                return false;
        }
        return true;
    }

    /**
     * Calculate the number of spaces on the left/right of the fox
     * Throws IllegalArgumentException when input coordinates is invalid
     * @param players The positions of the figures
     * @param dim Dimension of the board
     * @param fox Column position of the fox
     * @return An array that contains left & right spaces
     */
    private static int[] leftRightSpace(String[] players, int dim, int fox) {
        int[] playersRow = new int[players.length - 1];
        int[] playersCol = new int[players.length - 1];
        int leftCount = 0;
        int rightCount = 0;
        for (int i = 0; i < players.length - 1; i++) {
            playersRow[i] = FoxHoundUI.parsePosition(players[i])[0];
            playersCol[i] = FoxHoundUI.parsePosition(players[i])[1];
        }
        for (int i = 0; i < dim; i++) {
            for (int j = 0; j < dim; j++) {
                if (!contains(i, filterLeft(playersCol, playersRow, fox)) && j < fox)
                    leftCount += 1;
                if (!contains(i, filterRight(playersCol, playersRow, fox)) && j > fox)
                    rightCount += 1;
            }
        }
        // Calculate left & right difference
        for (int col: playersCol) {
            if (col < fox)
                leftCount -= 5;
            else if (col > fox)
                rightCount -= 5;
        }
        System.out.println(leftCount + " " + rightCount);
        return new int[] {leftCount, rightCount};
    }

    private static boolean contains(int element, int[] array) {
        for (int target: array) {
            if (element == target)
                return true;
        }
        return false;
    }

    private static int[] filterLeft(int[] nums, int[] rows, int fox) {
        int count = 0;
        for (int num: nums) {
            if (num < fox)
                count++;
        }
        int[] filter = new int[count];
        int index = 0;
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] < fox) {
                filter[index] = rows[i];
                index++;
            }
        }
        return filter;
    }

    private static int[] filterRight(int[] nums, int[] rows, int fox) {
        int count = 0;
        for (int num: nums) {
            if (num > fox)
                count++;
        }
        int[] filter = new int[count];
        int index = 0;
        for (int i = 0; i < nums.length; i++) {
            if (nums[i] > fox) {
                filter[index] = rows[i];
                index++;
            }
        }
        return filter;
    }

    private static String breakThrough(String[] players, int dim, int fox) {
        int[] playersCol = new int[players.length - 1];
        for (int i = 0; i < players.length - 1; i++) {
            playersCol[i] = FoxHoundUI.parsePosition(players[i])[1];
        }
        for (int i = 0; i < fox - 1; i++) {
            if (!contains(i, playersCol) && !(contains(i+1, playersCol))) {
                return "left";
            }
        }
        for (int i = fox + 1; i < dim - 1; i++) {
            if (!contains(i, playersCol) && !(contains(i+1, playersCol))) {
                return "right";
            }
        }
        return "no match";
    }

}

