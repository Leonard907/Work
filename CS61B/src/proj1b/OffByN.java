package proj1b;

public class OffByN implements CharacterComparator {
    private final int dist;

    public OffByN(int dist) {
        this.dist = dist;
    }

    @Override
    public boolean equalChars(char x, char y) {
        return Math.abs(x-y) == dist;
    }
}
