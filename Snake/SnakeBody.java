import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

public class SnakeBody {
    private List<Point> body = new ArrayList<>();
    private String next;
    private final int dimension;
    private int length;

    public SnakeBody(int dimension, int initialLength, String next) {
        this.dimension = dimension;
        this.length = initialLength;
        this.next = next;
        initialise();
    }

    private void initialise() {
        for (int i = 0; i < length; i++) {
            body.add(new Point(dimension / 2 + i, dimension / 2));
        }
    }

    public void acquireFood() {
        length++;
        String next = "";
        if (body.get(body.size() - 1).x > body.get(body.size() - 2).x)
            next = Snake.RIGHT;
        else if (body.get(body.size() - 1).x < body.get(body.size() - 2).x)
            next = Snake.LEFT;
        else if (body.get(body.size() - 1).y > body.get(body.size() - 2).y)
            next = Snake.UP;
        else if (body.get(body.size() - 1).y < body.get(body.size() - 2).y)
            next = Snake.DOWN;

        body.add(nextPoint(body.get(body.size() - 1), next));
    }

    public void move() {
        body.remove(body.size() - 1);
        Point head = body.get(0);
        Point newHead = nextPoint(head, next);
        body.add(0, newHead);
    }

    private Point nextPoint(Point head, String next) {
        Point newHead;

        switch (next) {
            case Snake.LEFT:
                newHead = new Point(head.x - 1, head.y);
                break;
            case Snake.DOWN:
                newHead = new Point(head.x, head.y - 1);
                break;
            case Snake.RIGHT:
                newHead = new Point(head.x + 1, head.y);
                break;
            case Snake.UP:
                newHead = new Point(head.x, head.y + 1);
                break;
            default:
                newHead = null;
        }
        return newHead;
    }

    public List<Point> getBody() {
        return body;
    }

    public boolean isLoss() {
        Point start = body.get(0);
        boolean outOfBounds =
                start.x == 0 || start.y == 0 || start.x > dimension || start.y > dimension;
        body.remove(0);
        boolean hitBody = body.contains(start);
        body.add(0, start);
        return outOfBounds || hitBody;
    }

    public boolean getFood(Point food) {
        return body.get(0).equals(food);
    }

    public void setNext(String next) {
        this.next = next;
    }

    public String getNext() {
        return next;
    }
}
