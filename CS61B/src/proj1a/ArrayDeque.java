package proj1a;

import java.util.Iterator;

public class ArrayDeque<T extends Object> implements Deque<T> {
    private Object[] items;
    private int startIndex;
    private int endIndex;
    private int size = 10;

    public ArrayDeque() {
        items = new Object[size];
        startIndex = size / 2;
        endIndex = size / 2 + 1;
    }

    public ArrayDeque(ArrayDeque<T> other) {
        items = new Object[other.size];
        startIndex = other.startIndex;
        endIndex = other.endIndex;
        size = other.size;
        System.arraycopy(other.items, startIndex + 1, items, startIndex + 1,
                endIndex - startIndex - 1);
    }

    @Override
    public void addFirst(T item) {
        items[startIndex--] = item;
        if (startIndex == -1) {
            size = size * 2;
            Object[] temp = new Object[size];
            System.arraycopy(items, 0, temp, size / 2, size / 2);
            items = temp;
        }
    }

    @Override
    public void addLast(T item) {
        items[endIndex++] = item;
        if (endIndex == size) {
            size = size * 2;
            Object[] temp = new Object[size];
            System.arraycopy(items, 0, temp, 0, size / 2);
            items = temp;
        }
    }

    @Override
    public boolean isEmpty() {
        return endIndex - startIndex == 1;
    }

    @Override
    public int size() {
        return endIndex - startIndex - 1;
    }

    @Override
    public T removeLast() {
        if (endIndex - startIndex == 1) {
            throw new IllegalArgumentException("No elements in the list");
        }
        T item = (T) items[--endIndex];
        return item;
    }

    @Override
    public T removeFirst() {
        if (endIndex - startIndex == 1) {
            throw new IllegalArgumentException("No elements in the list");
        }
        T item = (T) items[++startIndex];
        return item;
    }

    @Override
    public void printDeque() {
        System.out.print("[");
        for (int i = startIndex + 1; i < endIndex; i++) {
            if (i == endIndex - 1) {
                System.out.print(items[i]);
            } else {
                System.out.print(items[i] + ", ");
            }
        }
        System.out.print("]");
    }

    @Override
    public T get(int index) {
        return (T) items[startIndex + index + 1];
    }

    @Override
    public Iterator<T> iterator() {
        return new ArrayIterator();
    }

    private class ArrayIterator implements Iterator<T> {
        private int begin = startIndex + 1;

        @Override
        public boolean hasNext() {
            return begin < endIndex;
        }

        @Override
        public T next() {
            return (T) items[begin++];
        }
    }
}
