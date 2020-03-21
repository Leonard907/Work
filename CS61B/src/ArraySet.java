import java.util.*;

public class ArraySet<T> implements Iterable<T> {

    private T[] items;
    private int size;

    /**
     * Iterator for ArraySet
     */
    private class ArraySetIterator implements Iterator<T> {
        private int next = 0;

        @Override
        public boolean hasNext() {
            return next != size;
        }

        @Override
        public T next() {
            return items[next++];
        }
    }

    public ArraySet() {
        items = (T[]) new Object[100];
        size = 0;
    }

    public Iterator<T> iterator() {return new ArraySetIterator();}

    /* Returns true if this map contains a mapping for the specified key.
     */
    public boolean contains(T x) {
        for (int i = 0; i < size; i++) {
            try {
                if (x.equals(items[i])) return true;
            } catch (NullPointerException ex) {
                if (x == null && items[i] == null) return true;
            }
        }
        return false;
    }

    /* Associates the specified value with the specified key in this map.
       Throws an IllegalArgumentException if the key is null. */
    public void add(T x) {
        if (!contains(x)) {
            items[size++] = x;
        }
        if (size == items.length - 1) {
            T[] doubleSize = (T[]) new Object[items.length * 2];
            System.arraycopy(items, 0, doubleSize, 0, items.length);
            items = doubleSize;
        }
    }

    /* Returns the number of key-value mappings in this map. */
    public int size() {
        return size;
    }

    public static void main(String[] args) {
        ArraySet<String> s = new ArraySet<>();
        s.add(null);
        s.add("horse");
        s.add("fish");
        s.add("house");
        s.add("fish");
        System.out.println(s);
    }

    /* Also to do:
    1. Make ArraySet implement the Iterable<T> interface.
    2. Implement a toString method.
    3. Implement an equals() method.
    */

    @Override
    public String toString() {
        StringBuilder listItems = new StringBuilder("{");
        for (int i = 0; i < size; i++) {
            listItems.append(items[i]);
            if (i != size - 1) listItems.append(", ");
        }
        return listItems.append('}').toString();
    }
}
