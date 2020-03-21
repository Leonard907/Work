package proj1a;

import java.util.Iterator;

public interface Deque<T> extends Iterable<T> {
    /** Adds an item of type T to the front of the deque. */
    public void addFirst(T item);
    /** Adds an item of type T to the back of the deque. */
    public void addLast(T item);
    /** @return true if deque is empty, false otherwise. */
    public boolean isEmpty();
    /** @return the number of items in the deque. */
    public int size();
    /** Prints the items in the deque from first to last,
     * separated by a space. Once all the items
     * have been printed, print out a new line.
     */
    public void printDeque();

    /**
     * Removes and returns the item at the front of the deque.
     * If no such item exists, returns null.
     * @return the first item removed.
     */
    public T removeFirst();
    /**
     * Removes and returns the item at the back of the deque.
     * If no such item exists, returns null.
     * @return the last item removed.
     */
    public T removeLast();
    /** Gets the item at the given index, where 0 is the front,
     * 1 is the next item, and so forth. If no such item exists,
     * returns null. Must not alter the deque!
     * @return the ith item in the deque
     */
    public T get(int index);

    /**
     * Return the iterator of the array.
     * @return the iterator of the array.
     */
    public Iterator<T> iterator();
}
