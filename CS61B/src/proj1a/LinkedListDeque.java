package proj1a;

import java.util.Iterator;

public class LinkedListDeque<T> implements Deque<T> {
    /**
     * A class of the node used in the deque
     */
    private class Node {
        private T item;
        private Node next;
        private Node previous;

        public Node(T i, Node n, Node p) {
            item = i; next = n; previous = p;
        }
    }

    /** circular sentinel node in the list. */
    private Node sentinel;
    /** size of the linked list. */
    private int size;

    public LinkedListDeque() {
        sentinel = new Node(null, null, null);
        size = 0;
    }

    public LinkedListDeque(LinkedListDeque<T> other) {
        sentinel = new Node(null, null, null);
        size = 0;
        // Add all elements to the sentinel
        for (int i = 0; i < other.size; i++) {
            addLast(other.get(i));
        }
    }

    @Override
    public void addFirst(T item) {
        if (size > 0) {
            sentinel.next.previous = new Node(item, sentinel.next, sentinel);
            sentinel.next = sentinel.next.previous;
        } else {
            /* If the list is empty we need to set the next node to have
               both pointers pointing at the sentinel.
            */
            sentinel.next = new Node(item, sentinel, sentinel);
            sentinel.previous = sentinel.next;
        }
        size++;
    }

    @Override
    public void addLast(T item) {
        if (size > 0) {
            sentinel.previous.next = new Node(item, sentinel, sentinel.previous);
            sentinel.previous = sentinel.previous.next;
        } else {
            /* If the list is empty we need to set the next node to have
               both pointers pointing at the sentinel.
             */
            sentinel.next = new Node(item, sentinel, sentinel);
            sentinel.previous = sentinel.next;
        }
        size++;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public T removeFirst() {
        T item = sentinel.next.item;
        if (size > 1) {
            sentinel.next.next.previous = sentinel;
            sentinel.next = sentinel.next.next;
        } else {
            // Only 1 element, initialise sentinel
            sentinel = new Node(null, null, null);
        }
        size--;
        return item;
    }

    @Override
    public T removeLast() {
        T item = sentinel.previous.item;
        if (size > 1) {
            sentinel.previous.previous.next = sentinel;
            sentinel.previous = sentinel.previous.previous;
        } else {
            // Only 1 element, initialise sentinel
            sentinel = new Node(null, null, null);
        }
        size--;
        return item;
    }

    @Override
    public void printDeque() {
        Node start = sentinel.next;
        // Print start border
        System.out.print("[");
        // Print body
        while (true) {
            System.out.print(start.item);
            start = start.next;
            if (start.item != null) {
                System.out.print(", ");
            } else {
                break;
            }
        }
        System.out.println("]");
    }

    @Override
    public T get(int index) {
        int count = 0;
        Node start = sentinel.next;
        while (count < index) {
            if (start.item == null) {
                throw new IllegalArgumentException("Index out of bounds.");
            }
            start = start.next;
            count++;
        }
        return start.item;
    }

    @Override
    public Iterator<T> iterator() {
        return new LinkedListIterator();
    }

    private class LinkedListIterator implements Iterator<T> {
        private Node start = sentinel.next;
        private int sizeCount = 0;
        @Override
        public boolean hasNext() {
            return sizeCount < size;
        }

        @Override
        public T next() {
            T item = start.item;
            start = start.next;
            sizeCount++;
            return item;
        }
    }
}
