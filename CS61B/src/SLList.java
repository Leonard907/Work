package proj1a;

import java.util.ArrayList;

public class SLList<T> {
    private class Node {
        private T item;
        private Node next;

        public Node(T i, Node n) {
            item = i; next = n;
        }

        public T getItem() {
            return item;
        }

        public Node getNext() {
            return next;
        }
        public void setItem(T item) {
            this.item = item;
        }

        public void setNext(Node next) {
            this.next = next;
        }
    }
        
    private Node first;
    private int size;

    public SLList(T item) {
        first = new Node(item, null);
        size = 1;
    }

    public void addFirst(T item) {
        first = new Node(item, first);
        size++;
    }

    public T getFirst() {
        return first.getNext().getItem();
    }

    public int size() {
        return size;
    }

    public void addLast(T item) {
        Node current = first;
        while (current.getNext() != null) {
            current = current.getNext();
        }
        current.setNext(new Node(item, null));
        size++;
    }

    @Override
    public String toString() {
        Node current = first;
        StringBuilder contentsDisplay = new StringBuilder();
        contentsDisplay.append("[");
        while (current.getNext() != null) {
            contentsDisplay.append(current.getItem()).append(", ");
            current = current.getNext();
        }
        contentsDisplay.append(current.getItem()).append("]");
        return contentsDisplay.toString();
    }
}




