package proj1a;

import java.util.ArrayList;
import java.util.List;

public class Test {

    public static void main(String[] args) {
        SL a = new SL();
        System.out.println(a);
    }


}

class SL {
    private class IntNode {
        public int item;
        public IntNode next;
        public IntNode(int item, IntNode next) {
            this.item = item;
            this.next = next;
        }
    }

    private IntNode first;

    public void addFirst(int x) {
         first = new IntNode(x, first);
    }

    public void reverse() {
        IntNode start = first;
        addFirst(start.item);
        start = start.next;
        first.next = null;
        while (start != null) {
            addFirst(start.item);
            start = start.next;
        }
    }

    @Override
    public String toString() {
        IntNode start = first;
        String des = "{";
        while (start.next != null) {
            des += start.item;
            des += ", ";
            start = start.next;
        }
        des += start.item;
        des += "}";
        return des;
    }
}

