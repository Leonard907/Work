package proj1gold;

import static org.junit.Assert.*;

import edu.princeton.cs.introcs.StdRandom;
import org.junit.Test;

public class TestArrayDequeGold {
    @Test
    public void checkArrayDeque() {
        StudentArrayDeque<Integer> l1 = new StudentArrayDeque<>();
        ArrayDequeSolution<Integer> l2 = new ArrayDequeSolution<>();
        for (int i = 0; i < 1000; i++) {
            double num = StdRandom.uniform();
            if (num < 0.5) {
                l1.addLast(i); l2.addLast(i);
            } else {
                l1.addFirst(i); l2.addFirst(i);
            }
        }
        for (int i = 0; i < 1000; i++) {
            double num = StdRandom.uniform();
            if (num < 0.5) {
                try {
                    assertEquals(l2.removeLast(), l1.removeLast());
                } catch (AssertionError ex) {
                    System.out.println(ex.getMessage());
                    System.out.println(i);
                    throw new AssertionError();
                }
            } else {
                try {
                    assertEquals(l2.removeFirst(), l1.removeFirst());
                } catch (AssertionError ex) {
                    System.out.println(ex.getMessage());
                    System.out.println(i);
                    throw new AssertionError();
                }
            }
        }
    }
}
