package week4;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class IsTriangularTest {

    @Test
    public void testIsTri() {
        // Is triangle cases
        assertTrue(IsTriangular.isTri(1.0, 1.0, 1.0));
        assertTrue(IsTriangular.isTri(3.0, 4.0, 5.0));

        assertTrue(IsTriangular.isTri(1.0, 1.0, 1.9));
        assertTrue(IsTriangular.isTri(1.0, 1.9, 1.0));
        assertTrue(IsTriangular.isTri(1.9, 1.0, 1.0));

        // Is not triangle cases
        assertFalse(IsTriangular.isTri(1.0, 1.0, 2.0));
        assertFalse(IsTriangular.isTri(1.0, 2.0, 1.0));
        assertFalse(IsTriangular.isTri(2.0, 1.0, 1.0));

        assertFalse(IsTriangular.isTri(3.0, 1.0, 1.0));
        assertFalse(IsTriangular.isTri(20.0, 60.0, 20.0));
        assertFalse(IsTriangular.isTri(1.0, 1.0, 2.01));
    }

}
