package week5;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class AbsolutePathTest {

    @Test
    public void testEnsureAbsolute() {
        String workingDir = System.getProperty("user.dir");

        // Test relative path
        String path = AbsolutePath.ensureAbsolute("work/weedfertilisers.pdf");
        assertEquals(workingDir + "/work/weedfertilisers.pdf", path);

        // Test already absolute path
        path = AbsolutePath.ensureAbsolute("/etc/httpd.conf");
        assertEquals("/etc/httpd.conf", path);
    }

    @Test
    public void testAbsoluteSplitPath() {
        String workingDir = System.getProperty("user.dir");

        // Test relative path
        String[] components = AbsolutePath.absoluteSplitPath("work/weedfertilisers.pdf");
        String[] expectedComponents = { workingDir + "/work/", "weedfertilisers.pdf", "weedfertilisers", ".pdf" };
        assertArrayEquals(expectedComponents, components);

        // Test already absolute path
        components = AbsolutePath.absoluteSplitPath("/etc/httpd.conf");
        expectedComponents = new String[] { "/etc/", "httpd.conf", "httpd", ".conf" };
        assertArrayEquals(expectedComponents, components);
    }

}
