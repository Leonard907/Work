package week5;

import static org.junit.Assert.assertArrayEquals;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.junit.Test;

public class SplitPathNameTest {

    @Test
    public void testSplitPath_fullPath() {

        // Test full path
        String[] components = SplitPathName.splitPath("/home/bill/work/weedfertilisers.pdf");
        String[] expectedComponents = { "/home/bill/work/", "weedfertilisers.pdf", "weedfertilisers", ".pdf" };
        assertArrayEquals(expectedComponents, components);

        // Test file only
        components = SplitPathName.splitPath("todo.txt");
        expectedComponents = new String[] { "", "todo.txt", "todo", ".txt" };
        assertArrayEquals(expectedComponents, components);

        // Test relative path
        components = SplitPathName.splitPath("work/myfile.txt");
        expectedComponents = new String[] { "work/", "myfile.txt", "myfile", ".txt" };
        assertArrayEquals(expectedComponents, components);

        // Test file without extension
        components = SplitPathName.splitPath("weedfertilisers");
        expectedComponents = new String[] { "", "weedfertilisers", "weedfertilisers", "" };
        assertArrayEquals(expectedComponents, components);
    }

    /**
     * Temporarily captures the output to the standard output stream, then
     * restores the standard output stream once complete.
     *
     * Note: the implementation details of this function are advanced. If you
     * don't understand it just now, don't worry.
     *
     * @param args
     *            arguments to pass to main function of class to be tested
     * @return output result of calling main function of class to be tested
     */
    private String captureOutputOfMain(String args[]) {
        OutputStream outputStream = new ByteArrayOutputStream();
        PrintStream originalOut = System.out;
        System.setOut(new PrintStream(outputStream));
        try {
            SplitPathName.main(args);
        }
        finally {
            System.setOut(originalOut);
        }
        return outputStream.toString().trim();
    }

    @Test
    public void testCallingMain() {
        // Call SplitPathName.main() and check the output is as expected.
        String[] args = { "/home/ben/todo.txt", "/home/bob/work/flowerpotlist.xls", "/programs/MyClass.java" };
        String result = captureOutputOfMain(args);

        // Split the result into individual lines
        String[] resultLines = result.split("[\r\n]+");

        String[] expectedResults = {
                "File: todo.txt Type: .txt [/home/ben/]",
                "File: flowerpotlist.xls Type: .xls [/home/bob/work/]",
                "File: MyClass.java Type: .java [/programs/]"
        };

        assertArrayEquals(expectedResults, resultLines);
    }

}
