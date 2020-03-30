import org.junit.Test;

import static org.junit.Assert.assertEquals;

public abstract class CommandTest {

    protected static final String TITLE_ARGUMENT = "TITLE";
    protected static final String AUTHOR_ARGUMENT = "AUTHOR";

    protected static final String BLANK_ARGUMENT = "";

    protected LibraryCommand testCommand;
    protected LibraryData testLibrary;

    public CommandTest() {
        testCommand = null;
        testLibrary = null;
    }

    protected abstract CommandType getCmdType();

    // ------------------------- initialisation tests --------------------

    @Test
    public void testClassCommandExtension() {
        assertEquals(testCommand.getClass() + " has unexpected superclass.", LibraryCommand.class,
                testCommand.getClass().getSuperclass());
    }

    @Test
    public void testCtorSuperclassCall() {
        CommandTestUtils.checkCtorSuperclassCall(testCommand, getCmdType());
    }

    // ------------------------- parseArguments tests --------------------

    @Test
    public void testIsParseArgumentsOverridden() {
        CommandTestUtils.checkIfParseArgumentsIsOverridden(testCommand);
    }

    // ------------------------- execute tests --------------------

    @Test(expected = NullPointerException.class)
    public void testExecuteLibraryDataNull() {
        testCommand.execute(null);
    }
}