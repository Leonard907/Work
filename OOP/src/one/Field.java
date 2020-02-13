package one;
/**
 * A wrapper for a playing field with an integer value.
 *
 * It can be used for a Sudoku game.
 */
public class Field {

    /** 
     * Indicates whether this field was set from the start of the game.
     *
     * This member vaiable is immutable.
     */
    private final boolean initial;

    /** indicates the value of this playing field. */
    private int value;

    /** 
     * Creates a playing field which is not 
     * marked as initial with a value of zero.
     */
    public Field() { 
        initial = false;
    }

    /**
     * Creates a playing field with the given value and initial marker.
     *
     * @param val the value of the field
     * @param init indicator if the field is initial to the start of the game
     */
    public Field(int val, boolean init) {
        setValue(val);
        initial = init;
    }

    /**
     * Returns this field's value.
     * @return this field's value
     */
    public int getValue() {
        return value;
    }

    /**
     * Set this field's value to the given argument.
     * @param val use for this field's value 
     */
    public void setValue(int val) {
        value = val;
    }

    /**
     * Check if this field has been set from the start of the game or not.
     * @return true if this field has been set from the start of the game, false otherwise.
     */
    public boolean isInitial() {
        return initial;
    }

    /**
     * A representation of this field's value.
     *
     * A '*' inidicates if this field was set from the beginning of the game.
     *
     * @return a representation of this field's value.
     */
    public String toString() {
        if (initial) return value + "'";
        else return value + " ";
    }
}
