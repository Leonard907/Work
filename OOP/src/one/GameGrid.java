package one;
import java.util.Objects;

/**
 * Encapsulates Sudoku grid data and provides functionality to 
 * access and modify it.
 */
public class GameGrid {

    // Constants for coordinate boundaries and Sudoku numbers
    public static final int GRID_DIM = 9;
    public static final int SUBGRID_DIM = GRID_DIM / 3;
    public static final int MAX_VAL = 9;
    public static final int MIN_VAL = 1;
    public static final int EMPTY_VAL = 0;

    /** Sudoku grid data */
    private final Field[][] grid;

    /**
     * Create a new GameGrid with data based on the given array.
     * @param grid Sudoku game data. 
     */
    public GameGrid(int[][] grid) {
        Objects.requireNonNull(grid);
        this.grid = initialiseGrid(grid);
    }

    /**
     * Create a GameGrid by creating a deep copy of the given one.
     * @param cpy Game grid data to be copied
     */
    public GameGrid(GameGrid cpy) {
        Objects.requireNonNull(cpy);

        grid = new Field[GRID_DIM][GRID_DIM];

        for(int row = 0; row < GRID_DIM; row++) {
            for(int column = 0; column < GRID_DIM; column++) {
                // get value and initial flag from grid to be copied
                grid[row][column] = new Field(
                        cpy.grid[row][column].getValue(), 
                        cpy.grid[row][column].isInitial()
                        );
            }
        }
    }

    /**
     * Copy the given GameGrid instance by selecting the
     * copy constructor depending on the instance's type.
     *
     * @param original the GameGrid instance to be copied.
     * @return a copy of the given GameGrid based on the correct subtype.
     *
     */
    public static GameGrid copyGameGrid(GameGrid original) {
        return new GameGrid(original);
    }

    /** 
     * Initialise this GameGrid's grid member with the given data.
     *
     * @param gridData Sudoku grid data to be used for initialisation
     */
    private Field[][] initialiseGrid(int[][] gridData) {
        Field[][] result = new Field[GRID_DIM][GRID_DIM];
        for(int row = 0; row < gridData.length; row++) {
            for(int column = 0; column < gridData[row].length; column++) {
                int value = gridData[row][column];
                if (value == EMPTY_VAL) result[row][column] = new Field();
                // during initialisation all grid values are initial values
                else result[row][column] = new Field(value, true);             
            }
        }

        return result;
    }


    /**
     * Return the number of free fields in this grid.
     * @return number of free fields
     */
    public int getFreeFields() {
        int free = 0;
        for(int column = 0; column < GRID_DIM; column++) {
            for(int row = 0; row < GRID_DIM; row++) {
                if(grid[row][column].getValue() == EMPTY_VAL) free++;
            }
        }
        return free;
    }

    /** 
     * Check if the field at the given coordinates was set from the beginning
     * of the game.
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @return true if this field was set from the beginning of the game, false otherwise.
     */
    public boolean isInitial(int column, int row) {
        if (column < 0 || column >= GRID_DIM || row < 0 || row >= GRID_DIM)
            throw new IllegalArgumentException("Given dimensions invalid: " + column + "x" + row);
        return grid[row][column].isInitial();
    }

    /**
     * Return the grid value specified by the given coordinates.
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @return the specified Sudoku value between 0 and 9
     */
    public int getField(int column, int row) {
        if (column < 0 || column >= GRID_DIM || row < 0 || row >= GRID_DIM)
            throw new IllegalArgumentException("Given dimensions invalid: " + column + "x" + row);
        return grid[row][column].getValue();
    }

    /**
     * Sets the grid at the given coordinate to the given value.
     * 
     * If the value is not allowed according to Sudoku rules, 
     * the field is not set and false is returned.
     * If setting was successful, true is returned. 
     *
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @param value the Sudoku number to be set between 1 and 9
     * @return true if successful, false otherwise
     */
    public boolean setField(int column, int row, int value) {
        if (column < 0 || column >= GRID_DIM || row < 0 || row >= GRID_DIM)
            throw new IllegalArgumentException("Given dimensions invalid: " + column + "x" + row);
        if (value < MIN_VAL || value > MAX_VAL)
            throw new IllegalArgumentException("Given value invalid: " + value);

        if(!isInitial(column, row) && isValid(column,row,value)) {
            grid[row][column].setValue(value);
            return true;
        }

        return false;
    }

    /**
     * Clear the specified Sudoku field.
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8    
     */
    public void clearField(int column, int row) {
        if (column < 0 || column >= GRID_DIM || row < 0 || row >= GRID_DIM)
            throw new IllegalArgumentException("Given dimensions invalid: " + column + "x" + row);

        grid[row][column].setValue(EMPTY_VAL);
    }

    /**
     * Check if the specified value is allowed at the specified position
     * according to Sudoku rules.
     *
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @param value the Sudoku number to be set between 1 and 9
     * @return true if allowed, false otherwise 
     */
    protected boolean isValid(int column, int row, int value) {
        return checkRow(column,row,value) && 
               checkColumn(column,row,value) && 
               checkSubGrid(column,row,value);
    }

    /**
     * Check if the specified value is allowed in the specified row
     * according to Sudoku rules.
     *
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @param value the Sudoku number to be set between 1 and 9
     * @return true if allowed, false otherwise 
     */   
    private boolean checkRow(int column, int row, int value) {
        boolean result = true;        
        for(int colIdx = 0; colIdx < GRID_DIM; colIdx++) {
            if (colIdx != column && grid[row][colIdx].getValue() - 1 == value) {
                result = false;
                break;
            }
        }
        return result;
    }

    /**
     * Check if the specified value is allowed in the specified column
     * according to Sudoku rules.
     *
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @param value the Sudoku number to be set between 1 and 9
     * @return true if allowed, false otherwise 
     */   
    private boolean checkColumn(int column, int row, int value) {
        boolean result = true;        
        for(int rowIdx = 0; rowIdx < GRID_DIM; rowIdx++) {
            if (rowIdx != row && grid[rowIdx][column].getValue() - 1 == value) {
                result = false;
                break;
            }
        }
        return result;
    }

    /**
     * Check if the specified value is allowed in the specified subgrid
     * according to Sudoku rules.
     *
     * @param column x coordinate between 0 and 8
     * @param row y coordinate between 0 and 8
     * @param value the Sudoku number to be set between 1 and 9
     * @return true if allowed, false otherwise 
     */   
    private boolean checkSubGrid(int column, int row, int value) {
        // calculate sub 3x4 grid start values
        final int column_start = column / SUBGRID_DIM * SUBGRID_DIM;
        final int row_start = row / SUBGRID_DIM * SUBGRID_DIM;

        boolean result = true;
        for(int colIdx = column_start; colIdx < column_start + SUBGRID_DIM; colIdx++) {
            for(int rowIdx = row_start; rowIdx < row_start + SUBGRID_DIM; rowIdx++) {
                if (colIdx != column && rowIdx != row && grid[rowIdx][colIdx].getValue() - 1 == value) {
                    result = false;
                    break;
                }
            }
        }
        return result;
    }

    /**
     * Return an ASCII representation of the game grid. 
     * @param grid the game grid to be printed
     */
    public String toString() {
        String result = "";
        
        for (int row = 0; row < GRID_DIM; row++) {
            for(int column = 0; column < GRID_DIM; column++) {
                result += grid[row][column].toString();
                if(column % SUBGRID_DIM == 2) result += " ";
                result += " ";
            }

            if(row % SUBGRID_DIM == 2) result += "\n";
            result += "\n";
        }

        return result;
    }

}
