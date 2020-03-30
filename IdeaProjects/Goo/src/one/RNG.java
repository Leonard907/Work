package one;

import java.util.Random;

/*
   Generates random number.
   Used to determine who plays first in each round of the game.
 */
class RNG {
    private int max;
    private int min;

    RNG(int max, int min) {
        setMax(max);
        setMin(min);
    }

    private void setMax(int max) {
        this.max = max;
    }

    private void setMin(int min) {
        this.min = min;
    }

    private int getMax() {
        return max;
    }

    private int getMin() {
        return min;
    }

    int generateNumber() {
        Random rand = new Random();
        return rand.nextInt(max - min + 1) + min;
    }
}
