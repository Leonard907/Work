package week7;

import java.util.*;

public class WordCounter {

    HashMap<Integer, Integer> freqDistribution;

    public WordCounter(String[] tokens) {
        wordLengthFreq(tokens);
    }

    public void wordLengthFreq(String[] tokens) {
        freqDistribution = new HashMap<>();
        for (String token: tokens) {
            int length = token.length();
            if (freqDistribution.containsKey(length)) {
                freqDistribution.put(length, freqDistribution.get(length) + 1);
            }
            else {
                freqDistribution.put(length, 1);
            }
        }
    }

    public HashMap<Integer, Integer> getFreqDist() {
        return freqDistribution;
    }

    public int maxVal() {
        try {
            return Collections.max(freqDistribution.values());
        }
        catch (NoSuchElementException ex) {
            return 0;
        }
    }

    public double[] map2array() {
        try {
            double[] array = new double[Collections.max(freqDistribution.keySet()) > 0 ? (Collections.max(freqDistribution.keySet()) + 1) : 0];
            System.out.println(freqDistribution.keySet());
            for (int num: freqDistribution.keySet()) {
                array[num] = (double) freqDistribution.get(num) / maxVal() * 100;
            }
            return array;
        }
        catch (NoSuchElementException e) {
            return new double[] {};
        }

    }
}
