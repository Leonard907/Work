package week4;

import java.util.*;
import java.util.stream.DoubleStream;

public class ArrayOps {

    public static double findMax(double[] data) {
        double max = data[0];
        for (int i = 1; i < data.length; i++) {
            if (data[i] > max) {
                max = data[i];
            }
        }
        return max;
    }

    public static double[] normalise(double[] data) {
        double sum = DoubleStream.of(data).sum();
        double[] normalizedData = data.clone();
        for (int i = 0; i < data.length; i++) {
            normalizedData[i] = normalizedData[i] / sum;
        }
        return normalizedData;
    }

    public static void normaliseInPlace(double[] data) {
        double sum = DoubleStream.of(data).sum();
        for (int i = 0; i < data.length; i++) {
            data[i] = data[i] / sum;
        }
    }

    public static double[] reverse(double[] data) {
        double[] reversedData = new double[data.length];
        for (int i = 0; i < data.length; i++) {
            reversedData[i] = data[data.length - i - 1];
        }
        return reversedData;
    }

    public static void reverseInPlace(double[] data) {
        for (int i = 0; i < data.length / 2; i++) {
            double temp = data[i];
            data[i] = data[data.length - i - 1];
            data[data.length - i - 1] = temp;
        }
    }

    public static void swap(double[] data1, double[] data2) {
        for (int i = 0; i < data1.length; i++) {
            double temp = data1[i];
            data1[i] = data2[i];
            data2[i] = temp;
        }
    }

}
