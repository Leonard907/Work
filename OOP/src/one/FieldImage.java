package one;
import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class FieldImage {
    private static BufferedImage cow, football, house, keanu, rain, rudolph, sun, superwoman, tree;

    static {
        try {
            String prefixPath = "~/Downloads/Appdroid/res/";

            cow = ImageIO.read(new File("cow.png"));
            football = ImageIO.read(new File("football.png"));
            house = ImageIO.read(new File("house.png"));
            keanu = ImageIO.read(new File("keanu.png"));
            rain = ImageIO.read(new File("rain.png"));
            rudolph = ImageIO.read(new File("rudolph.png"));
            sun = ImageIO.read(new File("sun.png"));
            superwoman = ImageIO.read(new File("superwoman.png"));
            tree = ImageIO.read(new File("tree.png"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static ImageIcon[] icons = new ImageIcon[] {
        new StretchIcon(sun),
        new StretchIcon(cow),
        new StretchIcon(football),
        new StretchIcon(house),
        new StretchIcon(keanu),
        new StretchIcon(rain),
        new StretchIcon(rudolph),
        new StretchIcon(sun),
        new StretchIcon(superwoman),
        new StretchIcon(tree)
    };
}
