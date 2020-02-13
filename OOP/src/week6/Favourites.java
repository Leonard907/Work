package week6;

import java.util.*;

public class Favourites {
    private ArrayList<MusicTrack> favourites = new ArrayList<>();

    public void addTrack(String artist, String title) {
        MusicTrack favourite = new MusicTrack(artist, title);
        if (favourites.size() < 5)
            favourites.add(favourite);
        else
            System.out.println("Sorry, can't add: " + favourite.toString() + "\n");
    }

    public void showFavourites() {
        favourites.forEach(e -> System.out.println(e));
    }
}
