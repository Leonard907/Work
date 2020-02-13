package week6;

public class MusicTrack {
    String artist;
    String title;

    public MusicTrack(String artist, String title) {
        this.artist = artist;
        this.title = title;
    }

    public String getArtist() {
        return artist;
    }

    public String getTitle() {
        return title;
    }

    public String toString() {
        return title + " | " + artist;
    }
}
