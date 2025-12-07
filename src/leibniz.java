import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;

public class leibniz {
    public static void main(String[] args) throws FileNotFoundException {
        Scanner s = new Scanner(new File("rounds.txt"));
        long rounds = s.nextLong();
        s.close();

        double sum = 0.0;
        double flip = -1.0;
        for (long i = 1; i <= rounds; i++) {
            flip *= -1.0;
            sum += flip / (2 * i - 1);
        }

        System.out.println(sum * 4.0);
    }
}
