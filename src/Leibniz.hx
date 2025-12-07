import sys.io.File;

class Leibniz {
    static function main() {
        var content = File.getContent("rounds.txt");
        var rounds = Std.parseInt(StringTools.trim(content));

        var x:Float = 1.0;
        var pi:Float = 1.0;

        var i = 2;
        while (i <= rounds + 2) {
            x *= -1.0;
            pi += x / (2 * i - 1);
            i++;
        }

        pi *= 4.0;

        Sys.print(pi);
    }
}
