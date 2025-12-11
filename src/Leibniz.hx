import sys.io.File;

class Leibniz {
    static function main() {
        var content = File.getContent("rounds.txt");
        var rounds = Std.parseInt(StringTools.trim(content));
        var stop:Float = rounds + 2.0;

        var x:Float = 1.0;
        var pi:Float = 1.0;

        var i:Float = 2.0;
        while (i <= stop) {
            x = -x;
            pi += x / (2.0 * i - 1.0);
            i += 1.0;
        }

        pi *= 4.0;

        Sys.print(pi);
    }
}
