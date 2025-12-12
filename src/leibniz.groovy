import groovy.transform.CompileStatic

@CompileStatic
class Leibniz {
    static void main(String[] args) {
        int rounds = new File("rounds.txt").text.trim().toInteger()

        double x = 1.0d
        double pi = 1.0d

        for (int i = 2; i <= rounds + 2; i++) {
            x = -x
            pi += x / (2 * i - 1)
        }

        pi *= 4.0d

        print pi
    }
}
