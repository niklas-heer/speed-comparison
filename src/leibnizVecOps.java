import java.nio.charset.Charset;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import jdk.incubator.vector.DoubleVector;
import jdk.incubator.vector.VectorOperators;
import jdk.incubator.vector.VectorSpecies;
import jdk.incubator.vector.VectorMask;
import jdk.incubator.vector.VectorShuffle;



public class leibnizVecOps {
  static String readFile(String path, Charset encoding) throws IOException {
    byte[] encoded = Files.readAllBytes(Paths.get(path));
    return new String(encoded, encoding);
  }

  public static void main(String[] args) {
    String data = "";

    try {
      data = readFile("rounds.txt", StandardCharsets.UTF_8);
    } catch (IOException err) {
      System.out.println("Couldn't read file:\n" + err.getMessage());
    }

    int rounds = Integer.parseInt(data.replace("\n", "").replace("\r", ""));

    final VectorSpecies<Double> species = DoubleVector.SPECIES_PREFERRED;
    final int vecLen = species.length();
    final int nVecGroups = rounds / vecLen;

    final DoubleVector divConstants = DoubleVector.fromArray(species, new double[]{1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0},0);
    final DoubleVector aones = DoubleVector.fromArray(species, new double[]{1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0},0);
    final DoubleVector ones = DoubleVector.broadcast(species, 1.0);
    DoubleVector sumTarget = DoubleVector.broadcast(species, 0.0);
    final int dvecLen = vecLen*2;


    for(int vidx = 0; vidx < nVecGroups; ++vidx) {
      sumTarget = sumTarget.add(ones.div(divConstants.add(dvecLen*vidx).mul(aones)));
    }

    double pi = sumTarget.reduceLanes(VectorOperators.ADD);
    for(int idx = nVecGroups * vecLen; idx < rounds; ++idx) {
      final double x = 1.0 - (2.0 * (idx & 0x1));
      pi += (x / (1 + (2*idx)));
    }

    pi *= 4;
    System.out.println(pi);
  }
}
