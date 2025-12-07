import 'dart:io';

void main() {
  final rounds = int.parse(File('rounds.txt').readAsStringSync().trim());

  double pi = 1.0;
  double x = 1.0;

  for (int i = 2; i <= rounds + 1; i++) {
    x = -x;
    pi += x / (2 * i - 1);
  }

  pi *= 4.0;
  print(pi.toStringAsFixed(16));
}
