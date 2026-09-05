% Calculate Pi using the Leibniz formula (vectorised)

rounds = csvread('rounds.txt');

% Bound the temporary vectors while preserving the signed denominator sequence.
total = 0;
for first = (1 + mod(rounds,2) * 2 - 2 * rounds):(4 * 1000000):(2 * rounds)
    last = min(first + 4 * (1000000 - 1), 2 * rounds);
    total = total + sum(1 ./ (first:4:last));
end
pi = 4 * total;

fprintf('%0.16f\n', pi)

