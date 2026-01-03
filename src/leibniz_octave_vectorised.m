% Calculate Pi using the Leibniz formula (vectorised)

rounds = csvread('rounds.txt');

pi = 4 * sum(1 ./ (1 - 2 * rounds : 4 : 2 * rounds));

fprintf('%0.16f\n', pi)

