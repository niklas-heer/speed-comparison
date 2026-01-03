% Calculate Pi using the Leibniz formula

rounds = csvread('rounds.txt');

pi = 1;

for ii=2:rounds
    pi = pi + (-1) ^ (ii - 1) / (2 * ii - 1);
end

pi = pi * 4;
fprintf('%0.16f\n', pi)

