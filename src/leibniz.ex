defmodule Leibniz do
  def main() do
    rounds = File.read!("rounds.txt") |> String.trim |> String.to_integer()

    pi = calculate_pi(rounds)

    IO.puts pi
  end

  def calculate_pi(rounds) do
    calculate(1.0, 1.0, (rounds + 2))
  end

  def calculate(x, pi, rounds) when rounds > 1 do
    x_new = x * (-1.0)
    pi_new = pi + (x_new / (2 * rounds - 1))
    calculate(x_new, pi_new, rounds - 1)
  end

  def calculate(_x, pi, 1) do
    pi * 4.0
  end
end

Leibniz.main()
