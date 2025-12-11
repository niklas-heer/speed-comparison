defmodule Leibniz do
  def main() do
    rounds = File.read!("rounds.txt") |> String.trim |> String.to_integer()
    stop = (rounds + 2) / 1

    pi = calculate(1.0, 1.0, 2.0, stop)

    IO.puts pi
  end

  def calculate(x, pi, i, stop) when i <= stop do
    x_new = -x
    pi_new = pi + (x_new / (2.0 * i - 1.0))
    calculate(x_new, pi_new, i + 1.0, stop)
  end

  def calculate(_x, pi, _i, _stop) do
    pi * 4.0
  end
end

Leibniz.main()
