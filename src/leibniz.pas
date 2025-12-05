program leibniz;

var
  f: text;
  line: string;
  rounds, i: longint;
  pi, x: double;

begin
  assign(f, 'rounds.txt');
  reset(f);
  readln(f, line);
  close(f);

  val(line, rounds);

  pi := 1.0;
  x := 1.0;

  for i := 2 to rounds + 1 do
  begin
    x := x * -1;
    pi := pi + x / (2 * i - 1);
  end;

  pi := pi * 4;
  writeln(pi);
end.
