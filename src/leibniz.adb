with Ada.Text_IO; use Ada.Text_IO;

procedure Leibniz is
	type Rounds is mod 2**32;
	type Decimal is digits 15;

	function ComputePi (N: Rounds) return Decimal is
		Flip : Decimal := -1.0;
		Pi : Decimal := 1.0;
	begin
		-- Compute arctan(1) according to the Leibniz series approximation.
		for K in 2 .. (N + 2) loop
			Pi := Pi + Flip / (2.0 * Decimal(K) - 1.0);
			Flip := -Flip;
		end loop;
		Pi := 4.0 * Pi;
		return Pi;
	end ComputePi;

	package Rounds_IO is new Modular_IO (Rounds); use Rounds_IO;
	package Decimal_IO is new Float_IO (Decimal); use Decimal_IO;

	N : Rounds;
	Pi : Decimal;
begin
	declare
		F : File_Type;
		File_Name : constant String := "rounds.txt";
	begin
		Open (F, In_File, File_Name);
		Get (F, N);  -- Read in number of rounds.
		Close (F);
	end;

	Pi := ComputePi (N);
	Put (Pi, Fore => 1, Aft => 16, Exp => 0);
end Leibniz;
