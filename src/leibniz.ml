let calc rounds =
  let sum = ref 0. in
  let double = 2 * rounds in
  let curr = ref (1 - double) in
  let upto = double + 1 in
  while !curr < upto do
    sum := !sum +. 1. /. float_of_int !curr;
    curr := !curr + 4
  done;
  !sum

let () =
  let rounds_txt = open_in_bin "rounds.txt" in
  let rounds = int_of_string (input_line rounds_txt) in
  close_in rounds_txt;

  let pi = 4. *. calc rounds in
  Printf.printf "%.16f\n" pi

