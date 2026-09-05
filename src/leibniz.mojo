from std.io.file import open
from std.ffi import external_call

def main() raises:
    var file = open("rounds.txt", "r")
    var rounds = Int(file.read().strip())
    var result = Float64(0)
    var sign = Float64(1)
    for i in range(rounds):
        result += sign / Float64(2 * i + 1)
        sign = -sign
    var fmt = String("%.16f\n")
    _ = external_call["printf", Int32, num_fixed_args=1](
        fmt.as_c_string_slice().unsafe_ptr(), result * 4
    )
