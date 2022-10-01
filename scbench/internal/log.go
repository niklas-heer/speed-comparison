package internal

import "fmt"

// colors
const (
	RED    = "\033[31m"
	GREEN  = "\033[32m"
	YELLOW = "\033[33m"
	PURPLE = "\033[35m"
	CYAN   = "\033[36m"
	RESET  = "\033[0m"
)

// NO_COLOR is a global variable that can be set to disable color output.
var NO_COLOR bool = false

// Log prints the given message in the given color.
func Log(color, message string) {
	if NO_COLOR {
		fmt.Println(message)
		return
	}

	switch color {
	case "red":
		fmt.Println(RED + message + RESET)
	case "green":
		fmt.Println(GREEN + message + RESET)
	case "yellow":
		fmt.Println(YELLOW + message + RESET)
	case "purple":
		fmt.Println(PURPLE + message + RESET)
	case "cyan":
		fmt.Println(CYAN + message + RESET)
	default:
		fmt.Println(message)
	}
}
