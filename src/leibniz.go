package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	file, err := ioutil.ReadFile("rounds.txt")
	if err != nil {
		fmt.Print(err)
	}

	rounds, _ := strconv.Atoi(strings.TrimSpace(string(file)))

	x := 1.0
	pi := 1.0

	rounds += 2 // do this outside the loop

	for i := 2; i < rounds; i++ {
		x *= -1
		pi += x / float64(2*i-1)
	}

	pi *= 4
	fmt.Println(pi)
}
