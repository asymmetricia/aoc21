package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var horiz, depth int
	for _, line := range lines {
		words := strings.Split(line, " ")
		amt, _ := strconv.Atoi(words[1])
		switch words[0] {
		case "forward":
			horiz += amt
		case "up":
			depth -= amt
		case "down":
			depth += amt
		}
	}
	fmt.Println(horiz * depth)
}
