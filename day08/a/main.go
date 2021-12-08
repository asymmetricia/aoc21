package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	count := 0

	for _, line := range lines {
		parts := strings.Split(line, "|")
		digits := strings.Split(strings.TrimSpace(parts[1]), " ")
		for _, digit := range digits {
			switch len(digit) {
			case 2: count++ // one
			case 3: count++ // seven
			case 4: count++ // four
			case 7: count++ // eight
			}
		}
	}
	fmt.Println(count)
}
