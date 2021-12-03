package main

import (
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

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var bits = map[int]int{}
	for _, line := range lines {
		for i, c := range line {
			if c == '1' {
				bits[i]++
			}
		}
	}
	width := len(lines[0])
	var gamma, epsilon int
	for i := 0; i < width; i++ {
		if bits[i] > len(lines)/2 {
			gamma |= 1 << (width - i - 1)
		} else {
			epsilon |= 1 << (width - i - 1)
		}
	}
	fmt.Println(gamma, epsilon, gamma * epsilon)
}
