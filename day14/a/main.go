package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	template := lines[0]
	lines = lines[2:]
	fmt.Println(template, lines)
	var pairs = map[string]string{}
	for _, line := range lines {
		if line == "" {
			continue
		}
		before := aoc.Before(line, " -> ")
		after := aoc.After(line, " -> ")
		pairs[before] = after
	}

	for step := 0; step < 10; step++ {
		s := ""
		for i := 0; i < len(template)-1; i++ {
			if repl, ok := pairs[template[i:i+2]]; ok {
				s += string(template[i])
				s += repl
			} else {
				s += string(template[i])
			}
		}
		s += string(template[len(template)-1])
		fmt.Println(s)
		template = s
	}

	var freq = map[rune]int{}
	for _, c := range template {
		freq[c]++
	}
	var min, max rune
	for c, f := range freq {
		if min == 0 || f < freq[min] {
			min = c
		}
		if max == 0 || f > freq[max] {
			max = c
		}
	}
	fmt.Println(freq[max] - freq[min])
}
