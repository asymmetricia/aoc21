package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"
	"time"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func countFreq(s string) map[string]int {
	ret := map[string]int{}
	for _, c := range s {
		ret[string(c)]++
	}
	return ret
}

func sum(summands ...map[string]int) map[string]int {
	ret := map[string]int{}
	for _, summand := range summands {
		for c, v := range summand {
			ret[c] += v
		}
	}
	return ret
}

var memo = map[struct {
	string
	int
}]map[string]int{}

func process(s string, steps int, pairs map[string]string) (inserts map[string]int) {
	if memo, ok := memo[struct {
		string
		int
	}{string: s, int: steps}]; ok {
		return memo
	}

	pair, ok := pairs[s]
	if !ok {
		panic(pair)
	}
	if steps == 1 {
		return map[string]int{pair: 1}
	}

	a := string(s[0]) + pair
	b := pair + string(s[1])
	indent := ""
	for i := 10 - steps; i > 0; i-- {
		indent += " "
	}
	ret := sum(
		map[string]int{pair: 1},
		process(a, steps-1, pairs),
		process(b, steps-1, pairs))
	memo[struct {
		string
		int
	}{string: s, int: steps}] = ret
	return ret
}

func main() {
	start := time.Now()
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

	freq := countFreq(template)
	for i := 0; i < len(template)-1; i++ {
		inserts := process(template[i:i+2], 40, pairs)
		fmt.Println(template[i:i+2], "->", inserts)
		freq = sum(freq, inserts)
	}
	fmt.Println(freq)
	var min, max string
	for c, f := range freq {
		if min == "" || f < freq[min] {
			min = c
		}
		if max == "" || f > freq[max] {
			max = c
		}
	}
	fmt.Println(freq[max] - freq[min])
	fmt.Println(time.Since(start).Seconds())
}
