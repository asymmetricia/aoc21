package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

type memoKey struct {
	coord.Coord
	int
}

var memo = map[memoKey]bool{}

func compute(c coord.Coord, init map[coord.Coord]bool, algo string, iterations int) bool {
	if iterations == 0 {
		return init[c]
	}

	if r, ok := memo[memoKey{c, iterations}]; ok {
		return r
	}

	v := 0
	for _, dy := range []int{-1, 0, 1} {
		for _, dx := range []int{-1, 0, 1} {
			v = v << 1
			if compute(c.Plus(coord.C(dx, dy)), init, algo, iterations-1) {
				v |= 1
			}
		}
	}

	memo[memoKey{c, iterations}] = algo[v] == '#'
	return algo[v] == '#'
}

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	algo := lines[0]
	lines = lines[2:]

	lit := map[coord.Coord]bool{}
	for y, line := range lines {
		for x, bit := range line {
			if bit == '#' {
				lit[coord.C(x, y)] = true
			}
		}
	}

	out := map[coord.Coord]bool{}
	const iterations = 2
	for y := -iterations; y < len(lines)+iterations; y++ {
		fmt.Println(y)
		for x := -iterations; x < len(lines[0])+iterations; x++ {
			c := coord.C(x, y)
			if compute(c, lit, algo, iterations) {
				out[c] = true
			}
		}
	}

	fmt.Println(len(out))
}
