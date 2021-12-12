package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc21/coord"
	"github.com/asymmetricia/aoc21/term"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func step(tangle map[coord.Coord]int) int {
	var flashes int
	for c := range tangle {
		tangle[c]++
	}

	flashed := true
	for flashed {
		flashed = false
		for c, v := range tangle {
			if v > 9 {
				flashes++
				flashed = true
				tangle[c] = math.MinInt
				for _, nc := range c.Neighbors(true) {
					if _, ok := tangle[nc]; ok {
						tangle[nc]++
					}
				}
			}
		}
	}
	for c, v := range tangle {
		if v <= 0 {
			tangle[c] = 0
		}
	}
	return flashes
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	tangle := map[coord.Coord]int{}
	for y, line := range lines {
		for x, energy := range line {
			tangle[coord.C(x, y)] = int(energy - '0')
		}
	}

	for y := range lines {
		line := ""
		for x := range lines[y] {
			line += strconv.Itoa(tangle[coord.C(x, y)])
		}
		log.Print(0, " ", line)
	}

	var flashes int
	i := 0
	for flashes != 100 {
		flashes = step(tangle)
		log.Print(i+1, flashes)

		for y := range lines {
			line := ""
			for x := range lines[y] {
				v := tangle[coord.C(x, y)]
				if v == 0 {
					line += term.Scolor(255, 255, 255) +
						strconv.Itoa(v) +
						term.ScolorReset()
				} else {
					line += strconv.Itoa(v)
				}
			}
			log.Print(i+1, " ", line)
		}
		i++
	}
	fmt.Println(flashes)
}
