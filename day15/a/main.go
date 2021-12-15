package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

var grid = map[coord.Coord]int{}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	dimx := len(lines[0])
	dimy := len(lines)
	end := coord.C(dimx-1, dimy-1)
	for y, line := range lines {
		for x, risk := range line {
			grid[coord.C(x, y)] = int(risk - '0')
		}
	}

	path := aoc.AStarGrid(
		grid,
		coord.Coord{},
		end,
		func(_, to coord.Coord) int { return grid[to] },
		false,
	)

	risk := 0
	for _, c := range path[1:] {
		risk += grid[c]
	}
	fmt.Println(risk)
}
