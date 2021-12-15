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
	for y, line := range lines {
		for x, risk := range line {
			grid[coord.C(x, y)] = int(risk - '0')
		}
	}
	for x := 0; x < 5*dimx; x++ {
		for y := 0; y < 5*dimy; y++ {
			grid[coord.C(x, y)] = (grid[coord.C(x%dimx, y%dimy)] + x/dimx + y/dimy)
			for grid[coord.C(x, y)] > 9 {
				grid[coord.C(x, y)] -= 9
			}
		}
	}
	dimx *= 5
	dimy *= 5
	end := coord.C(dimx-1, dimy-1)

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
