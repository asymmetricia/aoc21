package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/coord"
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

	grid := map[coord.Coord]int{}
	for y, line := range lines {
		for x, pt := range []byte(line) {
			grid[coord.Coord{x, y}] = int(pt - '0')
		}
	}

	var mins []coord.Coord
checks:
	for c, val := range grid {
		cards := []coord.Coord{c.North(), c.South(), c.East(), c.West()}
		for _, adj := range cards {
			if v, ok := grid[adj]; ok && v <= val {
				continue checks
			}
		}
		fmt.Print(c, grid[c], " ")
		for _, card := range cards {
			if _, ok := grid[card]; ok {
				fmt.Print(grid[card], " ")
			}
		}
		fmt.Println()
		mins = append(mins, c)
	}

	var total int
	for _, v := range mins {
		total += grid[v] + 1
	}
	fmt.Println(total)
}
