package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	points := aoc.MapGrid{}
	for _, line := range lines {
		from := coord.MustFromComma(aoc.Before(line, "->"))
		to := coord.MustFromComma(aoc.After(line, "->"))
		if from.X > to.X ||
			(from.X == to.X && from.Y > to.Y) {
			from, to = to, from
		}
		if from.X != to.X && from.Y != to.Y {
			continue // ignore non-hv lines
		}
		for x := from.X; x <= to.X; x++ {
			for y := from.Y; y <= to.Y; y++ {
				points.Inc(coord.Coord{x, y})
			}
		}
	}

	var count int
	for _, row := range points {
		for _, cell := range row {
			if cell > 1 {
				count++
			}
		}
	}
	fmt.Println(count)
}
