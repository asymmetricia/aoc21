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

		var slope coord.Coord
		if from.X < to.X {
			slope.X = 1
		} else if from.X > to.X {
			slope.X = -1
		}
		if from.Y < to.Y {
			slope.Y = 1
		} else if from.Y > to.Y {
			slope.Y = -1
		}

		pt := from
		for {
			points.Inc(pt)
			if pt.Equal(to) {
				break
			}
			pt = pt.Plus(slope)
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
