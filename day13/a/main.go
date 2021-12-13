package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
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

	var grid = map[coord.Coord]bool{}
	type instruction struct {
		isHoriz bool
		val     int
	}
	var strux []instruction
	var mode int
	for _, line := range lines {
		if line == "" {
			mode++
			continue
		}
		if mode == 0 {
			grid[coord.MustFromComma(line)] = true
		} else {
			i, err := strconv.Atoi(aoc.After(line, "="))
			if err != nil {
				panic(err)
			}
			strux = append(strux, instruction{isHoriz: strings.Contains(line, "y="), val: i})
		}
	}

	fmt.Println(len(grid))
	for i, struc := range strux {
		log.Println(struc)
		after := map[coord.Coord]bool{}
		if struc.isHoriz {
			for c := range grid {
				if c.Y < struc.val {
					after[c] = true
				} else {
					after[coord.C(c.X, 2*struc.val-c.Y)] = true
				}
			}
		} else {
			for c := range grid {
				if c.X < struc.val {
					after[c] = true
				} else {
					after[coord.C(2*struc.val-c.X, c.Y)] = true
				}
			}
		}
		grid = after
		log.Printf("%d: %d", i, len(grid))
		break
	}
}
