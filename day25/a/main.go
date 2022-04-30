package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/asymmetricia/aoc21/term"
	"io/ioutil"
	"strings"

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
	input = bytes.Replace(input, []byte("\r"), []byte(""), -1)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var w coord.World = &coord.DenseWorld{}
	coord.Load(w, lines)
	w.Print()
	fmt.Println()

	changed := true
	_, _, maxx, maxy := w.Rect()
	var steps int
	for changed {
		steps++
		changed = false
		nextWorld := w.Copy()
		w.Each(func(c coord.Coord) bool {
			if w.At(c) == '>' {
				dest := c.East()
				if dest.X > maxx {
					dest.X = 0
				}
				if w.At(dest) == '.' {
					nextWorld.Set(c, '.')
					nextWorld.Set(dest, '>')
					changed = true
				}
			}
			return false
		})

		w = nextWorld
		nextWorld = w.Copy()
		w.Each(func(c coord.Coord) (stop bool) {
			if w.At(c) == 'v' {
				dest := c.South()
				if dest.Y > maxy {
					dest.Y = 0
				}
				if w.At(dest) == '.' {
					nextWorld.Set(c, '.')
					nextWorld.Set(dest, 'v')
					changed = true
				}
			}
			return false
		})

		w = nextWorld
		term.Clear()
		term.MoveCursor(1, 1)
		w.Print()
		fmt.Println()
	}
	log.Print(steps)
}
