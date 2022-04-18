package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"io/ioutil"
	"math"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

var hallway = map[int]bool{
	1: true, 2: true,
	4:  true,
	6:  true,
	8:  true,
	10: true, 11: true,
}

var homes = map[rune]int{
	'A': 3,
	'B': 5,
	'C': 7,
	'D': 9,
}

func legalMoves(w world, from coord.Coord) []coord.Coord {
	fm := w.At(from)
	home, ok := homes[fm]

	// not movable
	if !ok {
		return nil
	}

	// we're locked in at home (south)
	if from.X == home && from.Y == 3 {
		return nil
	}

	// we're locked in at home (north)
	if from.X == home && from.Y == 2 && w.At(from.South()) == fm {
		return nil
	}

	// in a room and can't get out
	if from.Y == 3 && w.At(from.North()) != '.' {
		return nil
	}

	// in the hallway, can only go to locked position
	if from.Y == 1 && w.At(coord.C(home, 2)) == '.' {
		switch w.At(coord.C(home, 3)) {
		case '.':
			break
		case fm:
			break
		default:
			return nil
		}
		if from.X < home {
			for x := from.X + 2; x <= home; x += 2 {
				if w.At(coord.C(x, 1)) != '.' {
					// way home is blocked
					return nil
				}
			}
		} else {
			for x := from.X - 2; x >= home; x -= 2 {
				if w.At(coord.C(x, 1)) != '.' {
					// way home is blocked
					return nil
				}
			}
		}
		if w.At(coord.C(home, 3)) == '.' {
			return []coord.Coord{{home, 3}}
		}
		return []coord.Coord{{home, 2}}
	}

	var ret []coord.Coord
	for x := from.X; x >= 1; x-- {
		if !hallway[x] {
			continue
		}
		if w[1][x] != '.' {
			break
		}
		ret = append(ret, coord.C(x, 1))
	}
	for x := from.X; x <= 11; x++ {
		if !hallway[x] {
			continue
		}
		if w[1][x] != '.' {
			break
		}
		ret = append(ret, coord.C(x, 1))
	}
	return ret
}

func move(w world, from, to coord.Coord) int {
	cost := map[rune]int{
		'A': 1,
		'B': 10,
		'C': 100,
		'D': 1000,
	}[w.At(from)]

	if w.At(to) != '.' {
		log.Fatalf("cannot move to a %c at %s", w.At(to), to)
	}

	if cost == 0 {
		w.Print()
		log.Fatalf("cannot move a %c from %s", w.At(from), from)
	}

	w.Set(to, w.At(from))
	w.Set(from, '.')
	// taxi geometry!
	return aoc.Abs(from.X-to.X)*cost + aoc.Abs(from.Y-to.Y)*cost
}

func Complete(w world) bool {
	for c, x := range homes {
		if w.At(coord.C(x, 2)) != c ||
			w.At(coord.C(x, 3)) != c {
			return false
		}
	}
	return true
}

func (w world) Set(c coord.Coord, r rune) {
	if len(w) <= c.Y {
		panic("set out of bounds Y")
	}
	if len(w[c.Y]) <= c.X {
		panic("set out of bounds X")
	}
	w[c.Y][c.X] = r
}

func (w world) At(c coord.Coord) rune {
	if len(w) <= c.Y {
		return ' '
	}
	if len(w[c.Y]) <= c.X {
		return ' '
	}
	return w[c.Y][c.X]
}

var SolutionCache = map[string]struct {
	r [][2]coord.Coord
	c int
}{}

func Solution(w world) ([][2]coord.Coord, int) {
	ck := w.String()
	if c, ok := SolutionCache[ck]; ok {
		return c.r, c.c
	}

	var bestRoute [][2]coord.Coord
	var bestCost int = math.MaxInt

	if Complete(w) {
		return nil, 0
	}

	for y := 1; y <= 3; y++ {
		for x := 1; x <= 11; x++ {
			from := coord.C(x, y)
			for _, to := range legalMoves(w, coord.C(x, y)) {
				moveCost := move(w, from, to)
				solnRoute, solnCost := Solution(w)
				move(w, to, from)

				if solnCost != math.MaxInt && moveCost+solnCost < bestCost {
					if len(solnRoute) > 0 && solnRoute[len(solnRoute)-1] == [2]coord.Coord{from, to} {
						panic("double move")
					}
					bestRoute = append(solnRoute, [2]coord.Coord{from, to})
					bestCost = moveCost + solnCost
					//return bestRoute, bestCost
				}
			}
		}
	}

	SolutionCache[ck] = struct {
		r [][2]coord.Coord
		c int
	}{r: make([][2]coord.Coord, len(bestRoute)), c: bestCost}
	copy(SolutionCache[ck].r, bestRoute)
	return bestRoute, bestCost
}

type world [][]rune

func (w world) String() string {
	var sb = new(strings.Builder)
	for _, line := range w {
		sb.WriteString(string(line))
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (w world) Print() {
	fmt.Print(w.String())
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	var w world
	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		w = append(w, []rune(line))
	}

	r, c := Solution(w)
	fmt.Println(r)
	w.Print()
	for i := len(r) - 1; i >= 0; i-- {
		m := r[i]
		move(w, m[0], m[1])
		w.Print()
	}
	fmt.Println("cost=", c)
}
