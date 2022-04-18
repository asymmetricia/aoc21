package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"math"
	"math/rand"
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

	if from.X == home {
		locked := true
		for y := len(w) - 2; y > from.Y; y-- {
			if w.At(coord.C(home, y)) != fm {
				locked = false
				break
			}
		}
		if locked {
			return nil
		}

		for y := from.Y + 1; y < 1; y-- {
			if w.At(coord.C(home, y)) != '.' {
				return nil
			}
		}
	}

	// in the hallway, can only go to locked position
	if from.Y == 1 {
		// we can only go home if home is empty or stacked with housemates
		best := 2
		for y := 2; y < len(w)-1; y++ {
			if r := w.At(coord.C(home, y)); r != '.' && r != fm {
				return nil
			} else if r == '.' {
				best = y
			}
		}

		if from.X < home {
			for x := from.X + 1; x <= home; x++ {
				if w.At(coord.C(x, 1)) != '.' {
					// way home is blocked
					return nil
				}
			}
		} else {
			for x := from.X - 1; x >= home; x-- {
				if w.At(coord.C(x, 1)) != '.' {
					// way home is blocked
					return nil
				}
			}
		}
		return []coord.Coord{{home, best}}
	}

	for y := from.Y - 1; y > 1; y-- {
		if w.At(coord.C(from.X, y)) != '.' {
			return nil
		}
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
		for y := 2; y < len(w)-1; y++ {
			if w.At(coord.C(x, y)) != c {
				return false
			}
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
	var bestCost = math.MaxInt

	if Complete(w) {
		return nil, 0
	}

	for y := 1; y < len(w)-1; y++ {
		for x := 1; x <= 11; x++ {
			from := coord.C(x, y)
			for _, to := range legalMoves(w, coord.C(x, y)) {
				moveCost := move(w, from, to)
				solutionRoute, solutionCost := Solution(w)
				move(w, to, from)

				if solutionCost != math.MaxInt && moveCost+solutionCost < bestCost {
					if len(solutionRoute) > 0 && solutionRoute[len(solutionRoute)-1] == [2]coord.Coord{from, to} {
						panic("double move")
					}
					bestRoute = append(solutionRoute, [2]coord.Coord{from, to})
					bestCost = moveCost + solutionCost
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

func (w world) Image() *image.Paletted {
	return w.ImageSteps(nil, color.Black)
}

func (w world) ImageSteps(steps []coord.Coord, stepColor color.Color) *image.Paletted {
	const scale = 4
	img := image.NewPaletted(
		image.Rect(0, 0, aoc.GlyphWidth*len(w[0])*scale, aoc.LineHeight*len(w)*scale),
		aoc.TolVibrant,
	)

	stepsMap := map[coord.Coord]bool{}
	for _, s := range steps {
		stepsMap[s] = true
	}

	for y, row := range w {
		for x, c := range row {
			var col color.Color = aoc.TolVibrantGrey
			switch c {
			case 'A':
				col = aoc.TolVibrantBlue
			case 'B':
				col = aoc.TolVibrantRed
			case 'C':
				col = aoc.TolVibrantCyan
			case 'D':
				col = aoc.TolVibrantMagenta
			case '.':
				if stepsMap[coord.C(x, y)] {
					col = stepColor
				}
			}
			aoc.Typeset(
				img,
				image.Pt(x*aoc.GlyphWidth*scale, y*aoc.LineHeight*scale),
				string(c),
				col,
				aoc.TypesetOpts{Scale: scale},
			)
		}
	}
	return img
}

func moves(from, to coord.Coord) []coord.Coord {
	var steps = []coord.Coord{from}
	for from != to {
		switch {
		case from.Y > to.Y:
			from = from.North()
		case from.X < to.X:
			from = from.East()
		case from.X > to.X:
			from = from.West()
		case from.Y < to.Y:
			from = from.South()
		}
		steps = append(steps, from)
	}
	return steps
}

func drawMove(w world, from, to coord.Coord) []*image.Paletted {
	var ret []*image.Paletted
	steps := moves(from, to)
	for _, next := range steps[1:] {
		move(w, from, next)
		from = next
		ret = append(ret, w.ImageSteps(steps, aoc.TolVibrantTeal))
	}
	return ret
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
	if len(r) == 0 {
		panic("no solution?")
	}
	fmt.Println(r)

	anim := &gif.GIF{
		Image:    []*image.Paletted{w.Image()},
		Delay:    []int{15},
		Disposal: []byte{gif.DisposalNone},
	}

	for i := len(r) - 1; i >= 0; i-- {
		for y, row := range w {
			for x := range row {
				from := coord.C(x, y)
				legalMoves := legalMoves(w, from)
				rand.Shuffle(len(legalMoves), func(i, j int) { legalMoves[i], legalMoves[j] = legalMoves[j], legalMoves[i] })
				for _, to := range legalMoves {
					anim.Image = append(anim.Image, w.ImageSteps(moves(from, to), aoc.TolVibrantOrange))
					anim.Delay = append(anim.Delay, 1)
					anim.Disposal = append(anim.Disposal, gif.DisposalNone)
				}
			}
		}
		for _, moveImage := range drawMove(w, r[i][0], r[i][1]) {
			anim.Image = append(anim.Image, moveImage)
			anim.Delay = append(anim.Delay, 1)
			anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		}
	}

	anim.Image = append(anim.Image, w.Image())
	anim.Delay = append(anim.Delay, 300)
	anim.Disposal = append(anim.Disposal, gif.DisposalNone)

	aoc.Optimize(anim.Image)
	aoc.SaveGIF(anim, "day23b.gif")
	fmt.Println("cost=", c)
}
