package coord

import (
	"fmt"
	"math"
	"strings"
)

type SparseWorld map[Coord]rune

func (w SparseWorld) Print() {
	minx, maxx, miny, maxy := math.MaxInt, math.MinInt, math.MaxInt, math.MinInt
	for c := range w {
		if c.X < minx {
			minx = c.X
		}
		if c.X > maxx {
			maxx = c.X
		}
		if c.Y < miny {
			miny = c.Y
		}
		if c.Y > maxy {
			maxy = c.Y
		}
	}

	for y := miny; y <= maxy; y++ {
		sb := strings.Builder{}
		for x := minx; x <= maxx; x++ {
			if ch, ok := w[C(x, y)]; ok {
				sb.WriteRune(ch)
			} else {
				sb.WriteRune(' ')
			}
		}
		fmt.Println(sb.String())
	}
}

func (w SparseWorld) At(coord Coord) rune {
	if r, ok := w[coord]; !ok {
		return -1
	} else {
		return r
	}
}

func (w SparseWorld) Set(coord Coord, r rune) {
	w[coord] = r
}

type DenseWorld [][]rune

func (d DenseWorld) Print() {
	sb := &strings.Builder{}
	for _, row := range d {
		sb.Reset()
		for _, cell := range row {
			sb.WriteRune(cell)
		}
		fmt.Println(sb.String())
	}
}

func (d DenseWorld) At(coord Coord) rune {
	if len(d) <= coord.Y {
		return -1
	}
	if len(d[coord.Y]) <= coord.X {
		return -1
	}
	return d[coord.Y][coord.X]
}

func (d *DenseWorld) Set(coord Coord, r rune) {
	if len(*d) <= coord.Y {
		*d = append(*d, make([][]rune, len(*d)-coord.Y+1)...)
	}
	if len((*d)[coord.Y]) <= coord.X {
		(*d)[coord.Y] = append((*d)[coord.Y], make([]rune, len((*d)[coord.Y])-coord.X+1)...)
	}
	(*d)[coord.Y][coord.X] = r
}

type World interface {
	Print()
	At(Coord) rune
	Set(Coord, rune)
}

var _ World = (*SparseWorld)(nil)
var _ World = (*DenseWorld)(nil)

func Load(w World, lines []string) {
	for y, row := range lines {
		for x, char := range row {
			w.Set(C(x, y), char)
		}
	}
}
