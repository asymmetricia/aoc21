package coord

import (
	"fmt"
	"strconv"
	"strings"
)

type Coord struct {
	X, Y int
}

func MustFromComma(xy string) Coord {
	c, e := FromComma(xy)
	if e != nil {
		panic(e)
	}
	return c
}

func FromComma(xy string) (Coord, error) {
	parts := strings.Split(strings.TrimSpace(xy), ",")
	if len(parts) != 2 {
		return Coord{}, fmt.Errorf("expected two ,-separated parts, got %d", len(parts))
	}
	var ret Coord
	var err error
	ret.X, err = strconv.Atoi(parts[0])
	if err != nil {
		return Coord{}, fmt.Errorf("bad X coordinate %q: %w", parts[0], err)
	}
	ret.Y, err = strconv.Atoi(parts[1])
	if err != nil {
		return Coord{}, fmt.Errorf("bad Y coordinate %q: %w", parts[1], err)
	}
	return ret, nil
}

func (c Coord) East() Coord {
	return Coord{c.X + 1, c.Y}
}
func (c Coord) West() Coord {
	return Coord{c.X - 1, c.Y}
}
func (c Coord) NorthEast() Coord {
	return Coord{c.X + 1, c.Y + 1}
}
func (c Coord) SouthEast() Coord {
	return Coord{c.X + 1, c.Y - 1}
}
func (c Coord) NorthWest() Coord {
	return Coord{c.X - 1, c.Y + 1}
}
func (c Coord) SouthWest() Coord {
	return Coord{c.X - 1, c.Y - 1}
}

func (c Coord) Execute(steps []string) Coord {
	for _, step := range steps {
		switch step {
		case "e":
			c = c.East()
		case "w":
			c = c.West()
		case "ne":
			c = c.NorthEast()
		case "nw":
			c = c.NorthWest()
		case "se":
			c = c.SouthEast()
		case "sw":
			c = c.SouthWest()
		default:
			panic(step)
		}
	}
	return c
}

func (c Coord) Plus(a Coord) Coord {
	return Coord{c.X + a.X, c.Y + a.Y}
}
func (c Coord) Equal(a Coord) bool {
	return c.X == a.X && c.Y == a.Y
}
