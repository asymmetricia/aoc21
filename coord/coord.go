package coord

type Coord struct {
	X, Y int
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
