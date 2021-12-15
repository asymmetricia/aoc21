package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/gif"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

var grid = map[coord.Coord]int{}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	dimx := len(lines[0])
	dimy := len(lines)
	for y, line := range lines {
		for x, risk := range line {
			grid[coord.C(x, y)] = int(risk - '0')
		}
	}
	for x := 0; x < 5*dimx; x++ {
		for y := 0; y < 5*dimy; y++ {
			grid[coord.C(x, y)] = (grid[coord.C(x%dimx, y%dimy)] + x/dimx + y/dimy)
			for grid[coord.C(x, y)] > 9 {
				grid[coord.C(x, y)] -= 9
			}
		}
	}
	dimx *= 5
	dimy *= 5
	end := coord.C(dimx-1, dimy-1)

	anim := &gif.GIF{}

	const scale = 2
	frame := 0

	path := aoc.AStarGrid(
		grid,
		coord.Coord{},
		end,
		func(_, to coord.Coord) int { return grid[to] },
		func(from coord.Coord) int { return end.X + end.Y - from.X - from.Y },
		false,
		func(
			openSet map[coord.Coord]bool,
			cameFrom map[coord.Coord]coord.Coord,
			gScore map[coord.Coord]int,
			fScore map[coord.Coord]int,
			current coord.Coord) {
			frame++
			if current != end && (frame-1)%1000 != 0 {
				return
			}
			img := image.NewPaletted(image.Rect(0, 0, dimx*scale, dimy*scale), aoc.TolVibrant)
			var best coord.Coord
			for y := 0; y < dimy; y++ {
				for x := 0; x < dimx; x++ {
					c := coord.C(x, y)
					var col color.Color = aoc.TolVibrantGrey
					if _, ok := openSet[c]; ok {
						col = aoc.TolVibrantMagenta
						if end.X+end.Y-c.X-c.Y < end.X+end.Y-best.X-best.Y {
							best = c
						}
					} else if _, ok := gScore[c]; ok {
						col = color.White
					}
					draw.Draw(img, image.Rect(x*scale, y*scale, x*scale+scale, y*scale+scale), image.NewUniform(col), image.Pt(0, 0), draw.Src)
				}
			}
			fmt.Println(current, best)
			for c := best; c != coord.C(0, 0); c = cameFrom[c] {
				draw.Draw(img, image.Rect(c.X*scale, c.Y*scale, c.X*scale+scale, c.Y*scale+scale), image.NewUniform(aoc.TolVibrantCyan), image.Pt(0, 0), draw.Src)
			}
			anim.Image = append(anim.Image, img)
			anim.Delay = append(anim.Delay, 2)
			anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		},
	)
	risk := 0
	for _, c := range path[1:] {
		risk += grid[c]
	}
	fmt.Println(risk)

	anim.Delay[len(anim.Delay)-1] = 200
	aoc.SaveGIF(anim, "out.gif")
}
