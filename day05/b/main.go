package main

import (
	"fmt"
	"image"
	"image/color"
	gif2 "image/gif"
	"io/ioutil"
	"math"
	"os"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func min(ints ...int) int {
	var ret *int
	for _, i := range ints {
		if ret == nil {
			ret = new(int)
			*ret = i
		} else if i < *ret {
			*ret = i
		}
	}
	return *ret
}

func max(ints ...int) int {
	var ret *int
	for _, i := range ints {
		if ret == nil {
			ret = new(int)
			*ret = i
		} else if i > *ret {
			*ret = i
		}
	}
	return *ret
}
func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	rect := image.Rectangle{
		Min: image.Point{math.MaxInt, math.MaxInt},
		Max: image.Point{math.MinInt, math.MinInt},
	}

	for _, line := range lines {
		from := coord.MustFromComma(aoc.Before(line, "->"))
		to := coord.MustFromComma(aoc.After(line, "->"))
		rect.Min.X = min(rect.Min.X, from.X, to.X)
		rect.Max.X = max(rect.Max.X, from.X, to.X)
		rect.Min.Y = min(rect.Min.Y, from.Y, to.Y)
		rect.Max.Y = max(rect.Max.Y, from.Y, to.Y)
	}

	palette := []color.Color{
		color.Transparent,
		color.Black,
		color.RGBA{0x33, 0x33, 0x33, 0xFF},
	}
	cmap := map[int]uint8{
		1: 2,
	}
	for i := uint8(2); i < 8; i++ {
		palette = append(palette, color.RGBA{
			0x33 * (i - 2),
			0xFF,
			0xFF,
			0xFF,
		})
		cmap[int(i)] = i + 1
	}

	gif := &gif2.GIF{
		Image: []*image.Paletted{},
	}

	points := aoc.MapGrid{}
	for i, line := range lines {
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

		if i%5 == 0 {
			gif.Image = append(gif.Image, points.Draw(rect, color.Black, cmap, palette))
			gif.Delay = append(gif.Delay, 1)
			gif.Disposal = append(gif.Disposal, gif2.DisposalNone)
			fmt.Println(i, "/", len(lines))
		}
	}

	aoc.Optimize(gif.Image)

	out, err := os.OpenFile("anim.gif", os.O_TRUNC|os.O_CREATE|os.O_WRONLY, 0644)
	if err == nil {
		err = gif2.EncodeAll(out, gif)
	}
	if err == nil {
		err = out.Sync()
	}
	if err == nil {
		err = out.Close()
	}
	if err != nil {
		log.WithError(err).Fatal()
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
