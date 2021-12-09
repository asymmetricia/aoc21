package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/coord"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type Point struct {
	Height int
	Basin  coord.Coord
	Min    bool
}

var seed = time.Now().Nanosecond()

func DrawFrame(width, height int, grid map[coord.Coord]*Point, mode string, anim *gif.GIF, delay int) {
	imgW := (width + 2) * aoc.GlyphWidth
	imgH := (height + 3) * aoc.LineHeight

	img := image.NewPaletted(
		image.Rect(0, 0, imgW, imgH),
		aoc.TolVibrant,
	)

	aoc.Typeset(img, image.Pt(0, 0), "\u250f", aoc.TolVibrantMagenta)
	aoc.Typeset(img, image.Pt(imgW-aoc.GlyphWidth, 0), "\u2513", aoc.TolVibrantMagenta)
	aoc.Typeset(img, image.Pt(imgW-aoc.GlyphWidth, imgH-aoc.LineHeight), "\u251b", aoc.TolVibrantMagenta)
	aoc.Typeset(img, image.Pt(0, imgH-aoc.LineHeight), "\u2517", aoc.TolVibrantMagenta)
	var hline string
	for i := 0; i < width; i++ {
		hline += "\u2501"
	}
	aoc.Typeset(img, image.Pt(aoc.GlyphWidth, 0), hline, aoc.TolVibrantMagenta)
	aoc.Typeset(img, image.Pt(aoc.GlyphWidth, imgH-aoc.LineHeight), hline, aoc.TolVibrantMagenta)

	var vline string
	for i := 0; i < height+1; i++ {
		vline += "\u2503\n"
	}

	aoc.Typeset(img, image.Pt(0, aoc.LineHeight), vline, aoc.TolVibrantMagenta)
	aoc.Typeset(img, image.Pt(imgW-aoc.GlyphWidth, aoc.LineHeight), vline, aoc.TolVibrantMagenta)

	status := "MODE: " + mode
	aoc.Typeset(
		img,
		image.Pt((imgW-len(status)*aoc.GlyphWidth)/2, imgH-aoc.LineHeight*2),
		status,
		aoc.TolVibrantOrange,
	)

	palette := aoc.TolVibrant[3:9]

	for c, pt := range grid {
		var col color.Color = aoc.TolVibrantGrey
		if pt.Basin != (coord.Coord{-1, -1}) {
			col = palette[(seed^pt.Basin.X^pt.Basin.Y)%len(palette)]
		}
		aoc.Typeset(
			img,
			image.Pt((c.X+1)*aoc.GlyphWidth,
				(c.Y+1)*aoc.LineHeight),
			strconv.Itoa(pt.Height),
			col,
		)
	}

	anim.Image = append(anim.Image, img)
	anim.Delay = append(anim.Delay, delay)
	anim.Disposal = append(anim.Disposal, gif.DisposalNone)
}

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	anim := &gif.GIF{}

	grid := map[coord.Coord]*Point{}
	for y, line := range lines {
		for x, pt := range []byte(line) {
			grid[coord.Coord{x, y}] = &Point{Height: int(pt - '0'), Basin: coord.Coord{-1, -1}}
		}
		DrawFrame(len(lines[0]), len(lines), grid, "LOADING", anim, 2)
	}

	var basins []coord.Coord
	for y, line := range lines {
	checks:
		for x := range line {
			c := coord.Coord{x, y}
			pt := grid[c]
			cards := []coord.Coord{c.North(), c.South(), c.East(), c.West()}
			for _, adj := range cards {
				if v, ok := grid[adj]; ok && v.Height <= pt.Height {
					continue checks
				}
			}
			grid[c].Min = true
			grid[c].Basin = c
			basins = append(basins, c)
			MarkBasin(c, grid)
			DrawFrame(len(lines[0]), len(lines), grid, "SCANNING", anim, 3)
		}
	}

	sort.Slice(basins, func(i, j int) bool {
		return BasinSize(basins[i], grid) < BasinSize(basins[j], grid)
	})

	for _, basin := range basins[:len(basins)-3] {
		ClearBasin(basin, grid)
		DrawFrame(len(lines[0]), len(lines), grid, "ANALYZING", anim, 2)
	}

	value := 1
	for _, basin := range basins[len(basins)-3:] {
		value *= BasinSize(basin, grid)
		DrawFrame(len(lines[0]), len(lines), grid, fmt.Sprintf("COMPUTING: %d", value), anim, 50)
	}

	DrawFrame(len(lines[0]), len(lines), grid, fmt.Sprintf("DONE: %d", value), anim, 200)

	aoc.Optimize(anim.Image)
	f, err := os.OpenFile("out.gif", os.O_TRUNC|os.O_CREATE|os.O_WRONLY, 0644)
	if err == nil {
		err = gif.EncodeAll(f, anim)
	}
	if err == nil {
		err = f.Sync()
	}
	if err == nil {
		err = f.Close()
	}
	if err != nil {
		panic(err)
	}
}

func ClearBasin(c coord.Coord, grid map[coord.Coord]*Point) {
	for _, pt := range grid {
		if pt.Basin == c {
			pt.Basin = coord.Coord{-1, -1}
		}
	}
}
func BasinSize(c coord.Coord, grid map[coord.Coord]*Point) int {
	var ret int
	for _, pt := range grid {
		if pt.Basin == c {
			ret++
		}
	}
	return ret
}

func MarkBasin(c coord.Coord, grid map[coord.Coord]*Point) {
	pt := grid[c]
	adjs := []coord.Coord{c.North(), c.South(), c.East(), c.West()}
	for _, adj := range adjs {
		g, ok := grid[adj]
		if !ok {
			continue
		}
		if g.Height == 9 {
			continue
		}
		if g.Height <= pt.Height {
			continue
		}
		g.Basin = pt.Basin
		MarkBasin(adj, grid)
	}
}
