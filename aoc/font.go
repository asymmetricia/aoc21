package aoc

import (
	"bytes"
	_ "embed"
	"image"
	"image/color"
	"image/draw"
)

//go:embed "font.txt"
var fontData []byte

const LineHeight = 12

var Glyphs = map[rune]image.Image{}

func init() {
	glyphdata := bytes.Split(bytes.TrimSpace(fontData), []byte("\n\n"))
	for _, g := range glyphdata {
		rows := bytes.Split(g, []byte("\n"))
		r := rune(rows[0][0])
		rows = rows[1:]
		i := image.NewRGBA(image.Rect(0, 0, len(rows[0]), len(rows)))
		draw.Draw(i, i.Bounds(), image.Transparent, image.Point{}, draw.Src)
		for y, row := range rows {
			for x, pt := range row {
				switch pt {
				case '#':
					i.Set(x, y, color.White)
				}
			}
		}
		Glyphs[r] = i
	}
}

type TypesetOpts struct {
	Scale int
}

func Typeset(img draw.Image, cursor image.Point, line string, color color.Color, opts ...TypesetOpts) {
	if len(opts) == 0 {
		opts = []TypesetOpts{{}}
	}

	scale := 1
	if opts[0].Scale != 0 {
		scale = opts[0].Scale
	}

	initX := cursor.X
	for _, g := range line {
		switch g {
		case '\n':
			cursor.Y += LineHeight * scale
			cursor.X = initX
		default:
			g, ok := Glyphs[g]
			if ok {
				for x := 0; x < g.Bounds().Size().X*scale; x++ {
					for y := 0; y < g.Bounds().Size().Y*scale; y++ {
						c := g.At(x/scale, y/scale)
						_, _, _, a := c.RGBA()
						if a > 0 {
							img.Set(cursor.X+x, cursor.Y+y, color)
						}
					}
				}
			}
			cursor.X += 8 * scale
		}
	}
}
