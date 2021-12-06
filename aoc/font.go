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

var glyphs = map[rune]image.Image{}

func init() {
	glyphdata := bytes.Split(bytes.TrimSpace(fontData), []byte("\n\n"))
	for _, g := range glyphdata {
		rows := bytes.Split(g, []byte("\n"))
		r := rune(rows[0][0])
		rows = rows[1:]
		i := image.NewRGBA(image.Rect(0, 0, 8, 8))
		draw.Draw(i, i.Bounds(), image.Transparent, image.Point{}, draw.Src)
		for y, row := range rows {
			for x, pt := range row {
				switch pt {
				case '#':
					i.Set(x, y, color.White)
				}
			}
		}
		glyphs[r] = i
	}
}

func Typeset(img draw.Image, cursor image.Point, line string, color color.Color) {
	initX := cursor.X
	for _, g := range line {
		switch g {
		case '\n':
			cursor.Y += 8
			cursor.X = initX
		default:
			g, ok := glyphs[g]
			if ok {
				draw.DrawMask(
					img,
					image.Rectangle{cursor, cursor.Add(g.Bounds().Size())},
					image.NewUniform(color),
					image.Point{},
					g,
					image.Point{},
					draw.Over,
				)
			}
			cursor.X += 8
		}
	}
}
