package aoc

import (
	"image"
	"image/color"
)

func Optimize(imgs []*image.Paletted) {
	if len(imgs) < 2 {
		return
	}
	accum := image.NewPaletted(imgs[0].Rect, imgs[0].Palette)
	copy(accum.Pix, imgs[0].Pix)

	tr := imgs[0].Palette.Index(color.Transparent)
	for _, img := range imgs[1:] {
		for i, v := range img.Pix {
			if v == accum.Pix[i] {
				img.Pix[i] = uint8(tr)
			} else {
				accum.Pix[i] = img.Pix[i]
			}
		}
	}
}
