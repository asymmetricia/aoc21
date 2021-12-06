package aoc

import "image/color"

// TolVibrant is Paul Tol's Vibrant qualitative color palette that should be
// color-blind accessible; see https://personal.sron.nl/~pault/
var TolVibrant = color.Palette{
	color.Black,
	color.Transparent,
	color.White,
	TolVibrantBlue,
	TolVibrantCyan,
	TolVibrantTeal,
	TolVibrantOrange,
	TolVibrantRed,
	TolVibrantMagenta,
	TolVibrantGrey,
}

var (
	TolVibrantBlue    = color.RGBA{0, 119, 187, 255}
	TolVibrantCyan    = color.RGBA{51, 187, 238, 255}
	TolVibrantTeal    = color.RGBA{0, 153, 136, 255}
	TolVibrantOrange  = color.RGBA{238, 119, 51, 255}
	TolVibrantRed     = color.RGBA{204, 51, 17, 255}
	TolVibrantMagenta = color.RGBA{238, 51, 119, 255}
	TolVibrantGrey    = color.RGBA{187, 187, 187, 255}
)
