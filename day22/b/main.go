package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/aoc"
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"math"
	"regexp"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type Cuboid struct {
	Ax, Bx, Ay, By, Az, Bz int
}

func (c Cuboid) Count() int {
	return (c.Bx - c.Ax + 1) *
		(c.By - c.Ay + 1) *
		(c.Bz - c.Az + 1)
}

func (c Cuboid) String() string {
	return fmt.Sprintf("[(%d,%d),(%d,%d),(%d,%d)] == %d",
		c.Ax, c.Bx,
		c.Ay, c.By,
		c.Az, c.Bz,
		c.Count(),
	)
}

func (c Cuboid) Overlap(b Cuboid) bool {
	if c.Ax > b.Bx || c.Bx < b.Ax ||
		c.Ay > b.By || c.By < b.Ay ||
		c.Az > b.Bz || c.Bz < b.Az {
		return false
	}
	return true
}

func (c Cuboid) Plus(b Cuboid) []Cuboid {
	if !c.Overlap(b) {
		return []Cuboid{c, b}
	}

	if c.Contains(b) {
		return []Cuboid{c}
	}

	if b.Contains(c) {
		return []Cuboid{b}
	}

	return append([]Cuboid{c}, b.Subtract(c)...)
}

func (c Cuboid) Contains(b Cuboid) bool {
	return c.Ax <= b.Ax && c.Bx >= b.Bx &&
		c.Ay <= b.Ay && c.By >= b.By &&
		c.Az <= b.Az && c.Bz >= b.Bz
}

func (c Cuboid) Subtract(b Cuboid) []Cuboid {
	if !c.Overlap(b) {
		return []Cuboid{c}
	}

	b.Ax = aoc.Max(b.Ax, c.Ax)
	b.Bx = aoc.Min(b.Bx, c.Bx)
	b.Ay = aoc.Max(b.Ay, c.Ay)
	b.By = aoc.Min(b.By, c.By)
	b.Az = aoc.Max(b.Az, c.Az)
	b.Bz = aoc.Min(b.Bz, c.Bz)

	var ret []Cuboid

	// left cuboid, [cax,bax), [cay,cby], [caz,cbz]
	if c.Ax < b.Ax {
		ret = append(ret, Cuboid{c.Ax, b.Ax - 1, c.Ay, c.By, c.Az, c.Bz})
	}

	// front and back are "small"     --------  <- top, big
	// top and bottom are "big"       |      |  <- front, small
	//                                --------  <- bottom, big

	// back cuboid, [bax,bbx], (bby, cby], [baz, bbz]
	if b.By < c.By {
		ret = append(ret, Cuboid{b.Ax, b.Bx, b.By + 1, c.By, b.Az, b.Bz})
	}

	// front cuboid, [bax,bbx], [cay, bay), [baz, bbz]

	if c.Ay < b.Ay {
		ret = append(ret, Cuboid{b.Ax, b.Bx, c.Ay, b.Ay - 1, b.Az, b.Bz})
	}

	// top cuboid, [bax, bbx], [cay, cby], (bbz, cbz]
	if b.Bz < c.Bz {
		ret = append(ret, Cuboid{b.Ax, b.Bx, c.Ay, c.By, b.Bz + 1, c.Bz})
	}

	// bottom cuboid, [bax, bbx], [cay, cby], [caz, baz)
	if c.Az < b.Az {
		ret = append(ret, Cuboid{b.Ax, b.Bx, c.Ay, c.By, c.Az, b.Az - 1})
	}

	// right cuboid, (bbx, cbx], [cay, cby], [caz, cbz]
	if b.Bx < c.Bx {
		ret = append(ret, Cuboid{b.Bx + 1, c.Bx, c.Ay, c.By, c.Az, c.Bz})
	}

	return ret
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var cuboids []Cuboid

	re := regexp.MustCompile(`(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)`)

	var min, max int = math.MaxInt, math.MinInt
	for _, line := range lines {
		m := re.FindStringSubmatch(line)
		ax := aoc.MustAtoi(m[2])
		bx := aoc.MustAtoi(m[3])
		ay := aoc.MustAtoi(m[4])
		by := aoc.MustAtoi(m[5])
		min = aoc.Min(min, ax, bx, ay, by)
		max = aoc.Max(max, ax, bx, ay, by)
	}
	dim := max - min

	const res = 2048
	scale := dim / (res - 1)
	log.Print(dim, scale)

	//bg := image.NewPaletted(image.Rect(0, 1024, 0, 1024), aoc.TolSequentialSmoothRainbow)
	//anim := &gif.GIF{Image: []*image.Paletted{bg}}

	var frames [][][]int

	var maxVal int
	//var ms runtime.MemStats
	for _, line := range lines {
		//runtime.ReadMemStats(&ms)
		//log.WithField("line", i).WithField("mem_mi", ms.Alloc/1024/1024).Print(line)
		frame := make([][]int, res)
		for row := range frame {
			frame[row] = make([]int, res)
		}

		m := re.FindStringSubmatch(line)
		ax := aoc.MustAtoi(m[2])
		bx := aoc.MustAtoi(m[3])
		ay := aoc.MustAtoi(m[4])
		by := aoc.MustAtoi(m[5])
		az := aoc.MustAtoi(m[6])
		bz := aoc.MustAtoi(m[7])

		thisC := []Cuboid{{ax, bx, ay, by, az, bz}}

		// for "on", subtract each existing cuboid from this cuboid, then add what's left to the world.
		if m[1] == "on" {
			for _, c := range cuboids {
				var remaining []Cuboid
				for _, t := range thisC {
					remaining = append(remaining, t.Subtract(c)...)
				}
				thisC = remaining
				if len(thisC) == 0 {
					break
				}
			}
			cuboids = append(cuboids, thisC...)
		} else {
			// for "off", subtract this cuboid from each existing cuboid.
			var result []Cuboid
			for _, c := range cuboids {
				r := c.Subtract(thisC[0])
				result = append(result, r...)
			}
			cuboids = result
		}

		for _, c := range cuboids {
			for x := c.Ax; x < c.Bx; x += scale {
				for y := c.Ay; y < c.By; y += scale {
					scrX := (x - min) / scale
					if scrX >= res {
						scrX = res - 1
					}

					scrY := (y - min) / scale
					if scrY >= res {
						scrY = res - 1
					}

					frame[scrY][scrX] += c.Bz - c.Az + 1
					if frame[scrY][scrX] > maxVal {
						maxVal = frame[scrY][scrX]
					}
				}
			}
		}
		frames = append(frames, frame)
	}
	count := 0
	for _, c := range cuboids {
		count += c.Count()
	}
	log.Print(count)

	anim := &gif.GIF{}
	for _, frame := range frames {
		img := image.NewPaletted(image.Rect(0, 0, res, res), aoc.TolSequentialSmoothRainbow)
		for y, row := range frame {
			for x, v := range row {
				if v == 0 {
					img.Set(x, y, color.White)
				} else {
					img.Set(x, y, aoc.TolScale(0, maxVal, v))
				}
			}
		}
		anim.Image = append(anim.Image, img)
		anim.Delay = append(anim.Delay, 5)
		anim.Disposal = append(anim.Disposal, gif.DisposalNone)
	}
	anim.Delay[len(anim.Delay)-1] = 300
	aoc.Optimize(anim.Image)
	aoc.SaveGIF(anim, "day22.gif")

}
