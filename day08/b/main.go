package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/gif"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func dmask(s string) uint8 {
	var ret uint8 = 0
	for _, b := range s {
		ret |= 1 << (b - 'a')
	}
	return ret
}

var fc int

func renderFrame(g *gif.GIF, input []string, solutions map[int]map[uint8]int, total int) {
	const cols = 2
	fc++

	if fc > 150 && fc%5 != 0 {
		return
	}

	if fc > 300 && fc%10 != 0 {
		return
	}

	if fc > 450 && fc%20 != 0 {
		return
	}

	w := 0
	for _, line := range input {
		if len(line) > w {
			w = len(line)
		}
	}

	type Word struct {
		string
		color.Color
	}
	type Line []Word

	var cooked []Line

	for i, line := range input {
		var l Line
		for _, word := range strings.Split(line, " ") {
			if word == "|" {
				l = append(l, Word{"|", color.White})
				continue
			}
			s, ok := solutions[i]
			if ok {
				v, ok := s[dmask(word)]
				if ok {
					l = append(l, Word{
						//fmt.Sprintf("% "+strconv.Itoa(len(word))+"d", v),
						strconv.Itoa(v),
						aoc.TolVibrantTeal})
				} else {
					l = append(l, Word{word, aoc.TolVibrantRed})
				}
			} else {
				l = append(l, Word{word, color.White})
			}
		}
		cooked = append(cooked, l)
	}
	cooked = append(cooked, Line{{"Total:", aoc.TolVibrantBlue},
		{strconv.Itoa(total), aoc.TolVibrantCyan}})

	rows := int(math.Ceil(float64(len(input)+1) / cols))

	img := image.NewPaletted(image.Rect(0, 0, (w*cols+2)*8, rows*aoc.LineHeight), aoc.TolVibrant)
	draw.Draw(img, img.Bounds(), image.Black, image.Point{}, draw.Over)
	for i, line := range cooked {
		cursor := 0
		for _, word := range line {
			aoc.Typeset(
				img,
				image.Pt(i%cols*(w+1)*8+cursor*8, i/cols*aoc.LineHeight),
				word.string,
				word.Color,
			)
			cursor += len(word.string) + 1
		}
	}

	del := 10 - fc/10
	if del < 2 {
		del = 2
	}
	g.Image = append(g.Image, img)
	g.Delay = append(g.Delay, del)
	g.Disposal = append(g.Disposal, gif.DisposalNone)
}

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var solutions = map[int]map[uint8]int{}

	g := &gif.GIF{}

	totalAmount := 0
	for i, line := range lines {
		solutions[i] = map[uint8]int{}
		parts := strings.Split(line, " ")
		var one, four, six, seven, eight, nine, zero uint8
		for _, digit := range parts {
			if digit == "|" {
				break
			}
			dmask := dmask(digit)
			switch len(digit) {
			case 2:
				one = dmask
				solutions[i][one] = 1
				renderFrame(g, lines, solutions, totalAmount)
			case 3:
				seven = dmask
				solutions[i][seven] = 7
				renderFrame(g, lines, solutions, totalAmount)
			case 4:
				four = dmask
				solutions[i][four] = 4
				renderFrame(g, lines, solutions, totalAmount)
			case 7:
				eight = dmask
				solutions[i][eight] = 8
				renderFrame(g, lines, solutions, totalAmount)
			}
		}

		for _, digit := range parts {
			if digit == "|" {
				break
			}

			dmask := dmask(digit)

			if len(digit) == 6 && dmask&one == one && dmask&four != four && dmask&seven == seven {
				zero = dmask
				solutions[i][zero] = 0
				renderFrame(g, lines, solutions, totalAmount)
			} else if len(digit) == 6 && dmask&one == one && dmask&four == four {
				nine = dmask
				solutions[i][nine] = 9
				renderFrame(g, lines, solutions, totalAmount)
			} else if len(digit) == 6 && dmask&one != one && dmask&four != four {
				six = dmask
				solutions[i][six] = 6
				renderFrame(g, lines, solutions, totalAmount)
			}
		}

		var top = seven &^ one
		var mid = four &^ zero
		var bl = eight &^ nine
		var tr = eight &^ six
		var bot = nine &^ four &^ top

		solutions[i][one|top|mid|bot] = 3
		renderFrame(g, lines, solutions, totalAmount)

		var tl = eight &^ (one | top | mid | bot) &^ bl

		solutions[i][top|tr|mid|bl|bot] = 2
		renderFrame(g, lines, solutions, totalAmount)

		var br = eight &^ tr &^ tl &^ bl &^ bot &^ mid &^ top
		solutions[i][top|tl|mid|br|bot] = 5

		var value int
		var mode = 0
		for _, digit := range parts {
			if digit == "|" {
				mode++
				continue
			}
			if mode == 0 {
				continue
			}

			dmask := dmask(digit)

			v, ok := solutions[i][dmask]
			if !ok {
				panic(fmt.Sprintf("%07b -> %s", dmask, digit))
			}
			value = value*10 + v
		}
		totalAmount += value

		renderFrame(g, lines, solutions, totalAmount)

		fmt.Println(i)
	}
	fmt.Println(totalAmount)

	g.Delay[len(g.Delay)-1] = 200

	aoc.Optimize(g.Image)

	f, err := os.OpenFile("out.gif", os.O_TRUNC|os.O_WRONLY|os.O_CREATE, 0644)
	if err == nil {
		err = gif.EncodeAll(f, g)
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
