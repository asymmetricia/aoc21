package main

import (
	"fmt"
	"image"
	"image/color"
	"image/gif"
	"io/ioutil"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func win(board [][]int, draws map[int]bool) bool {
	for y := range board {
		win := true
		for _, v := range board[y] {
			win = win && draws[v]
		}
		if win {
			return true
		}
	}
	for x := range board[0] {
		win := true
		for y := range board {
			win = win && draws[board[y][x]]
		}
		if win {
			return true
		}
	}
	return false
}

func drawBoards(boards [][][]int, draws map[int]bool) *image.Paletted {
	cellWidth := 0
	for _, board := range boards {
		for _, row := range board {
			for _, num := range row {
				l := math.Log10(float64(num))
				w := int(math.Floor(l)) + 1
				if w > cellWidth {
					cellWidth = w
				}
			}
		}
	}

	// boardWidth is the number of rows or columns
	boardWidth := len(boards[0])

	// fieldWidth is the number of boards wide
	fieldWidth := int(math.Ceil(math.Sqrt(float64(len(boards)))))

	width := fieldWidth * (boardWidth + 1) * (cellWidth + 1) * 8 - 25
	field := image.NewPaletted(image.Rect(0, 0, width, width), aoc.TolVibrant)

	for i, board := range boards {
		win := win(board, draws)
		for y, row := range board {
			for x, num := range row {
				pt := image.Pt(
					(i%fieldWidth)*(boardWidth+1)*(cellWidth+1)*8+x*(cellWidth+1)*8,
					(i/fieldWidth)*(boardWidth+1)*(cellWidth+1)*8+y*(cellWidth+1)*8)
				var c color.Color = color.White
				if win && y % 2 == x % 2 {
					c = aoc.TolVibrantMagenta
				} else if win {
					c = aoc.TolVibrantTeal
				} else if draws[num] {
					c = aoc.TolVibrantCyan
				}
				aoc.Typeset(field, pt, strconv.Itoa(num), c)
			}
		}
	}

	return field
}

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = []byte(regexp.MustCompile(` +`).ReplaceAllString(string(input), " "))
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	order := lines[0]
	lines = lines[2:]
	var boards [][][]int
	var board [][]int
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			boards = append(boards, board)
			board = nil
			continue
		}
		var bline []int
		for _, num := range strings.Split(line, " ") {
			n, _ := strconv.Atoi(num)
			bline = append(bline, n)
		}
		board = append(board, bline)
	}
	if board != nil {
		boards = append(boards, board)
	}

	anim := &gif.GIF{}

	allBoards := make([][][]int, len(boards))
	copy(allBoards, boards)
	draws := map[int]bool{}
	for _, draw := range strings.Split(order, ",") {
		n, _ := strconv.Atoi(draw)
		draws[n] = true
		for i := 0; i < len(boards); i++ {
			board := boards[i]
			if win(board, draws) {
				score := 0
				for _, row := range board {
					for _, cell := range row {
						if !draws[cell] {
							score += cell
						}
					}
				}

				boards = append(boards[:i], boards[i+1:]...)
				i--
				fmt.Println(n * score)
			}
		}
		anim.Image = append(anim.Image, drawBoards(allBoards, draws))
		anim.Delay = append(anim.Delay, 1)
		anim.Disposal = append(anim.Disposal, gif.DisposalNone)
		if len(boards) == 0 {
			break
		}
	}
	anim.Delay[len(anim.Delay)-1] = 100

	aoc.Optimize(anim.Image)

	f, err := os.OpenFile("anim.gif", os.O_TRUNC|os.O_WRONLY|os.O_CREATE, 0644)
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
