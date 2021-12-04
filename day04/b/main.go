package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"

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
	}

}
