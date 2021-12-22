package main

import (
	"fmt"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	var score1, score2, rolls int
	nextDie := 1
	pos1 := 9
	pos2 := 6
	roll := func() int {
		result := nextDie
		nextDie++
		rolls++
		if nextDie > 100 {
			nextDie = 1
		}
		return result
	}
	for {
		pos1 = (pos1 + roll() + roll() + roll()) % 10
		score1 += pos1 + 1
		if score1 > 1000 {
			break
		}

		pos2 = (pos2 + roll() + roll() + roll()) % 10
		score2 += pos2 + 1
		if score2 > 1000 {
			break
		}
	}
	if score1 > 1000 {
		fmt.Println(score2 * rolls)
	} else {
		fmt.Println(score1 * rolls)
	}
}
