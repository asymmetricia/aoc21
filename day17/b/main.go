package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func simulate(x, y, dx, dy int) (int, bool) {
	var maxy = y
	for {
		if x > txb {
			return 0, false
		}
		if y < tya {
			return 0, false
		}
		if x >= txa && x <= txb && y <= tyb && y >= tya {
			return maxy, true
		}

		x += dx
		y += dy
		if y > maxy {
			maxy = y
		}
		if dx > 0 {
			dx--
		}
		dy -= 1
	}
}

var txa, txb, tya, tyb int

func main() {
	input, err := ioutil.ReadFile("input")
	// input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	parts := regexp.
		MustCompile(`target area: x=([0-9-]+)..([0-9-]+), y=([0-9-]+)..([0-9-]+)`).
		FindStringSubmatch(lines[0])
	if parts == nil {
		panic(lines[0])
	}

	txa, err = strconv.Atoi(parts[1])
	if err == nil {
		txb, err = strconv.Atoi(parts[2])
	}
	if err == nil {
		tya, err = strconv.Atoi(parts[3])
	}
	if err == nil {
		tyb, err = strconv.Atoi(parts[4])
	}
	if err != nil {
		panic(err)
	}

	fmt.Println(txa, txb, tya, tyb)

	best := 0
	count := 0
	for dx := 19; dx <= txb; dx++ {
		for dy := tya; dy <= -tya; dy++ {
			maxy, ok := simulate(0, 0, dx, dy)
			if ok {
				if maxy > best {
					best = maxy
				}
				count++
			}
		}
	}

	fmt.Println("(A) Highest Y:      ", best)
	fmt.Println("(B) Total On Target:", count)
}
