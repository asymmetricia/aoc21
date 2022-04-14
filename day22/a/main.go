package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/aoc"
	"io/ioutil"
	"regexp"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	grid := map[[3]int]bool{}

	re := regexp.MustCompile(`(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)`)
	for _, line := range lines {
		m := re.FindStringSubmatch(line)
		ax := aoc.MustAtoi(m[2])
		bx := aoc.MustAtoi(m[3])
		ay := aoc.MustAtoi(m[4])
		by := aoc.MustAtoi(m[5])
		az := aoc.MustAtoi(m[6])
		bz := aoc.MustAtoi(m[7])

		for x := ax; x <= bx; x++ {
			if x < -50 || x > 50 {
				continue
			}
			for y := ay; y <= by; y++ {
				if y < -50 || y > 50 {
					continue
				}
				for z := az; z <= bz; z++ {
					if z < -50 || z > 50 {
						continue
					}
					grid[[3]int{x, y, z}] = m[1] == "on"
				}
			}
		}
		fmt.Println(line)
		fmt.Printf("%s x=%d..%d,y=%d..%d,z=%d..%d\n", m[1], ax, bx, ay, by, az, bz)
	}

	count := 0
	for x := -50; x <= 50; x++ {
		for y := -50; y <= 50; y++ {
			for z := -50; z <= 50; z++ {
				if grid[[3]int{x, y, z}] {
					count++
				}
			}
		}
	}

	log.Print(count)
}
