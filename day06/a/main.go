package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

const refrac = 6
const child = 8

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}

	//input = []byte("3,4,3,1,2")
	fishS := strings.Split(strings.TrimSpace(string(input)), ",")
	var fishes []int
	for _, fish := range fishS {
		f, _ := strconv.Atoi(fish)
		fishes = append(fishes, f)
	}

	for day := 1; day <= 80; day++ {
		next := make([]int, len(fishes))

		for i, f := range fishes {
			if f == 0 {
				next[i] = refrac
				next = append(next, child)
			} else {
				next[i] = f - 1
			}
		}
		fishes = next
		fmt.Println(day, fishes)
	}
	fmt.Println(len(fishes))
}
