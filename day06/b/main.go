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

	var ages = map[int]int{}
	for _, fish := range fishes {
		ages[fish]++
	}

	fmt.Println(ages)
	for day := 1; day <= 256; day++ {
		next := map[int]int{}
		next[0] = ages[1]
		next[1] = ages[2]
		next[2] = ages[3]
		next[3] = ages[4]
		next[4] = ages[5]
		next[5] = ages[6]
		next[6] = ages[7]
		next[7] = ages[8]
		next[child] += ages[0]
		next[refrac] += ages[0]
		ages = next
		fmt.Println(day, ages)
	}
	var t int
	for _, count := range ages {
		t += count
	}
	fmt.Println(t)
}
