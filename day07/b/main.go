package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	//lines[0] = "16,1,2,0,4,2,7,1,2,14"
	hzpos := strings.Split(lines[0], ",")
	fmt.Println(hzpos)
	var sum int
	var hzpi []int
	var max = 0
	for _, hzp := range hzpos {
		i, err := strconv.Atoi(hzp)
		if err != nil {
			panic(err)
		}
		hzpi = append(hzpi, i)
		sum += i
		if i > max {
			max =i
		}
	}

	var bestp, bestv = 0, math.MaxInt
	for avg := 0; avg < max; avg ++ {
		fuel := 0
		for _, hzp := range hzpi {
			diff := avg - hzp
			if hzp > avg {
				diff = hzp - avg
			}
			for i := 1; i <= diff; i++ {
				fuel += i
			}
		}
		if fuel < bestv {
			bestp = avg
			bestv = fuel
		}
	}
	fmt.Println(bestp, bestv)
}
