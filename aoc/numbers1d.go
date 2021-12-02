package aoc

import (
	"bytes"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

func Numbers1D() []int {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		logrus.WithError(err).Fatal("could not parse input")
	}
	input = bytes.TrimSpace(input)
	lines := strings.Split(string(input), "\n")
	var nums []int
	for i, line := range lines {
		log := logrus.WithField("line", i).WithField("text", line)
		num, err := strconv.Atoi(line)
		if err != nil {
			log.WithError(err).Fatal("could not parse line")
		}
		nums = append(nums, num)
	}
	return nums
}
