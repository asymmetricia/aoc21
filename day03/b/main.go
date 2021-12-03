package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var numbers [][]int
	for _, line := range lines {
		var number []int
		for _, c := range line {
			number = append(number, int(c-'0'))
		}
		numbers = append(numbers, number)
	}

	width := len(numbers[0])

	oxygen := make([][]int, len(numbers))
	copy(oxygen, numbers)
	for pos := range numbers[0] {
		target := common(oxygen, pos)
		for i := 0; i < len(oxygen); i++ {
			if len(oxygen) == 1 {
				break
			}
			number := oxygen[i]
			if number[pos] != target {
				oxygen = append(oxygen[:i], oxygen[i+1:]...)
				i--
			}
		}
	}
	var oxygenVal int
	for i := 0; i < width; i++ {
		oxygenVal |= oxygen[0][i] << (width - i - 1)
	}

	co2 := make([][]int, len(numbers))
	copy(co2, numbers)
	for pos := range numbers[0] {
		target := common(co2, pos)
		for i := 0; i < len(co2); i++ {
			if len(co2) == 1 {
				break
			}
			number := co2[i]
			if number[pos] == target {
				co2 = append(co2[:i], co2[i+1:]...)
				i--
			}
		}
	}
	var co2Val int
	for i := 0; i < width; i++ {
		co2Val |= co2[0][i] << (width - i - 1)
	}
	fmt.Println(oxygenVal, co2Val, oxygenVal*co2Val)
}

func common(numbers [][]int, pos int) int {
	var ones int
	for _, number := range numbers {
		if number[pos] == 1 {
			ones++
		}
	}
	if ones >= len(numbers)-ones {
		return 1
	}
	return 0
}
