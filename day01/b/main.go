package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func main() {
	in, err := ioutil.ReadFile("input")
	if err != nil {
		panic(err)
	}
	in = bytes.TrimSpace(in)
	lines := strings.Split(string(in), "\n")

	var depths []int
	for _, line := range lines {
		depth, err := strconv.Atoi(line)
		if err != nil {
			panic(err)
		}
		depths = append(depths, depth)
	}

	count := 0
	for i := 2; i  <len(depths)-1; i++ {
		a := depths[i-2] + depths[i-1] + depths[i]
		b := depths[i-1] + depths[i] + depths[i+1]
		if b > a {
			count++
		}
	}
	fmt.Println(count)
}
