package main

import (
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
	lines := strings.Split(string(in), "\n")

	var prev *int
	count := 0
	for _, line := range lines {
		if prev == nil {
			prev = new(int)
			*prev, _ = strconv.Atoi(line)
		} else {
			this, _ := strconv.Atoi(line)
			if this > *prev {
				count++
			}
			*prev = this
		}
	}
	fmt.Println(count)
}
