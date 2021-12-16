package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/asymmetricia/aoc21/bits"
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

	decoded, err := bits.Decode(lines[0])
	if err != nil {
		log.Fatal(err)
	}
	packet, _, err := bits.Parse(decoded)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(packet.String())
	fmt.Println(bits.Execute(packet))
}
