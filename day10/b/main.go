package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"sort"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

var closers = map[rune]rune{
	'(': ')',
	'[': ']',
	'{': '}',
	'<': '>',
}

func isCloser(r rune) bool {
	return r == ')' || r == ']' || r == '}' || r == '>'
}

func isOpener(r rune) bool {
	return r == '(' || r == '[' || r == '{' || r == '<'
}

func isValid(s string, open rune) (int, bool) {
	for i := 0; i < len(s); {
		if rune(s[i]) == closers[open] {
			return i + 1, true
		}

		if isCloser(rune(s[i])) {
			return 0, false
		}

		consumed, valid := isValid(s[i+1:], rune(s[i]))
		if !valid {
			return 0, false
		}

		i += consumed + 1
	}
	return len(s), true
}

func complete(s string, open rune) (int, string) {
	for i := 0; i < len(s); {
		if open != 0 && rune(s[i]) == closers[open] {
			return i + 1, ""
		}
		consumed, missing := complete(s[i+1:], rune(s[i]))
		if missing != "" {
			return 0, fmt.Sprintf("%s%c", missing, closers[open])
		}
		i += consumed + 1
	}
	return len(s), string(closers[open])
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var scores []int
	for _, line := range lines {
		score := 0
		if !isOpener(rune(line[0])) {
			continue
		}
		_, ok := isValid(line[1:], rune(line[0]))
		if ok {
			_, c := complete(line, 0)
			for _, c := range c {
				switch c {
				case ')':
					score = score*5 + 1
				case ']':
					score = score*5 + 2
				case '}':
					score = score*5 + 3
				case '>':
					score = score*5 + 4
				}
			}
			scores = append(scores, score)
		}
	}
	sort.Ints(scores)
	fmt.Println(scores[len(scores)/2])
}
