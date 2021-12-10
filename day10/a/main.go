package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
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

func Parse(s string, open rune) (int, error, rune) {
	for i := 0; i < len(s); {
		if rune(s[i]) == closers[open] {
			return i + 1, nil, 0
		}

		if isCloser(rune(s[i])) {
			return 0, fmt.Errorf("unexpected closer %c", s[i]), rune(s[i])
		}

		consumed, err, errRune := Parse(s[i+1:], rune(s[i]))
		if err != nil {
			return 0, err, errRune
		}

		i += consumed + 1
	}
	return len(s), nil, 0
}

func main() {
	input, err := ioutil.ReadFile("input")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var errScore int
	for _, line := range lines {
		if !isOpener(rune(line[0])) {
			continue
		}
		_, err, errRune := Parse(line[1:], rune(line[0]))
		if err != nil {
			switch errRune {
			case ')':
				errScore += 3
			case ']':
				errScore += 57
			case '}':
				errScore += 1197
			case '>':
				errScore += 25137
			}
			continue
		}
	}
	fmt.Println(errScore)
}
