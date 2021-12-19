package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type SnailfishNumber struct {
	Left  *SnailfishNumber
	Right *SnailfishNumber
	Int   int
}

type Type string

const (
	OpenBracket  Type = "["
	Literal           = "#"
	Comma             = ","
	CloseBracket      = "]"
)

type Token struct {
	Type
	int
}

func (t Token) String() string {
	if t.Type == Literal {
		return strconv.Itoa(t.int)
	}
	return string(t.Type)
}

type Stream []Token

func (s Stream) String() string {
	var ret string
	for _, t := range s {
		ret += t.String()
	}
	return ret
}

func Tokenize(s string) Stream {
	var ret Stream
	for _, c := range s {
		switch c {
		case '[':
			ret = append(ret, Token{Type: OpenBracket})
		case ',':
			ret = append(ret, Token{Type: Comma})
		case ']':
			ret = append(ret, Token{Type: CloseBracket})
		default:
			if last := ret[len(ret)-1]; last.Type == Literal {
				ret[len(ret)-1] = Token{
					Type: Literal,
					int:  last.int*10 + int(c-'0'),
				}
			} else {
				ret = append(ret, Token{Type: Literal, int: int(c - '0')})
			}
		}
	}
	return ret
}

func Reduce(s Stream) Stream {
	for {
		ex, modified := Explode(s)
		if modified {
			//fmt.Println("Explode")
			//fmt.Println("  ", s)
			//fmt.Println("  ", ex)
			s = ex
			continue
		}

		spl, modified := Split(s)
		if modified {
			//fmt.Println("Split")
			//fmt.Println("  ", s)
			//fmt.Println("  ", spl)
			s = spl
			continue
		}

		break
	}

	return s
}

func Split(s Stream) (Stream, bool) {
	var ret Stream
	for i, t := range s {
		if t.Type == Literal && t.int > 9 {
			ret = append(ret, s[:i]...)
			ret = append(ret,
				Token{Type: OpenBracket},
				Token{Type: Literal, int: t.int / 2},
				Token{Type: Comma},
				Token{Type: Literal, int: t.int/2 + t.int%2},
				Token{Type: CloseBracket},
			)
			ret = append(ret, s[i+1:]...)
			return ret, true
		}
	}
	return s, false
}

func Explode(s Stream) (Stream, bool) {
	depth := 0
	for i, token := range s {
		switch token.Type {
		case OpenBracket:
			depth++
		case CloseBracket:
			depth--
		case Literal:
			if depth < 5 {
				continue
			}

			right := s[i+2]
			if right.Type != Literal {
				continue
			}
			left := token
			before := s[:i-1]
			after := s[i+4:]

			for j := len(before) - 1; j >= 0; j-- {
				if before[j].Type == Literal {
					newBefore := append(Stream{}, before[:j]...)
					newBefore = append(newBefore, Token{Type: Literal, int: before[j].int + left.int})
					newBefore = append(newBefore, before[j+1:]...)
					before = newBefore
					break
				}
			}
			for j := 0; j < len(after); j++ {
				if after[j].Type == Literal {
					newAfter := append(Stream{}, after[:j]...)
					newAfter = append(newAfter, Token{Type: Literal, int: after[j].int + right.int})
					newAfter = append(newAfter, after[j+1:]...)
					after = newAfter
					break
				}
			}

			ret := append(Stream{}, before...)
			ret = append(ret, Token{Type: Literal, int: 0})
			ret = append(ret, after...)
			return ret, true
		}
	}
	return s, false
}

func Add(s1, s2 Stream) Stream {
	ret := make(Stream, 0, len(s1)+len(s2)+3)
	ret = append(ret, Token{Type: OpenBracket})
	ret = append(ret, s1...)
	ret = append(ret, Token{Type: Comma})
	ret = append(ret, s2...)
	ret = append(ret, Token{Type: CloseBracket})
	return ret
}

func Magnitude(s Stream) (m int, c int) {
	c = 1 // open bracket

	switch s[1].Type {
	case OpenBracket:
		left, leftConsumed := Magnitude(s[1:])
		c += leftConsumed
		m = left * 3
	case Literal:
		c++
		m = s[1].int * 3
	}

	c++ // comma

	switch s[c].Type {
	case OpenBracket:
		right, rightConsumed := Magnitude(s[c:])
		c += rightConsumed
		m += right * 2
	case Literal:
		c++
		m += s[c-1].int * 2
	}

	c++ // close bracket

	return m, c
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var best int
	for _, x := range lines {
		for _, y := range lines {
			m, _ := Magnitude(Reduce(Add(Tokenize(x), Tokenize(y))))
			if m > best {
				best = m
			}
		}
	}
	fmt.Println(best)
}
