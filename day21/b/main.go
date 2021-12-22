package main

import (
	"fmt"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type Phase int

const (
	P1Roll1 Phase = iota
	P1Roll2
	P1Roll3
	Score1
	P2Roll1
	P2Roll2
	P2Roll3
	Score2
)

type State struct {
	Phase
	Pos1, Score1           int
	Pos2, Score2           int
	Accum1, Accum2, Accum3 int
}

func (s State) A1(i int) State {
	s.Accum1 = i
	s.Phase++
	return s
}

func (s State) A2(i int) State {
	s.Accum2 = i
	s.Phase++
	return s
}

func (s State) A3(i int) State {
	s.Accum3 = i
	s.Phase++
	return s
}

var memo = map[State]int{}

func Play(s State) int {
	if i, ok := memo[s]; ok {
		return i
	}
	var r int
	switch s.Phase {
	case P1Roll1:
		r = Play(s.A1(1)) + Play(s.A1(2)) + Play(s.A1(3))
	case P1Roll2:
		r = Play(s.A2(1)) + Play(s.A2(2)) + Play(s.A2(3))
	case P1Roll3:
		r = Play(s.A3(1)) + Play(s.A3(2)) + Play(s.A3(3))
	case Score1:
		s := s
		s.Pos1 = (s.Pos1 + s.Accum1 + s.Accum2 + s.Accum3) % 10
		s.Score1 += s.Pos1 + 1
		if s.Score1 >= 21 {
			r = 1
		} else {
			s.Phase++
			r = Play(s)
		}
	case P2Roll1:
		r = Play(s.A1(1)) + Play(s.A1(2)) + Play(s.A1(3))
	case P2Roll2:
		r = Play(s.A2(1)) + Play(s.A2(2)) + Play(s.A2(3))
	case P2Roll3:
		r = Play(s.A3(1)) + Play(s.A3(2)) + Play(s.A3(3))
	case Score2:
		s := s
		s.Pos2 = (s.Pos2 + s.Accum1 + s.Accum2 + s.Accum3) % 10
		s.Score2 += s.Pos2 + 1
		if s.Score2 >= 21 {
			r = 0
		} else {
			s.Phase = P1Roll1
			r = Play(s)
		}
	}
	memo[s] = r
	return r
}

func main() {
	fmt.Println(Play(State{Pos1: 9, Pos2: 6}))
}
