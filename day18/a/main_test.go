package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestExplode(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		{"noop", "[1,2]", "[1,2]"},
		{"easy", "[[[[[1,2],3],4],5],6]", "[[[[0,5],4],5],6]"},
		{"ex1", "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"},
		{"ex2", "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"},
		{"ex3", "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"},
		{"ex4", "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"},
		{"ex5", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, _ := Explode(Tokenize(tt.in))
			require.Equal(t, Tokenize(tt.want), got)
		})
	}
}

func TestSplit(t *testing.T) {
	tests := []struct {
		name     string
		in, want string
	}{
		{"ex1", "[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"},
		{"ex2", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, _ := Split(Tokenize(tt.in))
			require.Equal(t, Tokenize(tt.want), got)
		})
	}
}

func TestReduce(t *testing.T) {
	tests := []struct {
		name string
		in   string
		want string
	}{
		{"ex1", "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			require.Equal(t, Tokenize(tt.want), Reduce(Tokenize(tt.in)))
		})
	}
}
