package main

import (
	"github.com/stretchr/testify/require"
	"testing"
)

func TestCuboid_Subtract(t *testing.T) {
	tests := []struct {
		name string
		a, b Cuboid
		want []Cuboid
	}{
		{"exact match = zero cuboid",
			Cuboid{-10, 10, -20, 20, -30, 30},
			Cuboid{-10, 10, -20, 20, -30, 30},
			nil},
		{"1-thick hollow",
			Cuboid{0, 2, 0, 2, 0, 2},
			Cuboid{1, 1, 1, 1, 1, 1},
			[]Cuboid{
				{0, 0, 0, 2, 0, 2},
				{1, 1, 2, 2, 1, 1},
				{1, 1, 0, 0, 1, 1},
				{1, 1, 0, 2, 2, 2},
				{1, 1, 0, 2, 0, 0},
				{2, 2, 0, 2, 0, 2},
			},
		},
		{"corner",
			Cuboid{0, 2, 0, 2, 0, 2},
			Cuboid{1, 2, 0, 1, 0, 1},
			[]Cuboid{
				{0, 0, 0, 2, 0, 2},
				{1, 2, 2, 2, 0, 1},
				{1, 2, 0, 2, 2, 2},
			}},
		{"minus left side",
			Cuboid{0, 2, 0, 2, 0, 2},
			Cuboid{0, 0, 0, 2, 0, 2},
			[]Cuboid{
				{1, 2, 0, 2, 0, 2},
			}},
		{
			"example 1",
			Cuboid{10, 12, 10, 12, 10, 12},
			Cuboid{9, 11, 9, 11, 9, 11},
			[]Cuboid{
				{10, 11, 12, 12, 10, 11},
				{10, 11, 10, 12, 12, 12},
				{12, 12, 10, 12, 10, 12},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			require.Equal(t, tt.want, tt.a.Subtract(tt.b))
		})
	}
}
