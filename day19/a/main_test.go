package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestScanner_Translate(t *testing.T) {
	var tests = []struct {
		name  string
		in    Scanner
		xlate Beacon
		want  Scanner
	}{
		{"simple",
			Scanner{Beacons: []Beacon{{0, 0, 0}}},
			Beacon{1, 2, 3},
			Scanner{Beacons: []Beacon{{1, 2, 3}}}},
		{"simple 2",
			Scanner{Beacons: []Beacon{{0, 0, 0}, {1, 2, 3}}},
			Beacon{1, 2, 3},
			Scanner{Beacons: []Beacon{{1, 2, 3}, {2, 4, 6}}}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			require.ElementsMatch(t, tt.in.Translate(tt.xlate).Beacons, tt.want.Beacons)
		})
	}
}
