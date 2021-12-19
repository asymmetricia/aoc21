package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"

	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type Beacon [3]int

func (b Beacon) Times(m int) Beacon {
	return Beacon{b[0] * m, b[1] * m, b[2] * m}
}

func (b Beacon) Plus(o Beacon) Beacon {
	return Beacon{
		b[0] + o[0],
		b[1] + o[1],
		b[2] + o[2],
	}
}

func (b Beacon) Minus(o Beacon) Beacon {
	return b.Plus(o.Times(-1))
}

type Scanner struct {
	Location Beacon
	Beacons  []Beacon
}

func (b Beacon) Cross(m [3][3]int) Beacon {
	return Beacon{
		b[0]*m[0][0] + b[1]*m[1][0] + b[2]*m[2][0],
		b[0]*m[0][1] + b[1]*m[1][1] + b[2]*m[2][1],
		b[0]*m[0][2] + b[1]*m[1][2] + b[2]*m[2][2],
	}
}

var (
	RotateX = [3][3]int{
		{1, 0, 0},
		{0, 0, 1},
		{0, -1, 0},
	}
	RotateY = [3][3]int{
		{0, 0, -1},
		{0, 1, 0},
		{1, 0, 0},
	}
	RotateZ = [3][3]int{
		{0, 1, 0},
		{-1, 0, 0},
		{0, 0, 1},
	}
)

// Has returns true if the scanner has the given beacon
func (s Scanner) Has(b Beacon) bool {
	for _, sb := range s.Beacons {
		if sb == b {
			return true
		}
	}
	return false
}

func (s Scanner) Translate(o Beacon) Scanner {
	var ret = Scanner{
		Location: s.Location.Plus(o),
	}
	for _, beacon := range s.Beacons {
		ret.Beacons = append(ret.Beacons, beacon.Plus(o))
	}
	return ret
}

func (s Scanner) Cross(mm [3][3]int) Scanner {
	var ret = Scanner{}
	for _, beacon := range s.Beacons {
		ret.Beacons = append(ret.Beacons, beacon.Cross(mm))
	}
	return ret
}

// Match returns true if the scanners have at least twelve overlapping beacons.
func (s Scanner) Match(b Scanner) (sIndex Beacon, bIndex Beacon, overlap bool) {
	// Try each beacon in scanner as the s-index
	for _, sb := range s.Beacons {
		sIndexed := s.Translate(sb.Times(-1))

		// Align the s-index beacon with each beacon in b
		for _, bb := range b.Beacons {
			bIndexed := b.Translate(bb.Times(-1))

			// See whether enough other beacons in s align with beacons in b
			var overlap int
			for _, sIndexedBeacon := range sIndexed.Beacons {
				if bIndexed.Has(sIndexedBeacon) {
					overlap++
				}
			}

			if overlap >= 12 {
				return sb, bb, true
			}
		}
	}

	return Beacon{}, Beacon{}, false
}

// Orient tries to find an orientation of Scanner `b` that matches Scanner `a`.
// If so, it returns the oriented Scanner `b` and true.
func Orient(a, b Scanner) (Scanner, bool) {
	for _, rots := range [][][3][3]int{
		{},
		{RotateY},
		{RotateY, RotateY},
		{RotateY, RotateY, RotateY},
		{RotateZ},
		{RotateZ, RotateX},
		{RotateZ, RotateX, RotateX},
		{RotateZ, RotateX, RotateX, RotateX},
		{RotateZ, RotateZ},
		{RotateZ, RotateZ, RotateY},
		{RotateZ, RotateZ, RotateY, RotateY},
		{RotateZ, RotateZ, RotateY, RotateY, RotateY},
		{RotateZ, RotateZ, RotateZ},
		{RotateZ, RotateZ, RotateZ, RotateX},
		{RotateZ, RotateZ, RotateZ, RotateX, RotateX},
		{RotateZ, RotateZ, RotateZ, RotateX, RotateX, RotateX},
		{RotateX},
		{RotateX, RotateZ},
		{RotateX, RotateZ, RotateZ},
		{RotateX, RotateZ, RotateZ, RotateZ},
		{RotateX, RotateX, RotateX},
		{RotateX, RotateX, RotateX, RotateZ},
		{RotateX, RotateX, RotateX, RotateZ, RotateZ},
		{RotateX, RotateX, RotateX, RotateZ, RotateZ, RotateZ},
	} {
		rotated := b
		for _, rot := range rots {
			rotated = rotated.Cross(rot)
		}

		aIndex, bIndex, match := a.Match(rotated)
		if match {
			return rotated.Translate(bIndex.Times(-1)).Translate(aIndex), true
		}
	}

	return Scanner{}, false
}

func (s Scanner) Sort() {
	sort.Slice(s.Beacons, func(i, j int) bool {
		ib := s.Beacons[i]
		jb := s.Beacons[j]
		return ib[0] < jb[0] ||
			ib[0] == jb[0] && ib[1] < jb[1] ||
			ib[0] == jb[0] && ib[1] == jb[1] && ib[2] < jb[2]
	})
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var toMatch = map[int]*Scanner{}
	var id int
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		if strings.HasPrefix(line, "---") {
			id, err = strconv.Atoi(strings.Split(line, " ")[2])
			if err != nil {
				panic(fmt.Sprintf("could not parse scanner id %q: %v", id, err))
			}
			toMatch[id] = &Scanner{}
			continue
		}

		parts := strings.Split(line, ",")

		var x, y, z int
		x, err = strconv.Atoi(parts[0])
		if err == nil {
			y, err = strconv.Atoi(parts[1])
		}
		if err == nil {
			z, err = strconv.Atoi(parts[2])
		}
		if err != nil {
			panic(fmt.Sprintf("parsing %q: %v", line, err))
		}

		toMatch[id].Beacons = append(toMatch[id].Beacons, Beacon{x, y, z})
	}

	aligned := map[int]*Scanner{
		0: toMatch[0],
	}
	aligned[0].Sort()
	delete(toMatch, 0)

	alignedIds := []int{0}

	for len(toMatch) > 0 {
		for i := 0; i < len(alignedIds); i++ {
			id := alignedIds[i]
			var toDelete []int
			for matchId, s := range toMatch {
				oriented, match := Orient(*aligned[id], *s)
				if match {
					fmt.Printf("found %d -> %d @ %v\n", id, matchId, oriented.Location)
					oriented.Sort()
					aligned[matchId] = new(Scanner)
					*aligned[matchId] = oriented
					alignedIds = append(alignedIds, matchId)
					toDelete = append(toDelete, matchId)
				}
			}
			for _, d := range toDelete {
				delete(toMatch, d)
			}
		}
	}

	beacons := map[[3]int]bool{}
	for _, s := range aligned {
		for _, b := range s.Beacons {
			beacons[b] = true
		}
	}
	fmt.Println(len(beacons))

	var farthest int
	for a := 0; a < len(aligned); a++ {
		for b := a + 1; b < len(aligned); b++ {
			var dist int
			for _, v := range aligned[a].Location.Minus(aligned[b].Location) {
				if v < 0 {
					dist -= v
				} else {
					dist += v
				}
			}
			if dist > farthest {
				farthest = dist
			}
		}
	}
	fmt.Println(farthest)
}
