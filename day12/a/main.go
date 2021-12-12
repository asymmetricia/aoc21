package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"sort"
	"strings"

	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
)

var log = logrus.StandardLogger()

type Cave struct {
	Big         bool
	Visited     bool
	Connections []string
}

type System map[string]*Cave

func (s System) Dot() string {
	var nodes []string
	for name, cave := range s {
		if cave.Big {
			name += " [label=\"" + name + " (big)\"]"
		}
		name += ";"
		nodes = append(nodes, name)
	}
	sort.Strings(nodes)

	var edges []string
	for name, cave := range s {
		for _, other := range cave.Connections {
			if name > other {
				continue
			}
			edges = append(edges, fmt.Sprintf("%s -- %s;", name, other))
		}
	}
	sort.Strings(edges)
	return fmt.Sprintf(
		"graph cave {\n%s\n%s\n}",
		strings.Join(nodes, "\n"),
		strings.Join(edges, "\n"),
	)
}

func (s System) Copy() System {
	ret := make(System, len(s))
	for k, v := range s {
		ret[k] = new(Cave)
		*(ret[k]) = *v
	}
	return ret
}

//func (s System) String() string {
//	var ret []string
//	for name, cave := range s {
//		str := name + " -- "
//		for _, cxName := range cave.Connections {
//			if name > cxName {
//				continue
//			}
//			if s[cxName].Visited {
//				str += "[" + cxName + "]"
//			}
//		}
//	}
//}

func (s System) Route(from string) [][]string {
	if from == "end" {
		return [][]string{{"end"}}
	}

	var ret [][]string

	for _, cxName := range s[from].Connections {
		cx := s[cxName]
		if cx.Visited {
			continue
		}

		log.Printf("%s -- %s", from, cxName)

		ss := s
		if !cx.Big {
			ss = s.Copy()
			ss[cxName].Visited = true
		}

		for _, cxRt := range ss.Route(cxName) {
			ret = append(ret, append([]string{from}, cxRt...))
		}
	}

	return ret
}

func (s System) Graph() {
	buf := bytes.NewBufferString(s.Dot())
	cmd := exec.Command("dot", "-Tpng")
	f, err := os.OpenFile("graph.png", os.O_TRUNC|os.O_WRONLY|os.O_CREATE, 0644)
	if err == nil {
		cmd.Stdin = buf
		cmd.Stdout = f
		cmd.Stderr = os.Stderr
		err = cmd.Run()
	}
	if err == nil {
		err = f.Sync()
	}
	if err == nil {
		err = f.Close()
	}
	if err != nil {
		log.Print(buf.String())
		log.Fatal(err)
	}
}

func main() {
	input, err := ioutil.ReadFile("input")
	//input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	system := System{}
	for _, line := range lines {
		a, b := aoc.Split2(line, "-")

		if _, ok := system[a]; !ok {
			system[a] = &Cave{
				Big: strings.ToUpper(a) == a}
		}

		if _, ok := system[b]; !ok {
			system[b] = &Cave{
				Big: strings.ToUpper(b) == b}
		}

		system[a].Connections = append(system[a].Connections, b)
		system[b].Connections = append(system[b].Connections, a)
	}
	system["start"].Visited = true

	routes := system.Route("start")
	for _, rt := range routes {
		log.Print(strings.Join(rt, ","))
	}
	log.Print(len(routes))
}
