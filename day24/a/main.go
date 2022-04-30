package main

import (
	"bytes"
	"fmt"
	"github.com/asymmetricia/aoc21/day24/a/winnow"
	"github.com/asymmetricia/aoc21/term"
	"github.com/sirupsen/logrus"
	"io/ioutil"
	"net/http"
	_ "net/http/pprof"
	"os"
	"reflect"
	"sort"
	"strings"
	"sync/atomic"
	"time"
)

var log = logrus.StandardLogger()

func main() {
	//log.SetLevel(logrus.DebugLevel)
	go func() {
		log.Println(http.ListenAndServe("localhost:6060", nil))
	}()

	input, err := ioutil.ReadFile("input")
	// input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	log.Print("compiling...")
	program := Compile(lines)
	//tree := winnow.CompileTree(lines)
	log.Print("done!")

	var c Computer
	inputs := [][14]int{{}}

	if len(os.Args) == 15 {
		for i := 0; i < 14; i++ {
			var newInputs [][14]int
			for _, input := range inputs {
				for _, c := range os.Args[i+1] {
					input[i] = int(c - '0')
					newInputs = append(newInputs, input)
				}
			}
			inputs = newInputs
		}
	}

	log.Print(inputs)

	for lineno, instruction := range program {
		log.Printf("%d: %s (%d inputs)", lineno, instruction, len(inputs))
		if instruction.Op == winnow.Inp {
			newInputs := make([][14]int, 0, len(inputs)*9)
			in := make(chan [14]int, 100)
			var prog int32
			total := len(inputs) * 9
			go func() {
				defer close(in)
				for n, input := range inputs {
					if input[instruction.BImm] != 0 {
						in <- input
					} else {
						for i := 1; i <= 9; i++ {
							atomic.StoreInt32(&prog, int32(n*9+i+1))
							input[instruction.BImm] = i
							in <- input
						}
					}
				}
			}()
			var workers []reflect.SelectCase
			for i := 0; i < 10; i++ {
				out := make(chan [14]int, 10)
				workers = append(workers, reflect.SelectCase{Chan: reflect.ValueOf(out), Dir: reflect.SelectRecv})
				go func(in <-chan [14]int, out chan<- [14]int) {
					defer close(out)
					for in := range in {
						cc := Computer{}
						cc.Run(program, in)
						if cc.Z.HasZero() {
							out <- in
						}
					}
				}(in, out)
			}

			lp := time.Now()
			for {
				i, v, ok := reflect.Select(workers)
				if !ok {
					workers = append(workers[:i], workers[i+1:]...)
					if len(workers) == 0 {
						break
					}
					continue
				}
				input := v.Interface().([14]int)
				if time.Since(lp) > time.Second {
					fmt.Print("\r")
					term.ClearLine()
					fmt.Printf("%d: %v %d/%d/%d", lineno, input, len(newInputs), prog, total)
					lp = time.Now()
				}
				newInputs = append(newInputs, input)
			}

			fmt.Println()
			if len(newInputs) == 0 {
				log.Fatal("no solutions?!")
			}
			inputs = newInputs
			continue
		}
		c.Execute(instruction, inputs)
	}
	log.Println(len(c.Z), "hypothetical outputs")

	var inputInts []uint64
	for _, input := range inputs {
		var iint uint64
		for _, i := range input {
			iint = iint*10 + uint64(i)
		}
		log.Print(input, iint)
		inputInts = append(inputInts, iint)
	}
	sort.Slice(inputInts, func(i, j int) bool {
		return inputInts[i] < inputInts[j]
	})
	log.Printf("%d inputs", len(inputs))
	log.Printf("worst: %d", inputInts[0])
	log.Printf("best: %d", inputInts[len(inputInts)-1])
}
