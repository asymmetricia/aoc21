package main

import (
	"bytes"
	"errors"
	"fmt"
	"github.com/sirupsen/logrus"
	"io"
	"io/ioutil"
	"reflect"
	"strconv"
	"strings"
	"time"
)

var log = logrus.StandardLogger()

const Digits = 14

type ALU struct {
	Register [4]int
	Input    [Digits]int
	InputPtr int
}

func (a *ALU) Reset() {
	a.Register = [4]int{}
	a.InputPtr = 0
}

const (
	RegW int = iota
	RegX
	RegY
	RegZ
)

func MathOp(name string, op func(a, b int) (int, error)) func(alu *ALU, arg1, arg2 Argument) error {
	return func(alu *ALU, arg1, arg2 Argument) error {
		if arg1.Register < 0 {
			return fmt.Errorf("%q: first argument must be register", name)
		}

		val1 := alu.Register[arg1.Register]

		val2 := arg2.Immediate
		if arg2.Register >= 0 {
			val2 = alu.Register[arg2.Register]
		}

		result, err := op(val1, val2)
		if err != nil {
			return fmt.Errorf("%s: op failed: %v", name, err)
		}
		alu.Register[arg1.Register] = result
		return nil
	}
}

type OpCode int

const (
	INP OpCode = iota
	ADD
	DIV
	EQL
	MOD
	MUL
)

func (o OpCode) String() string {
	return []string{"INP", "ADD", "DIV", "EQL", "MOD", "MUL"}[o]
}

type Argument struct {
	Register  int
	Immediate int
}

func (o Argument) String() string {
	if o.Register == -2 {
		return ""
	}
	if o.Register == -1 {
		return strconv.Itoa(o.Immediate)
	}
	return []string{"W", "X", "Y", "Z"}[o.Register]
}

var Ops = map[OpCode]func(*ALU, Argument, Argument) error{
	INP: func(alu *ALU, arg1, _ Argument) error {
		if alu.InputPtr >= len(alu.Input) {
			return io.EOF
		}

		if arg1.Register < 0 {
			return fmt.Errorf("inp: argument must be register")
		}

		alu.Register[arg1.Register] = alu.Input[alu.InputPtr]
		alu.Input[alu.InputPtr] = 0
		alu.InputPtr++
		return nil
	},
	ADD: MathOp("add", func(a, b int) (int, error) { return a + b, nil }),
	DIV: MathOp("div", func(a, b int) (int, error) {
		if b == 0 {
			return 0, errors.New("divide by zero")
		}
		return a / b, nil
	}),
	EQL: MathOp("eql", func(a, b int) (int, error) {
		if a == b {
			return 1, nil
		} else {
			return 0, nil
		}
	}),
	MOD: MathOp("mod", func(a, b int) (int, error) { return a % b, nil }),
	MUL: MathOp("mul", func(a, b int) (int, error) { return a * b, nil }),
}

type Statement struct {
	OpCode OpCode
	Arg1   Argument
	Arg2   Argument
}

func (s Statement) Execute(a *ALU) error {
	return Ops[s.OpCode](a, s.Arg1, s.Arg2)
}

func Compile(statement string) (Statement, error) {
	var ret Statement
	parts := strings.Split(statement, " ")
	ret.OpCode = map[string]OpCode{
		"ADD": ADD,
		"DIV": DIV,
		"EQL": EQL,
		"INP": INP,
		"MOD": MOD,
		"MUL": MUL,
	}[strings.ToUpper(parts[0])]

	var ok bool
	ret.Arg1.Register, ok = map[string]int{
		"W": RegW,
		"X": RegX,
		"Y": RegY,
		"Z": RegZ,
	}[strings.ToUpper(parts[1])]
	if !ok {
		ret.Arg1.Register = -1
		var err error
		ret.Arg1.Immediate, err = strconv.Atoi(parts[1])
		if err != nil {
			return Statement{}, fmt.Errorf("statement %q: %v", statement, err)
		}
	}

	if len(parts) >= 3 {
		ret.Arg2.Register, ok = map[string]int{
			"W": RegW,
			"X": RegX,
			"Y": RegY,
			"Z": RegZ,
		}[strings.ToUpper(parts[2])]
		if !ok {
			ret.Arg2.Register = -1
			var err error
			ret.Arg2.Immediate, err = strconv.Atoi(parts[2])
			if err != nil {
				return Statement{}, fmt.Errorf("statement %q: %v", statement, err)
			}
		}
	} else {
		ret.Arg2.Register = -2
	}

	return ret, nil
}

type Program []Statement

type ProgramCacheKey struct {
	ALU    ALU
	LineNo int
}

var ProgramCache = map[ProgramCacheKey]bool{}

// Run runs the program from the given line (an input) until just before the next input, at which point it recurses
func (p Program) Run(line int, alu *ALU) bool {
	//pck := ProgramCacheKey{*alu, line}
	//if result, ok := ProgramCache[pck]; ok {
	//	return result
	//}

	for statementNumber, statement := range p[line:] {
		if statement.OpCode == INP && statementNumber > 0 {
			result := p.Run(line+statementNumber, alu)
			//ProgramCache[pck] = result
			return result
		}
		if err := statement.Execute(alu); err != nil {
			log.Fatal(err)
		}
	}

	//ProgramCache[pck] = alu.Register[RegZ] == 0
	return alu.Register[RegZ] == 0
}

func main() {
	input, err := ioutil.ReadFile("input")
	// input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	var program Program
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}

		stmt, err := Compile(line)
		if err != nil {
			log.Fatal(err)
		}
		program = append(program, stmt)
	}

	var alu ALU
	for i := range alu.Input {
		alu.Input[i] = 9
	}

	reqs := make(chan [Digits]int, 100)
	var workers []reflect.SelectCase
	for i := 0; i < 100; i++ {
		res := make(chan [Digits]int)
		workers = append(workers, reflect.SelectCase{
			Dir:  reflect.SelectRecv,
			Chan: reflect.ValueOf(res),
		})
		go func(reqs <-chan [Digits]int, res chan<- [Digits]int) {
			var alu ALU
			for req := range reqs {
				alu.Input = req
				alu.Reset()
				if program.Run(0, &alu) {
					res <- req
				}
			}
			close(res)
		}(reqs, res)
	}

	go func(reqs chan<- [Digits]int) {
		var i int64
		start := time.Now()
		last := time.Now()
		var aluInput [Digits]int
		for i := range aluInput {
			aluInput[i] = 1
		}
		for {
			i++
			if time.Since(last) >= time.Second {
				ps := i / int64(time.Since(start).Seconds())
				fmt.Print("\r", aluInput, ps, 9*9*9*9*9*9*9*9*9*9*9*9*9*9/ps/3600/24)
				last = last.Add(time.Second)
			}
			reqs <- aluInput
			for i := Digits - 1; i >= 0; i-- {
				if aluInput[i] == 9 {
					if i == 0 {
						close(reqs)
						return
					}
					aluInput[i] = 1
				} else {
					aluInput[i]++
					break
				}
			}
		}
	}(reqs)

	_, res, ok := reflect.Select(workers)
	if !ok {
		panic("not ok receive?")
	}
	fmt.Println(res.Interface().([Digits]int))
}
