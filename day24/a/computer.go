package main

import (
	"fmt"
	"github.com/asymmetricia/aoc21/aoc"
	"github.com/asymmetricia/aoc21/day24/a/winnow"
	"strings"
	"time"
)

type Computer struct {
	X, Y, Z, W Register
}

func (c *Computer) String() string {
	return strings.Join([]string{
		fmt.Sprintf("X: %v", c.X),
		fmt.Sprintf("Y: %v", c.Y),
		fmt.Sprintf("Z: %v", c.Z),
		fmt.Sprintf("W: %v", c.W),
	}, "\n")
}

func (c *Computer) Run(program []*Instruction, input [14]int) {
	for _, i := range program {
		c.Execute(i, [][14]int{input})
	}
}

func (c *Computer) Execute(instruction *Instruction, input [][14]int) {
	a := c.Register(instruction.AReg)

	if instruction.Op == winnow.Inp {
		*a = Register{}
		for _, i := range input {
			*a = append(*a, Input(i[instruction.BImm])...)
		}
		*a = aoc.Unique(*a)
		return
	}

	if instruction.Op == winnow.Mul && instruction.BReg == nil && instruction.BImm == 0 {
		*a = Register{0}
		return
	}

	var b *Register
	if instruction.BReg == nil {
		b = &Register{instruction.BImm}
	} else {
		b = c.Register(*instruction.BReg)
	}

	values := Register{}
	for _, a := range *a {
		for _, b := range *b {
			var v int
			switch instruction.Op {
			case winnow.Add:
				v = a + b
			case winnow.Div:
				v = a / b
			case winnow.Mod:
				v = a % b
			case winnow.Mul:
				v = a * b
			case winnow.Eql:
				if a == b {
					v = 1
				}
			}
			values = append(values, v)
		}
	}

	values = aoc.Unique(values)
	*a = values
}

func (c *Computer) ExecuteWinnow(line int, instruction *Instruction, tree map[string]*winnow.TreeInstruction, inputs [][14]int) [][14]int {
	log := log.WithField("line", line)

	if instruction.Op != winnow.Inp {
		panic("cannot ExecuteWinnow non-inp op " + instruction.Op.String())
	}

	if inputs == nil {
		inputs = [][14]int{{}}
	}

	inputIdx := instruction.BImm
	log = log.WithField("input", inputIdx)
	// Check every value for this input
	var ret [][14]int
	var options []int

	total := len(inputs) * 9
	for c, input := range inputs {
		for i := 1; i <= 9; i++ {
			start := time.Now()
			input[inputIdx] = i
			if tree["z"].CanZero(input) {
				options = append(options, i)
				ret = append(ret, input)
				log.Debugf("%v -> yes! (%0.2fs, %d%%)", input, time.Since(start).Seconds(), (c*9+i-1)*100/total)
			} else {
				log.Debugf("%v -> no (%0.2fs, %d%%)", input, time.Since(start).Seconds(), (c*9+i-1)*100/total)
			}
		}
	}
	options = aoc.Unique(options)
	log.Printf("%d options for input %d (%v), %d candidates", len(options), inputIdx, options, len(ret))

	if len(ret) == 0 {
		panic("no solutions?!")
	}

	saveTo := c.Register(instruction.AReg)
	*saveTo = options
	return ret
}

func (c *Computer) Register(r rune) *Register {
	var ret *Register
	switch r {
	case 'x':
		ret = &c.X
	case 'y':
		ret = &c.Y
	case 'z':
		ret = &c.Z
	case 'w':
		ret = &c.W
	}
	if len(*ret) == 0 {
		*ret = Register{0}
	}
	return ret
}

func (c Computer) Copy() Computer {
	ret := Computer{
		X: make(Register, len(c.X)),
		Y: make(Register, len(c.Y)),
		Z: make(Register, len(c.Z)),
		W: make(Register, len(c.W)),
	}
	copy(ret.X, c.X)
	copy(ret.Y, c.Y)
	copy(ret.Z, c.Z)
	copy(ret.W, c.W)
	return ret
}
