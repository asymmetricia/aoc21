package winnow

import (
	"github.com/asymmetricia/aoc21/aoc"
	"github.com/sirupsen/logrus"
	"strconv"
	"strings"
)

type TreeInstruction struct {
	Op         OpCode
	Immediate  int
	Reg1, Reg2 *TreeInstruction
	Outputs    []int
}

type OpCode uint8

func (o OpCode) String() string {
	return OpNames[o]
}

const (
	Imm OpCode = iota
	Inp
	Add
	Mul
	Div
	Mod
	Eql
)

var OpCodes = map[string]OpCode{
	"imm": Imm,
	"inp": Inp,
	"add": Add,
	"mul": Mul,
	"div": Div,
	"mod": Mod,
	"eql": Eql,
}

var OpNames = map[OpCode]string{}

func init() {
	for n, c := range OpCodes {
		OpNames[c] = n
	}
}

func (t *TreeInstruction) String() string {
	if t.Op == Imm {
		return strconv.Itoa(t.Immediate)
	}
	if t.Op == Inp {
		return "$" + strconv.Itoa(t.Immediate)
	}
	return "(" + t.Op.String() + " " + t.Reg1.String() + " " + t.Reg2.String() + ")"
}

func Immediate(v int) *TreeInstruction {
	ret := &TreeInstruction{
		Op:        Imm,
		Immediate: v,
	}
	ret.Outputs = []int{v}
	return ret
}

func CompileTree(lines []string) map[string]*TreeInstruction {
	registers := map[string]*TreeInstruction{}
	var inputIdx int

	for lineno, line := range lines {
		_ = lineno
		if strings.TrimSpace(line) == "" {
			continue
		}

		parts := strings.Split(line, " ")
		instruction := &TreeInstruction{}
		opc, ok := OpCodes[parts[0]]
		if !ok {
			panic(parts[0])
		}
		instruction.Op = opc

		if instruction.Op == Inp {
			instruction.Immediate = inputIdx
			inputIdx++
			registers[parts[1]] = instruction
			instruction.Outputs = []int{1, 2, 3, 4, 5, 6, 7, 8, 9}
			continue
		}

		// arg1 must always be a register
		if reg1, ok := registers[parts[1]]; ok {
			instruction.Reg1 = reg1
		} else {
			instruction.Reg1 = Immediate(0)
		}

		if v, err := strconv.Atoi(parts[2]); err == nil {
			instruction.Reg2 = Immediate(v)
		} else if reg2, ok := registers[parts[2]]; ok {
			instruction.Reg2 = reg2
		} else {
			instruction.Reg2 = Immediate(0)
		}

		// optimize!
		switch instruction.Op {
		case Mul:
			if instruction.Reg1.Op == Imm && instruction.Reg2.Op == Imm {
				instruction = Immediate(instruction.Reg1.Immediate * instruction.Reg2.Immediate)
			} else if instruction.Reg1.Op == Imm && instruction.Reg1.Immediate == 0 ||
				instruction.Reg2.Op == Imm && instruction.Reg2.Immediate == 0 {
				instruction = Immediate(0)
			} else if instruction.Reg1.Op == Imm && instruction.Reg1.Immediate == 1 {
				instruction = instruction.Reg2
			} else if instruction.Reg2.Op == Imm && instruction.Reg2.Immediate == 1 {
				instruction = instruction.Reg1
			}
		case Add:
			if instruction.Reg1.Op == Imm && instruction.Reg2.Op == Imm {
				instruction = Immediate(instruction.Reg1.Immediate + instruction.Reg2.Immediate)
			} else if instruction.Reg2.Op == Imm && instruction.Reg2.Immediate == 0 {
				instruction = instruction.Reg1
			} else if instruction.Reg1.Op == Imm && instruction.Reg1.Immediate == 0 {
				instruction = instruction.Reg2
			}
		case Div:
			if instruction.Reg1.Op == Imm && instruction.Reg2.Op == Imm {
				instruction = Immediate(instruction.Reg1.Immediate / instruction.Reg2.Immediate)
			} else if instruction.Reg2.Op == Imm && instruction.Reg2.Immediate == 1 {
				instruction = instruction.Reg1
			}
		case Eql:
			if instruction.Reg1.Op == Imm && instruction.Reg2.Op == Imm {
				if instruction.Reg1.Immediate == instruction.Reg2.Immediate {
					instruction = Immediate(1)
				} else {
					instruction = Immediate(0)
				}
			}
		case Mod:
			if instruction.Reg1.Op == Imm && instruction.Reg2.Op == Imm {
				instruction = Immediate(instruction.Reg1.Immediate % instruction.Reg2.Immediate)
			}
		}

		instruction.ComputeOutputs([14]int{})
		if len(instruction.Outputs) == 1 {
			instruction = Immediate(instruction.Outputs[0])
		}
		registers[parts[1]] = instruction
	}

	return registers
}

func (t *TreeInstruction) ComputeOutputs(input [14]int) (bool, int) {
	if t.Op == Imm {
		if len(t.Outputs) != 1 {
			t.Outputs = []int{t.Immediate}
			return true, 1
		}
		return false, 1
	}

	if t.Op == Inp {
		targetValue := input[t.Immediate]
		if targetValue == 0 {
			if len(t.Outputs) != 9 {
				t.Outputs = []int{1, 2, 3, 4, 5, 6, 7, 8, 9}
				return true, 1
			}

			return false, 1
		}

		if len(t.Outputs) != 1 {
			t.Outputs = []int{targetValue}
			return true, 1
		}
		if t.Outputs[0] != targetValue {
			t.Outputs[0] = targetValue
			return true, 1
		}

		return false, 1
	}

	r1, v1 := t.Reg1.ComputeOutputs(input)
	r2, v2 := t.Reg2.ComputeOutputs(input)
	if !r1 && !r2 && t.Outputs != nil {
		return false, v1 + v2 + 1
	}

	t.Outputs = t.Outputs[0:0]
	for i := 0; i < len(t.Reg1.Outputs); i++ {
		a := t.Reg1.Outputs[i]
		for j := 0; j < len(t.Reg2.Outputs); j++ {
			b := t.Reg2.Outputs[j]
			var v int
			switch t.Op {
			case Add:
				v = a + b
			case Div:
				v = a / b
			case Mod:
				v = a % b
			case Mul:
				v = a * b
			case Eql:
				if a == b {
					v = 1
				}
			}
			t.Outputs = append(t.Outputs, v)
		}
	}
	t.Outputs = aoc.Unique(t.Outputs)
	return true, v1 + v2 + 1
}

func Merge(a, b [14]int) ([14]int, bool) {
	var ret [14]int
	for i := 0; i < 13; i++ {
		if a[i] == 0 || a[i] == b[i] {
			ret[i] = b[i]
		} else if b[i] == 0 {
			ret[i] = a[i]
		} else {
			return [14]int{}, false
		}
	}
	return ret, true
}

func (t *TreeInstruction) CanZero(input [14]int) bool {
	_, visits := t.ComputeOutputs(input)
	logrus.Debugf("canzero visited %d nodes", visits)
	for _, o := range t.Outputs {
		if o == 0 {
			return true
		}
	}
	return false
}
