package main

import (
	"bytes"
	"fmt"
	"github.com/sirupsen/logrus"
	"io/ioutil"
	"strconv"
	"strings"
)

var log = logrus.StandardLogger()

type Value struct {
	Inputs [14]int
	Output int
}

type Register struct {
	Op         string
	Immediate  int
	Reg1, Reg2 *Register
	Min, Max   int
	vc         []Value
}

var AnyInput = [14]int{}

// Compat returns true if the two input sets could exist simultaneously.
func Compat(a, b [14]int) bool {
	for i, va := range a {
		vb := b[i]
		if va != 0 && vb != 0 && va != vb {
			return false
		}
	}
	return true
}

func Merge(a, b [14]int) [14]int {
	var ret [14]int
	for i, va := range a {
		vb := b[i]
		if va == 0 {
			ret[i] = vb
		} else {
			ret[i] = va
		}
	}
	return ret
}

func (r *Register) Values(depth int) []Value {
	if r.vc != nil {
		return r.vc
	}

	switch r.Op {
	case "imm":
		r.vc = []Value{
			{AnyInput, r.Immediate},
		}
		return r.vc
	case "inp":
		for i := 1; i <= 9; i++ {
			inp := AnyInput
			inp[r.Immediate] = i
			r.vc = append(r.vc, Value{
				inp,
				i,
			})
		}
		return r.vc
	}

	r1v := r.Reg1.Values(depth + 1)
	r2v := r.Reg2.Values(depth + 1)
	fmt.Printf("%s@%d %dx%d\n", r.Op, depth, len(r1v), len(r2v))
	var ret []Value
	for _, i := range r1v {
		for _, j := range r2v {
			if !Compat(i.Inputs, j.Inputs) {
				continue
			}
			var v int
			switch r.Op {
			case "add":
				v = i.Output + j.Output
			case "mul":
				v = i.Output + j.Output
			case "div":
				v = i.Output / j.Output
			case "mod":
				v = i.Output % j.Output
			case "eql":
				if i.Output == j.Output {
					v = 1
				} else {
					v = 0
				}
			default:
				panic(fmt.Sprintf("%s.Values", r.Op))
			}
			ret = append(ret, Value{Merge(i.Inputs, j.Inputs), v})
		}
	}

	r.vc = ret
	return ret
}

//func (r *Register) Go() string {
//	switch r.Op {
//	case "inp":
//		return fmt.Sprintf("inputs[%d]", r.Immediate)
//	case "imm":
//		return strconv.Itoa(r.Immediate)
//	case "add":
//		return "(" + r.Reg1.Go() + "+" + r.Reg2.Go() + ")"
//	case "mul":
//		return r.Reg1.Go() + "*" + r.Reg2.Go()
//	case "div":
//		return r.Reg1.Go() + "/" + r.Reg2.Go()
//	case "mod":
//		return r.Reg1.Go() + "%" + r.Reg2.Go()
//	case "eql":
//
//		return fmt.Sprintf("eql(%s,%s)", r.Reg1.Go(), r.Reg2.Go())
//	default:
//		panic(r.Op)
//	}
//}

func (r *Register) String() string {
	if r.Op == "imm" {
		return strconv.Itoa(r.Immediate)
	}
	if r.Op == "inp" {
		return "$" + strconv.Itoa(r.Immediate)
	}
	if r.Reg2 == nil {
		return fmt.Sprintf("(%s %s ??)", r.Reg1.String(), r.Op)
	}
	return fmt.Sprintf("(%s %s %s)", r.Reg1.String(), r.Op, r.Reg2.String())
}

func (r *Register) Print() {
	if r == nil {
		return
	}

	if r.Op == "imm" {
		fmt.Print(r.Immediate)
		return
	}
	if r.Op == "inp" {
		fmt.Print("$", r.Immediate)
		return
	}

	fmt.Print("(", r.Op, " ")
	r.Reg1.Print()
	fmt.Print(" ")
	r.Reg2.Print()
	fmt.Print(")")
}

func (r *Register) Count() int {
	if r == nil || r.Op == "imm" {
		return 0
	}
	return 1 + r.Reg1.Count() + r.Reg2.Count()
}

func (r *Register) Run(inputs []int) int {
	switch r.Op {
	case "add":
		return r.Reg1.Run(inputs) + r.Reg2.Run(inputs)
	case "mul":
		return r.Reg1.Run(inputs) * r.Reg2.Run(inputs)
	case "div":
		return r.Reg1.Run(inputs) / r.Reg2.Run(inputs)
	case "mod":
		return r.Reg1.Run(inputs) % r.Reg2.Run(inputs)
	case "inp":
		return inputs[r.Immediate]
	case "imm":
		return r.Immediate
	case "eql":
		if r.Reg1.Run(inputs) == r.Reg2.Run(inputs) {
			return 1
		}
		return 0
	default:
		panic(r.Op)
	}
}

func Immediate(v int) *Register {
	return &Register{
		Op:        "imm",
		Immediate: v,
		Min:       v,
		Max:       v,
	}
}

func main() {
	input, err := ioutil.ReadFile("input")
	// input, err := ioutil.ReadFile("test")
	if err != nil {
		log.WithError(err).Fatal("could not read input")
	}
	input = bytes.TrimSpace(input)

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	registers := map[string]*Register{}
	inputIdx := 0

	for lineno, line := range lines {
		_ = lineno
		if strings.TrimSpace(line) == "" {
			continue
		}

		parts := strings.Split(line, " ")
		reg := &Register{
			Op: parts[0],
		}

		if reg.Op == "inp" {
			reg.Immediate = inputIdx
			reg.Min = 1
			reg.Max = 9
			inputIdx++
			//fmt.Println(line, reg.Count())
			registers[parts[1]] = reg
			continue
		}

		// arg1 must always be a register
		if reg1, ok := registers[parts[1]]; ok {
			reg.Reg1 = reg1
		} else {
			reg.Reg1 = Immediate(0)
		}

		if v, err := strconv.Atoi(parts[2]); err == nil {
			reg.Reg2 = Immediate(v)
		} else if reg2, ok := registers[parts[2]]; ok {
			reg.Reg2 = reg2
		} else {
			reg.Reg2 = Immediate(0)
		}

		switch reg.Op {
		case "mul":
			if reg.Reg1.Op == "imm" && reg.Reg2.Op == "imm" {
				reg = Immediate(reg.Reg1.Immediate * reg.Reg2.Immediate)
			} else if reg.Reg1.Op == "imm" && reg.Reg1.Immediate == 0 ||
				reg.Reg2.Op == "imm" && reg.Reg2.Immediate == 0 {
				reg = Immediate(0)
			} else if reg.Reg1.Op == "imm" && reg.Reg1.Immediate == 1 {
				reg = reg.Reg2
			} else if reg.Reg2.Op == "imm" && reg.Reg2.Immediate == 1 {
				reg = reg.Reg1
			} else {
				reg.Min = reg.Reg1.Min * reg.Reg2.Min
				reg.Max = reg.Reg1.Max * reg.Reg2.Max
			}
		case "add":
			if reg.Reg1.Op == "imm" && reg.Reg2.Op == "imm" {
				reg = Immediate(reg.Reg1.Immediate + reg.Reg2.Immediate)
			} else if reg.Reg2.Op == "imm" && reg.Reg2.Immediate == 0 {
				reg = reg.Reg1
			} else if reg.Reg1.Op == "imm" && reg.Reg1.Immediate == 0 {
				reg = reg.Reg2
			} else {
				reg.Min = reg.Reg1.Min + reg.Reg2.Min
				reg.Max = reg.Reg1.Max + reg.Reg2.Max
			}
		case "div":
			if reg.Reg1.Op == "imm" && reg.Reg2.Op == "imm" {
				reg = Immediate(reg.Reg1.Immediate / reg.Reg2.Immediate)
			} else if reg.Reg2.Op == "imm" && reg.Reg2.Immediate == 1 {
				reg = reg.Reg1
			} else {
				reg.Min = reg.Reg1.Min / reg.Reg2.Max
				reg.Max = reg.Reg1.Max / reg.Reg2.Min
			}
		case "eql":
			if reg.Reg1.Op == "imm" && reg.Reg2.Op == "imm" {
				if reg.Reg1.Immediate == reg.Reg2.Immediate {
					reg = Immediate(1)
				} else {
					reg = Immediate(0)
				}
			} else if reg.Reg1.Max < reg.Reg2.Min || reg.Reg1.Min > reg.Reg2.Max {
				reg = Immediate(0)
			} else {
				reg.Min = 0
				reg.Max = 1
			}
		case "mod":
			if reg.Reg1.Op == "imm" && reg.Reg2.Op == "imm" {
				reg = Immediate(reg.Reg1.Immediate % reg.Reg2.Immediate)
			} else if reg.Reg2.Min > reg.Reg1.Max {
				reg = reg.Reg1
			} else {
				reg.Min = 0
				reg.Max = reg.Reg2.Max - 1
			}
		}

		if reg.Min == reg.Max && reg.Op != "imm" {
			reg = Immediate(reg.Min)
		}

		//fmt.Println(lineno, ":", line, "->", reg.Count())
		//reg.Print()
		//fmt.Println()
		//if reg.Count() > 1000 {
		//	os.Exit(1)
		//}
		registers[parts[1]] = reg
	}

	fmt.Println(registers["z"].Values(0))
	//fmt.Println("strict digraph {")
	//registers["z"].Dot(0)
	//fmt.Println("}")

	//fmt.Println(registers["w"].Count())
	//fmt.Println(registers["x"].Count())
	//fmt.Println(registers["y"].Count())
	//fmt.Println(registers["z"].Count())
	//fmt.Println(registers["z"].Run())
	//fmt.Println(registers["z"].ToGet(0))
	//registers["z"].Print()
	//os.Exit(0)
	//var i int
	//inputs := []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}
	//last := time.Now()
	//start := time.Now()
	//for {
	//	i++
	//	if time.Since(last) >= time.Second {
	//		fmt.Print("\r", inputs, " ", i, " ", float64(i)/time.Since(start).Seconds())
	//		last = time.Now()
	//	}
	//	result := registers["z"].Run(inputs)
	//	if result == 0 {
	//		fmt.Println(inputs)
	//	}
	//	for i := len(inputs) - 1; i >= 0; i-- {
	//		if inputs[i] == 9 && i == 0 {
	//			os.Exit(1)
	//		} else if inputs[i] == 9 {
	//			inputs[i] = 1
	//		} else {
	//			inputs[i]++
	//			break
	//		}
	//	}
	//}
	//fmt.Println(registers["z"].Go())
}
