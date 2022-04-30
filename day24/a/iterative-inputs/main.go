package main

import (
	"bytes"
	"fmt"
	"github.com/sirupsen/logrus"
	"io/ioutil"
	"net/http"
	_ "net/http/pprof"
	"strconv"
	"strings"
	"time"
)

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

var log = logrus.StandardLogger()

type Register struct {
	Op         OpCode
	Immediate  int
	Reg1, Reg2 *Register
	Range      map[[2]int]int
	InputsFor  map[int][][14]int
}

// Compat returns true if the two input sets could exist simultaneously.
func Compat(a, b *[14]int) bool {
	return (a[0] == 0 || b[0] == 0 || a[0] == b[0]) &&
		(a[1] == 0 || b[1] == 0 || a[1] == b[1]) &&
		(a[2] == 0 || b[2] == 0 || a[2] == b[2]) &&
		(a[3] == 0 || b[3] == 0 || a[3] == b[3]) &&
		(a[4] == 0 || b[4] == 0 || a[4] == b[4]) &&
		(a[5] == 0 || b[5] == 0 || a[5] == b[5]) &&
		(a[6] == 0 || b[6] == 0 || a[6] == b[6]) &&
		(a[7] == 0 || b[7] == 0 || a[7] == b[7]) &&
		(a[8] == 0 || b[8] == 0 || a[8] == b[8]) &&
		(a[9] == 0 || b[9] == 0 || a[9] == b[9]) &&
		(a[10] == 0 || b[10] == 0 || a[10] == b[10]) &&
		(a[11] == 0 || b[11] == 0 || a[11] == b[11]) &&
		(a[12] == 0 || b[12] == 0 || a[12] == b[12]) &&
		(a[13] == 0 || b[13] == 0 || a[13] == b[13])
}

func Merge(a, b [][14]int) [][14]int {
	type ll struct {
		v    [14]int
		next *ll
	}
	var head, tail *ll
	var count int
	for aIdx := 0; aIdx < len(a); aIdx++ {
		for bIdx := 0; bIdx < len(b); bIdx++ {
			if !Compat(&a[aIdx], &b[bIdx]) {
				continue
			}
			count++
			if head == nil {
				head = new(ll)
				tail = head
			} else {
				tail.next = new(ll)
				tail = tail.next
			}

			for digit := 0; digit < 14; digit++ {
				if av := a[aIdx][digit]; av == 0 {
					tail.v[digit] = b[bIdx][digit]
				} else {
					tail.v[digit] = av
				}
			}
		}
	}

	ret := make([][14]int, count)
	for head != nil {
		count--
		ret[count] = head.v
		head = head.next
	}

	return ret
}

func (r *Register) String() string {
	if r.Op == Imm {
		return strconv.Itoa(r.Immediate)
	}
	if r.Op == Inp {
		return "$" + strconv.Itoa(r.Immediate)
	}
	if r.Reg2 == nil {
		return fmt.Sprintf("(%s %s ??)", r.Reg1.String(), OpNames[r.Op])
	}
	return fmt.Sprintf("(%s %s %s)", r.Reg1.String(), OpNames[r.Op], r.Reg2.String())
}

func (r *Register) Print() {
	if r == nil {
		return
	}

	if r.Op == Imm {
		fmt.Print(r.Immediate)
		return
	}
	if r.Op == Inp {
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
	if r == nil || r.Op == Imm {
		return 0
	}
	return 1 + r.Reg1.Count() + r.Reg2.Count()
}

func (r *Register) Run(inputs []int) int {
	switch r.Op {
	case Add:
		return r.Reg1.Run(inputs) + r.Reg2.Run(inputs)
	case Mul:
		return r.Reg1.Run(inputs) * r.Reg2.Run(inputs)
	case Div:
		return r.Reg1.Run(inputs) / r.Reg2.Run(inputs)
	case Mod:
		return r.Reg1.Run(inputs) % r.Reg2.Run(inputs)
	case Inp:
		return inputs[r.Immediate]
	case Imm:
		return r.Immediate
	case Eql:
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
		Op:        Imm,
		Immediate: v,
	}
}

var last time.Time

func plogf(fmt string, args ...interface{}) {
	if time.Since(last) > time.Second {
		last = time.Now()
		log.Printf(fmt, args...)
	}
}

func (r *Register) GetInputs() map[int][][14]int {
	if r.Op == Imm {
		return map[int][][14]int{
			r.Immediate: {{}},
		}
	}

	if r.Op == Inp {
		ret := map[int][][14]int{}
		for i := 1; i <= 9; i++ {
			ret[i] = [][14]int{{}}
			ret[i][0][r.Immediate] = i
		}
		return ret
	}

	ret := map[int][][14]int{}
	for k, v := range r.InputsFor {
		for _, i := range v {
			ret[k] = append(ret[k], i)
		}
	}
	return ret
}

var registers = map[string]*Register{}

func main() {
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

	inputIdx := 0

	log.Print("compiling...")

	for lineno, line := range lines {
		log := log.WithFields(map[string]interface{}{
			"lineno": lineno,
			"line":   line,
		})
		_ = lineno
		if strings.TrimSpace(line) == "" {
			continue
		}
		//fmt.Print("\r\x1B[K", time.Now().Format("[03:04:05]"), " ", lineno, "/", len(lines), ": ", line)

		parts := strings.Split(line, " ")
		reg := &Register{}
		opc, ok := OpCodes[parts[0]]
		if !ok {
			panic(parts[0])
		}
		reg.Op = opc

		if reg.Op == Inp {
			reg.Immediate = inputIdx
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
		case Mul:
			if reg.Reg1.Op == Imm && reg.Reg2.Op == Imm {
				reg = Immediate(reg.Reg1.Immediate * reg.Reg2.Immediate)
			} else if reg.Reg1.Op == Imm && reg.Reg1.Immediate == 0 ||
				reg.Reg2.Op == Imm && reg.Reg2.Immediate == 0 {
				reg = Immediate(0)
			} else if reg.Reg1.Op == Imm && reg.Reg1.Immediate == 1 {
				reg = reg.Reg2
			} else if reg.Reg2.Op == Imm && reg.Reg2.Immediate == 1 {
				reg = reg.Reg1
			}
		case Add:
			if reg.Reg1.Op == Imm && reg.Reg2.Op == Imm {
				reg = Immediate(reg.Reg1.Immediate + reg.Reg2.Immediate)
			} else if reg.Reg2.Op == Imm && reg.Reg2.Immediate == 0 {
				reg = reg.Reg1
			} else if reg.Reg1.Op == Imm && reg.Reg1.Immediate == 0 {
				reg = reg.Reg2
			}
		case Div:
			if reg.Reg1.Op == Imm && reg.Reg2.Op == Imm {
				reg = Immediate(reg.Reg1.Immediate / reg.Reg2.Immediate)
			} else if reg.Reg2.Op == Imm && reg.Reg2.Immediate == 1 {
				reg = reg.Reg1
			}
		case Eql:
			if reg.Reg1.Op == Imm && reg.Reg2.Op == Imm {
				if reg.Reg1.Immediate == reg.Reg2.Immediate {
					reg = Immediate(1)
				} else {
					reg = Immediate(0)
				}
			}
		case Mod:
			if reg.Reg1.Op == Imm && reg.Reg2.Op == Imm {
				reg = Immediate(reg.Reg1.Immediate % reg.Reg2.Immediate)
			}
		}

		log = log.WithField("op", reg.Op)

		if reg.Op != Imm && reg.Op != Inp {
			r1inputs, r2inputs := reg.Reg1.GetInputs(), reg.Reg2.GetInputs()
			log.Printf("computing %d x %d = %d points", len(r1inputs), len(r2inputs), len(r1inputs)*len(r2inputs))

			reg.InputsFor = make(map[int][][14]int, len(r1inputs)*len(r2inputs))

			for x, r1ifs := range r1inputs {
				for y, r2ifs := range r2inputs {
					merged := Merge(r1ifs, r2ifs)
					if len(merged) == 0 {
						continue
					}
					var v int
					switch reg.Op {
					case Add:
						v = x + y

					case Div:
						if y == 0 {
							continue
						}
						v = x / y
					case Eql:
						if x == y {
							v = 1
						} else {
							v = 0
						}
					case Mod:
						v = x % y
					case Mul:
						v = x * y
					}

					reg.InputsFor[v] = append(reg.InputsFor[v], merged...)
				}
				plogf("%d outputs", len(reg.InputsFor))
			}
		}

		if len(reg.InputsFor) == 1 {
			var regValue int
			for k := range reg.InputsFor {
				regValue = k
			}
			reg = Immediate(regValue)
		}

		log = log.WithField("op", reg.Op)
		log.Printf("%d in range, %d in if", len(reg.Range), len(reg.InputsFor))

		registers[parts[1]] = reg
	}
	log.Print("done!")

	fmt.Println(len(registers["z"].Range))
	fmt.Println(registers["z"].InputsFor[0])
}
