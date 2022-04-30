package main

import (
	"fmt"
	. "github.com/asymmetricia/aoc21/day24/a/winnow"
	"strconv"
	"strings"
)

type Instruction struct {
	Op   OpCode
	AReg rune
	BReg *rune
	BImm int
}

func (i *Instruction) String() string {
	if i.BReg == nil {
		return fmt.Sprintf("%s %c %d", i.Op, i.AReg, i.BImm)
	}
	return fmt.Sprintf("%s %c %c", i.Op, i.AReg, *i.BReg)
}

func Compile(lines []string) []*Instruction {
	var instructions []*Instruction
	var inputIdx int

	for lineno, line := range lines {
		_ = lineno
		if strings.TrimSpace(line) == "" {
			continue
		}

		parts := strings.Split(line, " ")
		instruction := &Instruction{}
		opc, ok := OpCodes[parts[0]]
		if !ok {
			panic("bad opcode " + parts[0])
		}
		instruction.Op = opc
		instruction.AReg = rune(parts[1][0])
		instructions = append(instructions, instruction)

		if instruction.Op == Inp {
			instruction.BImm = inputIdx
			inputIdx++
			continue
		}

		if v, err := strconv.Atoi(parts[2]); err == nil {
			instruction.BImm = v
		} else {
			instruction.BReg = new(rune)
			*instruction.BReg = rune(parts[2][0])
		}
	}

	return instructions
}

type Register []int

func (r Register) String() string {
	var ret strings.Builder
	for _, v := range r {
		ret.WriteString(strconv.Itoa(v))
		ret.WriteString(", ")
	}
	ret.WriteString("\n   ")
	return strings.TrimSpace(ret.String())
}

func (r Register) HasZero() bool {
	for _, i := range r {
		if i == 0 {
			return true
		}
	}
	return false
}

func Input(i int) Register {
	if i == 0 {
		return Register{1, 2, 3, 4, 5, 6, 7, 8, 9}
	} else {
		return Register{i}
	}
}
