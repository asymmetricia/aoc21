package aoc

import "strings"

func Before(haystack, needle string) string {
	pos := strings.Index(haystack, needle)
	if pos < 0 {
		return haystack
	}
	return haystack[:pos]
}

func After(haystack, needle string) string {
	pos := strings.Index(haystack, needle)
	if pos < 0 {
		return ""
	}
	return haystack[pos+len(needle):]
}
