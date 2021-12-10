package main

import "testing"

func TestParse(t *testing.T) {
	tests := []struct {
		name    string
		s       string
		open    rune
		want    int
		wantErr bool
	}{
		{"basic", "]", '[', 1, false},
		{"basic", "()]", '[', 3, false},
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := Parse(tt.s, tt.open)
			if (err != nil) != tt.wantErr {
				t.Errorf("Parse() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if got != tt.want {
				t.Errorf("Parse() got = %v, want %v", got, tt.want)
			}
		})
	}
}
