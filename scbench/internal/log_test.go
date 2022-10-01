package internal

import "testing"

func TestLog(t *testing.T) {
	type args struct {
		color   string
		message string
	}
	tests := []struct {
		name string
		args args
	}{
		{"red", args{"red", "yes this is red text"}},
		{"green", args{"green", "yes this is green text"}},
		{"cyan", args{"cyan", "yes this is cyan text"}},
		{"yellow", args{"yellow", "yes this is yellow text"}},
		{"purple", args{"purple", "yes this is purple text"}},
		{"white", args{"white", "yes this is white text"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			Log(tt.args.color, tt.args.message)
		})
	}

	NO_COLOR = true
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			Log(tt.args.color, tt.args.message)
		})
	}

	t.Cleanup(func() {
		NO_COLOR = false
	})
}
