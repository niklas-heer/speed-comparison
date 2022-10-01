package internal

import (
	"testing"
	"os"
)

func Test_format(t *testing.T) {
	type args struct {
		text   string
		params map[string]string
	}

	tests := []struct {
		name string
		args args
		want string
	}{
		{
			name: "empty test", 
			args: args{"", map[string]string{}}, 
			want: "",
		},

		{
			name: "name test", 
			args: args{"hello this is me, ${name}", map[string]string{"name": "Shravan"}}, 
			want: "hello this is me, Shravan",
		},

		{
			name: "long sentence test", 
			args: args{"${go} offers cool concurrency features like ${c1} and ${c2}. and it's ${adj}!", map[string]string{"go": "Golang", "c1": "goroutines", "c2": "channels", "adj": "amazing"}}, 
			want: "Golang offers cool concurrency features like goroutines and channels. and it's amazing!",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := format(tt.args.text, tt.args.params); got != tt.want {
				t.Errorf("format() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_writeToFile(t *testing.T) {
	type args struct {
		text     string
		filename string
	}
	tests := []struct {
		name    string
		args    args
		wantErr bool
	}{
		{
			name: "write to file",
			args: args{
				text:     "hello this is me, name",
				filename: "test.txt",
			},
			wantErr: false,
		},
		{
			name: "write to file error",
			args: args{
				text:     "this test must fail",
				filename: "&*$*hvsgrv87@#$/|\\",
			},
			wantErr: true,
		},
	}
	
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := writeToFile(tt.args.text, tt.args.filename); (err != nil) != tt.wantErr {
				t.Errorf("writeToFile() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}

	t.Cleanup(func() {
		if err := os.Remove("./test.txt"); err != nil {
			t.Errorf("Error removing file: %v", err)
		}
	})

}
