package internal

import (
	"encoding/json"
	"os"
	"text/template"
)

var summaryNoColor = `
Benchmarking Summary
--------------------

Started:            {{ .Started }} 
Ended:              {{ .Ended }} 
Language:           {{ .Language }} 
Version:            {{ .Version }} 
Executed Command:   {{ .Command }} 
Accuracy:           {{ .Accuracy }} 
Calculated Pi:      {{ .CalculatedPi }} 
Total iterations:   {{ .Iterations }} 
Average time taken: {{ .Average }} 
`

var summaryColor = `
${blue}Benchmarking Summary ${reset}
${blue}-------------------- ${reset}

${yellow}Started:            ${green}{{ .Started }} ${reset}
${yellow}Ended:              ${green}{{ .Ended }} ${reset}
${yellow}Language:           ${green}{{ .Language }} ${reset}
${yellow}Version:            ${green}{{ .Version }} ${reset}
${yellow}Executed Command:   ${green}{{ .Command }} ${reset}
${yellow}Accuracy:           ${green}{{ .Accuracy }} ${reset}
${yellow}Calculated Pi:      ${green}{{ .CalculatedPi }} ${reset}
${yellow}Total iterations:   ${green}{{ .Iterations }} ${reset}
${yellow}Average time taken: ${green}{{ .Average }} ${reset}
`

// Consolify prints the benchmark summary of the Result struct to the console, with color codes.
func (result *Result) Consolify() {
	// * result text
	text := format(summaryColor,
		map[string]string{"blue": CYAN, "yellow": YELLOW, "green": GREEN, "reset": RESET})

	if NO_COLOR {
		text = summaryNoColor
	}

	// * parsing the template
	tmpl, err := template.New("result").Parse(text)
	if err != nil {
		panic(err)
	}
	err = tmpl.Execute(os.Stdout, result)
	if err != nil {
		panic(err)
	}
}

// textify writes the benchmark summary of the Result struct to a text file.
func textify(r *Result) {
	text := summaryNoColor

	tmpl, err := template.New("summary").Parse(text)
	if err != nil {
		panic(err)
	}

	f, ferr := os.Create("scbench-summary.txt")
	if ferr != nil {
		Log("red", "Failed to create the file.")
	}
	defer f.Close()
	if terr := tmpl.Execute(f, r); terr != nil {
		Log("red", "Failed to write to the file.")
	}
}

// jsonify converts the Result struct to JSON.
func jsonify(r *Result) ([]byte, error) {
	return json.MarshalIndent(r, "", "    ")
}

// Export writes the benchmark summary of the Result struct to a file in the specified format.
func (result *Result) Export(exportFormat string) {
	// * exporting the results
	if exportFormat == "json" {
		jsonText, e := jsonify(result)
		if e != nil {
			Log("red", "Failed to export the results to json.")
			return
		}
		writeToFile(string(jsonText), "scbench-summary.json")

	} else if exportFormat == "text" {
		textify(result)

	} else if exportFormat != "none" {
		Log("red", "Invalid export format: " + exportFormat + ".")
	}
}
