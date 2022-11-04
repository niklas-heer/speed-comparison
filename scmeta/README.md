# scmeta

This tool sit's on top of the hyperfine results and combines it with out metadata about a given language.

Files needed as input: (see example-files)
    hyperfine.json
    pi.txt

## Implementation

This implementation uses Clojure and [Babashka](https://github.com/babashka/babashka) to work.

## Usage

```plaintext
SCMeta.

Usage:
  scmeta [options]

Options:
  --ln=<name>   Name of the language.
  --lv=<cmd>    Command to get the language version.
  --lvi=<int>   If there are multiple versions for `lv`, select the index [default: 0].
  --hf=<json>   Hyperfine JSON file path.
  --pi=<txt>    Path to pi.txt file.
  --out=<json>  Path to write the output to
```

This is an example for the Crystal language:

```bash
./scmeta.clj --hf=./example-files/hyperfine.json --pi="./example-files/pi.txt" --out="./out.json" --ln="Crystal" --lv="crystal --version"
```
