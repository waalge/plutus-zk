# Plutus-zk 

> The validator of sudoku snark

## About

This repo uses nix flakes. 

The main program is `./groth16`.
From a devshell and from `./groth16` directory run the program with 
```
  cabal run .
```

The program expects to find three files to read in:

- `params.json`
- `datum.json`
- `redeemer.json`

as produced by the [sudoku-snark](https://github.com/waalge/sudoku-snark) rust package. 

It prints:

- the verifier's result success or fail 
ie whether the groth16 setup and proof are valid
- stats on the validator's size. 
- the compiled code

## Sources

This repo borrows heavily from the plutus repo 
