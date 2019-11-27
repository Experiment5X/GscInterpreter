# GscInterpreter

[![Build Status](https://travis-ci.org/Experiment5X/GscInterpreter.svg?branch=master)](https://travis-ci.org/Experiment5X/GscInterpreter)

This is an interpreter for the game scripting language used by MW2, CoD WaW, and CoD 4. 

So far this application will parse the code (aka check for syntax errors) and interpret basic mathematical expressions. 

### Usage
To compile the application run:
```
stack install
```

To run the application:
```
GscInterpeter-exe [files]
```
You can optionally supply the names of files to parse. If there are parse errors the application will print out a few lines before the one that has issues, and display an arrow pointing to the exact character that the parser choked on. Due to the way this is written, the arrow may not be in the most helpful place but I'm working on improving it. 

All of the unit tests can be run with:
```
stack test
```

### Parser
```
➤ GscInterpreter-exe "/Users/adamspindler/Developer/MW2 Code/test/test.gsc"
1    isAwardExclusive( ref )
2    {
3        return ((5 + 2) * 5;
                            ⬆
(line 3, column 24):
unexpected ";"
expecting term
```

If the file parses successfully, it will print out the AST data structure.

### Interpreter
```
~> 3 + 2 * 6
VInt 15
~> (3 + 2) * 6
VInt 30
~> 1 / 8
VInt 0
~> 5 / 6
VInt 0
~> 5.0 / 4
VDouble 1.25
```
