# GscInterpreter

[![Build Status](https://travis-ci.org/Experiment5X/GscInterpreter.svg?branch=master)](https://travis-ci.org/Experiment5X/GscInterpreter)

This is an interpreter for the game scripting language used by MW2, CoD WaW, and CoD 4. 

This application will interpret basic GSC files and will check the syntax of any GSC file. Currently the features that are supported by the interpreter are:
 - Math expressions
 - Functions
 - Objects
 - For and while loops
 - If statements
 - Switch statements
 - Printing

### Usage
To compile the application run:
```
stack install
```

To run the application:
```
codgsc [FILES]
```

Usage:
```
Call of Duty Gsc Interpreter v1.0, Adam Spindler

codgsc [OPTIONS] [FILE]

Common flags:
  -c --check            Only check the files for syntax
  -p --printast         Print the parsed AST data structure when checking for
                        syntax errors
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```
All of the unit tests can be run with:
```
stack test
```

A GSC file can be run by supplying the path to it as the only command line argument.

### Syntax Checker
```
➤ codgsc -c "/Users/adamspindler/Developer/MW2 Code/test/test.gsc"
/Users/adamspindler/Developer/MW2 Code/test/test.gsc
1    isAwardExclusive( ref )
2    {
3        return ((5 + 2) * 5;
                            ⬆
(line 3, column 24):
unexpected ";"
expecting term
```

If the file parses successfully it won't print anything out. You can optionally print out the parsed AST data structure by supplying the `-p` option.

### Interpreter
If no arguments are supplied to the application it will enter a REPL. Here are some examples of the things you can do.
```
~> "Hello " + "world"
"Hello world"
~> ["apple", "orange", "strawberry"]
[0: "apple", 1: "orange", 2: "strawberry"]
~> person = [];
~> person.name = "Adam";
~> person.age = 24;
~> person
["age": 24, "name": "Adam"]
~> square(n) { return n * n; }
~> square(5)
25
~> value = 3 + 6 * 2;
~> value
15
```

There are additional examples in the `./examples/` directory which can be run with `codgsc FILENAME`. For example:
```
codgsc ./examples/merge_sort.gsc 
2
2
3
4
4
6
7
7
9
14
18
43
```

### Code Structure Overview
The application is composed 3 modules:
 - **Parser** : Contains all of the logic to parse the GSC language.
 - **Interpreter** : Contains all of the logic to interpret the parsed GSC code. The main component is a custom monad, GscM which is a combination of the Maybe monad and the State monad. If an error is encountered, evaluation will halt just like the Maybe monad. It also contains all the state of the application which includes the variables and their values as the code is being evaluated. This module depends on the Parser because it uses the parsed AST for evaluation.
 - **Main** : Contains all of the UI logic. This handles parsing the command line arguments and the logic for running the REPL and sending the input to the interpreter. This module depends on the interpreter because it calls it to execute the code that users give to the application.

### Library Dependencies
 - **Parsec** : This library is used for performing the actual parsing of the code. It supplies a method for parsing mathematical expressions easily and provides a `Parser` monad.
 - **HUnit** : This library is used for unit testing and provides methods to easily run all of the tests at once.
 - **containers** : This library provides the `Data.Map` data structure which is used for keeping track of the local variables and their values.
 - **haskline** : This library provides a function for reading input from stdin that allows users to delete characters they've previously entered and use the up arrow to scroll through previous inputs they've entered.
 - **cmdargs** : This library provides functionality for parsing command line arguments easily by creating a data type with all of the possible options. 

### Lessons Learned
Working on this project gave me a better understanding of how programming languages work. Particularly how variables are scoped when calling functions. For example, implementing closures required storing the variables available where the function was defined along with the function definition.

While implementing switch statements and if statements it was possible to consolidate both of them into a single data structure. The parser builds the same `ConditionalStatement` object and then the interpreter doesn't know which conditional statement type the user wrote.

### Abandoned Features
Originally the goal of the project was to include support for threads and communication between them. The GSC language includes the ability to easily spawn threads by doing:
```
thread myFunction(arguments);
```
Threads can wait on events from other threads by doing:
```
waittil("myEvent");
```
Other threads can push events with:
```
notify("myEvent");
```
The goal was to implement this as a single threaded event loop similar to JavaScript, however it was very difficult coming up with a monad design that would be easy to work with. The idea was to have a monad expire after a certain amount of steps, and have a thread manager that would swap the monads out after they expire. This proved to be too difficult for a project of this scale.
The threading feature could also be implemented using STM which may be easier than trying to keep it as an event loop.