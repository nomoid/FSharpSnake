# Card Game Snake
Programming language for creating turn-based card games.

Test out my project at [this link](http://assisstion.github.io/projects/cardgamelang/).

## Prerequisites
- [.NET Core](https://www.microsoft.com/net/download)

## Usage
- Build: `dotnet build`
- Run file: `dotnet run <filename>`

## Program Features
- Parsing & Interpreting
    - Multi-step parsing
    - Input validation
    - Line number information for debugging
    - Fail-fast exceptions
- Web Client
    - Parsing
    - Interpreting
    - Printing output

## Language Features
- Function definitions
    - Entry point (main() function)
- Function calls
    - Functions with arguments
    - Recursion
- Scope blocks
    - Indentation
- Variables
    - let statement
    - Assignment
    - Lexical scoping
    - Persistent
- Control flow
    - return statement
    - if/elif/else statements
    - while statement
    - for in statement
- Operators
    - Numeric (+, -, *, /, %)
    - Boolean (&, |)
    - Comparison (==, !=, <=, >=, <, >)
    - Unary (-, +, !)
    - Operator assignment (+=, -=, *=, /=, %=, &=, |=)
- Parenthases
- Lists
    - List accessor (e.g. a[b])
    - List setter (e.g. a[b] = c)
    - List builtin functions
        - concat(list1, list2)
            - Returns a new list with the two lists combined
        - len(list)
            - Returns the length of a list
        - pushf(list, item)
            - Pushes an item to the start of a list
        - popf(list)
            - Pops and returns an item from the start of a list
        - range(start, end)
            - Returns a list containing the interval [start, end) in ascending order
- Literals
    - String (e.g. "Hello, world!")
    - Integer (e.g. 5, -12)
    - Boolean (true/false)
    - List (e.g. [3, "abc", ["def", 12]])
    - Reference (this)
- References
    - Property accessor (e.g. print(player.name))
    - Property function call  (e.g. player.turn())
    - Property assignment (e.g. player.name = "Markus")
    - Creating new instances (e.g. player())

## Upcoming Features
- Card game engine

## Potential features
- Dictionaries
- Escape characters in strings
- String concatenation
- Multi-line statements/expressions
- Floating point numbers
- Lambda expressions
- Multi-file support
- External language support