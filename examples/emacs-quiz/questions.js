// Emacs Lisp Quiz Questions - Based on Official GNU Emacs Lisp Reference Manual
// Questions are organized by difficulty level and cover a wide range of topics

const QUIZ_QUESTIONS = [
    // BEGINNER LEVEL (Questions 1-20)
    {
        id: 1,
        difficulty: 'beginner',
        question: 'What does the function `car` return when applied to a list?',
        options: [
            'The first element',
            'The last element', 
            'The list length',
            'An empty list'
        ],
        correct: 0,
        explanation: 'The `car` function returns the first element of a list. For example, (car \'(a b c)) returns `a`.',
        reference: 'GNU Emacs Lisp Reference Manual - List Elements',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html'
    },
    {
        id: 2,
        difficulty: 'beginner',
        question: 'What does the function `cdr` return when applied to a list?',
        options: [
            'The first element',
            'The remaining elements',
            'The last element',
            'The element count'
        ],
        correct: 1,
        explanation: 'The `cdr` function returns the rest of the list after removing the first element. For example, (cdr \'(a b c)) returns (b c).',
        reference: 'GNU Emacs Lisp Reference Manual - List Elements',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html'
    },
    {
        id: 3,
        difficulty: 'beginner',
        question: 'Which special form is used to define a function in Emacs Lisp?',
        options: [
            'define',
            'defun',
            'function',
            'def'
        ],
        correct: 1,
        explanation: '`defun` is the standard special form for defining functions in Emacs Lisp. It creates a function and binds it to a symbol.',
        reference: 'GNU Emacs Lisp Reference Manual - Defining Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Functions.html'
    },
    {
        id: 4,
        difficulty: 'beginner',
        question: 'What is the empty list in Emacs Lisp?',
        options: [
            '()',
            'nil',
            'Both () and nil',
            'empty'
        ],
        correct: 2,
        explanation: 'In Emacs Lisp, the empty list is represented by both `()` and `nil`. They are equivalent and interchangeable.',
        reference: 'GNU Emacs Lisp Reference Manual - Cons Cell Type',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Cons-Cell-Type.html'
    },
    {
        id: 5,
        difficulty: 'beginner',
        question: 'Which function is used to construct a list from elements?',
        options: [
            'make-list',
            'list',
            'create-list',
            'new-list'
        ],
        correct: 1,
        explanation: 'The `list` function creates a list from its arguments. For example, (list 1 2 3) creates the list (1 2 3).',
        reference: 'GNU Emacs Lisp Reference Manual - Building Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html'
    },
    {
        id: 6,
        difficulty: 'beginner',
        question: 'What does the `cons` function do?',
        options: [
            'Builds a cons cell pair',
            'Counts list elements',
            'Converts string data',
            'Compares two lists'
        ],
        correct: 0,
        explanation: 'The `cons` function constructs a cons cell with its first argument as the CAR and second argument as the CDR. For example, (cons 1 2) creates (1 . 2).',
        reference: 'GNU Emacs Lisp Reference Manual - Building Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html'
    },
    {
        id: 7,
        difficulty: 'beginner',
        question: 'Which function is used to set the value of a variable?',
        options: [
            'set',
            'setq',
            'assign',
            'let'
        ],
        correct: 1,
        explanation: '`setq` is the most commonly used function to set variable values. It quotes its first argument (the variable name) automatically.',
        reference: 'GNU Emacs Lisp Reference Manual - Setting Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Variables.html'
    },
    {
        id: 8,
        difficulty: 'beginner',
        question: 'What is the truth value representing false in Emacs Lisp?',
        options: [
            'false',
            'nil',
            '()',
            'Both nil and ()'
        ],
        correct: 3,
        explanation: 'In Emacs Lisp, both `nil` and `()` represent false. Any non-nil value is considered true.',
        reference: 'GNU Emacs Lisp Reference Manual - nil and t',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html'
    },
    {
        id: 9,
        difficulty: 'beginner',
        question: 'What is the preferred way to represent the truth value true?',
        options: [
            'true',
            't',
            '1',
            'yes'
        ],
        correct: 1,
        explanation: 'The symbol `t` is the preferred way to represent the truth value true in Emacs Lisp, though any non-nil value is considered true.',
        reference: 'GNU Emacs Lisp Reference Manual - nil and t',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html'
    },
    {
        id: 10,
        difficulty: 'beginner',
        question: 'Which special form is used for conditional execution?',
        options: [
            'when',
            'if',
            'cond',
            'All of the above'
        ],
        correct: 3,
        explanation: 'Emacs Lisp provides several conditional forms: `if` for simple conditions, `when` for single-branch conditionals, and `cond` for multiple conditions.',
        reference: 'GNU Emacs Lisp Reference Manual - Conditionals',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html'
    },
    {
        id: 11,
        difficulty: 'beginner',
        question: 'How do you create a comment in Emacs Lisp?',
        options: [
            '// This is a comment',
            '/* This is a comment */',
            '; This is a comment',
            '# This is a comment'
        ],
        correct: 2,
        explanation: 'Comments in Emacs Lisp start with a semicolon (;) and continue to the end of the line.',
        reference: 'GNU Emacs Lisp Reference Manual - Comments',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Comments.html'
    },
    {
        id: 12,
        difficulty: 'beginner',
        question: 'What does the quote special form do?',
        options: [
            'Prints text to the screen',
            'Prevents evaluation of its argument',
            'Creates a string from its argument',
            'Calls a function with arguments'
        ],
        correct: 1,
        explanation: 'The `quote` special form (or its shorthand \') prevents evaluation of its argument and returns it literally.',
        reference: 'GNU Emacs Lisp Reference Manual - Quoting', 
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Quoting.html'
    },
    {
        id: 13,
        difficulty: 'beginner',
        question: 'Which function returns the length of a list?',
        options: [
            'size',
            'count',
            'length',
            'len'
        ],
        correct: 2,
        explanation: 'The `length` function returns the number of elements in a sequence, including lists.',
        reference: 'GNU Emacs Lisp Reference Manual - Sequence Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html'
    },
    {
        id: 14,
        difficulty: 'beginner',
        question: 'What is the result of (+ 3 4 5) in Emacs Lisp?',
        options: [
            '7',
            '12',
            '345',
            'Error'
        ],
        correct: 1,
        explanation: 'The + function adds all its arguments together. (+ 3 4 5) evaluates to 12.',
        reference: 'GNU Emacs Lisp Reference Manual - Arithmetic Operations',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Arithmetic-Operations.html'
    },
    {
        id: 15,
        difficulty: 'beginner',
        question: 'Which special form is used to create local variable bindings?',
        options: [
            'var',
            'local',
            'let',
            'bind'
        ],
        correct: 2,
        explanation: '`let` creates local variable bindings that are only valid within its body. Variables outside the let form are not affected.',
        reference: 'GNU Emacs Lisp Reference Manual - Local Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html'
    },
    {
        id: 16,
        difficulty: 'beginner',
        question: 'What does (listp \'(a b c)) return?',
        options: [
            'nil',
            't',
            '3',
            '(a b c)'
        ],
        correct: 1,
        explanation: '`listp` is a predicate function that returns `t` if its argument is a list, and `nil` otherwise. (a b c) is a list.',
        reference: 'GNU Emacs Lisp Reference Manual - List Predicates',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Functions.html'
    },
    {
        id: 17,
        difficulty: 'beginner',
        question: 'Which function is used to check if a value is a number?',
        options: [
            'number?',
            'numberp',
            'is-number',
            'numeric'
        ],
        correct: 1,
        explanation: '`numberp` is the predicate function that returns `t` if its argument is a number, `nil` otherwise. Most predicates in Emacs Lisp end with "p".',
        reference: 'GNU Emacs Lisp Reference Manual - Predicates on Numbers',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Predicates-on-Numbers.html'
    },
    {
        id: 18,
        difficulty: 'beginner',
        question: 'What is the difference between `eq` and `equal`?',
        options: [
            'No difference between them',
            'eq tests identity, equal tests contents',
            'eq is for numbers, equal for strings',
            'eq is deprecated, use equal instead'
        ],
        correct: 1,
        explanation: '`eq` tests whether two objects are the same object, while `equal` tests whether they have the same structure and contents.',
        reference: 'GNU Emacs Lisp Reference Manual - Equality Predicates',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html'
    },
    {
        id: 19,
        difficulty: 'beginner',
        question: 'How do you make a function interactive (callable with M-x)?',
        options: [
            'Add (interactive) to function body',
            'Use definteractive instead of defun',
            'Add :interactive t to the definition',
            'Functions are interactive by default'
        ],
        correct: 0,
        explanation: 'Adding `(interactive)` to a function body makes it callable interactively with M-x. It should be placed after the documentation string.',
        reference: 'GNU Emacs Lisp Reference Manual - Interactive Call',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Call.html'
    },
    {
        id: 20,
        difficulty: 'beginner',
        question: 'What does the `message` function do?',
        options: [
            'Sends an email message',
            'Displays a message in the echo area',
            'Creates a message buffer',
            'Logs a message to a file'
        ],
        correct: 1,
        explanation: 'The `message` function displays a formatted message in the echo area at the bottom of the Emacs frame.',
        reference: 'GNU Emacs Lisp Reference Manual - The Echo Area',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html'
    },

    // INTERMEDIATE LEVEL (Questions 21-60)
    {
        id: 21,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `mapcar` function?',
        options: [
            'Maps hash table pairs',
            'Applies function to list elements',
            'Changes cons cell cars',
            'Converts to mapping structure'
        ],
        correct: 1,
        explanation: '`mapcar` applies a function to each element of a list and returns a new list containing the results. For example, (mapcar \'1+ \'(1 2 3)) returns (2 3 4).',
        reference: 'GNU Emacs Lisp Reference Manual - Mapping Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html'
    },
    {
        id: 22,
        difficulty: 'intermediate',
        question: 'What is a lambda expression in Emacs Lisp?',
        options: [
            'A special variable type',
            'An anonymous function definition',
            'A loop construct variant',
            'A conditional statement form'
        ],
        correct: 1,
        explanation: 'A lambda expression creates an anonymous function that can be used without giving it a name. It starts with the symbol `lambda` followed by parameters and body.',
        reference: 'GNU Emacs Lisp Reference Manual - Lambda Expressions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Lambda-Expressions.html'
    },
    {
        id: 23,
        difficulty: 'intermediate',
        question: 'What does the `apply` function do?',
        options: [
            'Applies a function to a list of arguments',
            'Applies a style to text',
            'Applies a transformation to a buffer',
            'Applies a filter to data'
        ],
        correct: 0,
        explanation: '`apply` calls a function with arguments taken from a list. For example, (apply \'+ \'(1 2 3)) is equivalent to (+ 1 2 3).',
        reference: 'GNU Emacs Lisp Reference Manual - Calling Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html'
    },
    {
        id: 24,
        difficulty: 'intermediate',
        question: 'What is the difference between `let` and `let*`?',
        options: [
            'let* has better performance characteristics',
            'let* allows sequential variable binding',
            'let* is the deprecated legacy version',
            'let* creates global instead of local scope'
        ],
        correct: 1,
        explanation: 'In `let*`, variables are bound sequentially, so later bindings can refer to earlier ones. In `let`, all variables are bound simultaneously.',
        reference: 'GNU Emacs Lisp Reference Manual - Local Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Local-Variables.html'
    },
    {
        id: 25,
        difficulty: 'intermediate',
        question: 'Which function is used to add an element to the front of a list?',
        options: [
            'push',
            'prepend',
            'cons',
            'add-to-list'
        ],
        correct: 2,
        explanation: 'While `cons` is the basic function to add an element to the front of a list, `push` is a macro that modifies a variable by consing onto it.',
        reference: 'GNU Emacs Lisp Reference Manual - Building Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html'
    },
    {
        id: 26,
        difficulty: 'intermediate',
        question: 'What does the `funcall` function do?',
        options: [
            'Returns the function definition of a symbol',
            'Calls a function with specified arguments',
            'Checks if a symbol is a function',
            'Creates a function call expression'
        ],
        correct: 1,
        explanation: '`funcall` calls a function with the specified arguments. The first argument is the function, and the rest are arguments to that function.',
        reference: 'GNU Emacs Lisp Reference Manual - Calling Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html'
    },
    {
        id: 27,
        difficulty: 'intermediate',
        question: 'What is the purpose of backquote (`) in Emacs Lisp?',
        options: [
            'It creates regular expressions patterns',
            'It allows selective evaluation in quotes',
            'It starts multi-line comment blocks',
            'It defines macro transformation rules'
        ],
        correct: 1,
        explanation: 'Backquote (`) is like quote but allows selective evaluation of parts marked with comma (,) or comma-at (,@) for splicing.',
        reference: 'GNU Emacs Lisp Reference Manual - Backquote',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html'
    },
    {
        id: 28,
        difficulty: 'intermediate',
        question: 'What is a closure in Emacs Lisp?',
        options: [
            'A function that closes buffer operations',
            'A function capturing lexical environment',
            'A function that terminates programs',
            'A function that completes user input'
        ],
        correct: 1,
        explanation: 'A closure is a function that captures variables from its lexical environment. In Emacs Lisp, closures are created when lexical-binding is enabled.',
        reference: 'GNU Emacs Lisp Reference Manual - Closures',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html'
    },
    {
        id: 29,
        difficulty: 'intermediate',
        question: 'What does the `while` loop do when its condition becomes nil?',
        options: [
            'It raises an error condition',
            'It returns nil and exits loop',
            'It continues with next iteration',
            'It returns last evaluated expression'
        ],
        correct: 1,
        explanation: 'A `while` loop continues executing its body as long as the condition is non-nil. When the condition becomes nil, the loop exits and returns nil.',
        reference: 'GNU Emacs Lisp Reference Manual - Iteration',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html'
    },
    {
        id: 30,
        difficulty: 'intermediate',
        question: 'Which function is used to join strings together?',
        options: [
            'join',
            'concat',
            'append',
            'merge'
        ],
        correct: 1,
        explanation: '`concat` concatenates its arguments into a single string. For example, (concat "hello" " " "world") returns "hello world".',
        reference: 'GNU Emacs Lisp Reference Manual - Creating Strings',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Strings.html'
    },
    {
        id: 31,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `catch` and `throw` mechanisms?',
        options: [
            'Error handling and recovery',
            'Non-local exits from nested calls',
            'Exception handling like try-catch',
            'Memory management and cleanup'
        ],
        correct: 1,
        explanation: '`catch` and `throw` provide a non-local exit mechanism, allowing you to jump out of deeply nested function calls to a specific catch point.',
        reference: 'GNU Emacs Lisp Reference Manual - Catch and Throw',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Catch-and-Throw.html'
    },
    {
        id: 32,
        difficulty: 'intermediate',
        question: 'What does the `nthcdr` function do?',
        options: [
            'Returns the nth element of a list',
            'Returns the cdr applied n times to a list',
            'Returns the first n elements of a list',
            'Returns the last n elements of a list'
        ],
        correct: 1,
        explanation: '`nthcdr` applies `cdr` n times to a list. For example, (nthcdr 2 \'(a b c d)) returns (c d).',
        reference: 'GNU Emacs Lisp Reference Manual - List Elements',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html'
    },
    {
        id: 33,
        difficulty: 'intermediate',
        question: 'Which special form is used to handle errors in Emacs Lisp?',
        options: [
            'try-catch',
            'condition-case',
            'error-handler',
            'handle-error'
        ],
        correct: 1,
        explanation: '`condition-case` is used to handle errors in Emacs Lisp. It catches errors and allows you to handle them gracefully.',
        reference: 'GNU Emacs Lisp Reference Manual - Handling Errors',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Handling-Errors.html'
    },
    {
        id: 34,
        difficulty: 'intermediate',
        question: 'What is the difference between a function and a macro?',
        options: [
            'Macros have superior performance',
            'Functions evaluate args, macros do not',
            'Macros only work interactively',
            'Functions compile, macros interpret'
        ],
        correct: 1,
        explanation: 'Functions evaluate their arguments before being called, while macros receive their arguments unevaluated and can manipulate them as code.',
        reference: 'GNU Emacs Lisp Reference Manual - Macros',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html'
    },
    {
        id: 35,
        difficulty: 'intermediate',
        question: 'What does the `append` function do?',
        options: [
            'Adds an element to the end of a list',
            'Concatenates multiple lists into one list',
            'Appends text to a buffer',
            'Adds a property to an object'
        ],
        correct: 1,
        explanation: '`append` concatenates multiple lists into a single list. For example, (append \'(1 2) \'(3 4)) returns (1 2 3 4).',
        reference: 'GNU Emacs Lisp Reference Manual - Building Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Building-Lists.html'
    },
    {
        id: 36,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `dolist` macro?',
        options: [
            'To create a new list',
            'To iterate over elements of a list',
            'To delete elements from a list',
            'To sort a list'
        ],
        correct: 1,
        explanation: '`dolist` is a convenient macro for iterating over the elements of a list, executing the body for each element.',
        reference: 'GNU Emacs Lisp Reference Manual - Iteration',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html'
    },
    {
        id: 37,
        difficulty: 'intermediate',
        question: 'Which function removes the first occurrence of an element from a list?',
        options: [
            'remove',
            'delete',
            'Both remove and delete',
            'pop'
        ],
        correct: 2,
        explanation: 'Both `remove` and `delete` can remove elements from lists. `delete` modifies the original list destructively, while `remove` returns a new list.',
        reference: 'GNU Emacs Lisp Reference Manual - Sets and Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html'
    },
    {
        id: 38,
        difficulty: 'intermediate',
        question: 'What does the `reverse` function do?',
        options: [
            'Reverses the order of elements in a list',
            'Reverses the bits in a number',
            'Reverses a string character by character',
            'Undoes the last operation'
        ],
        correct: 0,
        explanation: '`reverse` returns a new list with the elements in reverse order. The original list is not modified.',
        reference: 'GNU Emacs Lisp Reference Manual - Rearranging Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Rearranging-Lists.html'
    },
    {
        id: 39,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `defvar` special form?',
        options: [
            'Defines a function variable',
            'Defines a global variable with documentation',
            'Defines a local variable',
            'Defines a constant variable'
        ],
        correct: 1,
        explanation: '`defvar` defines a global variable and allows you to provide documentation. It only sets the variable if it is currently unbound.',
        reference: 'GNU Emacs Lisp Reference Manual - Defining Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html'
    },
    {
        id: 40,
        difficulty: 'intermediate',
        question: 'What does the `assoc` function do?',
        options: [
            'Associates two values',
            'Finds a key-value pair in an association list',
            'Creates an association list',
            'Checks if two values are associated'
        ],
        correct: 1,
        explanation: '`assoc` searches an association list (alist) for a pair whose car equals the specified key, returning the entire pair if found.',
        reference: 'GNU Emacs Lisp Reference Manual - Association Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html'
    },
    {
        id: 41,
        difficulty: 'intermediate',
        question: 'What is a property list (plist) in Emacs Lisp?',
        options: [
            'A list of object properties',
            'A list with alternating keys and values',
            'A list sorted by properties',
            'A list of function properties'
        ],
        correct: 1,
        explanation: 'A property list (plist) is a list with alternating keys and values, like (key1 value1 key2 value2). It provides another way to associate keys with values.',
        reference: 'GNU Emacs Lisp Reference Manual - Property Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html'
    },
    {
        id: 42,
        difficulty: 'intermediate',
        question: 'What does the `when` macro do?',
        options: [
            'Executes code at a specific time',
            'Executes code only if a condition is true',
            'Executes code repeatedly while a condition is true',
            'Executes code after a delay'
        ],
        correct: 1,
        explanation: '`when` is a convenience macro that executes its body only if the condition is true. It is equivalent to an `if` without an else clause.',
        reference: 'GNU Emacs Lisp Reference Manual - Conditionals',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html'
    },
    {
        id: 43,
        difficulty: 'intermediate',
        question: 'Which function is used to test if a symbol has a function definition?',
        options: [
            'functionp',
            'fboundp',
            'function-boundp',
            'has-function'
        ],
        correct: 1,
        explanation: '`fboundp` tests whether a symbol has a function definition. It returns t if the symbol is fbound (function-bound).',
        reference: 'GNU Emacs Lisp Reference Manual - Function Cells',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Cells.html'
    },
    {
        id: 44,
        difficulty: 'intermediate',
        question: 'What is the difference between `mapc` and `mapcar`?',
        options: [
            'mapc works on lists, mapcar on arrays',
            'mapc discards results, mapcar builds list',
            'mapc has better performance characteristics',
            'mapc processes elements in reverse order'
        ],
        correct: 1,
        explanation: '`mapc` applies a function to each element like `mapcar` but discards the results and returns the original list. It is used for side effects.',
        reference: 'GNU Emacs Lisp Reference Manual - Mapping Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Mapping-Functions.html'
    },
    {
        id: 45,
        difficulty: 'intermediate',
        question: 'What does the `prog1` special form do?',
        options: [
            'Executes first form, returns its value',
            'Executes all forms, returns first value',
            'Executes sequentially, returns first value',
            'Starts a new program execution context'
        ],
        correct: 2,
        explanation: '`prog1` evaluates all its forms in sequence but returns the value of the first form. It is useful when you want to return a value but also perform side effects.',
        reference: 'GNU Emacs Lisp Reference Manual - Sequencing',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequencing.html'
    },
    {
        id: 46,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `unwind-protect` special form?',
        options: [
            'Protects against stack overflow errors',
            'Ensures cleanup code runs despite errors',
            'Protects variables from modification',
            'Prevents functions from redefinition'
        ],
        correct: 1,
        explanation: '`unwind-protect` ensures that cleanup code in its unwind-forms runs even if an error occurs in the protected form or if a non-local exit happens.',
        reference: 'GNU Emacs Lisp Reference Manual - Nonlocal Exits',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Nonlocal-Exits.html'
    },
    {
        id: 47,
        difficulty: 'intermediate',
        question: 'Which function is used to get the current buffer?',
        options: [
            'get-buffer',
            'current-buffer',
            'this-buffer',
            'active-buffer'
        ],
        correct: 1,
        explanation: '`current-buffer` returns the current buffer object. This is the buffer that most editing operations work on by default.',
        reference: 'GNU Emacs Lisp Reference Manual - Current Buffer',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html'
    },
    {
        id: 48,
        difficulty: 'intermediate',
        question: 'What does the `save-excursion` macro do?',
        options: [
            'Saves the current file',
            'Saves and restores point and mark',
            'Saves the current buffer state',
            'Saves the window configuration'
        ],
        correct: 1,
        explanation: '`save-excursion` saves the current point and mark, executes its body, then restores the point and mark to their original positions.',
        reference: 'GNU Emacs Lisp Reference Manual - Excursions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Excursions.html'
    },
    {
        id: 49,
        difficulty: 'intermediate',
        question: 'What is the purpose of hooks in Emacs?',
        options: [
            'To connect external programs',
            'To provide customization points in functions',
            'To create keyboard shortcuts',
            'To handle file operations'
        ],
        correct: 1,
        explanation: 'Hooks are lists of functions that are called at specific points in Emacs operation, providing customization points for users and packages.',
        reference: 'GNU Emacs Lisp Reference Manual - Hooks',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks.html'
    },
    {
        id: 50,
        difficulty: 'intermediate',
        question: 'Which function is used to add a function to a hook?',
        options: [
            'add-hook',
            'hook-add',
            'append-hook',
            'push-hook'
        ],
        correct: 0,
        explanation: '`add-hook` is the standard function for adding functions to hooks. It handles various edge cases and is the recommended way to modify hooks.',
        reference: 'GNU Emacs Lisp Reference Manual - Setting Hooks',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Hooks.html'
    },
    {
        id: 51,
        difficulty: 'intermediate',
        question: 'What does the `format` function do?',
        options: [
            'Formats text in a buffer',
            'Creates formatted strings using format specifiers',
            'Formats numbers for display',
            'Formats code according to style rules'
        ],
        correct: 1,
        explanation: '`format` creates formatted strings using format specifiers like %s for strings, %d for integers, similar to printf in C.',
        reference: 'GNU Emacs Lisp Reference Manual - Formatting Strings',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html'
    },
    {
        id: 52,
        difficulty: 'intermediate',
        question: 'What is the difference between `boundp` and `fboundp`?',
        options: [
            'boundp checks variables, fboundp checks functions',
            'boundp is for local variables, fboundp is for global variables',
            'boundp is deprecated, use fboundp instead',
            'They are the same function'
        ],
        correct: 0,
        explanation: '`boundp` tests if a symbol has a value (is bound as a variable), while `fboundp` tests if a symbol has a function definition.',
        reference: 'GNU Emacs Lisp Reference Manual - Variable Definitions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Variables.html'
    },
    {
        id: 53,
        difficulty: 'intermediate',
        question: 'What does the `nth` function do?',
        options: [
            'Returns the nth power of a number',
            'Returns the nth element of a list',
            'Returns the first n elements of a list',
            'Returns every nth element of a list'
        ],
        correct: 1,
        explanation: '`nth` returns the nth element of a list (0-indexed). For example, (nth 1 \'(a b c)) returns b.',
        reference: 'GNU Emacs Lisp Reference Manual - List Elements',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html'
    },
    {
        id: 54,
        difficulty: 'intermediate',
        question: 'Which macro is used to iterate over a range of numbers?',
        options: [
            'dotimes',
            'for',
            'repeat',
            'loop'
        ],
        correct: 0,
        explanation: '`dotimes` is a macro that executes its body a specified number of times, with a variable counting from 0 to n-1.',
        reference: 'GNU Emacs Lisp Reference Manual - Iteration',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Iteration.html'
    },
    {
        id: 55,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `ignore` function?',
        options: [
            'Ignores errors in code',
            'Does nothing and returns nil',
            'Ignores certain types of input',
            'Ignores warnings'
        ],
        correct: 1,
        explanation: '`ignore` is a function that does nothing and returns nil. It is useful as a placeholder or when you need a function that has no effect.',
        reference: 'GNU Emacs Lisp Reference Manual - Calling Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Calling-Functions.html'
    },
    {
        id: 56,
        difficulty: 'intermediate',
        question: 'What does the `last` function return?',
        options: [
            'The last element of a list',
            'The last cons cell of a list',
            'The position of the last element',
            'A boolean indicating if the list is complete'
        ],
        correct: 1,
        explanation: '`last` returns the last cons cell of a list, not just the last element. To get the last element, you would use (car (last list)).',
        reference: 'GNU Emacs Lisp Reference Manual - List Elements',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Elements.html'
    },
    {
        id: 57,
        difficulty: 'intermediate',
        question: 'Which function is used to check if a value is a string?',
        options: [
            'string?',
            'stringp',
            'is-string',
            'string-type-p'
        ],
        correct: 1,
        explanation: '`stringp` is the predicate function that returns t if its argument is a string, nil otherwise.',
        reference: 'GNU Emacs Lisp Reference Manual - Predicates for Strings',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Predicates-for-Strings.html'
    },
    {
        id: 58,
        difficulty: 'intermediate',
        question: 'What does the `member` function do?',
        options: [
            'Checks if an element is a member of a list',
            'Gets the nth member of a list',
            'Adds a member to a list',
            'Removes a member from a list'
        ],
        correct: 0,
        explanation: '`member` tests whether an element is a member of a list. It returns the sublist starting with the first occurrence of the element, or nil if not found.',
        reference: 'GNU Emacs Lisp Reference Manual - Sets and Lists',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Sets-And-Lists.html'
    },
    {
        id: 59,
        difficulty: 'intermediate',
        question: 'What is the purpose of the `defcustom` macro?',
        options: [
            'Defines a custom function',
            'Defines a customizable user option',
            'Defines a custom data type',
            'Defines a custom key binding'
        ],
        correct: 1,
        explanation: '`defcustom` defines a customizable user option that can be configured through Emacs\' customization interface.',
        reference: 'GNU Emacs Lisp Reference Manual - Variable Definitions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Definitions.html'
    },
    {
        id: 60,
        difficulty: 'intermediate',
        question: 'What does the `unless` macro do?',
        options: [
            'Executes code repeatedly until a condition is false',
            'Executes code only if a condition is false',
            'Executes code after a condition becomes false',
            'Executes code with exception handling'
        ],
        correct: 1,
        explanation: '`unless` is the opposite of `when`. It executes its body only if the condition is false (nil).',
        reference: 'GNU Emacs Lisp Reference Manual - Conditionals',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html'
    },

    // ADVANCED LEVEL (Questions 61-100)
    {
        id: 61,
        difficulty: 'advanced',
        question: 'What is the difference between dynamic and lexical scoping in Emacs Lisp?',
        options: [
            'Dynamic looks up at runtime, lexical at compile time',
            'Dynamic scoping has superior performance characteristics',
            'Dynamic scoping represents the newer modern approach',
            'There are no meaningful differences between them'
        ],
        correct: 0,
        explanation: 'Dynamic scoping looks up variables in the dynamic environment at runtime, while lexical scoping captures variables from the lexical environment where they are defined.',
        reference: 'GNU Emacs Lisp Reference Manual - Variable Scoping',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Scoping.html'
    },
    {
        id: 62,
        difficulty: 'advanced',
        question: 'What does the `eval` function do and why should it be used carefully?',
        options: [
            'Evaluates at runtime, poses security risks',
            'Only evaluates mathematical expressions',
            'It is deprecated and should not be used',
            'Evaluates faster than normal evaluation'
        ],
        correct: 0,
        explanation: '`eval` evaluates Lisp expressions at runtime. It should be used carefully as it can execute arbitrary code and may introduce security vulnerabilities.',
        reference: 'GNU Emacs Lisp Reference Manual - Eval',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Eval.html'
    },
    {
        id: 63,
        difficulty: 'advanced',
        question: 'What is the purpose of the `advice` system in Emacs?',
        options: [
            'To provide comprehensive help documentation',
            'To modify existing function behavior dynamically',
            'To give intelligent coding suggestions',
            'To handle compiler warnings and errors'
        ],
        correct: 1,
        explanation: 'The advice system allows you to modify the behavior of existing functions by adding code before, after, or around their execution without changing the original definition.',
        reference: 'GNU Emacs Lisp Reference Manual - Advising Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html'
    },
    {
        id: 64,
        difficulty: 'advanced',
        question: 'What does the `with-temp-buffer` macro do?',
        options: [
            'Creates a temporary buffer for operations',
            'Temporarily switches buffer focus',
            'Creates a backup buffer copy',
            'Temporarily disables buffer saving'
        ],
        correct: 0,
        explanation: '`with-temp-buffer` creates a temporary buffer, executes code in it, then automatically kills the buffer when done. Useful for string processing and temporary operations.',
        reference: 'GNU Emacs Lisp Reference Manual - Current Buffer',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html'
    },
    {
        id: 65,
        difficulty: 'advanced',
        question: 'What is the purpose of `gensym` in macro writing?',
        options: [
            'To generate random symbols',
            'To generate unique symbols to avoid variable capture',
            'To generate system symbols',
            'To generate symbols for debugging'
        ],
        correct: 1,
        explanation: '`gensym` generates unique symbols to avoid variable capture problems in macros, ensuring that macro-generated code doesn\'t accidentally refer to user variables.',
        reference: 'GNU Emacs Lisp Reference Manual - Problems with Macros',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Problems-with-Macros.html'
    },
    {
        id: 66,
        difficulty: 'advanced',
        question: 'What does the `macroexpand` function do?',
        options: [
            'Expands all macros in a file',
            'Shows the expansion of a macro call',
            'Optimizes macro performance',
            'Checks macro syntax'
        ],
        correct: 1,
        explanation: '`macroexpand` shows what a macro call expands to, which is useful for debugging macros and understanding their behavior.',
        reference: 'GNU Emacs Lisp Reference Manual - Expansion',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Expansion.html'
    },
    {
        id: 67,
        difficulty: 'advanced',
        question: 'What is the purpose of the `declare` form in function definitions?',
        options: [
            'To declare variable types',
            'To provide metadata about functions for the compiler and tools',
            'To declare function dependencies',
            'To declare function visibility'
        ],
        correct: 1,
        explanation: '`declare` provides metadata about functions, such as indentation rules, compiler optimizations, or other information for development tools.',
        reference: 'GNU Emacs Lisp Reference Manual - Declare Form',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Declare-Form.html'
    },
    {
        id: 68,
        difficulty: 'advanced',
        question: 'What is the difference between `symbol-value` and `symbol-function`?',
        options: [
            'symbol-value gets the variable value, symbol-function gets the function definition',
            'symbol-value is for local symbols, symbol-function is for global symbols',
            'symbol-value is deprecated',
            'They are the same function'
        ],
        correct: 0,
        explanation: '`symbol-value` accesses the value cell of a symbol (its variable binding), while `symbol-function` accesses the function cell (its function definition).',
        reference: 'GNU Emacs Lisp Reference Manual - Symbol Components',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Components.html'
    },
    {
        id: 69,
        difficulty: 'advanced',
        question: 'What is the purpose of the obarray in Emacs Lisp?',
        options: [
            'To store object arrays',
            'To store all interned symbols',
            'To store function definitions',
            'To store buffer objects'
        ],
        correct: 1,
        explanation: 'The obarray is a hash table that stores all interned symbols. When you create a symbol, it gets added to the obarray.',
        reference: 'GNU Emacs Lisp Reference Manual - Creating and Interning Symbols',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html'
    },
    {
        id: 70,
        difficulty: 'advanced',
        question: 'What does the `intern` function do?',
        options: [
            'Creates internal variables',
            'Finds or creates a symbol in the obarray',
            'Converts strings to internal format',
            'Optimizes internal function calls'
        ],
        correct: 1,
        explanation: '`intern` finds an existing symbol in the obarray or creates a new one if it doesn\'t exist. This is how symbols are "interned".',
        reference: 'GNU Emacs Lisp Reference Manual - Creating and Interning Symbols',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html'
    },
    {
        id: 71,
        difficulty: 'advanced',
        question: 'What is the difference between `eq`, `eql`, and `equal`?',
        options: [
            'eq tests identity, eql tests identity or numeric equality, equal tests structural equality',
            'They are all the same',
            'eq is fastest, equal is most comprehensive',
            'eq is for symbols, eql for numbers, equal for strings'
        ],
        correct: 0,
        explanation: '`eq` tests object identity, `eql` tests identity or numeric equality for numbers, and `equal` tests structural equality (same contents).',
        reference: 'GNU Emacs Lisp Reference Manual - Equality Predicates',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html'
    },
    {
        id: 72,
        difficulty: 'advanced',
        question: 'What is the purpose of the `special-variable-p` function?',
        options: [
            'To check if a variable has special powers',
            'To check if a variable is dynamically scoped',
            'To check if a variable is read-only',
            'To check if a variable is buffer-local'
        ],
        correct: 1,
        explanation: '`special-variable-p` checks if a variable is declared as dynamically scoped (special), affecting how it is looked up in different scoping contexts.',
        reference: 'GNU Emacs Lisp Reference Manual - Variable Scoping',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Scoping.html'
    },
    {
        id: 73,
        difficulty: 'advanced',
        question: 'What does the `byte-compile` function do?',
        options: [
            'Compiles Emacs Lisp to bytecode for faster execution',
            'Compiles C code to bytecode',
            'Compiles bytecode to machine code',
            'Compiles regular expressions'
        ],
        correct: 0,
        explanation: '`byte-compile` compiles Emacs Lisp code to bytecode, which executes faster than interpreted Lisp code.',
        reference: 'GNU Emacs Lisp Reference Manual - Byte Compilation',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html'
    },
    {
        id: 74,
        difficulty: 'advanced',
        question: 'What is the purpose of the `autoload` mechanism?',
        options: [
            'To automatically load files when Emacs starts',
            'To defer loading of function definitions until they are first called',
            'To automatically reload changed files',
            'To load functions faster'
        ],
        correct: 1,
        explanation: '`autoload` allows function definitions to be loaded on demand when the function is first called, improving startup time.',
        reference: 'GNU Emacs Lisp Reference Manual - Autoload',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html'
    },
    {
        id: 75,
        difficulty: 'advanced',
        question: 'What does the `load-path` variable contain?',
        options: [
            'The path to the current file being loaded',
            'A list of directories where Emacs looks for library files',
            'The loading progress of the current operation',
            'The path to the Emacs executable'
        ],
        correct: 1,
        explanation: '`load-path` is a list of directories that Emacs searches when loading library files with `load`, `require`, or `autoload`.',
        reference: 'GNU Emacs Lisp Reference Manual - Library Search',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html'
    },
    {
        id: 76,
        difficulty: 'advanced',
        question: 'What is the difference between `require` and `load`?',
        options: [
            'require checks if a feature is already loaded, load always loads the file',
            'require is for packages, load is for individual files',
            'require is newer than load',
            'require is more secure than load'
        ],
        correct: 0,
        explanation: '`require` loads a library only if its feature is not already loaded, while `load` always loads the specified file regardless.',
        reference: 'GNU Emacs Lisp Reference Manual - Features',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Features.html'
    },
    {
        id: 77,
        difficulty: 'advanced',
        question: 'What does the `provide` function do?',
        options: [
            'Provides a value to a variable',
            'Declares that a library provides a specific feature',
            'Provides error handling',
            'Provides memory allocation'
        ],
        correct: 1,
        explanation: '`provide` declares that the current library file provides a specific feature, which can then be loaded with `require`.',
        reference: 'GNU Emacs Lisp Reference Manual - Features',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Features.html'
    },
    {
        id: 78,
        difficulty: 'advanced',
        question: 'What is the purpose of the `features` variable?',
        options: [
            'Lists all available Emacs features',
            'Lists all currently loaded features',
            'Lists all enabled features',
            'Lists all optional features'
        ],
        correct: 1,
        explanation: 'The `features` variable contains a list of all features that have been loaded (provided) in the current Emacs session.',
        reference: 'GNU Emacs Lisp Reference Manual - Features',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Features.html'
    },
    {
        id: 79,
        difficulty: 'advanced',
        question: 'What does the `with-current-buffer` macro do?',
        options: [
            'Creates a new buffer and makes it current',
            'Temporarily makes a specified buffer current',
            'Saves the current buffer',
            'Switches permanently to a different buffer'
        ],
        correct: 1,
        explanation: '`with-current-buffer` temporarily makes the specified buffer current for the duration of its body, then restores the previous current buffer.',
        reference: 'GNU Emacs Lisp Reference Manual - Current Buffer',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html'
    },
    {
        id: 80,
        difficulty: 'advanced',
        question: 'What is the purpose of the `buffer-local-variables` function?',
        options: [
            'Creates buffer-local variables',
            'Returns a list of buffer-local variables and their values',
            'Deletes buffer-local variables',
            'Checks if variables are buffer-local'
        ],
        correct: 1,
        explanation: '`buffer-local-variables` returns an alist of buffer-local variables and their values in the current buffer.',
        reference: 'GNU Emacs Lisp Reference Manual - Buffer-Local Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer_002dLocal-Variables.html'
    },
    {
        id: 81,
        difficulty: 'advanced',
        question: 'What does the `make-variable-buffer-local` function do?',
        options: [
            'Makes a variable local to the current buffer only',
            'Makes a variable automatically buffer-local in any buffer where it is set',
            'Makes a variable local to a specific buffer',
            'Makes a variable shared between buffers'
        ],
        correct: 1,
        explanation: '`make-variable-buffer-local` makes a variable automatically buffer-local whenever it is set in any buffer.',
        reference: 'GNU Emacs Lisp Reference Manual - Buffer-Local Variables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer_002dLocal-Variables.html'
    },
    {
        id: 82,
        difficulty: 'advanced',
        question: 'What is the difference between `point` and `point-marker`?',
        options: [
            'point returns an integer, point-marker returns a marker object',
            'point is for the current buffer, point-marker is for any buffer',
            'point is mutable, point-marker is immutable',
            'They are the same function'
        ],
        correct: 0,
        explanation: '`point` returns the current position as an integer, while `point-marker` returns a marker object that tracks the position even if text is inserted or deleted.',
        reference: 'GNU Emacs Lisp Reference Manual - Point',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html'
    },
    {
        id: 83,
        difficulty: 'advanced',
        question: 'What does the `narrow-to-region` function do?',
        options: [
            'Narrows the buffer to show only the specified region',
            'Selects a narrow region of text',
            'Reduces the width of the buffer display',
            'Focuses editing on a specific region'
        ],
        correct: 0,
        explanation: '`narrow-to-region` restricts editing and most operations to the specified region, making the rest of the buffer temporarily inaccessible.',
        reference: 'GNU Emacs Lisp Reference Manual - Narrowing',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Narrowing.html'
    },
    {
        id: 84,
        difficulty: 'advanced',
        question: 'What is the purpose of the `save-restriction` macro?',
        options: [
            'Saves buffer restrictions and restores them after the body',
            'Saves access restrictions for security',
            'Saves memory usage restrictions',
            'Saves file permission restrictions'
        ],
        correct: 0,
        explanation: '`save-restriction` saves the current narrowing state (if any) and restores it after executing its body, even if the body changes the narrowing.',
        reference: 'GNU Emacs Lisp Reference Manual - Narrowing',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Narrowing.html'
    },
    {
        id: 85,
        difficulty: 'advanced',
        question: 'What does the `match-data` function return?',
        options: [
            'The data that was matched in the last search',
            'Information about the last successful regexp match',
            'The position of the last match',
            'The text that was matched'
        ],
        correct: 1,
        explanation: '`match-data` returns information about the last successful regexp match, including the positions of the match and any subgroups.',
        reference: 'GNU Emacs Lisp Reference Manual - Match Data',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Match-Data.html'
    },
    {
        id: 86,
        difficulty: 'advanced',
        question: 'What is the purpose of the `save-match-data` macro?',
        options: [
            'Saves match data to a file',
            'Saves and restores match data around code that might change it',
            'Saves the last search results',
            'Saves matched text to a variable'
        ],
        correct: 1,
        explanation: '`save-match-data` saves the current match data, executes its body, then restores the match data, protecting it from being overwritten.',
        reference: 'GNU Emacs Lisp Reference Manual - Saving Match Data',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Saving-Match-Data.html'
    },
    {
        id: 87,
        difficulty: 'advanced',
        question: 'What does the `read-from-string` function do?',
        options: [
            'Reads text from a string buffer',
            'Parses a Lisp expression from a string',
            'Reads user input from a string prompt',
            'Reads characters one by one from a string'
        ],
        correct: 1,
        explanation: '`read-from-string` parses a Lisp expression from a string and returns the parsed object, similar to how the Lisp reader works.',
        reference: 'GNU Emacs Lisp Reference Manual - Input Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Input-Functions.html'
    },
    {
        id: 88,
        difficulty: 'advanced',
        question: 'What is the purpose of the `prin1-to-string` function?',
        options: [
            'Prints the first element to a string',
            'Converts a Lisp object to its printed representation as a string',
            'Prints a string to the first available output',
            'Converts a string to printable format'
        ],
        correct: 1,
        explanation: '`prin1-to-string` converts a Lisp object to its printed representation as a string, which can be read back with `read-from-string`.',
        reference: 'GNU Emacs Lisp Reference Manual - Output Functions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Output-Functions.html'
    },
    {
        id: 89,
        difficulty: 'advanced',
        question: 'What does the `garbage-collect` function do?',
        options: [
            'Deletes unused files',
            'Manually triggers garbage collection to free unused memory',
            'Cleans up temporary variables',
            'Removes obsolete functions'
        ],
        correct: 1,
        explanation: '`garbage-collect` manually triggers garbage collection, which frees memory used by objects that are no longer referenced.',
        reference: 'GNU Emacs Lisp Reference Manual - Garbage Collection',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html'
    },
    {
        id: 90,
        difficulty: 'advanced',
        question: 'What is the purpose of weak references in Emacs Lisp?',
        options: [
            'To create references that don\'t prevent garbage collection',
            'To create references with reduced strength',
            'To create temporary references',
            'To create conditional references'
        ],
        correct: 0,
        explanation: 'Weak references allow objects to be garbage collected even if they are referenced, useful for caches and avoiding memory leaks.',
        reference: 'GNU Emacs Lisp Reference Manual - Weak Hash Tables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Weak-Hash-Tables.html'
    },
    {
        id: 91,
        difficulty: 'advanced',
        question: 'What does the `make-hash-table` function create?',
        options: [
            'A hash symbol table',
            'A hash table data structure for key-value mappings',
            'A hash function',
            'A hash code generator'
        ],
        correct: 1,
        explanation: '`make-hash-table` creates a hash table data structure that provides efficient key-value mappings with O(1) average lookup time.',
        reference: 'GNU Emacs Lisp Reference Manual - Creating Hash Tables',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Hash.html'
    },
    {
        id: 92,
        difficulty: 'advanced',
        question: 'What is the difference between `puthash` and `remhash`?',
        options: [
            'puthash adds key-value pairs, remhash removes them',
            'puthash is for putting data, remhash is for remote hashing',
            'puthash is synchronous, remhash is asynchronous',
            'puthash is for local hashes, remhash is for remote hashes'
        ],
        correct: 0,
        explanation: '`puthash` adds or updates a key-value pair in a hash table, while `remhash` removes a key-value pair from a hash table.',
        reference: 'GNU Emacs Lisp Reference Manual - Hash Access',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Access.html'
    },
    {
        id: 93,
        difficulty: 'advanced',
        question: 'What does the `maphash` function do?',
        options: [
            'Maps hash values to different values',
            'Applies a function to each key-value pair in a hash table',
            'Creates a mapping from one hash table to another',
            'Hashes a mapping function'
        ],
        correct: 1,
        explanation: '`maphash` applies a function to each key-value pair in a hash table, similar to how `mapcar` works with lists.',
        reference: 'GNU Emacs Lisp Reference Manual - Defining Hash Comparisons',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Hash.html'
    },
    {
        id: 94,
        difficulty: 'advanced',
        question: 'What is the purpose of the `cl-lib` library?',
        options: [
            'Provides Common Lisp compatibility functions',
            'Provides C library interfaces',
            'Provides command line utilities',
            'Provides client-server functionality'
        ],
        correct: 0,
        explanation: '`cl-lib` provides many Common Lisp functions and constructs that are not part of standard Emacs Lisp, like `cl-loop`, `cl-defstruct`, etc.',
        reference: 'GNU Emacs Lisp Reference Manual - Common Lisp Extensions',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/cl/index.html'
    },
    {
        id: 95,
        difficulty: 'advanced',
        question: 'What does the `define-derived-mode` macro do?',
        options: [
            'Defines a new major mode based on an existing one',
            'Defines a derived data type',
            'Defines a derived function',
            'Defines a mode for derived calculations'
        ],
        correct: 0,
        explanation: '`define-derived-mode` creates a new major mode that inherits behavior from a parent mode, commonly used for creating specialized editing modes.',
        reference: 'GNU Emacs Lisp Reference Manual - Derived Modes',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html'
    },
    {
        id: 96,
        difficulty: 'advanced',
        question: 'What is the purpose of the `keymap` data structure?',
        options: [
            'Maps keys to functions or other keymaps',
            'Maps keyboard layouts',
            'Maps file paths to keys',
            'Maps variable names to keys'
        ],
        correct: 0,
        explanation: 'A keymap is a data structure that maps key sequences to functions or other keymaps, defining how key presses are interpreted.',
        reference: 'GNU Emacs Lisp Reference Manual - Keymaps',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html'
    },
    {
        id: 97,
        difficulty: 'advanced',
        question: 'What does the `make-sparse-keymap` function create?',
        options: [
            'A keymap with few key bindings',
            'A memory-efficient keymap representation',
            'A keymap for special keys only',
            'A keymap with sparse documentation'
        ],
        correct: 1,
        explanation: '`make-sparse-keymap` creates a keymap optimized for memory efficiency when most keys are unbound, using a list-based representation.',
        reference: 'GNU Emacs Lisp Reference Manual - Creating Keymaps',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Keymaps.html'
    },
    {
        id: 98,
        difficulty: 'advanced',
        question: 'What is the purpose of the `timer` functionality in Emacs?',
        options: [
            'To time code execution',
            'To schedule functions to run at specific times or intervals',
            'To create time-based animations',
            'To measure system performance'
        ],
        correct: 1,
        explanation: 'Timers allow you to schedule functions to run at specific times, after delays, or at regular intervals, enabling time-based functionality.',
        reference: 'GNU Emacs Lisp Reference Manual - Timers',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html'
    },
    {
        id: 99,
        difficulty: 'advanced',
        question: 'What does the `run-with-idle-timer` function do?',
        options: [
            'Runs a function when the system is idle',
            'Runs a function when Emacs has been idle for a specified time',
            'Runs a function with low priority',
            'Runs a function in the background'
        ],
        correct: 1,
        explanation: '`run-with-idle-timer` schedules a function to run when Emacs has been idle (no user input) for a specified amount of time.',
        reference: 'GNU Emacs Lisp Reference Manual - Idle Timers',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html'
    },
    {
        id: 100,
        difficulty: 'advanced',
        question: 'What is the significance of the `lexical-binding` file-local variable?',
        options: [
            'It enables lexical scoping for the entire file',
            'It binds lexical variables',
            'It enables syntax highlighting',
            'It enables compilation optimizations'
        ],
        correct: 0,
        explanation: 'Setting `lexical-binding` to t at the top of a file enables lexical scoping for all variable bindings in that file, changing how variables are resolved.',
        reference: 'GNU Emacs Lisp Reference Manual - Lexical Binding',
        referenceUrl: 'https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html'
    }
];

// Test mode questions (subset for development)
const TEST_QUESTIONS = QUIZ_QUESTIONS.slice(0, 3);
