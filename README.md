# LSL PyOptimizer

LSL PyOptimizer is a LSL2 script optimizer written for Python 2. Currently it only supports code memory optimization (no speed optimization), only for Mono (no LSO), and only for the Second Life flavour of LSL (no OpenSim etc.).

The original LSL compiler does not do any optimizations whatsoever. Either the programmer does the optimization, or the scripts take more memory than necessary when writing something as simple as `a = -1;` (yes, the sign takes code memory!).

Given the 64K memory limit that applies to Mono scripts, this becomes a problem, leading to either difficult to read scripts, if optimized by hand, or in case of large scripts, to code taking valuable memory that could be used to embed more code or data.

The aim of this program is to act as a filter that performs the optimizations automatically, letting the programmer focus on writing readable code.

It also implements several syntax extensions to help improving the readability of scripts and the productivity of the programmer. It works well when combined with a C preprocessor such as _Boost::Wave_ (the one embedded in Firestorm) or `cpp`.

Firestorm does already incorporate an optimizer. However it is limited to removing unused global variables and functions, and does so by simple string analysis, not by syntactic analysis (e.g. if a local variable with the same name as a global is defined, the global isn't optimized out even if not used). In contrast, the program presented here does full syntax analysis and implements many more optimizations, including removing unused locals, simplifying many expressions, removing dead code, and more.

## Status

It is still considered alpha code. Please help by reporting any bugs you find.

## Features

Syntax extensions supported:

* `break` and `continue` statements.
* Expressions in globals.
* Extended assignment operators `&=`, `|=`, `^=`, `<<=`, `>>=`.
* Concatenation of key and string.
* C-like string juxtaposition: "str1" "str2" produces "str1str2".
* Allow type-cast of unary expressions that are not postfix, e.g. `(string)++x`.
* Allow multiple labels with the same name in the same function.
* `switch()` statements, for compatibility with Firestorm.
* Lazy list syntax (`identifier[index]`), for compatibility with Firestorm.

Optimizations supported:

* Constant folding, expression and statement simplification.
* Dead code removal, including unused global and local variables.
* Shrinking identifiers.
* Floats to integers where possible.
* Signs in numbers.

The next sections explain these features in detail.

## Syntax extensions

### `break` and `continue`

Support for `break` and `continue` statements, working as their C equivalents. It also supports `break n;` and `continue n;` for a constant integer `n` which indicates the number of nested loops to exit from (in the case of `break`) or to `continue` at. The default `n` when not specified is 1, meaning to break or continue at the current loop.

### Expressions in globals

Allow arbitrary expressions in globals, as long as they resolve to a single constant. The optimizer will evaluate the expression and substitute the result in the final script. For example, you can have a line like:
```
    float a = llPow(llSin(40*DEG_TO_RAD), 2);
```
in the globals section, and the optimizer will output the line:
```
    float a = 0.41317588;
```
instead. Needs constant folding optimization to be enabled. If the expression does not resolve to a constant, a warning will be emitted, and the compilation will fail at that line.

### Extended assignment operators

C-like `&=`, `|=`, `^=`, `<<=`, `>>=` assignment operator support.

### Concatenation of key and string

In LSL, keys are automatically cast to string in most cases. For example:
```
    llOwnerSay(llGetOwner());
```
works perfectly. However, one prominent case where that does not happen automatically is in string concatenation. Confusingly, while the above works, this doesn't:
```
    llOwnerSay("Your key is: " + llGetOwner());
```
and instead produces a type mismatch error. This syntax extension allows you to concatenate strings and keys, making the type cast transparent.

### C-like string juxtaposition.

For example `"a" "b"` resolves to the string `"ab"`. Very useful when combined with preprocessor macros, for not needing to add strings together to form a larger string. For example:

```
#define VERSION "1.13"
#define REVISION "b"
...
llOwnerSay("Program version " VERSION
           ", revision " REVISION);
```
or even
```
#define VERSION 1.13
#define REVISION b
#define XSTRINGIFY(x) #x
#define STRINGIFY(x) XSTRINGIFY(x)
...
llOwnerSay("Program version " STRINGIFY(VERSION)
           ", revision " STRINGIFY(REVISION));
```
will resolve to:
```
llOwnerSay("Program version 1.13, revision b");
```

instead of
```
llOwnerSay("Program version " + "1.13" + ", revision " + "b");
```
The latter can also be resolved by the optimizer, but by default that optimization is disabled as it can be counter-productive.

### Type-cast extension

For some reason, LSL only allows postfix expressions after a type cast. This extends the syntax to allow prefix expressions as well. This means that you can write e.g. `(string)(integer)a` instead of `(string)((integer)a)`, or `(string)++x` instead of `(string)(++x)`, sparing you from having to enter the extra parentheses.

### Multiple labels with the same name

Allows duplicate labels obeying the LSL scope rules. This one is tricky to understand. In LSL, syntax-wise, label identifiers obey the same scope rules as variables: when opening a new block, the labels defined inside it are invisible to any of the outer blocks. This means that you can have labels with the same name in different blocks, just like you can have variables with the same name in different blocks.

For example, you can have:

```
default
{
    state_entry()
    {
        {
            integer i;
            @label1;
        }
        {
            integer i;
            @label1;
        }
    }
}
```
Just as the variables, the labels are visible only within their own blocks or any nested block inside them. Unlike variables, labels are also visible if they are declared *after* the `jump` instruction that uses them, as long as it's within a block that encloses the label.

However, that's only the theory, The compiler is prepared to work like that, but the underlying assembler only jumps to the last label (in the case of LSO) or fails and doesn't let the script to be saved (in the case of Mono).

This syntax extension fixes that situation by renaming the labels in the output, to give each a different name and allow Mono scripts to be saved, making the compiler work as intended.

Again, one use is in preprocessor macros. Consider this example:
```
#define DO {@dowhile;
#define WHILE(expr) if (expr) jump dowhile;}
```
Without this option, you would only be able to have one DO...WHILE() loop per function.

## Compatibility Syntax extensions

These extensions are implemented for compatibility with the syntax extensions originally integrated in Emerald and currently in Firestorm. Their use is discouraged and they are considered legacy extensions.

### `switch()` statements

Enables use of C-like `switch` statements. These produce very awkward code, hard to optimize, and the argument is evaluated multiple times (as many as `case` labels are present).

The syntax of the `switch` statement as implemented, has two restrictions over its C counterpart:

1. `case` labels can't appear in nested blocks. That's because they are replaced by LSL labels, and as discussed in the *Multiple labels with the same name* section above, label scope rules prevent their visibility in an outer block, so once converted to labels, the corresponding `jump` instructions would not be able to find them. This limitation means that [Duff's device](https://en.wikipedia.org/wiki/Duff's_device) or similar constructs can't be implemented with this optimizer.
2. `switch()` needs to be followed by a block, not by a single statement; for example, whiile this works in C, it won't work in this optimizer: `switch(1) case 1: break;`. The reason is that `case` is treated by this parser as a statement, rather than as a label, making `break` be outside the `switch`. This limitation is probably only of theoretical importance and will not have any practical implication, since single-statement `switch` clauses are of no practical use (known to the author).

As an extension, and for compatibility with Firestorm, if there is a block beginning right after a `case` or `default` statement, the colon is optional. For example, all these are valid:
```
    switch (x) { case 1: ; default: ; }
    switch (x) { case 1 {} default {} }
```

### Lazy lists

That's how Firestorm calls an extended syntax for subindex values after individual list elements.

#### Assignment

The syntax for assignment is:
```
mylist[index] = value;
```
which is designed to be roughly a shortcut for this:
```
mylist = llListReplaceList(mylist, [value], index, index);
```
However, the implementation includes creating a function that performs the replacement. The function is called `lazy_list_set`. It can be user-overriden. If you define a function with this prototype:
```
list lazy_list_set(list target, integer index, list value)
```
which returns the list with the element replaced, then the optimizer will use yours rather than defining it twice.

For compatibility with Firestorm, when the index is greater than the number of elements in the list, the intermediate values are filled with integer zeros. If you don't want that, you may have a reason to override it.

Note that the value of the assignment as an expression is the whole list, not the element. For example, this will fail because it's assigning a list to an integer:
```
  list a; integer b = a[5] = 4;
```
But this will work:
```
  list a; integer b; a[5] = b = 4;
```
#### Reading

The syntax for reading an element is the same as for assigning, but it returns no type, therefore a type cast is mandatory. For example:

```
  list a; integer b = (integer)a[3];
```
That is converted at parsing time to:
```
  list a; integer b = llList2Integer(a, 3);
```
If the type it's cast to is list, it needs two parameters (starting and ending index), not one:
```
  list a; a = (list)a[3, 3];
```
That is a requirement of the underlying `llList2List` function used in this case.

## Optimizations

### Constant folding and expression simplification

The optimizer simplifies expressions as much as it knows, which is a fair amount, though there's still room for improvement in this area. Expressions that evaluate to a constant will be replaced with that constant. Other expressions such as a+3+1 are replaced with a+4, and so on. Note, however, that for floats, `(a+b)+c` may not equal `a+(b+c)`, so that optimization is not always done for floats. Also, as of this writing this optimization is only partial, so some expressions may not be optimized, e.g. `2+a+b+3` is not optimized to `a+b+5`. Many boolean expressions are simplified too (more are in the way). For example, `(TRUE&&(expression))` is simplified to `(expression)`, and `(FALSE&&(expression))` is simplified to `(FALSE)`. The famous `if (llListFindList(...)!=-1)` to `if (~llListFindList(...))` replacement is also performed automatically.

The constant folder is also responsible for simplifying certain statements, e.g. `if (FALSE) { statements; } ` is completely removed, and `if (TRUE) { statements1; } else { statements2; }` is replaced with just `{ statements1; }`, removing `{ statements2; }`. Similarly, `do...while(constant)` loops and other loops are optimized too.

This enables lots of things, like creating an `assert()` macro similar to that in C:
```
#define assert(x) do { if (DEBUG && !(x)) \
  llOwnerSay("ASSERTION FAILED: " #x); } while (0)
```
without worrying about the extra memory that it will take in production code once DEBUG is switched off, or about the loop taking actual code memory.

### Dead code removal

This part of the optimizer tracks execution and usage of statements and variables, and removes the ones that aren't used. It performs a function similar to that of the Firestorm optimizer, and much more. It can remove also unused locals, and isn't confused by having a global and a local with the same name.

It also replaces references to integer variables that are not written to (global or local), with their value. For example:
```
integer ANSWER = 42;
default
{
    state_entry()
    {
        llOwnerSay((string)X);
    }
}
```
will produce:
```
default
{
    state_entry()
    {
        llOwnerSay("42");
    }
}
```
after DCR and constant folding. This optimization has one of the largest impacts, as variables and parameters in general seem to take a lot of memory in Mono, and removing as much of them as possible produces good savings.

### Shrinking Identifiers

Long variable and parameter names are nice and readable, but when used as part of the globals or as function parameters, each character in the identifier takes at least one byte of code memory. In large programs, this can add up to a significant amount. This option replaces global and parameter identifiers with the shortest possible ones, also reusing as many as it can. The savings from this alone can be very significant in programs with a large number of globals or states.

### Floats

Floats under Mono are internally double precision, so float constants take four more bytes than integers. On the other hand, type cast from integer to float takes only one byte. This optimization substitutes floats with integers where possible, for an overall saving of three bytes per number.

### Signs

The sign at the beginning of a number (except in globals) takes one byte of code, unless prefixed by a type cast (which does not, under Mono, take code memory in itself if the destination type is the same). Small saving, but it adds up to the overall. Numbers are thus output with a type cast and surrounded by parentheses, e.g. `((float)-1.5)` instead of `-1.5`.

### String constant folding

This optimization is turned off by default, as it can be counter-productive. It enables concatenating constants together. However, consider this situation:
```
string a = "A very long string that only appears once" + ", or not";
string b = "A very long string that only appears once" + " or twice";
```
Since Mono keeps only one copy of each constant string in the code, making the optimizer concatenate them would be counter-productive, generating two long strings that take more code than the original string plus the shorter ones.

## Using the program

This program is designed to work as a filter. It can read from standard input if the file name argument is "-"; in any case it currently outputs the result to standard output and any errors to standard error.

Running it by hand to optimize your scripts can be cumbersome. The intention is for it to act as a transparent filter; however, as of this writing there's no support for any viewer or IDE, as it has just been released. Run it without parameters to see the invocation help, for example with `python main.py`. Redirect the output to a file if you want to store the result, possibly to open it with an editor and copy it to the clipboard. Or under _X Window_, you can pipe the output directly to `xclip -quiet -selection clipboard` to copy it to the clipboard, rather than using a file, so you can paste it directly into the viewer. Examples:
```
python main.py myscript.lsl | xclip -quiet -selection clipboard
```
will, under _X Window_, read `myscript.lsl`, optimize it, and copy the optimized result to the clipboard, ready to be pasted into the viewer.
```
python main.py myscript.lsl > temp.opt
notepad temp.opt
```
will, under any system which has an editor called `notepad`, read `myscript.lsl`, optimize it, and write the optimized result to `temp.opt`, then open it in the editor, enabling you to copy it and paste it into the viewer. Under _Windows_ version _Vista_ and above, apparently there's a command line application called `clip` that does the same as `xclip` does for _X Window_, enabling you to use this:
```
python main.py myscript.lsl | clip
```
to copy the optimized output to the clipboard. Under OS X, it seems `pbcopy` does the same as `xclip` and `clip`.

Future plans include writing a patch for Firestorm to enable it to run external script filtering programs instead of the internal preprocessor and optimizer. That would allow this program to be run using Firestorm's integrated machinery, making usage pretty transparent to the programmer.

Support for other IDEs like Eclipse is not planned, but the author encourages others to write it. Please notify Sei Lisa if you write one, so that it can be listed in this page.

The program uses two external files. One is `builtins.txt`, which is in the same format as needed by [lslint](https://github.com/pclewis/lslint), and an up-to-date copy can be obtained from the _kwdb_ project: [https://bitbucket.org/Sei_Lisa/kwdb/raw/tip/outputs/builtins.txt](https://bitbucket.org/Sei_Lisa/kwdb/raw/tip/outputs/builtins.txt). The other is `seftable.txt`, which is a list of functions that have no side effects (SEF stands for side-effect free), allowing them to be removed (optimized out) if nothing else has side effects in an expression and the result of the expression is not used.

## Other future plans

Making the optimizer smarter is one primary objective. Conversion to [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form) form is currently not performed, and would be nice to have, for one, to enable more optimizations where values are not used. There are a number of TODO items in the code about optimizations pending to implement.

Another goal is to add the possibility to link each output line with its source counterpart, so that in case of a server side compilation error, the source line corresponding to the actual line where the error happened can be displayed. Currently, server-side compilation errors should be rare (unless there are bugs in the optimizer) as there will be few situations in which they won't be caught by the optimizer's parser first. This would also allow warnings output by the optimizer (e.g. an expression in globals not resolving to a constant) to be related to a source line. Currently those warnings don't tell where the error occurred.

Lastly, implementation of some kind of machine-readable way to inform the invoker about the available options, is also in the plans. That would allow the plugin or viewer or whatever to present a dialog with the options for invoking the optimizer.

## License

This program is distributed under the terms of the GNU General Public License (GPL) version 3.

(C) Copyright 2015 Sei Lisa. All rights reserved.

## Author

Written by Sei Lisa. Sei Lisa is the author's pseudonym and user name in the Second Life virtual world.

Happy coding!
