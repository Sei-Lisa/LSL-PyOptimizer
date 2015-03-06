# LSL PyOptimizer

LSL PyOptimizer is a LSL2 script optimizer written for Python 2. Currently it only supports code memory optimization (no speed optimization), only for Mono (no LSO), and only for the Second Life flavour of LSL (no OpenSim etc.).

The original LSL compiler does not do any optimizations whatsoever. Either the programmer does the optimization, or the scripts take more memory than necessary when writing something as simple as `a = -1;` (yes, the sign takes code memory!).

Given the 64K memory limit that applies to Mono scripts, this becomes a problem, leading to either difficult to read scripts, if optimized by hand, or in case of large scripts, to code taking valuable memory that could be used to embed more code or data.

The aim of this program is to act as a filter that performs the optimizations automatically, letting the programmer focus on writing readable code.

It also implements several syntax extensions to help improving the readability of scripts and the productivity of the programmer. It works well when combined with a C preprocessor such as _Boost::Wave_ (the one embedded in Firestorm) or `cpp`.

Firestorm does already incorporate something that it calls "optimizer". However it is limited to removing unused global variables and functions, and does so by simple string analysis, not by syntactic analysis. It also sorts the globals in reverse alphabetical order, leading to compilation errors when one global depends on another that has been moved after it. In contrast, the program presented here does full syntax analysis and implements many more optimizations, including removing unused locals, simplifying many expressions, removing dead code, and more.

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
works perfectly. However, one case where that does not happen automatically is in string concatenation. Confusingly, while the above works, this doesn't:
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

Enables use of C-like `switch` statements. These produce very awkward code, hard to optimize, and evaluate the argument multiple times (as many as `case` labels are present).

In Firestorm, this feature is implemented as a search-and-replace of text, with no regards for the semantic meaning of the text, so a fragment like this:
```
llOwnerSay("Don't use switch(), just in case it doesn't work as you expect: it is buggy.");
```
produces this unexpected output:
```
[08:26:32] Object: Don't use {if(() == ( it doesn't work as you expect))jump c6hFK7;
, just in @c6hFK7;
 it is buggy.
```
This optimizer performs proper syntax analysis and processes the above example correctly.

The syntax of the `switch` statement as implemented, has two restrictions over its C counterpart:

1. `case` labels can't appear in nested blocks. That's because they are replaced by LSL labels, and as discussed in the *Multiple labels with the same name* section above, label scope rules prevent their visibility in an outer block, so once converted to labels, the corresponding `jump` instructions would not be able to find them. This limitation means that [Duff's device](https://en.wikipedia.org/wiki/Duff's_device) or similar constructs can't be implemented with this optimizer.
2. `switch()` needs to be followed by a block, not by a single statement; for example, whiile this works in C, it won't work in this optimizer: `switch(1) case 1: break;`. The reason is that `case` is treated by this parser as a statement, rather than as a label, making `break` be outside the `switch`. This limitation is probably only of theoretical importance and will not have any practical implication, since single-statement `switch` clauses are of no practical use (known to the author).

### Lazy lists

That's how Firestorm calls an extended syntax for assigning values to individual list elements. The syntax is:
```
mylist[index] = value;
```
It is designed to be a shortcut for this:
```
mylist = llListReplaceList(mylist, [value], index, index);
```
However, the implementation includes creating a function that performs the replacement. The function is called `lazy_list_set`. In Firestorm, this function can't be user-overriden: if you define your own, you'll get a duplicate identifier error. In this optimizer, if you define a function with this prototype:
```
list lazy_list_set(list target, integer index, list value)
```
then the optimizer will use yours rather than defining it twice.

For compatibility with Firestorm, when the index is greater than the number of elements in the list, the intermediate values are filled with integer zeros. If you don't want that, you may have a reason to override it. But best is to stay away from this syntax altogether, as the counterpart of using `mylist[index]` to read an element doesn't work, because the system doesn't know what type to extract it as (i.e. which of the `llList2XXX` functions to use to do the extraction).

## Using the program

This program is designed to work as a filter. It can read from standard input if the file name argument is "-"; in any case it currently outputs the result to standard output and any errors to standard error.

Running it by hand to optimize your scripts can be cumbersome. The intention is for it to act as a transparent filter; however, as of this writing there's no support for any viewer or IDE as it has just been released. Run it without parameters to see the invocation help, for example with `python main.py`.

Future plans include writing a patch for Firestorm to enable it to run external script filtering programs instead of the internal preprocessor and optimizer. That would allow this program to be run using Firestorm's integrated machinery, making usage pretty transparent to the programmer.

Extensions for other IDEs like Eclipse are not planned, but the author encourages others to write them. Please notify Sei Lisa if you write one, so that it can be listed in this page.

The program uses two external files. One is `builtins.txt`, which is in the same format as needed by [lslint](https://github.com/pclewis/lslint), and an up-to-date copy can be obtained from the _kwdb_ project: [https://bitbucket.org/Sei_Lisa/kwdb/raw/tip/outputs/builtins.txt](https://bitbucket.org/Sei_Lisa/kwdb/raw/tip/outputs/builtins.txt). The other is `seftable.txt`, which is a list of functions that have no side effects (SEF stands for side-effect free), allowing them to be removed (optimized out) if nothing else has side effects in an expression and the result of the expression is not used.

## Other future plans

Making the optimizer smarter is one primary objective. Conversion to [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form) form is currently not performed, and would be nice to have, for one, to enable more optimizations where values are not used.

Another goal is to add the possibility to link each output line with its source counterpart, so that in case of a server side compilation error, the source line corresponding to the actual line where the error happened can be displayed. Currently, server-side compilation errors should be rare (unless there are bugs in the optimizer) as there will be few situations in which they won't be caught by the optimizer's parser first.

Lastly, implementation of some kind of machine-readable way to inform the invoker about the available options, is also in the plans. That would allow the plugin or viewer or whatever to present a dialog with the options for invoking the optimizer.

## License

This program is distributed under the terms of the GNU General Public License (GPL) version 3.

(C) Copyright 2015 Sei Lisa. All rights reserved.

## Author

Written by Sei Lisa. Sei Lisa is the author's pseudonym and user name in the Second Life virtual world.

Happy coding!
