Unit testing suites. These tests are run via run-tests.py.

The testing is performed using the standard Python module 'unittest'. This module accepts some command-line options; we don't add any ourselves. Use 'python run-tests.py -h' for help, or look here for details: https://docs.python.org/2/library/unittest.html#command-line-interface

There are 5 directories defined in the code that contain tests, all of which end in .suite: regression.suite, expr.suite, lso.suite, coverage.suite, preproc.suite.

The files that all 5 contain are the tests themselves. The run-tests.py program reads all .run and .lsl files in each directory and takes them as tests to run.

A test is a series of files all with the same name and with different extensions. Running the test means running the optimizer with the parameters in the .run file (if present), giving it the .lsl file (if present) as input. The output to stdout is compared to the .out file and the output to stderr is compared to the .err file. The test passes if both match and no exception is raised; otherwise it fails.

These are the extensions that the test suite recognizes in more detail:

  .lsl: Source file to test. If the first line starts with '// ', then the text in the rest of the line will be set as the docstring for the corresponding test function (visible when requesting verbose coverage).
  .run: Command line parameters for the test. Default is: 'main.py -', except in expr.suite which defaults to 'main.py -O clear,optimize,constfold,addstrings,expr -'. The syntax for the parameters is that of the Bourne shell, but '$' expansion is not supported. The 'main.py' part is the value that argv[0] will receive, but is unused otherwise.
  .out: Expected stdout resulting from the test. If the first line is REGEX then the rest of the file will be taken as a regular expression to match against, as opposed to matching the whole file contents.
  .err: Expected stderr resulting from the test. It also allows regular expression.
  .skp: If present, the test will be skipped (not run). The contents of this file are taken as the reason for the expected failure.
  .fail: If present, the test will be run, and counted as an expected failure. If a .fail file is present and the test passes, that will be counted as an unexpected success, and the program will not report success at the end. The contents will be ignored; zero-length files are OK.

A test is considered such when either the .lsl or the .run file are present. The default stdin for the program is the .lsl file.

As for the meaning of each subdirectory:

- regression.suite contains normal regression tests.
- expr.suite contains regression tests that apply to expressions.
- lso.suite contains LSO mode tests. Note that LSO support is incomplete.
- coverage.suite contains tests that force certain code paths to execute, in order to maximize the exercised code coverage, even if some tests are trivial and unlikely to cause a regression.
- preproc.suite is for preprocessor-related tests.

As for the files in this directory:

- cat.py just copies standard input to standard output. Used as a preprocessor for some preprocessor tests.
- false.py just terminates with exit code 1.
- output-list.lsl is a utility script that dumps an arbitrary list to local chat, in a format very similar to the one that the expression suite outputs (it may require manual removal of timestamps). Float precision may differ.
