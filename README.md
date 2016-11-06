# psc-pane

Auto reloading compiler for PureScript inspired by
[ghcid](https://github.com/ndmitchell/ghcid).

`psc-pane` helps you get quick feedback on your PureScript code by watching
your source files for changes. It will

1. try to first compile the file that changed with psc-ide and show the first
   error if any. If there are no errors it will
2. run a full build using `psc` and show the first error if any. If there are
   no errors it will then
3. (optionally) run your test suite.

`psc-pane` is designed to be run in a terminal multiplexer like tmux side-by-side
with your editor, or in a normal terminal window. You can resize the terminal
window and the output is reformatted on the fly to fit the new height of the
window.

![screencast](http://anttih.github.io/psc-pane/psc-pane.gif)

### Installation

```
npm install psc-pane -g
```

### Usage

If you have a [pulp](https://github.com/bodil/pulp)-style project directory
structure you can run `psc-pane` with no arguments

    psc-pane

This will compile your project with `psc` and then start listening for changes
to files in `src/`. It will start a `psc-ide-server` for you in some port
between 4242-4245 and compile any file you save with psc-ide. If you change a
.js file then `psc` is used to compile the whole project.

By default `psc-pane` assumes you have installed your dependencies with bower
and uses `bower_components` as the dependency path. You can specify a different
directory with `--dependency-path`.

    psc-pane --dependency-path lib

You can change the source directory (`src` by default) with `--src-path`.

    psc-pane --src-path sources

For the full list of options see `--help`.

### Running tests

`psc-pane` won't run any tests unless you use the `-t` (`--test`) flag. When
`-t` is given the tests will be run after every successful rebuild. The default
location of test source files is `test`, you can change it with `--test-path`.
This path is watched for changes in addition to the src path (`--src-path`)
when `-t` is given.

By default the test suite is run by calling the `main` function from the
module `Test.Main`. You can change the module name with `--test-main`. If the
test suite exits with a non-zero exit code the output from the tests will be
displayed. The output from stderr is used but when empty, stdout is used.

### Turning off colors

You can turn off colors with `--nocolor`.

