# psc-pane

Auto reloading compiler for PureScript inspired by [ghcid](https://github.com/ndmitchell/ghcid).

`psc-pane` watches your source files for changes and compiles them using psc-ide-server.
Whenever the file you changed has no erros or warnings it will run a full rebuild
of your project. Only one error is displayed at a time and the output is made to fit
the window (only height currently).

### Installation

```
npm install psc-pane -g
```

### Usage

Run `psc-pane` in your PureScript project root directory:

```
psc-pane
```

This will start a psc-ide-server for you picking a port in range 4242-4252. It
will then watch for changes in .purs and .js files and rebuild and show errors
and warnings. When a .purs file is successfully rebuilt without warnings a full
rebuild is triggered using the build command. The default build command is

```
psc 'src/**/*.purs' 'bower_components/purescript-*/src/**/*.purs' --json-errors
```

but you can provide a different one using the `-c` flag as long as
that command returns errors in the psc JSON format:

```
psc-pane -c 'npm run build'
```

Changing a .js file will always trigger a full build.

