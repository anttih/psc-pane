# psc-pane

Auto reloading compiler for PureScript inspired by [ghcid](https://github.com/ndmitchell/ghcid).

`psc-pane` watches directories for changes and invokes `psc` when a change occurs.
Only one error is displayed at a time and the output is made to fit the window
(only height currently).

![screencast](http://anttih.github.io/psc-pane/psc-pane.gif)

### Installation

```
npm install psc-pane -g
```

You should also have `psc` in your `PATH`. To install both `psc` and `psc-pane`:

```
npm install purescript psc-pane -g
```

### Usage

If you have a [pulp](https://github.com/bodil/pulp/) style project layout with
source files under `src` and dependencies under `bower_components` you can run
`psc-pane` with no arguments

```
psc-pane
```

This is the same as running

```
psc-pane -w src -s 'src/**/*.purs' -s 'bower_components/purescript-*/src/**/*.purs' -f 'src/**/*.js' -f 'bower_components/purescript-*/src/**/*.js'
```
