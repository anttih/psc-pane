# psc-pane

Auto reloading compiler for PureScript inspired by [ghcid](https://github.com/ndmitchell/ghcid).

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

`psc-pane` watches directories for changes and invokes `psc` when a change occurs.
Only one error is displayed at a time and the output is made to fit the window
(only height currently).

Example:

```
psc-pane --watch-path src --ffi 'src/**/*.js' 'src/**/*.purs'
```
