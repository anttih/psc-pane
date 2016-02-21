# psc-pane

Auto reloading compiler for PureScript inspired by [ghcid](https://github.com/ndmitchell/ghcid).

`psc-pane` watches directories for changes and invokes a build command (`pulp build` by default)
when a change occurs.
Only one error is displayed at a time and the output is made to fit the window
(only height currently).

### Installation

```
npm install pulp psc-pane -g
```

### Usage

`psc-pane` uses `pulp build` by default as the build command. You can change
this using the `-c` flag, but just make sure the output is JSON.

