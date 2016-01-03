# psc-pane

Auto reloading PureScript compiler designed to be used in a small terminal
window beside your editor.

```
npm install psc-pane -g
```

![screencast](http://anttih.github.io/psc-pane/psc-pane.gif)

`psc-pane` watches directories for changes and invokes `psc` when a change occurs.
Only one error is displayed at a time and the output is made to fit the window
(only height currently).

Example:

```
psc-pane --watch-path src --ffi 'src/**/*.js' 'src/**/*.purs'
```
