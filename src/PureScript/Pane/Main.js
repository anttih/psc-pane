// module PureScript.Pane.Main

// Shamelessly stolen from Pulp:
// https://github.com/bodil/pulp/blob/master/src/Pulp/Watch.js

"use strict";

var concat = require('concat-stream');

exports.spawn = function (cmd, args, fail, success) {
  return function () {
    var proc = require('child_process').spawn(cmd, args);
    proc.stderr.pipe(concat(function (buf) {
      success(buf)();
    }));
  };
};

exports.watch = function watch(dirs) {
  return function(act) {
    return function() {
      var Watchpack = require("watchpack");
      var watchpack = new Watchpack();
      watchpack.watch([], dirs, Date.now() - 10000);
      watchpack.on("change", function(path) {
        act(path)();
      });
    };
  };
};

exports.minimatch = function(str) {
  return function(glob) {
    return require("minimatch")(str, glob);
  };
};

exports.write = function (str) {
  return function () {
    process.stdout.write(str);
  };
};

exports.rows = function () {
  if (process.stdout.isTTY) {
    return process.stdout.rows;
  } else {
    throw Error("Cannot get row count");
  }
};
