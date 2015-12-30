// module Main

// Shamelessly stolen from Pulp:
// https://github.com/bodil/pulp/blob/master/src/Pulp/Watch.js

"use strict";

exports.exec = function (cmd) {
  return function (fail) {
    return function (success) {
      return function () {
        return require('child_process').exec(cmd, function (err, stdout, stderr) {
          return success({stdout: stdout, stderr: stderr})();
        });
      };
    };
  };
};

exports.watch = function watch(directories) {
  return function(act) {
    return function() {
      var Watchpack = require("watchpack");
      var watchpack = new Watchpack();
      watchpack.watch([], directories, Date.now() - 10000);
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
