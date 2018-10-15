// module PscPane.Program

"use strict";

exports.minimatch = function(str) {
  return function(glob) {
    return require("minimatch")(str, glob);
  };
};

exports.rows = function () {
  if (process.stdout.isTTY) {
    return process.stdout.rows;
  } else {
    throw Error("Cannot get row count");
  }
};
