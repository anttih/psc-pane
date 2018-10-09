// module PscPane.Program

"use strict";

exports.minimatch = function(str) {
  return function(glob) {
    return require("minimatch")(str, glob);
  };
};