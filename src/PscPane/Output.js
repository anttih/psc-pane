"use strict";

exports.write = function (str) {
  return function () {
    process.stdout.write(str);
  };
};

