
var concat = require('concat-stream');

exports.spawn = function (cmd, success) {
  return function () {
    var proc = require('child_process').spawn("sh", ["-c", cmd]);
    proc.stderr.pipe(concat(function (buf) {
      success(buf)();
    }));
  };
};

exports.rows = function () {
  if (process.stdout.isTTY) {
    return process.stdout.rows;
  } else {
    throw Error("Cannot get row count");
  }
};
