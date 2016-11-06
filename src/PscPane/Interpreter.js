
exports.rows = function () {
  if (process.stdout.isTTY) {
    return process.stdout.rows;
  } else {
    throw Error("Cannot get row count");
  }
};
