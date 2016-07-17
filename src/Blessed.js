
var blessed = require('blessed');

exports.mkScreen = function (options) {
  return blessed.screen(options);
};

exports.mkBox = function (options) {
  return blessed.box(options);
};

exports.render = function (screen) {
  return function () {
    screen.render();
  };
};

exports.on = function (screen) {
  return function (event) {
    return function (cb) {
      return function () {
        screen.on(event, function () {
          cb()();
        })
      };
    };
  };
};

exports.append = function (screen) {
  return function (box) {
    return function () {
      screen.append(box);
    };
  };
};

exports.setContent = function (box) {
  return function (content) {
    return function () {
      box.setContent(content);
    };
  };
};
