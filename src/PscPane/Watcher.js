// module PscPane.Watcher

"use strict";

exports["watch'"] = function watch(dirs) {
  return function(act) {
    return function() {
      var Watchpack = require("watchpack");
      var watchpack = new Watchpack();
      watchpack.watch([], dirs, Date.now());
      watchpack.on("change", function(path) {
        act(path)();
      });
    };
  };
};

