"use strict";
var pako = require('pako');

exports.deflateImpl = function(options) {
  return function(bytes) {
    return function() {
      return pako.deflate(bytes, options);
    };
  };
};

exports.inflateImpl = function(bytes) {
  return function() {
    return pako.inflate(bytes);
  };
};
