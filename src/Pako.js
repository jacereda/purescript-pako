"use strict";
var pako = require('pako');

exports.deflateImpl = function(bytes) {
  return function() {
    return pako.deflate(bytes);
  };
};

exports.inflateImpl = function(bytes) {
  return function() {
    return pako.inflate(bytes);
  };
};
