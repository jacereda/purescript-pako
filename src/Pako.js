"use strict";
import pako from 'pako';

export const hello = () => console.log("hello");

export const deflateImpl = options => bytes => () => pako.deflate(bytes, options).buffer;

export const inflateImpl = bytes => () => pako.inflate(bytes).buffer;

// from https://developer.chrome.com/blog/how-to-convert-arraybuffer-to-and-from-string/
export const toString = buf => String.fromCharCode.apply(null, new Uint16Array(buf));

export const fromString = str => {
    var buf = new ArrayBuffer(str.length*2); // 2 bytes for each char
    var bufView = new Uint16Array(buf);
    for (var i=0, strLen=str.length; i < strLen; i++) {
        bufView[i] = str.charCodeAt(i);
    }
    return buf;
};
