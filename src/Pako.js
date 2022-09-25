"use strict";
import pako from 'pako';

export const deflateImpl = options => bytes => () => pako.deflate(bytes, options).buffer;

export const inflateImpl = bytes => () => pako.inflate(bytes).buffer;

const ENCODING = "utf-8" 

export const toString = arrayBuffer => new TextDecoder(ENCODING).decode(new Uint8Array(arrayBuffer));

export function fromString(str) {
    let u8arr = new TextEncoder(ENCODING).encode(str);
    return u8arr.buffer.slice(u8arr.byteOffset, u8arr.byteLength + u8arr.byteOffset)
}
