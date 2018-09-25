const AES = require('aes-js');

const str = AES.utils.utf8.toBytes(
  'The quick brown fox jumps over the laze dog.    '
);

const key = new Array(16);
const iv1 = new Array(16);
const iv2 = new Array(16);
for (var i = 0; i < 16; i++) {
  key[i] = Math.floor(256 * Math.random());
  iv1[i] = Math.floor(256 * Math.random());
  iv2[i] = Math.floor(256 * Math.random());
}

const o1 = new AES.ModeOfOperation.cbc(key, iv1);
const o2 = new AES.ModeOfOperation.cbc(key, iv1);
const o3 = new AES.ModeOfOperation.cbc(key, iv2);
const e1 = AES.utils.hex.fromBytes(o1.encrypt(str));
const e2 = AES.utils.hex.fromBytes(o2.encrypt(str));
const e3 = AES.utils.hex.fromBytes(o3.encrypt(str));