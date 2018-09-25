const AES = require('aes-js');
const fs = require('fs');

const pound = [
  '###############',
  '##############',
  '#############',
  '############',
  '###########',
  '##########',
  '#########',
  '########',
  '#######',
  '######',
  '#####',
  '####',
  '###',
  '##',
  '#'
];
const words = fs.readFileSync('./keys.txt').toString().split('\r\n');

const iv = AES.utils.hex.toBytes('010203040506070809000a0b0c0d1e0f');
const p = 'This is a top secret.';
const c = AES.utils.hex.toBytes('e5033b20160edd1face0bc8377f412f28f6af6ccdae5ad096bc35a94a5378943');
let k, o, s;
let start_time = Date.now();
for (let i = 0; i < words.length; i++) {
  k = AES.utils.utf8.toBytes(words[i] + pound[words[i].length - 1]);
  o = new AES.ModeOfOperation.cbc(k, iv);
  s = AES.utils.utf8.fromBytes(o.decrypt(c));
  if (s.startsWith(p)) break;
}
console.log(Date.now() - start_time);
k = AES.utils.utf8.fromBytes(k);