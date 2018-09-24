const AES = require('aes-js');
const fs = require('fs');

const org = new Array(1008);
org.fill('1');

const key = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
const iv = [21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36];
const str = AES.utils.utf8.toBytes(org.join(''));

function corrupt(mode) {
  const o = new AES.ModeOfOperation[mode](key, iv);
  const e = o.encrypt(str);
  e[54] = ~e[54];
  const d = AES.utils.utf8.fromBytes(o.decrypt(e));
  fs.writeFileSync('./' + mode + '.txt', d);
}

corrupt('ecb');
corrupt('cbc');
corrupt('cfb');
corrupt('ofb');