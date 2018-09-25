const AES = require('aes-js');

const key = AES.utils.hex.toBytes('00112233445566778899aabbccddeeff');
const iv1 = AES.utils.hex.toBytes('31323334353637383930313233343536');
const iv2 = AES.utils.hex.toBytes('31323334353637383930313233343537');

function crack(msg) {
  const b1 = AES.utils.utf8.toBytes(msg);
  const b2 = new Uint8Array(16);
  for (var i = 0; i < 16; i++)
    b2[i] = iv1[i] ^ iv2[i] ^ (i < b1.length ? b1[i] : 13);
  return b2;
}

const c1 = 'bef65565572ccee2a9f9553154ed9498';

const Bob = new AES.ModeOfOperation.cbc(key, iv2);
const p2 = crack('Yes');
const c2 = AES.utils.hex.fromBytes(Bob.encrypt(p2));