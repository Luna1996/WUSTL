const AES = require('aes-js');

function crack(p1, c1, c2) {
  const P1 = AES.utils.utf8.toBytes(p1);
  const C1 = AES.utils.hex.toBytes(c1);
  const C2 = AES.utils.hex.toBytes(c2);
  const P2 = new Uint8Array(C2.length);
  const O = new Uint8Array(P1.length);
  for (var i = 0; i < P1.length; i++)
    O[i] = P1[i] ^ C1[i];
  for (var i = 0; i < C2.length && i < O.length; i++)
    P2[i] = O[i] ^ C2[i];
  return AES.utils.utf8.fromBytes(P2);
}

const P2 = crack(
  'This is a known message!',
  'a469b1c502c1cab966965e50425438e1bb1b5f9037a4c159',
  'bf73bcd3509299d566c35b5d450337e1bb175f903fafc159'
);