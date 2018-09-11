#!/usr/bin/python3
from scapy/all import *

ip = IP(src="", dst="")
tcp = TCP(sport=0, dport=0, flags="0.0.0.0", seq=0, ack=0)
pkt = ip/tcp
ls(pkt)
send(pkt, verbose=0)