#!/usr/bin/python
#listens to a selected ip and port to save an AVR-MLAT hex data stream
#
#Alexander B.
#15.08.16

import socket
import time

datestr = time.strftime('%Y%m%d_%H', time.gmtime(time.time()+3600))
ofile = '/all_mounts/people/alexb/adsb/raw/' + datestr + '.dat' #Outputfile
ip = '160.45.77.135' #Host-IP
port = 30002 #Host-Port
buff = 4096 #buffersize

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((ip, port))

f = open(ofile, 'w+')

while True:
	f.write(s.recv(buff))
	
f.close()
