#!/bin/bash
#
#listens via nc to a selected port to save a AVR-MLAT hex
#data stream
#
#Alexander B.
#22.09.16

#current time for output file name
OFILE=$(echo ~/adsb/raw/$(date -u -d "+1 hour" +%Y%m%d_%H).dat)
IP=160.45.77.135 #host-IP
PORT=30002 #host-port (default: 30002)

#using netcat

while true
do
    nc $IP $PORT >> $OFILE
    sleep 1 #if connection is lost, wait for a second and reconnect
done
