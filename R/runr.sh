#!/bin/bash
#Auswertung von TEMP- und Mode-S-Daten in R - Initialisierung
#Alexander B.
#04.01.2017

USER="user" #SSH-User
TARG="target_host" #Zielserver
TDIR="/target/dir/" #Zielordner
HDIR="/home/user" #Home-Directory
DATS=$(date -u +%Y%m%d_%H)
DATS_2=$(date -u -d "-1 hour" +%Y%m%d_%H) #Date-String vor einer Stunde
DATS_3=$(date -u -d "-24 hour" +%Y%m%d_%H) #Date-String vor 24 Stunden

#hole TEMP-Daten
#case $(date -u -d '+240sec' +%H) in
#    1|07|13|19)
#    FILES=$(echo ftp://ftp-outgoing2.dwd.de/gds/OBS/TEMP/gda01-temp-$(date -u -d '-1 hour' +%Y%m%d%H)\*)
#    wget --user=gds13239 --password= -P -nc ~/adsb/raw/ $FILES
#    R CMD BATCH ~/adsb/R/temp.R
#    rm -r ~/adsb/raw/gda01-temp-*
#    ;;
#esac

R CMD BATCH ~/adsb/R/adsb-read2.R
OFILE=$(echo ~/adsb/raw/$DATS_2.dat)

#Kopiere zum og04-Server bzw. masp09
scp -r $OFILE $USER@$TARG:$TDIRraw/
rm $OFILE
scp -r $HDIR/adsb/data/modes_ber_$DATS.dat $USER@$TARG:$TDIR
scp -r $HDIR/adsb/data/plots/modes_ber_$DATS.png $USER@$TARG:$TDIR/plots/
scp -r $HDIR/adsb/data/means/modes_ber_means_$DATS.dat $USER@$TARG:$TDIR/means/
scp -r $HDIR/adsb/data/special_flights/*$DATS.dat $USER@$TARG:$TDIR/special_flights/



#loesche Dateien, die aelter als 24h sind
rm $HDIR/adsb/data/modes_ber_$DATS_3.dat
rm $HDIR/adsb/data/plots/modes_ber_$DATS_3.png
rm $HDIR/adsb/data/means/modes_ber_means_$DATS_3.dat
if [ -s $HDIR/adsb/data/special_flights/*$DATS_3.dat ]
then
    scp -r $HDIR/adsb/data/special_flights/*$DATS.dat $USER@$TARG:$TDIR/special_flights/
    rm $HDIR/adsb/data/special_flights/*$DATS_3.dat
fi

