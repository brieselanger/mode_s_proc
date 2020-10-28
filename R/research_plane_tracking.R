#Routine zum Tracking von Foschungsflugzeugen
#Alexander B.
#15.12.2016

setwd('/home/axel/Dropbox/Dokumente/WeWi/modes/adsb/R')

a=read.csv(file='./research_planes.csv', sep=';', skip=7, header=TRUE)

raw= #Rohdatensatz
