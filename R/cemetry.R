#ungenutzte Code-Reste, alternative Loesungen und Debugging
#Alexander B.
#20.03.16

#+++Extraktion aller mach-Meldungen, geordnet nach Flugzeuge+++
raw <- raw[which(raw$format %in% c("A0","A8")),]
icaos <- unique(raw$address)
liste <- vector("list",length(icaos))
for(i in 1:length(icaos)){
  icaos.length <- length(raw$address[raw$address == icaos[i]])
  if(icaos.length > 20){
    print(c(i,icaos.length,icaos[i]))
    liste[[i]] <- raw$mach[which(raw$address == icaos[i])]
    liste[[i]] <- liste[[i]][which(!is.na(liste[[i]]))]
  }
}


#+++Dekodierung MRAR+++
raw2 <- raw[raw$format!="8D",]

temp.bits <- paste0(substr(raw2$bin,37,37),substr(raw2$bin,67,67))
temp.rows <- which(temp.bits == "11")

#Temperatur
temp <- strtoi(substr(raw2$bin[temp.rows], 57, 66), base=2)/4
temp <- temp*(1-2*strtoi(substr(raw2$bin[temp.rows], 56, 56), base=2))

#Winjdgeschwindigkeit
wind.spd <- strtoi(substr(raw2$bin[temp.rows], 38, 46), base=2)

#Windrichtung
wind.dir <- strtoi(substr(raw2$bin[temp.rows], 47, 55), base=2)*180/256

#Feuchte
humid <- strtoi(substr(raw2$bin[temp.rows], 83, 88), base=2)*100/64

#average static pressure
press <- strtoi(substr(raw2$bin[temp.rows], 68, 78), base=2)

#megen
mrar <- data.frame(cbind(temp, wind.spd, wind.dir, humid, press))
names(mrar) <- c('temp','spd','dir','hum','press')

#Filterbedingungen, offiziell nach ICAO
#temperature requirement fuer static air temperature nach Annex 3: -80..60C
#range of wind speeds requirement nach Annex 3: 0..250Kt
#average static pressure: 100..1045hPa
mrar$temp[which((mrar$temp < -70) | (mrar$temp > 40))] <- NA
mrar$spd[which(mrar$spd > 150)] <- NA
mrar$press[which((mrar$press < 150)  | (mrar$press > 1045))] <- NA

#selektiere Daten raus, mit Statusbit fuer Feuchte = 0 aber Feuchtewert groesser 0
mrar$hum[which((strtoi(substr(raw2$bin[temp.rows], 82, 82), base=2) == 0) & (strtoi(substr(raw2$bin[temp.rows], 83, 88), base=2) > 0))] <- NA

mrar[which(is.na(mrar$temp) | is.na(mrar$spd) | is.na(mrar$hum) | is.na(mrar$press)), names(mrar)] <- NA

#+++ENDE+++
