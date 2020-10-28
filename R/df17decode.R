#Dekodierungsroutinen für DF17-Bloecke
#Alexander B.
#15.10.14

#+++Funktionen+++
df17altpos <- function(data, va, tc){
  
  nl <- function (rlati){
    n <- floor(2*pi*(acos(1-((1-(cos(pi/30)))/(cos(pi/180*abs(rlati))^2))))^-1)
    return(n)
  }
  
  #+++Initialisierung+++
  icaos <- unique(as.character(data$address))
  
  #+++CPR-Decoding+++
  dlat0 <- 6 #initialisiere Dlat0/1-Werte (entspricht Ausdehnung in Grad der Breitengradzonen, 6° und ~6,102°)
  dlat1 <- 360/59
  nb <- 2^17

  for (i in 1:length(icaos)){ #gruppiere nach Flugzeug und Zeitstempel, um passende Flagpaare zu finden. Maximale Zeitdifferenz: 15s zwischen Positionsmeldungen
    #print(as.character(data$address[va]))
    if (length(which(as.character(data$address[va]) %in% icaos[i])) > 1) {
      done <- FALSE #initialisiere default-Variable zur Prozedur zum Rueckschreiben in die raw-Matrix
      rows <- which(data$address == icaos[i])#extrahiere Reihen, in der das Flugzeug in der raw-Matrix vorhanden ist, um das temp-Datenfeld wieder in die raw-Matrix zurueckzuschreiben
      temp <- data[rows,!(names(data) %in% c("format","mess","bin","alt"))] #hole noetige Datenfelder
      temp <- cbind(rows, temp); rm(rows)
      temp <- temp[which(temp$type %in% tc),] #filtere die richtigen TYPE Formate
      pos.dec <- cbind(NA,NA,NA,NA) #initialisiere Variable fuer Positionspaare, Format: lat(0), lon(0), lat(1), lon(1)
      for (j in 2:nrow(temp)){
        #fuehre even- und odd-Paare zusammen, die weniger als 15s auseinadnerliegen
        if ((temp$flag[j] != temp$flag[j-1]) && ((temp$time[j]-temp$time[j-1]) <= 15) ){
          even <- j-(1*temp$flag[j]) #setze Koordinate fuer even-Eintrag (lat/lon(0))
          odd <- j-(1*(even-j+1)) #fuer odd-Eintrag (lat/lon(1))
          pos.dec <- c(temp$lat.dec[even], temp$lon.dec[even], temp$lat.dec[odd], temp$lon.dec[odd])
          la <- floor(((59*pos.dec[1]-60*pos.dec[3])/nb)+0.5) #j-Index (hier: la)
          rlat0 <- dlat0*((la %% 60)+pos.dec[1]/nb)
          rlat1 <- dlat1*((la %% 59)+pos.dec[3]/nb)
          if(nl(rlat0)==nl(rlat1)){ #ist NL(0) und NL(1) gleich groß?
            temp$lat[even] <- rlat0 #schreibe lat Werte zurueck in die temp-Matrix --> finale lat-Position
            temp$lat[odd] <- rlat1
            n0 <- nl(temp$lat[even])-temp$flag[even]
            n1 <- nl(temp$lat[odd])-temp$flag[odd]
            dlon0 <- 360/n0 #berechnet lon zone width in deg fuer letzten Frame eines Positionspaares
            dlon1 <- 360/n1
            m0 <- floor(((((nl(temp$lat[even])-1)*pos.dec[2])-(nl(temp$lat[even])*pos.dec[4]))/nb)+0.5)
            m1 <-floor(((((nl(temp$lat[odd])-1)*pos.dec[2])-(nl(temp$lat[odd])*pos.dec[4]))/nb)+0.5)
            rlon0 <- dlon0*((m0 %% n0)+(temp$lon.dec[even]/nb)) #finale lon-Position fuer even Paar
            rlon1 <- dlon1*((m1 %% n1)+(temp$lon.dec[odd]/nb))
            temp$lon[even] <- rlon0
            temp$lon[odd] <- rlon1
            done <- TRUE #Dekodierung erfolgreich abgeschlosse, done auf TRUE setzen, um rueckschreiben in raw-Matrix zu erzwingen
          }
        }
      }
      if(done==TRUE){
        data$lat[temp$rows] <- temp$lat
        data$lon[temp$rows] <- temp$lon
      }
    }
  }
  return(data)
}

#+++TC19-Decoding
df17tc19 <- function(data){
  
  rows <- which(data$type == 19) #selektiere Datenfelder
  temp <- data[rows,]
  
  st <- strtoi(substr(temp$bin, 38, 40), base=2) #bestimme Subtypes
  st1 <- which(st == 1)
  st3 <- which(st == 3)
  
  #Dekodierung für subtype 1
  #dekodiere groundspeed (gs), track (trk), vertical speed/rate (vs) und gnss alt. diff. (hdiff)
  ewv <- as.integer(substr(temp$bin[st1], 46, 46)) #east-west-velocity-bit (0=east, 1=west)
  nsv <- as.integer(substr(temp$bin[st1], 57, 57)) #north-south-velocity-bit (0=north, 1=south)
  gs.ew <- (strtoi(substr(temp$bin[st1], 47, 56), base=2)-1)*(-1+ewv*2) #Ost-West-Geschwindigkeit
  gs.ns <- (strtoi(substr(temp$bin[st1], 58, 67), base=2)-1)*(-1+nsv*2) #Nord-Sued-geschwindigkeit
  temp$gs[st1] <- sqrt(gs.ew^2+gs.ns^2) #berechne groundspeed
  temp$trk[st1] <- atan2(gs.ew,gs.ns)/pi*180+180 #berechne track
  #dekodiere vertical speed
  temp$vs[st1] <- (strtoi(substr(temp$bin[st1], 70, 78), base=2)-1)*64*(1-as.integer(substr(temp$bin[st1], 69, 69))*2)
  #dekodiere Hoehendifferenz zw. geom. und barom. Hoehe
  temp$hdiff[st1] <- strtoi(substr(temp$bin[st1], 82, 88), base=2)*25*(1-as.integer(substr(temp$bin[st1], 81, 81))*2)
  
  #Dekodierung für subtype 3
  hdg.flag <- which(strtoi(substr(temp$bin[st3], 46, 46),base=2) == 1) #dekodiere hdg
  hdg <- strtoi(substr(temp$bin[st3[hdg.flag]], 47, 56),base=2) 
  temp$hdg[st3[hdg.flag]] <- 360/1024*hdg
  spd <- strtoi(substr(temp$bin[st3], 58, 67),base=2) #dekodiere airspeed (kias oder tas)
  kias.flag <- which(strtoi(substr(temp$bin[st3], 57, 57),base=2) == 0)
  tas.flag <- which(strtoi(substr(temp$bin[st3], 57, 57),base=2) == 1)
  temp$kias[st3[kias.flag]] <- spd[kias.flag]
  temp$tas[st3[tas.flag]] <- spd[tas.flag]
  temp$vs[st3] <- (strtoi(substr(temp$bin[st3], 70, 78), base=2)-1)*64*(1-as.integer(substr(temp$bin[st3], 69, 69))*2) #dekodiere vertical speed
  temp$hdiff[st3] <- strtoi(substr(temp$bin[st3], 82, 88), base=2)*25*(1-as.integer(substr(temp$bin[st3], 81, 81))*2)  #dekodiere hdiff
  
  #filtern
  data$gs[which(data$gs > 600)] <- NA
  data$vs[which(abs(data$vs) > 5000 )] <- NA
  #data$hdiff[which(abs(data$hdiff) > 3000)] <- NA
  data$kias[which(data$kias > 400)] <- NA
  data$tas[which(data$tas > 600)] <- NA
  data[rows,] <- temp
  
  return(data)
}

#+++Hauptroutine DF17-Dekodierung+++
df17decode <- function(raw.bin.df17){
  
  #Initialisierung  
  typecodes <- c(9:18) #selektierte TYPE-Codes fuer Hoehen- und Positionsberechnung fuer DF17-Bloecke
  raw.bin.df17$type <- strtoi(substr(raw.bin.df17$bin, 33, 37), base=2) #extrahiere: TYPE format code, Bit 1-5
  validalt <- which(raw.bin.df17$type %in% typecodes) #zum filtern der relevanten Matrixelemente zur Dekodierung
  raw.bin.df17$alt[validalt] <- strtoi(paste0(substr(raw.bin.df17$bin[validalt], 41, 47), substr(raw.bin.df17$bin[validalt], 49, 52)), base=2)*25-1000 #Hoehe
  raw.bin.df17$flag[validalt] <- strtoi(substr(raw.bin.df17$bin[validalt], 54, 54)) #extrahiere F-Flags, wo TYPE-Codes zu gebrauchen sind
  raw.bin.df17$lat.dec[validalt] <- strtoi(substr(raw.bin.df17$bin[validalt], 55, 71), base=2)
  raw.bin.df17$lon.dec[validalt] <- strtoi(substr(raw.bin.df17$bin[validalt], 72, 88), base=2)
  raw.bin.df17$time <- as.integer(as.character(raw.bin.df17$time))
  
  #decodiere CPR und Hoehe
  raw.bin.df17 <- df17altpos(raw.bin.df17, validalt, typecodes)

  #decodiere BDS-Register 0,9 (subtypes 1 + 3) in DF17-messages
  raw.bin.df17 <- df17tc19(raw.bin.df17)
  
  raw.bin.df17$format2 <- 17 #setze format2-Klasse
  
  return(raw.bin.df17)
}
