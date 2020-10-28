#Berechnungsroutinen zur meteorologischen Auswertung
#Alexander B.
#07.11.15

#+++Funktionen+++

#Temperaturberechnung
tempcalc <- function(data.temp){
  #Die Temperatur in K ergbit sich aus dem quadrierten Verhältnis von true airspeed und der Mach-Zahl,
  #multipliziert mit einer Konstanten
  data.temp$temp <- (((data.temp$tas*0.514444444444)^2)/((data.temp$mach)^2)*2.4923E-3)-273.15
  #filtern
  data.temp <- data.temp[which(data.temp$temp >= -80 & data.temp$temp <= 50),]  

  return(data.temp)
}

#Windberechnung
windcalc <- function(data.wind){
  #Der Windvektor ergibt sich einfach aus dem Differenzvektor zwischen heading+true airspeed (umgebene Luft ist Bezugssystem)
  #und track+groundspeed (Boden ist Bezugssystem)
  #Deklinationskorrektur
  decl <- read.csv(file2, header = FALSE, sep = " ", col.names = c("lat","lon","dec"))
  
  data.wind <- cbind(data.wind,NA)
  names(data.wind) <- c(names(data.wind)[1:length(names(data.wind))-1],"decl")
  data.wind$decl <- decl[match(round(data.wind$lat,1)^2+round(data.wind$lon,1),(decl$lat^2+decl$lon)),3]
  data.wind$hdg <- data.wind$hdg+data.wind$decl
  data.wind$hdg[which(data.wind$hdg > 360)] <- data.wind$hdg[which(data.wind$hdg > 360)]-360
  data.wind$hdg[which(data.wind$hdg < 0)] <- data.wind$hdg[which(data.wind$hdg < 0)]+360
  #Zur Bildung des Differenzvektors muss zunaechst ins kartesiche Koordinatensystem gewechselt werden:
  ktas <- cbind(data.wind$tas*sin(data.wind$hdg*(pi/180)),data.wind$tas*cos(data.wind$hdg*(pi/180))) #u- und v-Komponente true airspeed
  kgs <- cbind(data.wind$gs*sin(data.wind$trk*(pi/180)),data.wind$gs*cos(data.wind$trk*(pi/180))) #u- und v-Komponente ground speed
  vdiff <- kgs-ktas #Differenzvektor
  data.wind$wdeg <- (atan2(vdiff[,1],vdiff[,2])*180/pi)+180
  data.wind$wspd <- sqrt((vdiff[,1])^2+(vdiff[,2])^2)
  #filtern
  data.wind <- data.wind[which(data.wind$wspd <= 200),]
  
  return(data.wind)
}

#Filterfunktion fuer Windgeschwindigkeit
#Bilde zunaechst Mittelwerte der Windgeschwindigkeit fuer 1000ft dicke Schichten,
#filtere anschliessend starke Abweichungen raus
windfilt <- function(data.filt){
  height.max <- round(max(data.filt$alt)/1000)*1000
  height.diff <- 1000 #Interpolationshoehenstufen
  inpol <- cbind(seq(0,height.max,height.diff),NA,NA)  #allokiere Dummy-Matrix fuer interpolierte 
  for(i in 0:nrow(inpol)){
    h1 <- inpol[i,1]-height.diff/2 #untere Grenze
    h2 <- inpol[i,1]+height.diff/2 #obere Grenze
    h1h2rows <- which(data$alt < h2 & data$alt >= h1)
    inpol[i,2] <- mean(data$temp[h1h2rows])
    inpol[i,3] <- sd(data$temp[h1h2rows])
  }
  return(inpol)
}

#+++Hauptroutine meteorologische Auswertung+++
meteocalc <- function(data){
  #Initialisierung
  #waehle Gitterbox
  #grid <- c(minlat,maxlat,minlon,maxlon)
  
  #Waehle maximale Zeitdifferenz zwischen den einzelnen asynchron ausgesendeten verschiedenen messages, welche zusammengefuehrt
  # werden sollen. Dieser Wert beeinflusst massgeblich, wieviele Einzelmessungen generiert werden koennen und welche Qualitaet
  #diese haben. Bei geringen Differenzen kommen weniger Messungen zusammen, die aber eine geringere Varianz aufweisen.
  diffsec <- 1
  
  #selektiere message-Tripel, sodass ein Tripel eine DF17 und zwei DF20/21 mit jeweils einem BDS 5,0 und 6,0 beinhaltet.
  #Der Zeitstempel wird dem fruehesten Block entnommen, sodass sich der Stempel des letzten Blocks durch time+tdiff ergibt.
  icaos <- unique(data$address)
  for(i in 1:length(icaos)){ #ueber alle Adressen
    data.selected <- data[which(data$address == icaos[i]),] #selektierte Daten fuer die eine jeweilige ICAO-Adresse
    timerange.prev <- 0
    if(nrow(data.selected) > 2){
      for(j in 1:(nrow(data.selected)-2)){ #gehe jede Zeit-Reihe durch, von den letzten beiden abgesehen
        t0 <- data.selected$time[j]
        timerange <- t0:(t0+diffsec)
        selected.rows <- which(data.selected$time %in% timerange)
        #Ueberspringe mehrfache quartettbildung der gleichen selektierten Bloecke, schreite nur mit mind. 4 messages
        #je Zeitabschnitt weiter fort und nur, wenn diese alle 4 messages beinhalten
        if(!all(timerange == timerange.prev) & (length(selected.rows) > 3) & 
             (length(unique(data.selected$format[selected.rows])) == 4)){ 
          #bilde Mittel fuer mehrfach vorhandene formate
          data.selected[selected.rows[1],-(1:3)] <-
            apply(data.selected[selected.rows,-(1:3)],2,mean, na.rm=TRUE)
          #zusaetlich: setze in format eine 1, womit angezeigt werden soll, dass die quartettbildung abgeschlossen ist.
          data.selected$format[selected.rows[1]] <- 1
        } 
        timerange.prev <- timerange
      }
    }
    data[which(data$address == icaos[i]),] <- data.selected
  }
  #filter gebildete Quartetts
  data <- data[which(data$format == 1),]
  #erweitere Dataframe fuer Windgeschwindigkeit, -Richtung, Temperatur und max. Zeitdifferenz der einzelnen Bloecke
  data <- cbind(data,NA,NA,NA)
  colnames(data) <- c(colnames(data[1:(ncol(data)-3)]), "wspd","wdeg","temp")
  #berechne geometrische Hoehe
  data$alt.geo <- (data$alt)+(data$hdiff)
  #berechne Temperatur
  data <- tempcalc(data)
  #berechne Wind
  data <- windcalc(data)

  return(data)
}
