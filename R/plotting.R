#Plotting-Routinen fuer Mode-S-Daten
#Alexander B.
#15.12.16

#Funktion zum Trimmen von Datensaetzen mit Ausreissern
trim <- function(x,perc=0.05){
  x.trim <- x[x <= quantile(x, prob=(1-perc), na.rm=TRUE) &
              x >= quantile(x, prob=(perc), na.rm=TRUE)]
  return(x.trim)
}

#Interpolationsfunktion fuer Wind- und Temperatur in aequidistanten
#Hoehenstufen und auf ausgewaehlten Standarddruckflaechen
interpol <- function(data, height.diff=500){  #Interpolationshoehenstufen (in Fuß)
  #Initialisiere
  height.max <- 45000 #Obergrenze fuer Interpolationswerte
  height.min <- -500 #Untergrenze
  #Hoehen in ft der "mandatory pressure levels" (925,850,700,500,400,300hPa)
  man.lvl <- c(2500,4781,9882,18289,23574,30065)
  #Umrechnung des Windes in kartesiche Vektorkomponenten
  #allokiere Dummy-Matrix fuer interpolierte Werte in aquidistanten
  #Struktur:
  #hoehe | mean(temp) | sd(temp) | Datenpunkte(temp) | {gleiches fuer x- und y-Wind}
  inpol <- data.frame(cbind(c(seq(height.min,height.max,height.diff),man.lvl),
                            NA,NA,NA,NA,NA,NA,NA,NA))
  names(inpol) <- c('alt','temp.mean','temp.sd','temp.count',
                    'x.mean','x.sd','y.mean','y.sd','xy.count')
  for (i in 0:nrow(inpol)){
    h1 <- inpol$alt[i]-height.diff/2 #untere Grenze
    h2 <- inpol$alt[i]+height.diff/2 #obere Grenze
    data.slice <- data[which(data$alt < h2 & data$alt >= h1),] #schneide Hoehenschicht aus
    if (nrow(data.slice) >= 20){ #mindestens 20 Messpunkte
      #Temperatur
      data.slice.temp <- trim(data.slice$temp)
      inpol$temp.mean[i] <- mean(data.slice.temp, na.rm=TRUE)
      inpol$temp.sd[i] <- sd(data.slice.temp, na.rm=TRUE)
      inpol$temp.count[i] <- length(data.slice.temp)
      #Wind
      #sortiere zunaechst >95% Perzentil der Windgeschwindigkeit aus
      data.slice <- data.slice[data.slice$wspd <= quantile(data.slice$wspd,prob=1-0.05),]
      #zerlege in X- und Y-Koordinaten
      data.slice.xy <- cbind(-data.slice$wspd*sin(data.slice$wdeg*pi/180),
                             -data.slice$wspd*cos(data.slice$wdeg*pi/180))
      inpol$x.mean[i] <- mean(data.slice.xy[,1], na.rm=TRUE)
      inpol$y.mean[i] <- mean(data.slice.xy[,2], na.rm=TRUE)
      inpol$x.sd[i] <- sd(data.slice.xy[,1], na.rm=TRUE)
      inpol$y.sd[i] <- sd(data.slice.xy[,2], na.rm=TRUE)
      inpol$xy.count[i] <- nrow(data.slice.xy)
    }
  }
  inpol <- as.data.frame(cbind(NA,inpol))
  names(inpol) <- c('press', names(inpol[-1]))
  inpol$press <- press(inpol$alt) #rechne Druckwerte in Druckkordinaten um
  inpol <- inpol[order(inpol$alt),]
  return(inpol)
}

#berechne Mittel fuer Standarddruckflaechen
interpol.gpot <- function(data, lvl.diff=500){ #Schichtdicke des Mittels in ft
  presslvl <- data.frame(cbind(c(925,850,700,500,400,300),
                               c(2500,4781,9882,18289,23574,30065),NA,NA,NA))
  names(presslvl) <- c('press','alt','gpot','gpot.sd','count')
  for (i in 1:nrow(presslvl)){
    data.slice <- data$alt.geo[which(data$alt <= presslvl$alt[i]+lvl.diff/2 & data$alt >= presslvl$alt[i]-lvl.diff/2)]
    if(length(data.slice) >= 20){ #Mindestzahl an Messungen!
      data.slice <- trim(data.slice)
      presslvl$gpot[i] <- mean(data.slice, na.rm=TRUE)*0.3048
      presslvl$gpot.sd[i] <- sd(data.slice, na.rm=TRUE)*0.3048
      presslvl$count[i] <- length(data.slice)
    }
  }
  return(presslvl)
}

#+++Domain selektieren+++
domain <- c(12,15,51.5,53.5) #lonmin,lonmax,latmin,latmax
raw <- raw[which(raw$lon >= domain[1] & raw$lon <= domain[2] & raw$lat >= domain[3] & raw$lat <= domain[4]),]

raw.pol <- interpol(raw) #T- und Windmittel berechnen
raw.pol.gpot <- interpol.gpot(raw) #Geopotential fuer Standarddruckflaechen

#+++Speichern interpolierter Groeßen+++

#+++Plotting+++
png(paste0(dir.plot, '/modes_ber_',datestr,'.png'), width=1200, height=800)

stdlvl <- -log(c(1000,925,850,700,500,400,300,200,150)) #plotte Standardlevel

raw.pol <- cbind(-log(raw.pol$press),raw.pol) #log-Skalierung des Drucks als Vertikalkoordinate
raw.pol.gpot <- cbind(-log(raw.pol.gpot$press), raw.pol.gpot)
names(raw.pol)[1] <- 'logp'
names(raw.pol.gpot)[1] <- 'logp'

#rechne kartesiche Windkoordinaten in Polarkoordinaten um
raw.pol <- cbind(raw.pol, (atan2(-raw.pol$x.mean, -raw.pol$y.mean)*180/pi)%%360, sqrt(raw.pol$x.mean^2+raw.pol$y.mean^2))
names(raw.pol) <- c(names(raw.pol)[-c(12,13)],'wdeg','wspd')

raw.pol.gpot <- raw.pol.gpot[!is.na(raw.pol.gpot$gpot),]

plot(0,0,ylim=c(-log(1030),-log(150)),xlim=c(-70,100), type='l', lwd=3,
     axes=FALSE, main=paste0('Berlin (Mode-S) ',substr(datestr,7,8),'.',substr(datestr,5,6),'.',
                             substr(datestr,1,4),' ',substr(datestr,10,11),'z'),
     ylab='Druckhoehe (hPa)', xlab='Temperatur (C)', yaxs='i')
abline(h=stdlvl, col='grey')
abline(v=seq(-80,40,10), col='grey')
if (length(raw.pol.gpot) > 0){
  text(rep(-70,length(raw.pol.gpot)),raw.pol.gpot$logp+0.02,paste0(round(raw.pol.gpot$gpot),'m'))
}

points(raw$temp,-log(raw$press), pch=20, cex=0.5, col=rgb(1,0,0,0.2))
points(raw.pol$temp.mean,raw.pol$logp, pch=20)
lines(raw.pol$temp.mean,raw.pol$logp, lwd=3)
par(las=1)
box()
axis(1, at=seq(-70,30,10), labels=seq(-70,30,10))
axis(2, at=stdlvl, labels=round(exp(-stdlvl)))
text(10,-log(200), paste0('Datenpunkte: ', nrow(raw)), font=2)
 
par(fig=c(0.6,1,0,1), new=TRUE)
plot(0,type='l', ylab='', xlab='ff (kt)', yaxt='n', axes=FALSE, xlim=c(0,300), ylim=c(-log(1030),-log(150)),
     xaxs='i',yaxs='i')
abline(v=seq(0,150,25), col='grey')
points(raw$wspd, -log(raw$press),  pch=20, cex=0.5, col=rgb(1,0,0,0.2))
points(raw.pol$wspd, raw.pol$logp, pch=20)
lines(raw.pol$wspd, raw.pol$logp, lwd=3)
box()
axis(1, at=seq(0,150,25), labels=seq(0,150,25))

par(fig=c(0.78,1,0,1), new=TRUE)
plot(0,
     type='l', ylab='', xlab='dd (deg)', yaxt='n', axes=FALSE, xlim=c(0,360), ylim=c(-log(1030),-log(150)),
     xaxs='i',yaxs='i')
abline(v=seq(0,360,90), col='grey')
points(raw$wdeg, -log(raw$press), pch=20, cex=0.5, col=rgb(1,0,0,0.2))
points(raw.pol$wdeg, raw.pol$logp, pch=20)
lines(raw.pol$wdeg, raw.pol$logp, lwd=3)
box()
axis(1, at=seq(0,360,90), labels=seq(0,360,90))

dev.off()

#loesche erst Hilfszeile, runden
raw.pol <- round(raw.pol[,-1],3)

#Speichern der gemittelten Daten
write.table(raw.pol, row.names=FALSE, file=paste0(dir.mean, 'modes_ber_means_', datestr, '.dat'))
