#!/net/opt/R/3.0.2/gcc472/7/x86_64/bin/Rscript
#Einlesen von TEMP-Daten
#Alexander B.
#08.02.2015
#
#Informationsquellen: http://weather.unisys.com/wxp/Appendices/Formats/TEMP.html
#                     http://www.uni-koeln.de/math-nat-fak/geomet/meteo/winfos/radiosonden/Europa/temp_frame.html
#Stand: Februar 2015

#+++Funktionen+++
int <- function(str,a,e){ #Funktion zur Umwandlung von char in int
  out <- as.integer(substr(str,a,e))
  return(out)
}

tem <- function(str,a){ #Temperatur- und Taupunktberechnung, Eingangsgroessen: string, pointer
  t <- c(NA,NA)
  t[1] <- int(str,a,a+2)*(1-2*(int(str,a+2,a+2)%%2))/10
  t[2] <- int(str,a+3,a+4)
  if (t[2] <= 50){
    t[2] <- t[1]-t[2]/10
  }
  else {
    t[2] <- t[1]-(t[2]-50)
  }
  return(t)
}

win <- function(str,a){ #Windberechnung
  w <- c(NA,NA)
  add <- int(str,a,a+2)%%5
  w[1] <- int(str,a,a+2)-add
  w[2] <- int(str,a+3,a+4)+add*100
  return(w)
}

#+++Einlesen von TEMP-Daten+++
files <- list.files(path='~/adsb/raw', pattern='gda01-temp-*')
n <- length(files)

getdata <- function(data){

}

df <- NULL

for (i in 1:n){
  dat  <- file(paste0('~/adsb/raw/',files[i]), open = "r")
  lines <- readLines(dat, warn=FALSE)
  close(dat)
  df <- c(df, lines)
}

df <- df[-which(df == "")] #loesche Leerzeilen

#Filter Daten fuer Station 10393 raus
strings <- c("TTAA ..... 10393", "TTBB ..... 10393") #Suchmaske
lines <- matrix(NA, nrow=2, ncol=2)
for (i in 1:2){
  lines[i,1] <- grep(strings[i], df)
  if(!is.na(lines[i,1])){
    pointer <- lines[i,1]
    while(is.na(lines[i,2])){
      if (length(grep("*=$",df[pointer])) == 1){
        lines[i,2] <- pointer
      }
      pointer <- pointer + 1
    }
  }
}

ttaa <- paste(df[lines[1,1]:lines[1,2]], collapse=' ')
ttbb <- paste(df[lines[2,1]:lines[2,2]], collapse=' ')

#+++TTAA-Decoding+++
#initialisiere Dummy-Matrix fuer mandatory-Werte (PRES,HGHT,TEMP,DEWP,WDEG,WSPD)
dat <- data.frame(matrix(NA, ncol=6, nrow=12))
names(dat) <- c('pres','hght','temp','dewp','wdeg','wspd')
dat[,1] <- c(NA,1000,925,850,700,500,400,300,250,200,150,100) #mandatory level, NA=surface level

#Section I
#YYGGI
yy <- FALSE  #if YY >50, the winds are in knts and 50 added to day.
if (int(ttaa,6,7) > 50){
  yy <- TRUE
}

i <- int(ttaa,10,10) #Top level which wind data is included.
i.lvl <- c(100,200,300,400,500,700,850,925,1000)
i <- i.lvl[i]

#Section II
#99PPP TTTDD dddff - Surface level information
#PPHHH TTTDD dddff - mandatory level information

#99PPP surface pressure, if PPP<100, add 1000
dat$pres[1] <- int(ttaa,20,22)
if (dat$pres[1] < 100){
  dat$pres[1] <- dat$pres[1]+1000
}

#PPHHH - geopotnetial heigth for mand. levels (PP)
#TTTDD temperature and dewpoint depression
#dddff wind direction and speed
j  <- 0
while (j < 12){
  if (j > 0){ #Geopotentielle Hoehe
    if (j == 1){ #1000hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)
      if (dat$hght[j+1] >= 500){
        dat$hght[j+1] <- 500-dat$hght[j+1]
      }
    }
    if (j == 1 & (dat$pres[1] <= 1000)){
      j <- j+1
    }
    if (j == 2){ #925hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)
    }
    if (j == 3){ #850hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)+1000
    }
    if (j == 4){ #700hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)
      if (dat$hght[j+1]  >= 500){
        dat$hght[j+1] <- dat$hght[j+1]+2000
      }
      else{
        dat$hght[j+1] <- dat$hght[j+1]+3000
      }
    }
    if (j %in% c(5,6)){ #500hPa, 400hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)*10
    }
    if (j %in% c(7,8)){ #300hPa, 250hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)
      if (dat$hght[j+1]  >= 500){
        dat$hght[j+1] <- dat$hght[j+1]*10
      }
      else{
        dat$hght[j+1] <- dat$hght[j+1]*10+10000
      }
    }
    if (j %in% c(9:11)){ #200hPa, 150hPa, 100hPa
      dat$hght[j+1] <- int(ttaa,20+j*18,22+j*18)*10+10000
    }
  }
  t <- tem(ttaa,24+j*18)
  dat$temp[j+1] <- t[1]
  dat$dewp[j+1] <- t[2]
  #Wind
  w <- win(ttaa,30+j*18)
  dat$wdeg[j+1] <- w[1]
  dat$wspd[j+1] <- w[2]
  #Section III
  #88PPP TTTDD dddff - tropopause level information
  #Section IV
  #77PPP dddff - maximum wind level information
  #WIRD JEWEILS NICHTS ENTSCHLUESSELT!
  j <- j+1
}
if (yy == TRUE){ #wspd in Knoten?
  dat$wspd <- dat$wsped*(3.6/1.852)
}

#+++TTBB-Decoding+++
#YYGGI
yy <- FALSE  #if YY >50, the winds are in knts and 50 added to day.
if (int(ttbb,6,7) > 50){
  yy <- TRUE
}
#Section V
#nnPPP TTTDD - significant level information (temperature, wind)
j <- 30 #zaehle Anzahl der signifikanten Level aus
k <- nrow(dat)+1
while (int(ttbb,j,j+4) != 21212){ #21212: Separator zu signifikanten Windlevel
  dat <- rbind(dat,NA)
  dat$pres[k] <- int(ttbb,j+2,j+4)
  t <- tem(ttbb,j+6)
  dat$temp[k] <- t[1]
  dat$dewp[k] <- t[2]
  j <- j+12
  k <- k+1
}

#nnPPP dddff - significant level information (wind)
j <- j+18
while (int(ttbb,j,j+4) != 31313){ #31313: Separator zur weiteren Angaben
  dat <- rbind(dat,NA)
  dat$pres[k] <- int(ttbb,j+2,j+4)
  w <- win(ttbb,j+6)
  dat$wdeg[k] <- w[1]
  dat$wspd[k] <- w[2]
  j <- j+12
  k <- k+1
}

#Sortieren und Speichern
dat <- dat[order(dat$pres, decreasing=FALSE),]
dat <- dat[-c(2:3),]

write.table(dat, row.names=FALSE,
	file=paste0('~/adsb/data/10393_',substr(files[1],12,21),'.dat'))

#+++Plotting+++
png(paste0('~/adsb/data/plots/10393_',substr(files[1],12,21),'.png'), width=1000, height=1000)
dat.logp <- -log(dat$pres) #log-Skalierung des Drucks als Vertikalkoordinate
t.rows <- which(!is.na(dat$temp)) #Level mit Temperaturmessungen
man.rows <- which(dat$pres %in% i.lvl) #mandatory rows

plot(0,0,ylim=c(-log(1030),-log(100)),xlim=c(-80,40), type='l', lwd=3,
     axes=FALSE, main=paste0('Lindenberg ',substr(files[1],18,19),'.',substr(files[1],16,17),'.',
                             substr(files[1],12,15),' ',substr(files[1],20,21),'z'),
     ylab='Druckhöhe (hPa)', xlab='Temperatur (°C)')
for (j in 1:length(i.lvl)){
  abline(h=dat.logp[man.rows],col='grey')
  text(-80,dat.logp[which(dat$pres==i.lvl[j])]+0.02,paste0(dat$hght[which(dat$pres==i.lvl[j])],'m'))
}
for (j in seq(-80,40,10)){
  abline(v=j, col='grey')
}
lines(dat$temp[t.rows],dat.logp[t.rows], lwd=3)
lines(dat$dewp[t.rows],dat.logp[t.rows])
par(las=1)
box()
axis(1, at=seq(-80,40,10), labels=seq(-80,40,10))
axis(2, at=dat.logp[man.rows], labels=i.lvl)
dev.off()
