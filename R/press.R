#Umrechnungsfunktionen von Druckwerten in Fuss in hPa nach ISA
#Alexander B.
#09.02.14

press <- function(dat){
  #Konstanten
  rd <- 287.058 #Gaskonstante trockene Luft
  t0 <- 288.15 #Referenztemperatur auf 0m
  tr <- 216.65 #auf 11km
  p0 <- 1013.25 #Referenzdruck auf 0m (hPa)
  pr <- 226.32 #Referenzdruck Tropopause (hPa)
  tg <- 0.0065 #Temperaturgradient Troposphaere
  g <- 9.80665 #Gravitationskonstante
  
  altpress <- dat*0.3048 #speicher temporaer metrische Hoehe ab
  dat.s <- which(altpress > 11000) #Zeilen mit Hoehen jenseits von 11000m/36089ft (Stratosphaere)
  dat.t <- which(altpress <= 11000) # Troposphaere
  
  altpress[dat.s] <- pr*exp(-g*(altpress[dat.s]-11000)/(rd*tr))
  altpress[dat.t] <- p0*(1-(tg*altpress[dat.t]/t0))^(g/(rd*tg))
  
  return(altpress)
}