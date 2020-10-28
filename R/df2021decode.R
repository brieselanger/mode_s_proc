#Dekodierungsroutinen für DF20/21 bzw. BDS-Register 5,0 und 6,0
#Alexander B.
#28.10.14

#+++Funktionen+++
#Routine fuer BDS-Register 5,0
bds5 <- function(data.selected){
  #dekodiere roll angle
  lwd <- as.integer(substr(data.selected$bin,34,34)) #left wing down sign bit (1=true/left wing down), 0=false/not/right wing down)
  data.selected$rola <- (strtoi(substr(data.selected$bin,35,43),base=2)*(45/256)*(1-2*lwd)+(lwd*90))*(1-2*lwd)
  #dekodiere true track angle
  ws <- as.integer(substr(data.selected$bin,45,45)) #west sign bit (1=true/west, 0=false/east)
  data.selected$trk <- strtoi(substr(data.selected$bin,46,55),base=2)*(90/512)+ws*180 #true track
  data.selected$gs <- strtoi(substr(data.selected$bin,57,66),base=2)*(1024/512)   #dekodiere ground speed
  data.selected$tas <- strtoi(substr(data.selected$bin,79,88),base=2)*2   #dekodiere true air speed
  
  #filtern
  data.selected$gs[which(data.selected$gs > 550)] <- NA
  data.selected$gs[which(data.selected$gs < 120)] <- NA
  data.selected$tas[which(data.selected$tas > 500)] <- NA
  data.selected$tas[which(data.selected$tas < 120)] <- NA
  if(max(abs(data.selected$gs-data.selected$tas), na.rm=TRUE) > 200){
    data.selected[which(abs(data.selected$gs-data.selected$tas) > 200),c("gs","tas")] <- NA
  }
  data.selected$rola[which(abs(data.selected$rola) > 30)] <- NA
  data.selected[which(is.na(data.selected$rola) | is.na(data.selected$trk) | is.na(data.selected$gs)
                      | is.na(data.selected$tas)),c("rola","trk","gs","tas")] <- NA
  
  return(data.selected)
}

#Routine fuer BDS-Register 6,0
bds6 <- function(data.selected){
  #dekodiere magnetic heading
  ws <- as.integer(substr(data.selected$bin,34,34)) #west sign bit (1=true/west, 0=false/east)
  data.selected$hdg <- strtoi(substr(data.selected$bin,35,44),base=2)*(90/512)+180*ws #magnetic heading
  data.selected$kias <- strtoi(substr(data.selected$bin,46,55),base=2)   #dekodiere indicated airspeed
  data.selected$mach <- strtoi(substr(data.selected$bin,57,66),base=2)*0.004   #dekodiere mach
  #inertial vertical rate +++DISABLED+++
  #signbit <- as.integer(substr(data.selected$bin,79,79))
  #data.selected$vs <- (strtoi(substr(data.selected$bin,80,88),base=2)*32*(1-2*signbit)+(signbit*16384))*(1-2*signbit)
  
  #filtern
  data.selected$kias[which(data.selected$kias > 380)] <- NA
  data.selected$kias[which(data.selected$kias < 120)] <- NA
  #data.selected$vs[which(abs(data.selected$vs) > 5000)] <- NA
  data.selected$mach[which((data.selected$mach > 0.9) | (data.selected$mach < 0.2))] <- NA
  data.selected[which(is.na(data.selected$hdg) | is.na(data.selected$kias) | is.na(data.selected$mach))
                ,c("hdg","kias","mach")] <- NA
  
  return(data.selected)
}

#+++Hauptroutine DF20/21-Dekodierung+++
df2021decode <- function(data){
  #Status-Bits fuer BDS 5,0 (im message-Teil Bits 1, 12, 24, 35 und 46)
  bds5.bits <- paste0(substr(data$bin,33,33),substr(data$bin,44,44),substr(data$bin,56,56),substr(data$bin,67,67),substr(data$bin,78,78))
  bds5.rows <- which(bds5.bits == "11111")
  data[bds5.rows,] <- bds5(data[bds5.rows,])
  #Status-Bits fuer BDS 6,0 (im message-Teil Bits 1, 13 und 24... Bit 35 und 46 (vertical speed) DISABLED)
  bds6.bits <- paste0(substr(data$bin,33,33),substr(data$bin,45,45),substr(data$bin,56,56),substr(data$bin,67,67),substr(data$bin,78,78))
  bds6.rows <- which(bds6.bits == "11111")
  data[bds6.rows,] <- bds6(data[bds6.rows,])
    
  return(data)
}
