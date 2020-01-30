Crustacea<-read.csv2("CrustaceaCOI_bold500.csv")

Crustacea$lat[is.na(Crustacea$lat)] <- 0
Crustacea$lon[is.na(Crustacea$lon)] <- 0

Location <- function(lat,long) {
  if ((lat >= 44 | lat <= 36) & (long <= -11 | long >= -2)) {
    print("Not Iberian Peninsula") 
  } else {
    if (lat>=42)  {
      print("Atlantic Iberian Peninsula")
    } else if (lat < 42 & long < -5.3) {
      print("Atlantic Iberian Peninsula")
    } else {
      print("Mediterranean Iberian Peninsula")
    } 
  }
}

Crustacea$presenceinAI<-mapply(Location, Crustacea$lat, Crustacea$lon)

write.csv2(Crustacea, "Crustacea_region.csv")


#lat<-max has to be 44, long has to -2
#lat<-min has to be 36, long has to be -5.3
#long<-min has to -11