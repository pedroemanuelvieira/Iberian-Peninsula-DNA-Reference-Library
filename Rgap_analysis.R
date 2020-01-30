Polychaeta<-read.csv2(file = "PolychaetaCOI_bold500.csv")
Polychaeta_checklist<-read.csv2(file = "Polychaeta checklist.txt")


uniquespecies<-as.vector(unique(Polychaeta$species_name))
species<-as.vector(Polychaeta_checklist$Species)


#uniquespecies<-unique(AnimalsCOI_BOLD500$species_name)

Gap_analysis<-species %in% uniquespecies

List<-cbind(species, Gap_analysis)

write.csv2(List,"Gap_analysis_Polychaeta.csv")


