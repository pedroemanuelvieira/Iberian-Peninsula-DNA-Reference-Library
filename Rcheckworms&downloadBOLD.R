getwd()
setwd("C:/Users/pedro/Desktop/Postdoc/Barbara/Biblioteca de referencia/")
species2<-read.csv2("Crustacea checklist.txt")
species<-as.vector(species2$Species)
library(bold)
library(worms)
library(tidyverse)
acceptednames<-wormsbynames(species)

write.csv2(acceptednames, "mollusca_worms.csv")
acceptednamesclean<-na.omit(acceptednames$valid_name)
taxon<-bold_seqspec(taxon=acceptednamesclean[1:300], format = "tsv", marker="COI-5P")
taxon2<-bold_seqspec(taxon=acceptednamesclean[301:600], format = "tsv", marker="COI-5P")
taxon3<-bold_seqspec(taxon=acceptednamesclean[601:900], format = "tsv", marker="COI-5P")
taxon4<-bold_seqspec(taxon=acceptednamesclean[901:1200], format = "tsv", marker="COI-5P")
taxon5<-bold_seqspec(taxon=acceptednamesclean[1201:1400], format = "tsv", marker="COI-5P")
taxon6<-bold_seqspec(taxon=acceptednamesclean[1401:1553], format = "tsv", marker="COI-5P")

Animals_BOLD<-rbind(taxon,taxon2, taxon3, taxon4, taxon5, taxon6)

Animals_BOLD$number<-str_count(Animals_BOLD$nucleotides, pattern="[A-Z]")
#Add information mined from genbank
Animals_BOLD$GenbankMined<-Animals_BOLD$institution_storing=="Mined from GenBank, NCBI"
#Select COI
AnimalsCOI_BOLD<-Animals_BOLD[Animals_BOLD$markercode=="COI-5P",]
#Save and export dataframe downloaded from BOLD containg COI
write.csv2(AnimalsCOI_BOLD, file="CrustaceaCOI_BOLD.csv")

AnimalsCOI_BOLD500<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$number>500,]
#Save and export dataframe downloaded from BOLD for COI>500
write.csv2(AnimalsCOI_BOLD500, file="CrustaceaCOI_bold500.csv")
