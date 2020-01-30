
library(seqRFLP)
library(bold)
library(data.table)
library(worms)
library(stringr)
library(readr)
library(fingerprint)
library(dplyr)
library(ggplot2)
library(snakecase)

Taxa<-read.csv2(file = "CrustaceaCOI_bold500.csv")

taxon8<-Taxa
taxon8<-taxon8[!(taxon8$bin_uri == "" | is.na(taxon8$bin_uri)), ]
num_species=table(taxon8$species_name)
num_species=as.data.frame(num_species)
names(num_species)=c("species_name","frequency_species")
taxon8$grade=NA
taxon9<-inner_join(taxon8,num_species)
taxon9=data.frame(taxon9$species_name,taxon9$bin_uri,taxon9$nucleotides,taxon9$country,taxon9$grade,taxon9$frequency_species,taxon9$number,taxon9$family_name,taxon9$order_name,taxon9$class_name,taxon9$sampleid,taxon9$processid)
names(taxon9)=c("species","BIN","sequence","country","grade","species_frequency","base_number","family","order","class","sampleid","processid")
taxon10<-taxon9%>% 
  group_by(species) %>%
  summarise(occurrence = n_distinct(BIN),
            BIN = str_c(unique(BIN), collapse = ","))
names(taxon10)<-c("species","bin_per_species","BIN")
taxon11<-taxon9%>% 
  group_by(BIN) %>%
  summarise(occurrence = n_distinct(species),
            species = str_c(unique(species), collapse = ","))
names(taxon11)<-c("BIN","species_per_bin","species")
taxon16<-full_join(taxon9,taxon10,by = "species")
taxon17<-data.frame(taxon16$species,taxon16$BIN.x,taxon16$sequence,taxon16$country,taxon16$grade,taxon16$species_frequency,taxon16$base_number,taxon16$bin_per_species,taxon16$family,taxon16$order,taxon16$class,taxon16$sampleid,taxon16$processid)
names(taxon17)<-c("species","BIN","COI_sequence","country","grade","species_frequency","base_number","BIN_per_species","family","order","class","sampleid","processid")
taxon18<-full_join(taxon17,taxon11,by="BIN")
colnames(taxon18)[colnames(taxon18)=="species.x"]<- "species"
taxon18$species.y<-NULL
taxon19<-taxon18 %>%
  mutate(grade = ifelse(species_per_bin>1,"Discordant",
                        ifelse(BIN_per_species>1 & species_per_bin==1,"Complex",
                             ifelse(BIN_per_species==1 & species_per_bin==1,"Concordant",NA))))
taxon19$BIN_per_species=NULL
taxon19$species_per_bin=NULL
taxon19$species_frequency=NULL
taxon19$sampleid=NULL

write.csv2(taxon19, "Crustacea_gradeSystem.csv")


