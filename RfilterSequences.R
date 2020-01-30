library(bold)
library(worms)
library(tidyverse)
getwd()
setwd("C:/Users/pedro/Desktop/Postdoc/Barbara/Biblioteca de referencia/")
AnimalsCOI_BOLD<-read.csv2("MolluscaCOI_bold500.csv")

AnimalsCOI_BOLD$nucleotides<-as.character(AnimalsCOI_BOLD$nucleotides)


busqueda <- regexpr("^\\-+", AnimalsCOI_BOLD$nucleotides)
regmatches(AnimalsCOI_BOLD$nucleotides, busqueda) <- ""
busqueda2 <- regexpr("\\-+$", AnimalsCOI_BOLD$nucleotides)
regmatches(AnimalsCOI_BOLD$nucleotides, busqueda2) <- ""



np<-(str_count(AnimalsCOI_BOLD$nucleotides, "N")/str_count(AnimalsCOI_BOLD$nucleotides, "[A-Z]"))*100
AnimalsCOI_BOLD$n_percent<-np
gaps<-(str_count(AnimalsCOI_BOLD$nucleotides, "-"))
AnimalsCOI_BOLD$gaps<-gaps

write.csv2(AnimalsCOI_BOLD, "Mollusca.csv")
AnimalsCOI_BOLDclean<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$n_percent==0 & AnimalsCOI_BOLD$gaps==0,]
AnimalsCOI_BOLDlessthan659<-AnimalsCOI_BOLDclean[AnimalsCOI_BOLDclean$number<659,]
AnimalsCOI_BOLD600<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$number>600,]
AnimalsCOI_BOLD650<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$number>650,]
AnimalsCOI_BOLD658<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$number==658,]



BINS<-as.character(unique(AnimalsCOI_BOLD$bin_uri))
BINS<-BINS[BINS!=""]

final = data.frame()

for (x in BINS){
  
  #658 first
  if (sum((AnimalsCOI_BOLD658$bin_uri==x)>2 & AnimalsCOI_BOLD658$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLD658[AnimalsCOI_BOLD658$bin_uri==x & AnimalsCOI_BOLD658$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
    
  } else if (sum(AnimalsCOI_BOLD658$bin_uri==x)>2) {
    y<-AnimalsCOI_BOLD658[AnimalsCOI_BOLD658$bin_uri==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
  
  #650
    } else if (sum((AnimalsCOI_BOLD650$bin_uri==x)>2 & AnimalsCOI_BOLD650$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLD650[AnimalsCOI_BOLD650$bin_uri==x & AnimalsCOI_BOLD650$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
  
    } else if (sum(AnimalsCOI_BOLD650$bin_uri==x)>2) {
    y<-AnimalsCOI_BOLD650[AnimalsCOI_BOLD650$bin_uri==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
  
    #600    
    } else if (sum((AnimalsCOI_BOLD600$bin_uri==x)>2 & AnimalsCOI_BOLD600$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLD600[AnimalsCOI_BOLD600$bin_uri==x & AnimalsCOI_BOLD600$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
      
    } else if (sum(AnimalsCOI_BOLD600$bin_uri==x)>2) {
    y<-AnimalsCOI_BOLD600[AnimalsCOI_BOLD600$bin_uri==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD600,AnimalsCOI_BOLD600$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)     

    #less than 659    
    } else if (sum((AnimalsCOI_BOLDlessthan659$bin_uri==x)>2 & AnimalsCOI_BOLDlessthan659$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$bin_uri==x & AnimalsCOI_BOLDlessthan659$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BBOLDlessthan659$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
      
    } else if (sum(AnimalsCOI_BOLDlessthan659$bin_uri==x)>2) {
    y<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$bin_uri==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)       

    #500 clean    
    } else if (sum((AnimalsCOI_BOLDclean$bin_uri==x)>2 & AnimalsCOI_BOLDclean$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLDclean[AnimalsCOI_BOLDclean$bin_uri==x & AnimalsCOI_BOLDclean$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
      
    } else if (sum(AnimalsCOI_BOLDclean$bin_uri==x)>2) {
    y<-AnimalsCOI_BOLDclean[AnimalsCOI_BOLDclean$bin_uri==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDclean,AnimalsCOI_BOLDclean$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)         

    #500 raw   
    } else if (sum((AnimalsCOI_BOLD$bin_uri==x)>2 & AnimalsCOI_BOLD$presenceinAI=="Atlantic Iberian Peninsula")) {
      y<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$bin_uri==x & AnimalsCOI_BOLD$presenceinAI=="Atlantic Iberian Peninsula",]
      random<-as.character(sample(y$processid, 3))
      AnimalsCOI_random1<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[1])
      AnimalsCOI_random2<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[2])
      AnimalsCOI_random3<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[3])
      AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
      final <<- rbind(final,AnimalsCOI_random) 
      
    } else if (sum(AnimalsCOI_BOLD$bin_uri==x)>2) {
      y<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$bin_uri==x,]
      random<-as.character(sample(y$processid, 3))
      AnimalsCOI_random1<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[1])
      AnimalsCOI_random2<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[2])
      AnimalsCOI_random3<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[3])
      AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
      final <<- rbind(final,AnimalsCOI_random)        
    
    # if less than 3 sequences are available
    } else {
    y<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$bin_uri==x,]
    final <<- rbind(final,y)
  }
}





NoBIN<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$bin_uri=="",]
Species<-as.character(unique(NoBIN$species_name))

for (x in Species){
  if (x %in% final$species_name){
   
  } else if (sum((AnimalsCOI_BOLD658$species_name==x)>2 & AnimalsCOI_BOLD658$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLD658[AnimalsCOI_BOLD658$species_name==x & AnimalsCOI_BOLD658$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD658,AnimalsCOI_BOLD658$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
    
  } else if (sum(AnimalsCOI_BOLD658$species_name==x)>2) {
    y<-AnimalsCOI_BOLD658[AnimalsCOI_BOLD658$species_name==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD650,AnimalsCOI_BOLD650$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
    
  } else if (sum((AnimalsCOI_BOLDlessthan659$species_name==x)>2 & AnimalsCOI_BOLDlessthan659$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLDlessthan659[BOLDlessthan659$species_name==x & AnimalsCOI_BOLDlessthan659$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
    
  } else if (sum(AnimalsCOI_BOLDlessthan659$species_name==x)>2) {
    y<-AnimalsCOI_BOLDlessthan659[AnimalsCOI_BOLDlessthan659$species_name==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLDlessthan659,AnimalsCOI_BOLDlessthan659$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
    
  } else if (sum((AnimalsCOI_BOLD$species_name==x)>2 & AnimalsCOI_BOLD$presenceinAI=="Atlantic Iberian Peninsula")) {
    y<-AnimalsCOI_BOLD[BOLD$species_name==x & AnimalsCOI_BOLD$presenceinAI=="Atlantic Iberian Peninsula",]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random) 
    
  } else if (sum(AnimalsCOI_BOLD$species_name==x)>2) {
    y<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$species_name==x,]
    random<-as.character(sample(y$processid, 3))
    AnimalsCOI_random1<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[1])
    AnimalsCOI_random2<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[2])
    AnimalsCOI_random3<-subset(AnimalsCOI_BOLD,AnimalsCOI_BOLD$processid==random[3])
    AnimalsCOI_random<-rbind(AnimalsCOI_random1,AnimalsCOI_random2,AnimalsCOI_random3)
    final <<- rbind(final,AnimalsCOI_random)
    
  # if less than 3 sequences are available
  } else {
    y<-AnimalsCOI_BOLD[AnimalsCOI_BOLD$species_name==x,]
    final <<- rbind(final,y)
  }
}

write.csv2(final, "Random_Mollusca3.csv")




