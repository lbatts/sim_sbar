
#NOTE~~=====
#This needs to be run in R version 4.
#devtools::install_github("james-thorson/FishLife")

library( FishLife )

#vignette("tutorial","FishLife")


# Get basic plot and extract values for lophius piscatorius
tmp<-Search_species(Genus="Lophius",Species="piscatorius")$match_taxonomy
Predict = Plot_taxa(tmp)

vals<-lapply(Predict,function(x){
  plogis(x$Mean_pred["logitbound_h"]) # thes are log space
  
  })

vals[3] #is the precited steepness for lophidae family
vals[2]
vals[1] #is the precited steepness for species


# Get basic plot and extract values for Clupea Harengus
tmp<-Search_species(Genus="Clupea",Species="harengus")$match_taxonomy
Predict = Plot_taxa(tmp[])

vals<-lapply(Predict,function(x){
  plogis(x$Mean_pred["logitbound_h"]) # thes are log space
  
})


vals[3] #is the precited steepness for clupeidae family

vals[1] #is the precited steepness for species
vals

vals[2]
