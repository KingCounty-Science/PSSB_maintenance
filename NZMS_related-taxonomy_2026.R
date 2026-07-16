# Explore NZMS and related taxonomy

# Kate discovered that some taxonomists enter NZMS as Truncatelloidea. The question is, are there other snails that taxonomists record as Truncatelloidea, that are most definitely not-mudsnails, or can we treat all Truncatelloidea in PSSB as Potamopyrgus antipodarum.

#Let's look at semi recent data stored in this project:

library(tidyverse)
library(janitor) #to clean names

##this function binds the textfiles from the taxa downloads from PSSB into one dataframe
taxaBind <- function(file.path) {
  
  path.files <- list.files(file.path)
  # read in files
  list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
  taxa<-do.call("rbind.data.frame", list.with.each.file)
  return(taxa)
  
  
}

file.path="./2026_screening_files/PSSB-visitdata/" ##data downloaded and current as of 04/06/2026-- all PSSB rivers and streams data
raw<-taxaBind(file.path) #every single entry in PSSB to date!

#now we just need all entries, with ranks

all_taxa <-raw |> select(Taxon.Serial.Number,
                         Taxon,
                         c(Rank:Subspecies)) |> 
  unique()

all_taxa[is.na(all_taxa)]<-""

all_taxa_unique <- all_taxa |> unique()


truncatelloidea <- all_taxa_unique |> filter(Superfamily == "Truncatelloidea")
NZMS <- all_taxa_unique |> filter(Genus == "Potamopyrgus")
