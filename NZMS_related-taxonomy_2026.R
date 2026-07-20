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

### Now I want to make a graph that has all the unknown locations on the y axis and time on the x axis. I want each square to be colored according to potamopyrgus present, Truncatelloidea present, or both. I'll need site, year, and two presence columns. That will allow me to make a third colulmn.

stat <- read_csv("site-master_export/NZMS-status-2026.csv")
unk_stat_names <- stat |> 
  filter_out(NZMS == "yes") |> 
  select("Site Name") |> 
  pull()

snail_observations_all <- raw |> 
  select(Sample.ID, Site.Code, Visit.Date, Taxon) |> 
  filter(Taxon == "Truncatelloidea" | Taxon == "Potamopyrgus antipodarum") |> 
  mutate(presence = 1) |> # add a presence column
  pivot_wider(names_from = Taxon, values_from = presence, values_fill = 0) |> 
  rename(NZMS = "Potamopyrgus antipodarum") |> 
  mutate(date =mdy(Visit.Date)) |> #change the character string to date
  mutate(year = year(date)) |>  #extract year from the date
  mutate(
    species = case_when(
    Truncatelloidea == 1 & NZMS == 0 ~ "Trun",
    Truncatelloidea == 0 & NZMS == 1 ~ "NZMS",
    Truncatelloidea == 1 & NZMS == 1 ~ "Both",
  ))


snail_observations <- snail_observations_all |> 
  filter(Site.Code %in% unk_stat_names) 

ggplot(snail_observations,aes(x = year, y = Site.Code, fill = species)) +
  geom_tile() +
  theme_minimal()

ggplot(snail_observations_all,aes(x = year, y = Site.Code, fill = species)) +
  geom_tile() +
  theme_minimal()







