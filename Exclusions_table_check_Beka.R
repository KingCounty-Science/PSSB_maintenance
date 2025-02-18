# This script reviews the taxa mapping table and finds excluded taxa that are mapped to something else that may not be excluded.

library(tidyverse)
library(readxl)
library(janitor)


PSSB_map <- read_excel("PSSB_mapping_BekafromPSSBdev.xlsx") %>% janitor::clean_names()
head(PSSB_map)
Exclude <- read_excel("PSSB_exclusions.xlsx") %>% janitor::clean_names()
head(Exclude)

# Simpler method! ###
taxon_ex<-Exclude %>% select(taxon_name)
taxon_change <- PSSB_map %>% select(alternate_name)

#What names are on the exclude list that are also on the PSSB list?

both <- inner_join(taxon_change, taxon_ex, by = c("alternate_name" = "taxon_name") )

# OR

inner_join(Exclude, PSSB_map, by = c("taxon_name" = "alternate_name") )



## Old method, kept for comparison. ###
#Drop irrelevant columns and only keep heirarchy 
Exclude_lean<-Exclude %>% select(!c(taxon_name, tsn, taxonomic_rank, excluded))

#Create a function for a row that finds the last not NA

loc <-function (x) {
  max(which(is.na(x) == FALSE))
}

nam_loc<-apply(Exclude_lean, 1, loc)

#Now, for each location of each row, I need to get the word/name in that cell.

#create empty dataframe ()
df <- tibble(
  name = character())

for (i in 1:length(nam_loc)){   
  x <-Exclude_lean[i, nam_loc[i]] %>% pull()
  df[i, "name"]<- x
}
df
#now I have a list of all the names! 

PSSB_map %>% filter(alternate_name %in% (df %>% pull()))
# There are four taxon that are mapped to Lepidoptera.
