#=== === === === === === === ===
# Script started by Rebekah Stiling March 2025
# This script compares the species that have attributes in PSSB against the total list of taxa that have been observed. The goal is to figure out which taxa have attributes only at a parent level, and which have attributes all the way to the species level. 
# rstiling@kingcounty.gov
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for reading in data and wrangling
library(writexl) #for saving as an excel file

# Prepare the two data sets ####

# Read in the attribute table.
pssb_atts <- read.csv("data_attributes/2012_taxa_attributes_PSSBmain.csv")

# Read in the current observations from PSSB
# this function binds the textfiles from the taxa downloads from PSSB into one dataframe
taxaBind <- function(file.path) {
  
  path.files <- list.files(file.path)
  # read in files
  list.with.each.file <- lapply(paste(file.path, list.files(file.path), sep = ''), function(y) read.delim(y, header=TRUE))
  taxa<-do.call("rbind.data.frame", list.with.each.file)
  return(taxa)
  
  
}

file.path="./PSSB_all_data/" ##data downloaded and current as of 12/16/2024-- all PSSB rivers and streams data
raw<-taxaBind(file.path)

names(raw)

#subset the raw data
PSSB_taxa<-unique(raw[,c(28, 29, 47:69)])
##there are some repeat entries that somewhere in the hierarchy have an NA instead of "". This yields multiples of the same taxa. Fix this.
PSSB_taxa[is.na(PSSB_taxa)]<-""
PSSB_taxa<-unique(PSSB_taxa) #we're generating a list of all taxa in PSSB samples
#put the NA's back
PSSB_taxa[PSSB_taxa == ""] <- NA


#pad the end of each column heading so that in the future we know which column came from which dataset.
a.colnames <-colnames(pssb_atts) #create a vector of just the column names
a.colnames <- str_c(a.colnames , ".atts") # add the suffix to each word in the vector
colnames(pssb_atts) <- a.colnames #take the vector and assign them as the new colnames for pssb atts

# Dropping this to avoid the taxa suffix later
# t.colnames <-colnames(PSSB_taxa) #create a vector of just the column names
# t.colnames <- str_c(t.colnames , ".taxa") # add the suffix 
# colnames(PSSB_taxa) <- t.colnames #take the vector and assign them as the new colnames for orwa

#Now we have a data set of all observations plus their traits, in addition we have some new taxa that have traits but that were never observed. (Generally higher ranked taxon that are referenced for attributes.)
taxa_atts <-  full_join(x = PSSB_taxa, 
                        y = pssb_atts, 
                        join_by ("Taxon" =="Taxon.Name.atts")) 

# Which Orders have more than 1 species without attributes?
taxa_atts |> filter(is.na(Fore.Wisseman.2012.Clinger.atts)) |>
  select("Order", "Family","Taxon", "Rank") |> 
  filter(Rank == "Species") |> 
  group_by(Order) |> 
  summarise(Species_noatts = n()) |> 
  filter(Species_noatts > 1)

# And, if we group our taxa by order, how many have attributes and how many do not?
# Create a binary column where the observations with attributes
taxa_atts_factor <- taxa_atts |> 
  mutate(att_stat = if_else(is.na(Fore.Wisseman.2012.Clinger.atts), 
                            "no_atts", 
                            "atts"))
taxa_atts_factor 

#When looking at the data grouped by order, how many species in each order have vs don't have attributes?
taxa_atts_factor |> 
  filter(Rank == "Species") |> 
  group_by(Order) |> 
  count(att_stat) |> 
  pivot_wider(names_from = "att_stat", 
              values_from = n,
              values_fill = 0)


long_OFGS_chart <-taxa_atts_factor |> 
  group_by(Order, Family, Genus, Species) |> 
  count(att_stat) |> 
  pivot_wider(names_from = "att_stat",
              values_from = n,
              values_fill = 0)

write.xlsx(long_OFGS_chart, "compare_attributes/long_OFGS_chart.xlsx")

# Some questions we can ask. How often does a species get ID to a higher taxonomic rank?
# We can tally up the total number of "Rank" that are species, and that also have "no_atts"
taxa_atts_factor |> 
  filter(Rank == "Species" & att_stat == "no_atts" ) |> 
  count(Family)
# 205 species, separated by order (#cut "Family" out of count() call to see number)

#write the full table to excel.
write_xlsx(taxa_atts_factor, "compare_attributes/observedtaxon_attributes.xlsx")

#PSSB assigns the attribute of the next available rank above it. I want to figure out how to add two columns to my table. The first column will be the taxon name that is used for attributes, the second column the rank of that taxon. This only needs to happen for the 

#trim the dataset to be just the taxa that don't have attributes
no_atts_df <-taxa_atts_factor |> filter(att_stat == "no_atts")
trim_noatts<-no_atts_df[,1:25]

#tighten up the attribute dataset
trim_butes<-pssb_atts |> select(Taxon.Name.atts, 
                                TSN.atts, 
                                Taxonomic.Rank.atts, 
                                Fore.Wisseman.2012.Clinger.atts,
                                Fore.Wisseman.2012.Intolerant.atts, 
                                Fore.Wisseman.2012.Long.Lived.atts, 
                                Fore.Wisseman.2012.Predator.atts,
                                Fore.Wisseman.2012.Tolerant.atts) 
colnames(trim_butes) <- c("Taxon.Name", "Att.TSN", "Att.Rank", "Clinger", "Intolerant", "Longlived", "Predator", "Tolerant")

# Create an empty data frame
attribs2<-data.frame(Taxon.Name=character(), 
                     Att.TSN = integer(),
                     Attribute.Rank = character(),
                     Clinger=character(),
                     Intolerant=character(), 
                     LongLived=character(),
                     Predator=character(), 
                     Tolerant=character(),
                     Taxon.Serial.Number=character(),
                     Taxon = character(),
                     Rank = character())

for (i in 1:(ncol(trim_noatts)-3)){
  k<-(ncol(trim_noatts)+1)-i
  attribs<-merge(trim_butes, 
                 trim_noatts[, c(k, 1,2,3)], 
                 by.x="Taxon.Name", 
                 by.y=names(trim_noatts[k]))
  
  attribs2<-rbind(attribs, attribs2)
  trim_noatts<-subset(trim_noatts, !Taxon %in% attribs2$Taxon)
}

write.csv(attribs2, "compare_attributes/atts_from_ranks_above.csv")
write.csv(trim_noatts, "compare_attributes/no_atts_after_assign.csv")

review <- (left_join(taxa_atts_factor, 
                     attribs2,
                     by = c("Taxon","Taxon.Serial.Number")))

write_xlsx(review, "compare_attributes/attributes_assigned_inferred.xlsx")
#old

# Create a list of all taxon in PSSB with their name and their TSN
raw_taxon <- raw |> select(Taxon, Taxon.Serial.Number, Order, Family) |> unique()

# Keep the list in PSSB, and then join by name and TSN, keeping the attributes from pssb.
taxa_atts <-  left_join(x = raw_taxon, 
                        y = pssb_atts, 
                        join_by ("Taxon" =="Taxon Name",
                                 "Taxon.Serial.Number" == "TSN")) 

PSSB_noatts <- taxa_atts |> filter(is.na(`Fore Wisseman 2012-Clinger`)) 

# I want to get a list that clarifies how many families have more than one taxa.
taxa_atts |> 
  group_by(Order) |> 
  summarize(num_in_fam = n()) |> 
  filter(num_in_fam > 3)

taxa_atts |> 
  group_by(Phylum) |> 
  summarize(num_in_ord = n()) |> 
  filter(num_in_ord > 2)


PSSB_noatts |> 
  group_by(Order, Family) |> 
  select(rank) ### 
  summarize(num_in_fam = n()) 

PSSB_noatts |> 
  group_by(Order) |> 
  summarize(num_in_ord = n()) 

#write the full table to excel.
write_xlsx(taxa_atts, "observedtaxon_attributes.xlsx")



         