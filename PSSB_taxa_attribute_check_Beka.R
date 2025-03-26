#=== === === === === === === ===
# Script started by Rebekah Stiling March 2025
# This script compares the species that have attributes in PSSB against the total list of taxa that have been observed. The goal is to figure out which taxa have attributes only at a parent level, and which have attributes all the way to the species level. 
# rstiling@kingcounty.gov
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for reading in data and wrangling
library(writexl) #for saving as an excel file
library(Microsoft365R) #for accessing sharepoint

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

write.xlsx(long_OFGS_chart, "long_OFGS_chart.xlsx")

# Some questions we can ask. How often does a species get ID to a higher taxonomic rank?
# We can tally up the total number of "Rank" that are species, and that also have "no_atts"
taxa_atts_factor |> 
  filter(Rank == "Species" & att_stat == "no_atts" ) |> 
  count(Family)
# 205 species, separated by order (#cut "Family" out of count() call to see number)

#write the full table to excel.
write_xlsx(taxa_atts_factor, "observedtaxon_attributes.xlsx")


#identify which site I want to write to:
list_sharepoint_sites() #list the sites I have access to by name
site <- get_sharepoint_site("​​​​​Science Files") #write the name of the site I want (a separate browser window opens and logs me into Sharepoint with my King County credentials) # I don't understand why those red dots make it work, but it does.

# default drive is the main page document library, so we need to find the other drives
site$list_drives() #list the drives. I see LSSG on there
drv <- site$get_drive("LSSG Files")
drv$list_files("Freshwater/Streams/Freshwater Macroinvertebrate Program/PSSB/PSSB 1.0", full_names=TRUE) #make sure I understand the file structure
drv$save_dataframe(taxa_atts_factor, "Freshwater/Streams/Freshwater Macroinvertebrate Program/PSSB/PSSB 1.0/observedtaxon_attributes.csv")


#PSSB assigns the attribute of the next available rank above it. I want to figure out how to add two columns to my table. The first column will be the taxon name that is used for attributes, the second column the rank of that taxon. This only needs to happen for the 
TSN.noatts <-taxa_atts_factor |> filter(att_stat == "no_atts") |> select(Taxon.Serial.Number) |> pull()

list.of.potential.ranks <- taxa_atts_factor |> 
  select(`Taxonomic Rank.atts`) |> 
  drop_na() |> 
  unique() |> 
  pull()

list.of.ranks.of.taxa.missing.atts <- taxa_atts_factor |> 
  filter(att_stat == "no_atts") |> 
  select(`Rank.taxa`) |> 
  drop_na() |> 
  unique() |> 
  pull() 

#referencing the list above, I created an ordered list of headings to consider.
list.of.rank.column.names <- c("Subspecies.taxa",
                               "Species.taxa",
                               "Subgenus.taxa",
                               "Species.Group.taxa",
                               "Genus.taxa",
                               "Genus.Group.taxa",
                               "Tribe.taxa",
                               "Subfamily.taxa",
                               "Family.taxa",
                               "Superfamily.taxa", #not on potential rank list
                               "Infraorder.taxa", #not on potential rank list
                               "Suborder.taxa",
                               "Order.taxa",
                               "Subclass.taxa",
                               "Class.taxa",
                               "Phylum.taxa")
i <-3
j<-3

#trim the dataset
trim<-taxa_atts_factor[,1:25]

##Abandoning project. Not clear how PSSB chooses the rank to use for attributes...

for (i in 1:(length(TSN.noatts)){ 
  taxa_row <- trim |> filter(Taxon.Serial.Number == TSN.noatts[i])
  content.index <- which(is.na(taxa_row) == FALSE)
  hi.low <- rev(content.index)
  for (j in 1:length(hi.low)) {
    rank.to.test.num <-hi.low[j+1]
    rank.to.test.word <- as.character(taxa_row[rank.to.test.num])
    rows.to.test <- which(taxa_atts_factor[,rank.to.test.num]==rank.to.test.word)
    test.mat1 <-taxa_atts_factor[rows.to.test,]
    if sum(test.mat1$att_stat == "atts")>0 
    col.nam <-colnames(taxa_row[rank.to.test.num])
    new.col.name <-paste0(col.nam,".atts")
    test.mat1[,26:56]
    else
      #I think the next step is a which test.mat1 matches at the needed row, but is NA for all the rest (but not attributes). If that is just 1 answer, then we use the attributes.
      next
    
  }
  
  rank.name <-taxa_row$Rank
  taxa_row
  
  rank.of.taxa <-taxa_row |> select(Rank.taxa)
  rank.of.taxa_renamed<-paste0(rank.of.taxa, ".taxa")
  indexed.rank <-which(list.of.rank.column.names == rank.of.taxa_renamed)
  next.rank <- list.of.rank.column.names[indexed.rank + 1]
  content <-taxa_row |> select(all_of(next.rank))
  if(is.na(content) == TRUE {
    is.na(taxa_row |> select(all_of(list.of.rank.column.names[indexed.rank + 2]))) == TRUE
  } else if {
    is.na(taxa_row |> select(all_of(list.of.rank.column.names[indexed.rank + 3]))) == TRUE
  } else if {
    is.na(taxa_row |> select(all_of(list.of.rank.column.names[indexed.rank + 4]))) == TRUE
  } else {
    rank.2.check <- list.of.rank.column.names[indexed.rank + 4]
  }
  taxa.2.check <-taxa_row |> select(all_of(rank.2.check))
  taxa_atts_factor |> filter(Family.taxa == "Caenidae") #work on semantics.
  
  a <- i
  print(a)
}

colnames(taxa_row)

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



         