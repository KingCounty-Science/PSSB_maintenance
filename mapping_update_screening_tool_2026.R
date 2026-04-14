#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# 
# Beka Stiling created this 2026 version starting April 2026
# The goal of this script is to go through the names of taxa observed in PSSB, the mapping recommended by the BCG working group, and the mapping recorded in PSSB. 
# The screening outcomes will:
# 1) Reveal what additional mapping needs to be added to the PSSB list.
# 2) Reveal what taxa are newly observed that the BCG work group should map.
# 3) Flag what taxa we should review for taxa mapping becasue the mapping may have changed.
#
#--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

library(openxlsx)
library(tidyverse)
library(janitor) #to clean names

#load the mapping used by PSSB, downloaded from "Analysis"  -> "Taxa Mapping"
PSSBmapping<-read_csv("2026_screening_files/Taxa-mapping_20260406.csv") |> 
  clean_names()#load the mapping used by PSSB

#load the most resent ORWA taxa translator from github.
BCGmapping<-read_csv("2026_screening_files/ORWA_TaxaTranslator_20250812.csv") |> 
  clean_names()# load the latest BCG mapping

#merge the two together by "alternate_name" (i.e., the name put in by the lab") and "taxon_orig" (i.e., the original taxa name prior to mapping to modern/current name.)
mapping_PSSB_BCG<-merge(PSSBmapping, BCGmapping, by.x=c("alternate_name"), by.y=c("taxon_orig"), all=T) #merge the two mappings together, keep all items in both dataframes

#All taxa in the PSSB list should be in the BCG list, however "mapping" has 4 more observations than the BCGmapping dataframe. Why?

#first, taxa are in the PSSBmapping list that are not on the BCG list? 
mapping_PSSB_BCG %>% filter(is.na(otu_metric_calc))
#There are 2.

#second, there are two taxa in PSSB that are listed twice. That adds two rows to the number of observations to the joined dataframe
PSSBmapping %>% 
  group_by(alternate_name) %>% 
  filter(n() > 1) #2 duplicates

#The two sets of two account for the four extra rows of data.

#out of curiosity, are there any duplicates in the BCG table? 
BCGmapping %>% 
  group_by(taxon_orig) %>% 
  filter(n() > 1)
# There are several...it seems to have to do with spaces. Pin for later conversation with Kate.

#I don't know what this is...delete for 2026?
test<-subset(mapping_PSSB_BCG, is.na(preferred_name) & alternate_name!=otu_metric_calc & otu_metric_calc!="DNI")
test2<-mapping_PSSB_BCG |> filter(is.na(preferred_name) & #which items don't have a prefered name (i.e., they are not on the PSSB mapping list)
                    alternate_name!= otu_metric_calc & #There original name in PSSB, is different than BCG recommended name
                    otu_metric_calc!="DNI") # don't include the taxa the BCG translates taxa to "DNI" meaning "Do Not Include".

#What are all the taxa on this "mapping" list that don't have a preferred name? I believe these are taxa that the BCG has taken the time to map, but we don't have in the PSSB taxa translation list. This is most likely because we have not observed these taxa in the Puget Sound region (or they have not been added to the list yet)
PSSBmapping %>%  #double check no fields in the preferred name column are blank in the PSSB mapping table.
  filter(is.na(preferred_name))

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

names(raw)

PSSB_taxa_unique<- raw |> select(Taxon.Serial.Number, Taxon, c(Rank:Subspecies)) |> unique()

PSSB_taxa_unique %>% 
  group_by(Taxon, Taxon.Serial.Number) %>% 
  filter(n() > 1)
##there are >1000 repeat entries that somewhere in the hierarchy have an NA or <NA> instead of "". This yields multiples of the same taxa. 
#Fix this.
PSSB_taxa_unique[is.na(PSSB_taxa_unique)]<-""
PSSB_taxa<-unique(PSSB_taxa_unique) #we're generating a list of all taxa in PSSB samples, heirarchy included.

#problem is fixed
PSSB_taxa %>% 
  group_by(Taxon, Taxon.Serial.Number) %>% 
  filter(n() > 1)

PSSB_taxa %>% 
  group_by(Taxon) %>% 
  filter(n() > 1)

mapping_PSSB_BCG |> 
  group_by(alternate_name) %>% 
  filter(n() > 1)

mapping_PSSB_BCG_unique <- mapping_PSSB_BCG |> 
  select(!rationale) |> 
  unique()

mapping_PSSB_BCG_unique |>   group_by(alternate_name) %>% 
  filter(n() > 1)

mapping<-merge(mapping_PSSB_BCG, PSSB_taxa, by.x=c("alternate_name"), by.y=c("Taxon"), all.y=T) #merge the PSSB taxa list with the mapping dataframe, and keep all taxa found in PSSB, the BCG list, and in the PSSB taxa translator.

#attempt to rebase
mapping2<-right_join(mapping_PSSB_BCG_unique, PSSB_taxa, 
                   join_by("alternate_name" == "Taxon" , "alternate_tsn" == "Taxon.Serial.Number"))#merge the PSSB taxa list with the mapping dataframe, and keep all taxa found in PSSB, the BCG list, and in the PSSB taxa translator.
subset(mapping2, 
       is.na(otu_metric_calc), 
       select="alternate_name")
mapping2 |> filter(is.na(otu_metric_calc)) |> select(Taxon)


mapping |> filter(alternate_name == "Onconeura")



## Now we begin to see which taxa are on what lists.
missingBCGmapping<-subset(mapping, 
                          is.na(otu_metric_calc), 
                          select="alternate_name")##these are taxa in PSSB samples that have not been translated by the BCG working group. 
write.csv(missingBCGmapping, "2026_screening_files/2026results/Missing_from_BCG_taxa_translator.csv")

#look at one example
mapping |> filter(alternate_name == "Antennella")

PSSB_taxa |> filter(Taxon == "Antennella")

#Could I have gotten here by just looking at the PSSB taxa list and the BCG list?
noBCGmapping<-anti_join(PSSB_taxa, BCGmapping, join_by("Taxon"=="taxon_orig")) |> select("Taxon")
#yes, this is an alternative path to the same thing.

missingPSSBmapping<-subset(mapping, 
                           is.na(preferred_name) & 
                             ((alternate_name!=otu_metric_calc & otu_metric_calc!="DNI")|
                                is.na(otu_metric_calc)))##these are taxa in PSSB samples that don't have a mapping assigned in PSSB. 

missingPSSBmapping_part1<-subset(mapping, 
                           is.na(preferred_name) & 
                             ((alternate_name!=otu_metric_calc & otu_metric_calc!="DNI")))
missingPSSBmapping_part2<-subset(mapping, 
                                 is.na(preferred_name) & 
                                   (is.na(otu_metric_calc)))

write.csv(missingPSSBmapping, "2026_screening_files/2026results/Missing_from_PSSB_mapping_table.csv")

#Could I have gotten here by just looking at the PSSB taxa list and the pssb mapping?
#no, we also need to know which taxa are DNI in the BCG list, and which taxa have a different name. This:
noPSSBmapping<-mapping |> select(alternate_name, preferred_name, otu_metric_calc) |> 
  filter(is.na(preferred_name)) |> 
  filter(otu_metric_calc!="DNI") |> 
  filter(alternate_name!=otu_metric_calc) #This is only the taxa that need mapping in PSSB, but there is BCG mapping, which is a little shorter than the full list of missingPSSBmapping - the missingPSSBmapping list includes taxa we'll need to request get added to BGC.
#noPSSBmapping is 55 observations. missingPSSBmapping is 69 observations. The difference are the 14 taxa we'll need to review and send to Sean.

###because there may be overlap between the last two, this next section will parse out the differences.

#1 Reveal what additional mapping needs to be added to the PSSB list? These taxa are missing PSSB mapping, but the BCG mapping does exist.
missingPSSBmappingBCGexists<-missingPSSBmapping$alternate_name[!missingPSSBmapping$alternate_name %in% missingBCGmapping$alternate_name] ##these are taxa in PSSB samples that have a BCG translation, but do not have a mapping assigned in PSSB. Update the PSSB mapping to include these. 

#NA taxa in PSSB samples that do not have a BCG translation, but do have a mapping assigned in PSSB. (None, likely because Beka has not done any PSSB mapping that Sean doesn't know about.)
missingBCG_PSSBmappingexists<-missingBCGmapping$alternate_name[missingBCGmapping$alternate_name %in% PSSBmapping$alternate_name]##these are taxa in PSSB samples that do not have a BCG translation, but do have a mapping assigned in PSSB. Probably a result of BAS editing some taxa names in PSSB for clarity, formatting and accuracy (i.e. Rhabdomastix (Rhabdomastix (Rhabdomastix)) was edited in PSSB taxonomy to simply Rhabdomastix (Rhabdomastix)). Let Sean know about these name changes in PSSB so he can update the BCG translation table with the edited names.

#2 Reveal what taxa are newly observed in the area that the BCG work group should map.
missingBCGPSSB<-missingBCGmapping$alternate_name[missingBCGmapping$alternate_name %in% missingPSSBmapping$alternate_name]##these are taxa in PSSB samples that do not have a BCG translation, and do not have a mapping assigned in PSSB. Send these to Sean Sullivan once a year for updating the BCG translation tables.

missingBCGmapping$alternate_name[missingBCGmapping$alternate_name %in% missingPSSBmapping$alternate_name] ###this output should be identical to the object missingBCGPSSB

###this next section checks to see if existing mappings have changed since the last version of the BCG table.
#3 Check the taxa mapping because it may have changed.
changedmapping <- mapping[which(mapping$preferred_name !=mapping$otu_metric_calc),c("alternate_name", "preferred_name", "otu_metric_calc")] ##in many cases, the BCG translates taxa to "DNI" meaning "Do Not Include". But since these taxa are not explicitly excluded in the PSSB calculations, we created mappings for these in PSSB based simply on copying the taxa name into the mapping column. BAS also did not wish to enter "Polycentropus sensu lato" as a new taxa in PSSB since "Polycentropus" already existed (and truly implementing "sensu lato" qualifications in PSSB isn't feasible). So BAS kept the mapping to "Polycentropus". Any other differences should be examined, and the PSSB mappings should be updated to reflect the current BCG translations. Check taxa attributes as well to see if new taxa names should have attributes assigned from old taxa names. 

#This is an alternative way to look at where preferred name is now different than the otu_metric_calc prefered name. Are these changes? To review with Kate.
PSSBmappingisdifferentfromBCG<-mapping %>% 
  filter(preferred_name != otu_metric_calc) %>% 
  select("alternate_name", "preferred_name", "otu_metric_calc")
