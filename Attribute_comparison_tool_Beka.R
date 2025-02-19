#=== === === === === === === ===
# Script started by Rebekah Stiling Feb 2025
# This script compares attributes in PSSB with attributes in recently updated ORWA attribute tables. In early Feb Kate was noticing places where the attributes in ORWA have been changed or updated compared to the attributes in PSSB. The goal of this script is to systematically compare the attributes between sources to get an idea of what is the same, and what has changed.
# adding _ at the end of the script name for now, while I seek to differentiate scripts I'm working with from ones Beth created.
# rstiling@kingcounty.gov
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for reading in data and wrangling


# Read in each attribute table and get an idea of the contents and structure of each table.
dev <- read_csv("data_attributes/2012_taxa_attributes_PSSBdev.csv")
pssb <- read_csv("data_attributes/2012_taxa_attributes_PSSBmain.csv")
orwa <- read_csv("data_attributes/ORWA_Attributes_20250211.csv")
orwa_old <- read_csv("data_attributes/ORWA_Attributes_20241121.csv")

# Which taxa in the main list are also in the orwa list?

#main taxon on the orwa taxon list. PSSB uses the phrase "Taxon Name" wheras ORWA uses the term "Taxon" for taxa
pssb_on_orwa <- left_join(x = pssb, y = orwa, join_by ("Taxon Name" =="Taxon"))
#rename the TSN columns so they are more intuitive.
pssb_on_orwa <- pssb_on_orwa %>% rename(TSN.pssb = TSN.x,
                                        TSN.orwa = TSN.y)

#Check on common things between the datasets ####
## 1. Are the TSNs the same? ####
pssb_on_orwa %>% filter(TSN.pssb != TSN.orwa) %>% select("Taxon Name", "TSN.pssb", "TSN.orwa")
#No, there are 16 taxon that have slightly different TSN's between the two tables. I looked up a few on ITIS, the differences are subtle and I need to discuss with Kate.

## 2. Are the traits the same? ####
# First we have to find all taxon that have clinger listed in habit, and create a column with True or False, same with predator

pssb_on_orwa_traits<-pssb_on_orwa %>% 
  mutate(ORWA_clinger = if_else((str_detect(Habit, "CN")), "TRUE", "FALSE", missing = "FALSE"))  %>% #if the habit column detects the string "CN" change the new "ORWA_clinger" column to "TRUE", if not, call it "FALSE, and call any cells in "Habit" missing values "FALSE"
  mutate(ORWA_predator = if_else((str_detect(FFG, "PR")), "TRUE", "FALSE", missing = "FALSE"))  
pssb_on_orwa_traits

## 2.1 Are the Clinger traits the same?
clinger_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Clinger` != ORWA_clinger) %>% 
  select("Taxon Name", "TSN.pssb", "Fore Wisseman 2012-Clinger", "ORWA_clinger", "Habit") %>% 
  add_column(clinger_dif = "clinger_dif")
clinger_difs


## 2.2 Are the longlived traits the same?
longlived_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Long Lived` != LONGLIVED) %>% 
  select("Taxon Name", "TSN.pssb", "Fore Wisseman 2012-Long Lived", "LONGLIVED", "Life_Cycle") %>% 
  add_column(longlived_dif = "longlived_dif")
  
longlived_difs

## 2.3 Are the predator traits the same?
predator_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Predator` != ORWA_predator) %>% 
  select("Taxon Name", "TSN.pssb", "Fore Wisseman 2012-Predator", "ORWA_predator", "FFG") %>% 
  add_column(pred_dif = "pred_dif")
predator_difs

# 3. How much overlap is there in these taxon lists?

clll<- full_join(clinger_difs, longlived_difs, join_by("Taxon Name", "TSN.pssb"))
clinger_longlived_predators <-full_join(clll, predator_difs, join_by("Taxon Name", TSN.pssb))
just_difs<-clinger_longlived_predators %>% select(TSN.pssb, clinger_dif, longlived_dif, pred_dif) 
traits_difs <- full_join(pssb_on_orwa_traits, just_difs, join_by(TSN.pssb)) 

write_csv(traits_difs, "compare_attributes.csv")



