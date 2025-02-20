#=== === === === === === === ===
# Script started by Rebekah Stiling Feb 2025
# This script compares attributes in PSSB with attributes in recently updated ORWA attribute tables. In early Feb Kate was noticing places where the attributes in ORWA have been changed or updated compared to the attributes in PSSB. The goal of this script is to systematically compare the attributes between sources to get an idea of what is the same, and what has changed.
# adding _ at the end of the script name for now, while I seek to differentiate scripts I'm working with from ones Beth created.
# rstiling@kingcounty.gov
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for reading in data and wrangling
library(writexl) #for saving as an excel file
library(Microsoft365R) #for accessing sharepoint

# Prepare the data sets ####

# Read in each attribute table and get an idea of the contents and structure of each table.
pssb <- read_csv("data_attributes/2012_taxa_attributes_PSSBmain.csv")
orwa <- read_csv("data_attributes/ORWA_Attributes_20250211.csv")

#pad the end of each column heading so that in the future we know which column came from which dataset.
p.colnames <-colnames(pssb) #create a vector of just the column names
p.colnames <- str_c(p.colnames , ".pssb") # add the suffix ".pssb" to each word in the vector
colnames(pssb) <- p.colnames #take the vector and assign them as the new colnames for pssb

o.colnames <-colnames(orwa) #create a vector of just the column names
o.colnames <- str_c(o.colnames , ".orwa") # add the suffix ".orwa" to each word in the vector
colnames(orwa) <- o.colnames #take the vector and assign them as the new colnames for orwa

#main taxon on the orwa taxon list. PSSB uses the phrase "Taxon Name" whereas ORWA uses the term "Taxon" for taxa
pssb_on_orwa <- left_join(x = pssb, y = orwa, join_by ("Taxon Name.pssb" =="Taxon.orwa"))

#Check on common things between the datasets ####
## 1. Are the TSNs the same? ####
pssb_on_orwa %>% filter(TSN.pssb != TSN.orwa) %>% select("Taxon Name.pssb", "TSN.pssb", "TSN.orwa")
#No, there are 16 taxon that have slightly different TSN's between the two tables. I looked up a few on ITIS, the differences are subtle and I need to discuss with Kate.

## 2. Are the traits the same? ####
# First we have to find all taxon that have clinger listed in habit, and create a column with True or False, same with predator

pssb_on_orwa_traits<-pssb_on_orwa %>% 
  mutate(binary_clinger = if_else((str_detect(Habit.orwa, "CN")), "TRUE", "FALSE", missing = "FALSE"))  %>% #if the habit column detects the string "CN" change the new "ORWA_clinger" column to "TRUE", if not, call it "FALSE, and call any cells in "Habit" missing values "FALSE"
  mutate(binary_predator = if_else((str_detect(FFG.orwa, "PR")), "TRUE", "FALSE", missing = "FALSE"))  
pssb_on_orwa_traits

## 2.1 Are the Clinger traits the same? #create a little table
clinger_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Clinger.pssb` != binary_clinger) %>% 
  select("Taxon Name.pssb", "TSN.pssb", "Fore Wisseman 2012-Clinger.pssb", "binary_clinger", "Habit.orwa") %>% 
  add_column(clinger_differences = "clinger_dif")
clinger_difs


## 2.2 Are the longlived traits the same? #create a little table
longlived_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Long Lived.pssb` != LONGLIVED.orwa) %>% 
  select("Taxon Name.pssb", "TSN.pssb", "Fore Wisseman 2012-Long Lived.pssb", "LONGLIVED.orwa", "Life_Cycle.orwa") %>% 
  add_column(longlived_differences = "longlived_dif")
  
longlived_difs

## 2.3 Are the predator traits the same? #create a little table
predator_difs<-pssb_on_orwa_traits %>% 
  filter(`Fore Wisseman 2012-Predator.pssb` != binary_predator) %>% 
  select("Taxon Name.pssb", "TSN.pssb", "Fore Wisseman 2012-Predator.pssb", "binary_predator", "FFG.orwa") %>% 
  add_column(pred_differences = "pred_dif")
predator_difs

## 3. How much overlap is there in these taxon lists? ####
clll<- full_join(clinger_difs, longlived_difs, join_by("Taxon Name.pssb", "TSN.pssb"))
clinger_longlived_predators <-full_join(clll, predator_difs, join_by("Taxon Name.pssb", TSN.pssb)) # table of all the differences
just_difs<-clinger_longlived_predators %>% select(TSN.pssb, clinger_differences, longlived_differences, pred_differences) #subset the just differences table by the new columns (differences columns) and isolate them so they can be appended to the full dataset, giving users all the data.

traits_difs <- full_join(pssb_on_orwa_traits, just_difs, join_by(TSN.pssb)) 

# reorder columns so that the spreadsheet is most useful
traits_difs_ord<- traits_difs |> relocate(c("Order.pssb", "Family.pssb", 'Taxon Name.pssb', TSN.pssb, 
                          "Fore Wisseman 2012-Clinger.pssb", "Habit.orwa", binary_clinger, clinger_differences,
                          "Fore Wisseman 2012-Long Lived.pssb", LONGLIVED.orwa, Life_Cycle.orwa, longlived_differences,
                          "Fore Wisseman 2012-Predator.pssb", "FFG.orwa", binary_predator, pred_differences), 
                        .before = Subkingdom.pssb)

write_csv(traits_difs_ord, "compare_attributes.csv")
write_xlsx(traits_difs_ord, "compare_attributes.xlsx")
#write directly to sharepoint


#identify which site I want to write to:
list_sharepoint_sites() #list the sites I have access to by name
site <- get_sharepoint_site("​​​​​Science Files") #write the name of the site I want (a separate browser window opens and logs me into Sharepoint with my King County credentials) # I don't understand why those red dots make it work, but it does.

# default drive is the main page document library, so we need to find the other drives
site$list_drives() #list the drives. I see LSSG on there
drv <- site$get_drive("LSSG Files")
drv$list_files("Streams/Freshwater Macroinvertebrate Program/PSSB/PSSB 1.0", full_names=TRUE) #make sure I understand the file structure
drv$save_dataframe(traits_difs_ord, "Streams/Freshwater Macroinvertebrate Program/PSSB/PSSB 1.0/compare_attributes.csv")



