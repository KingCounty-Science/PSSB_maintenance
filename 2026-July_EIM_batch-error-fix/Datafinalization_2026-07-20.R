#This is a scr2026-July_EIM_batch-error-fixipt that goes with the files in the folder "2025_Documents_to_Upload"

#From email: 
# Batch 1
# 1.	The Sample ID “08SAM2674_R” appears to be missing the year like the other Sample IDs have. I believe this should be changed to “08SAM2674_21_R”.
# 2.	Location ID “08LAK2827” has five samples with the Sample ID “08LAK2817_17_R”. Following the naming convention of the other samples, I think these should be changed to “08LAK2827_17_R”.
# 3.	The comments in the “Result Additional Comment” column appear to be duplicates of the Sample IDs for each result. Are these serving a purpose or could we remove the redundancy?
#   4.	Most of the errors are coming from a mismatch of the “Result Taxon Name” and the “Result Taxon TSN”. If the taxon doesn’t have a TSN in the EIM Valid Value table, then it doesn’t require one. I’ve separated out the results with warnings into their own file. Please reference this file to know which results to remove the TSN from.
# 
# Batch 2
# 1.	Location ID “BSE_8_268thAve” is slightly different than what is listed in the Location template. If this Location ID is being replaced by the existing Location ID, then this can be disregarded. If not, then this should match the Location template. Additionally, there is a “~” symbol in the Study Specific Location ID. It’s not necessarily wrong, but it looks out of place with the naming convention used for the other locations, so I wasn’t sure if that was intentional.
# 2.	Location ID “08LAK2827” has five samples with the Sample ID “08LAK2817_17_R”. Following the naming convention of the other samples, I think these should be changed to “08LAK2827_17_R”.
# 3.	The Sample ID “09BLA0772” appears to be missing the year like the other Sample IDs have. I believe this should be changed to “09BLA0772_15”.
# 4.	Sample ID “09SOO1022_18_Rep” uses a different naming convention than the other replicates. You don’t have to change this, but if you’d like it to match the others you could change it to “09SOO1022_18_R”.
# 5.	The comments in the “Result Additional Comment” column appear to be duplicates of the Sample IDs for each result. Are these serving a purpose or could we remove the redundancy?
#   6.	Like the other file, most of the errors are coming from a mismatch of the “Result Taxon Name” and the “Result Taxon TSN”. If the taxon doesn’t have a TSN in the EIM Valid Value table, then it doesn’t require one. I’ve separated out the results with warnings into their own file. Please reference this file to know which results to remove the TSN from.


#packages

library(tidyverse)

# read in the original uploads.####
batch1 <- read_csv("2026-July_EIM_batch-error-fix/2025_Documents_to_Upload/EIM_2015_2024_batch1.csv")
batch2 <- read_csv("2026-July_EIM_batch-error-fix/2025_Documents_to_Upload/EIM_2015_2024_batch2.csv")

#create a vector of Taxon Names that need to have the Result Taxon TSN removed. Read in csv from Caitlin first.
warnings_batch1 <- read_csv("2026-July_EIM_batch-error-fix/From-Caitlin/EIM_2015_2024_batch1_warnings.csv")
warnings_batch1_dropTSN <- warnings_batch1 |> select(Result_Taxon_Name ) |> pull()

warnings_batch2 <- read_csv("2026-July_EIM_batch-error-fix/From-Caitlin/EIM_2015_2024_batch2_warnings.csv")
warnings_batch2_dropTSN <- warnings_batch2 |> select(Result_Taxon_Name ) |> pull()

# Fix each error for batch 1 and save as new dataframe
batch1_corrected <- batch1 |> 
  mutate(Sample_ID = str_replace_all(Sample_ID, "08SAM2674_R", "08SAM2674_21_R")) |>  #Batch 1,1  
  mutate(Sample_ID = str_replace_all(Sample_ID, "08LAK2817_17_R", "08LAK2827_17_R")) |>  #Batch 1,2 
  mutate(Result_Additional_Comment = "") |>   #Batch1, 3
  mutate(Result_Taxon_TSN = if_else(Result_Taxon_Name %in% warnings_batch1_dropTSN, NA, Result_Taxon_TSN)) #batch 1, 4

# Fix each error for batch 1 and save as new dataframe
batch2_corrected <- batch2 |> 
  mutate(Study_Specific_Location_ID = str_replace_all(Study_Specific_Location_ID, "BSE_8_268thAve~", "BSE_8_268thAve")) |> ## Batch 2,1 
  mutate(Sample_ID = str_replace_all(Sample_ID, "08LAK2817_17_R", "08LAK2827_17_R")) |>  #Batch 2,2 (Actually there are 20 instances, not just 5)
  mutate(Sample_ID = str_replace_all(Sample_ID, "09BLA0772", "09BLA0772_15")) |> #Batch 2, 3
  mutate(Sample_ID = str_replace_all(Sample_ID, "09SOO1022_18_Rep", "09SOO1022_18_R")) |> #Batch 2, 4 
  mutate(Result_Additional_Comment = "") |>   #Batch2, 5
  mutate(Result_Taxon_TSN = if_else(Result_Taxon_Name %in% warnings_batch2_dropTSN, NA, Result_Taxon_TSN)) #batch 2, 6

# Write the corrected csvs.
write_csv(batch1_corrected, "2026-July_EIM_batch-error-fix/EIM_2015_2024_batch1_corrected.csv", na = "")
write_csv(batch2_corrected, "2026-July_EIM_batch-error-fix/EIM_2015_2024_batch2_corrected.csv", na = "")

#Bonus/Scratch pad.
# Extra exploration/confirmation of issues if desired. In reality, Beka completed a review of each error one by one prior to changing the dataframe. The reviews are below. ####
## BATCH 1#
# 1.	The Sample ID “08SAM2674_R” appears to be missing the year like the other Sample IDs have. I believe this should be changed to “08SAM2674_21_R”.
batch1 |> filter(Location_ID == "08SAM2674") |> select(Location_ID, Sample_ID, Field_Collection_Start_Date) |> unique()

# 2.	Location ID “08LAK2827” has five samples with the Sample ID “08LAK2817_17_R”. Following the naming convention of the other samples, I think these should be changed to “08LAK2827_17_R”.
batch1 |> 
  filter(Location_ID == "08LAK2827") |> 
  select(Sample_ID) |> 
  group_by(Sample_ID) |> summarise(n = n())

# 3.	The comments in the “Result Additional Comment” column appear to be duplicates of the Sample IDs for each result. Are these serving a purpose or could we remove the redundancy?
batch1 |> select(Sample_ID , Result_Additional_Comment) |> 
  mutate(equal_exact = Sample_ID == Result_Additional_Comment) |> 
  filter(equal_exact != TRUE)

# 4.	Most of the errors are coming from a mismatch of the “Result Taxon Name” and the “Result Taxon TSN”. If the taxon doesn’t have a TSN in the EIM Valid Value table, then it doesn’t require one. I’ve separated out the results with warnings into their own file. Please reference this file to know which results to remove the TSN from.

## Check it appears to work by selecting a specific taxon to see that it's TSN is removed.
batch1_corrected |> filter(Result_Taxon_Name == "Afghanurus") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)
batch1_corrected |> filter(Result_Taxon_Name == "Acari") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)
# and check on a specific sample we modified.
batch1_corrected |> filter(Sample_ID == "08SAM2674_21_R") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)


## BATCH 2#
# 1.	Location ID “BSE_8_268thAve” is slightly different than what is listed in the Location template. If this Location ID is being replaced by the existing Location ID, then this can be disregarded. If not, then this should match the Location template. Additionally, there is a “~” symbol in the Study Specific Location ID. It’s not necessarily wrong, but it looks out of place with the naming convention used for the other locations, so I wasn’t sure if that was intentional.
batch2 |> filter(Study_Specific_Location_ID == "BSE_8_268thAve~")
batch2_corrected |> filter(Study_Specific_Location_ID == "BSE_8_268thAve")

# 2.	Location ID “08LAK2827” has five samples with the Sample ID “08LAK2817_17_R”. Following the naming convention of the other samples, I think these should be changed to “08LAK2827_17_R”.
batch2 |> 
  filter(Location_ID == "08LAK2827") |> 
  select(Sample_ID) |> 
  group_by(Sample_ID) |> summarise(n = n())

# 3.	The Sample ID “09BLA0772” appears to be missing the year like the other Sample IDs have. I believe this should be changed to “09BLA0772_15”.
batch2 |> filter(Location_ID == "09BLA0772") |> select(Sample_ID, Field_Collection_Start_Date) |> unique()

# 4.	Sample ID “09SOO1022_18_Rep” uses a different naming convention than the other replicates. You don’t have to change this, but if you’d like it to match the others you could change it to “09SOO1022_18_R”.

batch2 |> filter(Sample_ID == "09SOO1022_18_Rep") |> select(Sample_ID)

# 5.	The comments in the “Result Additional Comment” column appear to be duplicates of the Sample IDs for each result. Are these serving a purpose or could we remove the redundancy?
batch2 |> select(Sample_ID , Result_Additional_Comment) |> 
  mutate(equal_exact = Sample_ID == Result_Additional_Comment) |> 
  filter(equal_exact != TRUE)

#   6.	Like the other file, most of the errors are coming from a mismatch of the “Result Taxon Name” and the “Result Taxon TSN”. If the taxon doesn’t have a TSN in the EIM Valid Value table, then it doesn’t require one. I’ve separated out the results with warnings into their own file. Please reference this file to know which results to remove the TSN from.

## Check it appears to work by selecting a specific taxon to see that it's TSN is removed.
batch2_corrected |> filter(Result_Taxon_Name == "Afghanurus") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)
batch2_corrected |> filter(Result_Taxon_Name == "Acari") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)
# and check on a specific sample we modified.
batch2_corrected |> filter(Sample_ID == "09BLA0772_15") |> select(Location_ID, Sample_ID, Result_Taxon_Name, Result_Taxon_TSN)



#is batch1 and batch2 unique data?
batch1_corrected |> filter(Location_ID == "08LAK2827") |> select(Sample_ID, Field_Collection_Start_Date) |> unique()
batch2_corrected |> filter(Location_ID == "08LAK2827") |> select(Sample_ID, Field_Collection_Start_Date) |> unique()

#They both have a 2017 R Sample

batch1_corrected |> filter(Sample_ID == "08LAK2827_17_R") |> select(Result_Taxon_Name)
batch2_corrected |> filter(Sample_ID == "08LAK2827_17_R") |> select(Result_Taxon_Name)




