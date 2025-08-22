#Necessary files and packages for Disposals processing
## Position in order: 1/10

# The working directory should automatically be set to where the project file is stored, if not add it manually:
#setwd("")

#Load all packages necessary 
library(openxlsx)
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)

# Load files

#1. Load in sample cereal csv with wheat and region data
# wheat area merge should not be needed in future years but region data will still be required
Sample<- read_excel("Sample - Cereal Stocks Survey MAIN sample.xlsx")

#2. Load in raw cereal csv for November
df_raw <- read_csv("QuickStatsExtract 677 All.csv")

#3. Load in raw cereal csv for June disposals 
df_raw_June <-  read_csv("QuickStatsExtract 679 All.csv")

#4. Load final production spreadsheet that has area, production and yield values for each crop type by season 
Final_production_all <- read_excel("Cereal Disposals - 2024 Crop year - Final Production .xlsx")

#5. Load full sample list (people invited to the survey) - Used to create partial returns list
full_dataset <- read_excel("Cereal Production and Disposal Survey - 2024-25 - Disposals - Materials - Sample - July sample mailmerge excluding friendly farm.xlsx")

#6. Load csv of test farms location codes
test_data <- read_csv("ARE - DISD - Agricultural Census - Cereal Production and Disposal Survey - 0 - Test data - Friendly Farmer Testing.csv", col_names = FALSE)%>%
  rename(parish = X1,
         holding = X2)

## Remove test data (Friendly farmers) from data sets ##

Sample <- Sample %>% 
  anti_join(test_data, by = c("parish", "holding") )

df_raw <- df_raw %>% 
  anti_join(test_data, by = c("parish", "holding") )

df_raw_June <- df_raw_June %>% 
  anti_join(test_data, by = c("parish", "holding") )

# Utility
column_names <- c("Opening_stock", "Merchants_for_Malting","Merchants_for_Feed", "Merchants_for_Milling",
                  "Merchants_for_Seed", "Merchants_for_Industrial", "Merchants_for_Other", "Farmers_in_Scotland" ,
                  "Farmers_outwith_Scotland", "Used_for_Seed" , "Used_for_Feed","Waste_Other", "Closing_stock", "Total_Disposals")
column_names2 <- c("Opening_stock", "Merchants_for_Malting","Merchants_for_Feed", "Merchants_for_Milling",
                  "Merchants_for_Seed", "Merchants_for_Industrial", "Merchants_for_Other", "Farmers_in_Scotland" ,
                  "Farmers_outwith_Scotland", "Used_for_Seed" , "Used_for_Feed","Waste_Other", "Closing_stock")
column_names_pattern <- c("Start_stock","Merchants_for_Malting","Merchants_for_Feed","Merchants_for_Milling", 
                            "Merchants_for_Seed","Merchants_for_Industrial", "Merchants_for_Other","Farmers_in_Scotland",   
                             "Farmers_outwith_Scotland", "Used_for_Seed","Used_for_Feed","Waste_Other","Total_disposed", "Closing_Stock")

id_cols <- c("CPH","parish","holding","Region","Crop","Crop_general")

regions_of_interest <- c("UKM5", "UKM6", "UKM7", "UKM8", "UKM9")

variables <- c("Merchants_for_Malting", "Merchants_for_Feed", "Merchants_for_Milling",  
               "Merchants_for_Seed", "Merchants_for_Industrial", "Merchants_for_Other",     
               "Farmers_in_Scotland", "Farmers_outwith_Scotland", "Used_for_Seed", 
               "Used_for_Feed", "Waste_Other")

variables2 <- c("Sum_Opening_stock","Sum_Merchants_for_Malting","Sum_Merchants_for_Feed","Sum_Merchants_for_Milling","Sum_Merchants_for_Seed",
                "Sum_Merchants_for_Industrial","Sum_Merchants_for_Other","Sum_Farmers_in_Scotland","Sum_Farmers_outwith_Scotland","Sum_Used_for_Seed",
                "Sum_Used_for_Feed","Sum_Waste_Other","Sum_Closing_stock")

