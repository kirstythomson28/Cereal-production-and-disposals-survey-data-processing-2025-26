## Disposals data ##
## Position in order: 1/ 11

## This file will clean the Survey Results and create a list of the partial responders ##
# To clean the data first run the November processing then the July results 
##### ##### November Production ##### ##### 

#combine parish/holding
df_raw$CPH <- paste(df_raw$parish, df_raw$holding, sep="/")
names(df_raw)

#move CPH to 1st column
df_raw <- df_raw%>%
  relocate(CPH)

####subset and alter wheat data ####

#extract and subset just the required wheat data
Wheat<-subset(df_raw, select=c(CPH, parish, holding, item40002:item40009))

#rename items
Wheat<-Wheat %>% 
  rename(
    Crop = item40002,
    Production = item40005,
    Yield = item40450,
    Moisture_content = item40008,
    Whole_cropped = item40009)

names(Wheat)


#Fill Crop column with "Wheat" check n obs! (here 17obs)

nrow(Wheat)
Wheat$Crop <- rep("Wheat",len=nrow(Wheat))

head(Wheat)
# NOTE: may need to add blank columns for Area so that rbind can be used to amalgamate allsubsetted tibbles when complete

####subset for Barely_S #####
#extract and subset just the required wheat data
Barley_S<-subset(df_raw, select=c(CPH, parish, holding, item40032, item40044,item40043,item40452, item40052, item40058 ))


#rename items
Barley_S<-Barley_S %>% 
  rename(
    Crop = item40032,
    Area = item40044,
    Production = item40043,
    Yield = item40452,
    Moisture_content = item40052,
    Whole_cropped = item40058)

names(Barley_S)

#Fill Crop column with "Wheat" check n obs (here 17obs)

Barley_S$Crop <- rep("Barley S",len=nrow(Barley_S))

head(Barley_S)

####subset for Barley_W #####
#extract and subset just the required Barley data

Barley_W<-subset(df_raw, select=c(CPH, parish, holding, item40032, item40040,item40042, item40451, item40051, item40057 ))

#rename items
Barley_W<-Barley_W %>% 
  rename(
    Crop = item40032,
    Area = item40040,
    Production = item40042,
    Yield = item40451,
    Moisture_content = item40051,
    Whole_cropped = item40057)

names(Barley_W)

#Fill Crop column with "Barley" check n obs (here 17obs)

Barley_W$Crop <- rep("Barley W",len=nrow(Barley_W))

head(Barley_W)


####subset for Oats_W #####
#extract and subset just the required Oats data

Oats_W<-subset(df_raw, select=c(CPH, parish, holding, item40110, item40047,item40117, item40453, item40123, item40126 ))


#rename items
Oats_W<-Oats_W %>% 
  rename(
    Crop = item40110,
    Area = item40047,
    Production = item40117,
    Yield = item40453,
    Moisture_content = item40123,
    Whole_cropped = item40126)

names(Oats_W)

#Fill Crop column with "Oats_W" check n obs (here 17obs)

Oats_W$Crop <- rep("Oats W",len=nrow(Oats_W))

head(Oats_W)


####subset for Oats_S #####
#extract and subset just the required Oats data

Oats_S<-subset(df_raw, select=c(CPH, parish, holding, item40110, item40048,item40118, item40454, item40124, item40127 ))


#rename items
Oats_S<-Oats_S %>% 
  rename(
    Crop = item40110,
    Area = item40048,
    Production = item40118,
    Yield = item40454,
    Moisture_content = item40124,
    Whole_cropped = item40127)

names(Oats_S)

#Fill Crop column with "Oats_S" check n obs (here 17obs)

Oats_S$Crop <- rep("Oats S",len=nrow(Oats_S))

head(Oats_S)

####subset for OSR_S #####
#extract and subset just the required Oats data

OSR_S<-subset(df_raw, select=c(CPH, parish, holding, item40406, item40059,item40414, item40456, item40420, item40423))


#rename items
OSR_S<-OSR_S %>% 
  rename(
    Crop = item40406,
    Area = item40059,
    Production = item40414,
    Yield = item40456,
    Moisture_content = item40420,
    Whole_cropped = item40423)

names(OSR_S)

#Fill Crop column with "OSR_S" check n obs (here 17obs)

OSR_S$Crop <- rep("OSRape S",len=nrow(OSR_S))

head(OSR_S)

####subset for OSR_W #####
#extract and subset just the required Oats data

OSR_W<-subset(df_raw, select=c(CPH, parish, holding, item40406, item40055,item40413, item40455, item40419, item40422))


#rename items
OSR_W<-OSR_W %>% 
  rename(
    Crop = item40406,
    Area = item40055,
    Production = item40413,
    Yield = item40455,
    Moisture_content = item40419,
    Whole_cropped = item40422)

names(OSR_W)

#Fill Crop column with "OSR_W" check n obs (here 17obs)

OSR_W$Crop <- rep("OSRape W",len=nrow(OSR_W))

head(OSR_W)

## next step is to get the crop area for wheat from the https://erdm.scotland.gov.uk:8443/documents/A45710991/details which is a csv - this will need to be bound to the wheat data already created HERE by merging or column binding it by CPH

## After that stack all the individual crop data tables and then match for region from the sample data


#https://www.infoworld.com/article/3454356/how-to-merge-data-in-r-using-r-merge-dplyr-or-datatable.html
#Merges with base R

#Now working with sample cereal csv with wheat and region data


#combine parish/holding
Sample$CPH <- paste(Sample$parish, Sample$holding, sep="/")
names(Sample)

#mmove CPH to 1st column
Sample_2<- Sample %>% relocate(CPH)
names(Sample_2)

Sample.Wheat<-subset(Sample_2, select=c(CPH, wheat))

joined_tibble <- left_join(Wheat,Sample.Wheat, by ="CPH" )

str(joined_tibble)

# change order of tibble to match the other crops
Wheat.b <-joined_tibble %>% relocate(wheat, .before = Production)

#rename wheat as Area
Wheat.b <-Wheat.b %>% 
  rename(
    Area = wheat)
#check
names(Wheat.b)


#append all the separate crop dfs
Crops_all = bind_rows(Wheat.b, Barley_S, Barley_W, Oats_S, Oats_W, OSR_S,OSR_W)


#Use Sample2 (created from sample with CPH formed) to extract NUTS2 and CPH
names(Sample_2)
Sample.Nuts2<-subset(Sample_2, select=c(CPH, nuts2))

#Join the data frames
joined_all <- left_join(Crops_all,Sample.Nuts2, by ="CPH" )
#check
str(joined_all)

#add format for wholecrop
joined_all <- joined_all %>%
  mutate(Wholecrop = case_when(Whole_cropped == 1 ~ 'YES',
                               Whole_cropped == 2 ~ 'NO'))
#add format for Region
joined_all <- joined_all %>%
  mutate(Region = case_when(nuts2 == 1 ~ 'UKM5',
                            nuts2 == 2 ~ 'UKM7',
                            nuts2 == 3 ~ 'UKM9',
                            nuts2 == 4 ~ 'UKM6',
                            nuts2 == 5 ~ 'UKM8'))

#move order
joined_all <- joined_all %>% relocate(CPH, parish, holding, Region, Crop, Area, Production, Moisture_content
                                      , Wholecrop, Whole_cropped, nuts2, Yield)

#remove records generated with no area, 
joined_all <- joined_all %>% 
  filter(Area>0)

#export csv
# filename appropriate for data upload to erdm
str1 <- "Cereal Production and Disposal Survey - 2024-25 - Production - "
str2 <- " - Data - Raw Data - Formatted data output - "
str3 <- ".csv"
outputname <- paste(str1, format(Sys.Date(), format="%Y"), str2, format(Sys.Date(), format="%d %B"), str3, sep = "")
write.csv(joined_all, outputname, row.names = FALSE)

##### ##### November DISPOSALS ##### ##### 

# Create crop dataframe as a subset of df_raw
Disposals_Wheat <- subset(df_raw, select=c(CPH, parish, holding, item40002,item40005, item40450, item40013:item40023, item40082))
Disposals_Wheat <- Disposals_Wheat %>%
  rename(
    Crop = item40002,
    Production = item40005,
    Yield = item40450,
    Merchants_for_Malting = item40013,
    Merchants_for_Feed = item40014,
    Merchants_for_Milling = item40015,
    Merchants_for_Seed = item40016,
    Merchants_for_Industrial = item40017,
    Merchants_for_Other = item40018,
    Farmers_in_Scotland = item40019,
    Farmers_outwith_Scotland = item40020,
    Used_for_Seed = item40021,
    Used_for_Feed = item40022,
    Waste_Other = item40023,
    October_Stock = item40082)

# Replace "na" with Crop type
Disposals_Wheat$Crop <- rep("Wheat",len=nrow(Disposals_Wheat))

# Create crop dataframe as a subset of df_raw
Disposals_Barley <- subset(df_raw, select=c(CPH, parish, holding, item40032, item40042,item40043, item40066:item40091))
Disposals_Barley <- Disposals_Barley%>%
  #  Work row by row in the dataframe
  rowwise %>%
  # Create a single production figure by combining winter and spring crop
  mutate(Production = coalesce(item40042,0) + coalesce(item40043,0))%>%
  rename(
    Crop = item40032,
    Merchants_for_Malting = item40066,
    Merchants_for_Feed = item40069,
    Merchants_for_Milling = item40072,
    Merchants_for_Seed = item40075,
    Merchants_for_Industrial = item40078,
    Merchants_for_Other = item40081,
    Farmers_in_Scotland = item40084,
    Farmers_outwith_Scotland = item40087,
    Used_for_Seed = item40090,
    Used_for_Feed = item40093,
    Waste_Other = item40096,
    October_Stock = item40091)%>%
  # Organising the dataframe, then removing winter and spring production 
  select(CPH, parish, holding, Crop, Production, everything(),-item40042, -item40043, -item40099)
# Replace "na" with Crop type
Disposals_Barley$Crop <- rep("Barley",len=nrow(Disposals_Barley))

# Create crop dataframe as a subset of df_raw
Disposals_Oats <- subset(df_raw, select=c(CPH, parish, holding, item40110, item40117, item40118, item40156:item40103))
Disposals_Oats <- Disposals_Oats %>%
  #  Work row by row in the dataframe
  rowwise %>%
  # Create a single production figure by combining winter and spring crop
  mutate(Production = coalesce(item40117,0) + coalesce(item40118,0))%>%
  rename(
    Crop = item40110,
    Merchants_for_Malting = item40156,
    Merchants_for_Feed = item40159,
    Merchants_for_Milling = item40162,
    Merchants_for_Seed = item40165,
    Merchants_for_Industrial = item40168,
    Merchants_for_Other = item40171,
    Farmers_in_Scotland = item40174,
    Farmers_outwith_Scotland = item40177,
    Used_for_Seed = item40180,
    Used_for_Feed = item40183,
    Waste_Other = item40186,
    October_Stock = item40103) %>%  #then removing winter and spring production 
  select(CPH, parish, holding, Crop, Production, everything(),-item40117, -item40118, -item40189)
# Replace "na" with Crop type
Disposals_Oats$Crop <- rep("Oats",len=nrow(Disposals_Oats))

# Append crop disposals dataframes
Disposals_ALL = bind_rows(Disposals_Wheat, Disposals_Barley, Disposals_Oats)
# Append and organise region data according to the CPH
Disposals_ALL <- left_join(Disposals_ALL,Sample.Nuts2, by ="CPH" )
# NUTS2 format for Region and filtering out 0 values (Filter production and set na to 0)
Disposals_ALL_Nov <- Disposals_ALL %>%
  mutate(
    Region = case_when(
      nuts2 == 1 ~ 'UKM5',
      nuts2 == 2 ~ 'UKM7',
      nuts2 == 3 ~ 'UKM9',
      nuts2 == 4 ~ 'UKM6',
      nuts2 == 5 ~ 'UKM8'))%>%
  select(
    CPH, parish, holding, Region, Crop, Production, everything(),-nuts2)%>%
  filter(
    Production>0)%>%
  mutate_all(
    ~ifelse(is.na(.), 0, .)) %>% 
  mutate(Month = "Oct",
         Crop_general = Crop)%>%
  relocate(Month, .before = Region) %>% 
  relocate(Crop_general, .before = Production) %>% 
  relocate(Yield, .after = October_Stock)

#  Disposals per crop type as a subset of Disposals_ALL
Disposals_Crop <- subset(Disposals_ALL_Nov, select=c(Crop, Production, Merchants_for_Malting, Merchants_for_Feed, Merchants_for_Milling,
                                                     Merchants_for_Seed, Merchants_for_Industrial, Merchants_for_Other, Farmers_in_Scotland,
                                                     Farmers_outwith_Scotland, Used_for_Seed, Used_for_Feed, Waste_Other, October_Stock))
Disposals_Crop <- Disposals_Crop%>%
  group_by(Crop)%>%
  summarise_all(sum)

head(Disposals_Crop)

#export csv
# # filename appropriate for data upload to erdm
# str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - November - "
# str5 <- " - Data - Raw Data - Formatted data output - "
# str6 <- ".csv"
# outputname <- paste(str4, format(Sys.Date(), format="%Y"), str5, format(Sys.Date(), format="%d %B"), str6, sep = "")
# write.csv(Disposals_ALL_Nov, outputname, row.names = FALSE)

##### ########## ########## ########## #####
##### ##### June DISPOSALS ##### ##### ##### 
##### ########## ########## ########## #####

df_raw_June$CPH <- paste(df_raw_June$parish, df_raw_June$holding, sep="/")


# Create crop dataframe as a subset of df_raw
Disposals_Wheat_June <- subset(df_raw_June, select=c(CPH, parish, holding, item40119, item40013:item40023, item40121))
Disposals_Wheat_June <- Disposals_Wheat_June %>%
  rename(
    Opening_Stock_June = item40119,
    Merchants_for_Malting = item40013,
    Merchants_for_Feed = item40014,
    Merchants_for_Milling = item40015,
    Merchants_for_Seed = item40016,
    Merchants_for_Industrial = item40017,
    Merchants_for_Other = item40018,
    Farmers_in_Scotland = item40019,
    Farmers_outwith_Scotland = item40020,
    Used_for_Seed = item40021,
    Used_for_Feed = item40022,
    Waste_Other = item40023,
    June_Closing_Stock = item40121)

# Replace "na" with Crop type
Disposals_Wheat_June$Crop <- rep("Wheat",len=nrow(Disposals_Wheat_June))

# Create crop dataframe as a subset of df_raw
Disposals_Barley_June <- subset(df_raw_June, select=c(CPH, parish, holding, item40140, 
                                                      item40066,
                                                      item40069,
                                                      item40072,
                                                      item40075, 
                                                      item40078,
                                                      item40081,
                                                      item40084,
                                                      item40087,
                                                      item40090,
                                                      item40093,
                                                      item40096,
                                                      item40142))
Disposals_Barley_June <- Disposals_Barley_June %>%
  rename(
    Opening_Stock_June = item40140,
    Merchants_for_Malting = item40066,
    Merchants_for_Feed = item40069,
    Merchants_for_Milling = item40072,
    Merchants_for_Seed = item40075,
    Merchants_for_Industrial = item40078,
    Merchants_for_Other = item40081,
    Farmers_in_Scotland = item40084,
    Farmers_outwith_Scotland = item40087,
    Used_for_Seed = item40090,
    Used_for_Feed = item40093,
    Waste_Other = item40096,
    June_Closing_Stock = item40142)
# Replace "na" with Crop type
Disposals_Barley_June$Crop <- rep("Barley",len=nrow(Disposals_Barley_June))

# Create crop dataframe as a subset of df_raw
Disposals_Oats_June <- subset(df_raw_June, select=c(CPH, parish, holding, item40136, item40156:item40186, item40138))
Disposals_Oats_June <- Disposals_Oats_June%>%
  rename(
    Opening_Stock_June = item40136,
    Merchants_for_Malting = item40156,
    Merchants_for_Feed = item40159,
    Merchants_for_Milling = item40162,
    Merchants_for_Seed = item40165,
    Merchants_for_Industrial = item40168,
    Merchants_for_Other = item40171,
    Farmers_in_Scotland = item40174,
    Farmers_outwith_Scotland = item40177,
    Used_for_Seed = item40180,
    Used_for_Feed = item40183,
    Waste_Other = item40186,
    June_Closing_Stock = item40138)
# Replace "na" with Crop type
Disposals_Oats_June$Crop <- rep("Oats",len=nrow(Disposals_Oats_June))

# Append crop disposals dataframes
Disposals_ALL_June = bind_rows(Disposals_Wheat_June, Disposals_Barley_June, Disposals_Oats_June)
# Append and organise region data according to the CPH
Disposals_ALL_June <- left_join(Disposals_ALL_June,Sample.Nuts2, by ="CPH" )
# NUTS2 format for Region and filtering out 0 values (Filter production and set na to 0)
Disposals_ALL_June <- Disposals_ALL_June %>%
  mutate(
    Region = case_when(
      nuts2 == 1 ~ 'UKM5',
      nuts2 == 2 ~ 'UKM7',
      nuts2 == 3 ~ 'UKM9',
      nuts2 == 4 ~ 'UKM6',
      nuts2 == 5 ~ 'UKM8'))%>%
  mutate_all(
    ~ifelse(is.na(.), 0, .))

Disposals_ALL_June <- Disposals_ALL_June %>%
  mutate(Month = "Jun",   
         Crop_general = Crop)%>%
  # Add the new column
  relocate(Month, .after = holding) %>%   # Move 'month' after 'holding'
  relocate(Region, .after = Month) %>%    # Move 'Region' after 'month'
  relocate(Crop, .after = Region) %>%     # Move 'Crop' after 'Region'
  relocate(Crop_general, .before = Opening_Stock_June) %>% 
  select(-nuts2)

#  Disposals per crop type as a subset of Disposals_ALL
Disposals_Crop_June <- subset(Disposals_ALL_June, select=c(Crop, Opening_Stock_June, Merchants_for_Malting, Merchants_for_Feed, Merchants_for_Milling,
                                                           Merchants_for_Seed, Merchants_for_Industrial, Merchants_for_Other, Farmers_in_Scotland,
                                                           Farmers_outwith_Scotland, Used_for_Seed, Used_for_Feed, Waste_Other,June_Closing_Stock))
Disposals_Crop_June <- Disposals_Crop_June%>%
  group_by(Crop)%>%
  summarise_all(sum)


# # Export XLSX with filename appropriate for data upload to ERDM, including date and time
# str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
# str5 <- " - Data - Raw Data - Formatted data output - "
# str6 <- ".xlsx"
# current_datetime <- format(Sys.time(), format="%Y")
# outputname <- paste(str4, format(Sys.Date(), format="%Y"), str5, format(Sys.Date(), format="%d %B"), str6, sep = "")
# write.xlsx(Disposals_ALL_June, outputname, rowNames = FALSE)




### Clean and combine survey results ###

disposals_nov <- Disposals_ALL_Nov %>%
  rename(Opening_stock = Production,
         Closing_stock = October_Stock)%>%
  select(-Yield)


disposals_june <- Disposals_ALL_June%>%
  rename(Opening_stock = Opening_Stock_June,
         Closing_stock = June_Closing_Stock)


disposals <- full_join(disposals_nov, disposals_june)

#export csv
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - Nov and Jul - "
str5 <- " - Data - Raw Data - Formatted data output - "
str6 <- ".xlsx"
current_datetime <- format(Sys.time(), format="%Y")
outputname <- paste(str4, format(Sys.Date(), format="%Y"), str5, format(Sys.Date(), format="%d %B"), str6, sep = "")
write.csv(disposals, outputname, row.names = FALSE)



#### Create list of partial returns ###
full_dataset2 <- disposals %>%
  mutate(Total_Disposals = rowSums(across(variables))) %>% 
  pivot_wider(
    id_cols = all_of(id_cols),
    names_from = Month,
    values_from = all_of(column_names),
    names_sep = "_"
  )

partial_returns <- full_dataset2 %>%
  filter(
    !is.na(Closing_stock_Oct) & Closing_stock_Oct > 0,  # Keep rows where Closing_stock_Oct > 0
    (is.na(Opening_stock_Jun) | Opening_stock_Jun == NA) # And Opening_stock_Jun is 0 or NA
  ) %>%
  select(
    -matches("_Oct$"),           # Remove all columns ending in _Oct
    Closing_stock_Oct            # But keep Closing_stock_Oct
  ) %>%
  select(everything(), Closing_stock_Oct)%>%
  relocate(Closing_stock_Oct, .before = Opening_stock_Jun )

#view(partial_returns)


#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - Nov and Jul -  "
str5 <- " - Data - Raw Data - List of partial returns  - "
str6 <- ".csv"
outputname <- paste(
  str4, 
  format(Sys.Date(), format="%Y"), 
  str5, 
  format(Sys.time(), format="%d %B"), 
  str6, 
  sep = ""
)
write.csv(partial_returns, outputname, row.names = FALSE)


