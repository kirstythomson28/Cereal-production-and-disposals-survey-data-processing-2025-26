## Data table (fixed) tab ###
## Position in order: 5/ 10

#Packages can be found and run on tab 0/11


## This tab is recreating the pivot table previously used in Excel##
Data_table_fixed <- disposals %>%
  # Group by Month, Region and crop type
  group_by(Month, Region, Crop) %>%
  # Calculate the sum of each category within column_names per crop type and region.
  # This will also add Sum_ to the start of the category names.
  summarise(across(all_of(column_names2), ~ sum(.x, na.rm = TRUE), .names = "Sum_{.col}")) %>%
  ungroup()

# Filter disposals to just columns are interested in and ignore any rows only containing 0s 
# These may occur where a farm has barley so are in the sample but have no wheat so
# enter all zeros 
filtered_disposals <- disposals %>%
  filter(rowSums(select(., 7:19) > 0) > 0)

#Calculate the number of unique parish and holding for each Month, Region, and Crop
unique_counts <- filtered_disposals %>%
  group_by(Month, Region, Crop) %>%
  summarise(
    count_parish_holdings = n_distinct(CPH),
    .groups = 'drop'
  )


# Join the unique counts with Data_table_fixed
Data_table_fixed <- Data_table_fixed %>%
  left_join(unique_counts, by = c("Month", "Region", "Crop"))

Data_table_fixed <- Data_table_fixed %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))


# 1. Calculate Monthly totals (sum across all regions and crops for each Month)
Monthly_totals <- Data_table_fixed %>%
  group_by(Month,Crop) %>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Region = "total",Month = paste0(Month, "_total"))

# 2. Calculate region totals (sum across all Months and crops for each region)
region_totals <- Data_table_fixed %>%
  group_by(Region, Crop) %>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Month = "total", Region = paste0(Region, "_total"))

# 3. Calculate grand totals 
grand_totals <- Data_table_fixed %>%
  group_by(Crop)%>%
  summarise(across(starts_with("Sum_"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(Region = "grand_total", Month = "grand_total")%>%
  relocate(Region, .before = Crop)%>%
  relocate(Month, .before = Region)

# 4. Combine the original data with the calculated totals
Data_table_fixed_with_totals <- bind_rows(Data_table_fixed, Monthly_totals, region_totals, grand_totals)
# Replace NA values with 0 in column 'x'
Data_table_fixed_with_totals$count_parish_holdings[is.na(Data_table_fixed_with_totals$count_parish_holdings)] <- 0

# 5. arrange the data
Data_table_fixed_with_totals <- Data_table_fixed_with_totals %>%
  arrange(Month, Region, Crop)

# View(Data_table_fixed_with_totals)









