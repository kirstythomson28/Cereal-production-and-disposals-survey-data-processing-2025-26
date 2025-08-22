## Disposal Results w estimates tab ATTEMPT  ###
## Position in order: 8 / 11

## Second version of Disposal results w estimates tab ###
### This version is work in progress ##
##It is to complete the same calculation as attempt 1 but with much less code ##

FPAC_subset <- Final_production_all_clean %>%
  select(Crop, Region, production) %>%
  # Combine all variations of Barley, Wheat, and Oats into a single category
  mutate(Crop = case_when(
    grepl("^Barley", Crop, ignore.case = TRUE) ~ "Barley",  # Combine all "Barley" variations into "Barley"
    grepl("^Wheat", Crop, ignore.case = TRUE) ~ "Wheat",    # Combine all "Wheat" variations into "Wheat"
    grepl("^Oats", Crop, ignore.case = TRUE) ~ "Oats",      # Combine all "Oats" variations into "Oats"
    TRUE ~ Crop                                             # Keep other values unchanged
  )) %>%
  filter(!(Crop %in% c("OSRape S", "OSRape W")))%>%
  # Group by Crop, Region, and other necessary columns, and sum the production values
  group_by(Crop, Region) %>% 
  summarize(production = sum(production, na.rm = TRUE), .groups = 'drop')%>%
  mutate(Month = "Oct")

total_production_by_crop_oct <- FPAC_subset %>%
  filter(Month == "Oct") %>%  # Filter rows for Month = "Oct"
  group_by(Crop) %>%          # Group by Crop
  summarise(production = sum(production, na.rm = TRUE), # Summarize total production
            .groups = 'drop')%>%
  mutate(Month = "Oct_total",
         Region = "total")

# Merge with Data_table_fixed_est_with_totals
DRT2 <- Data_table_fixed_est_with_totals %>%
  left_join(FPAC_subset, by = c("Crop", "Region", "Month")) %>%
  left_join(total_production_by_crop_oct, by = c("Crop", "Region", "Month")) %>%
  # Calculate Opening_stock based on the Month
  mutate(Start_stock = ifelse(Month == "Oct_total", production.y, production.x)) %>%
  select(-production.x, -production.y) %>% # Remove redundant production columns
  relocate(Start_stock, .before = Sum_Opening_stock)

DRT2 <- DRT2 %>%
  mutate(Merchants_for_Malting = NA,
         Merchants_for_Feed = NA,
         Merchants_for_Milling = NA,
         Merchants_for_Seed = NA,
         Merchants_for_Industrial = NA,
         Merchants_for_Other = NA,
         Farmers_in_Scotland = NA,
         Farmers_outwith_Scotland = NA,
         Used_for_Seed = NA,
         Used_for_Feed = NA,
         Waste_Other = NA)


# Calculate the totals for each variable over regions UKM5:UKM9 for each crop
totals_by_crop <- DRT2 %>%
  filter(Region %in% regions_of_interest) %>%
  group_by(Crop, Month) %>%
  summarize(
    across(
      all_of(variables2),
      ~ sum(.x, na.rm = TRUE),
      .names = "Sum_{.col}"
    ),
    Sum_Start_stock = sum(Start_stock, na.rm = TRUE),  # if you want to sum Start_stock separately
    .groups = "drop"
  )

# Perform the conditional computation for each variable
DRT2 <- DRT2 %>%
  # Join with the computed totals by crop
  left_join(totals_by_crop, by = c("Crop","Month")) %>%
  group_by(Month, Region, Crop) %>%
  # Create the 'condition' variable first
  mutate(condition = ifelse(count_parish_holdings > 4, 1, 0)) %>%
  # Use across to apply the conditional logic to each variable
  mutate(across(
    all_of(variables),
    ~ ifelse(
      condition == 1,
      (get(paste0("Sum_", cur_column())) / Sum_Opening_stock) * Start_stock,
      (get(paste0("Sum_Sum_", cur_column())) / Sum_Sum_Opening_stock) * Start_stock
    ),
    .names = "{.col}"
  )) %>%
  ungroup()

DRT2 <- DRT2 %>%
  # Calculate Total_disposed by summing across the specified variables
  rowwise() %>%
  mutate(Total_disposed = sum(c_across(all_of(variables)), na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate Closing_Stock
  mutate(Closing_Stock = Start_stock - Total_disposed)


oct_closing_stock <- DRT2 %>%
  filter(Month == "Oct") %>%
  select(Crop, Region, Closing_Stock) %>%
  rename(Closing_Stock_Oct = Closing_Stock)

# Update Opening_stock for Month = Jun with Closing_Stock from October
DRT2 <- DRT2 %>%
  left_join(oct_closing_stock, by = c("Crop", "Region")) %>%
  mutate(Start_stock = ifelse(Month == "Jun", Closing_Stock_Oct, Start_stock)) %>%
  select(-Closing_Stock_Oct) # Drop the temporary column


# Perform the conditional computation for each variable
DRT2 <- DRT2 %>%
  group_by(Month, Region, Crop) %>%
  # Create the 'condition' variable first
  mutate(condition = ifelse(count_parish_holdings > 4, 1, 0)) %>%
  # Use across to apply the conditional logic to each variable
  mutate(across(
    all_of(variables),
    ~ ifelse(
      condition == 1,
      (get(paste0("Sum_", cur_column())) / Sum_Opening_stock) * Start_stock,
      (get(paste0("Sum_Sum_", cur_column())) / Sum_Sum_Opening_stock) * Start_stock
    ),
    .names = "{.col}"
  )) %>%
  ungroup()


# Create the subset with only the relevant columns and filter for Months Jun and Oct
subset_DRT2 <- DRT2 %>%
  select(Crop, Month, Region, Start_stock, all_of(variables)) %>%  # Select relevant columns
  filter(Month %in% c("Jun", "Oct")) %>%  # Filter for Months Jun and Oct
  # Calculate Total_disposed by summing across the specified variables
  rowwise() %>% 
  mutate(Total_disposed = sum(c_across(all_of(variables)), na.rm = TRUE)) %>%
  ungroup() %>%  # Ungroup after rowwise calculations
  # Calculate Closing_Stock
  mutate(Closing_Stock = Start_stock - Total_disposed)

# Define the types of interest
specific_type <- "Specific"
overall_type <- "Overall"

# Add the 'type' variable to subset_DRT
subset_DRT2 <- subset_DRT2 %>%
  # Create a new column 'type' with values "Specific" and "Overall"
  mutate(type = case_when(
    Month %in% c("Jun", "Oct") ~ specific_type,
    TRUE ~ overall_type
  )) %>%
  # Arrange the columns so that 'type' comes after 'Month'
  select(Crop, Month, type, everything())

# Calculate the overall totals for each crop and Month, including Start_stock
overall_totals2 <- subset_DRT2 %>%
  # Filter rows with specific types only
  filter(type == "Specific") %>%
  # Group by Crop and Month, and summarize the totals
  group_by(Crop, Month) %>%
  summarize(across(all_of(variables), sum, na.rm = TRUE), 
            Total_disposed = sum(Total_disposed, na.rm = TRUE),
            Closing_Stock = sum(Closing_Stock, na.rm = TRUE),
            Start_stock = sum(Start_stock, na.rm = TRUE), # Include Start_stock
            .groups = 'drop') %>%
  # Add the 'type' column with value "Overall" and set Region to "Overall"
  mutate(type = "Overall", Region = "Overall") %>%
  # Arrange columns to match the structure of subset_DRT
  select(Crop, Month, Region, type, everything())

# Add the overall totals to the original subset_DRT
subset_DRT_with_overall2 <- subset_DRT2 %>%
  bind_rows(overall_totals2)


# Calculate the overall totals for each crop across both Months
overall_Month_totals2 <- subset_DRT_with_overall2 %>%
  # Filter for rows with type = Overall to ensure we are summing the correct rows
  filter(type == "Overall") %>%
  # Group by Crop to calculate totals across Months
  group_by(Crop) %>%
  summarize(across(all_of(variables), sum, na.rm = TRUE),
            Total_disposed = sum(Total_disposed, na.rm = TRUE),
            Closing_Stock = sum(Closing_Stock, na.rm = TRUE),
            Start_stock = sum(Start_stock, na.rm = TRUE),
            type = "Overall",
            Month = "Overall",
            Region = "Overall",
            .groups = 'drop')

# Combine this summary with the existing data frame
Disposals_results_w_estimates <- subset_DRT_with_overall2 %>%
  bind_rows(overall_Month_totals2)%>%
  select(-type)

# View the updated data frame
#View(Disposals_results_w_estimates)

#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
str5 <- " - Data - Raw Data - June Disposals final results with estimates - "
str6 <- ".csv"
outputname <- paste(
  str4,
  format(Sys.Date(), format="%Y"),
  str5,
  format(Sys.time(), format="%d %B"),
  str6,
  sep = "")

write.csv(Disposals_results_w_estimates, outputname, row.names = FALSE)


# Assuming final_subset_DRT is your final dataset with rows for 'Overall'
# Create the scottish_average summary table
scottish_average2 <- Disposals_results_w_estimates %>%
  # Filter for rows where Month and Region are 'Overall'
  filter(Month == "Overall", Region == "Overall") %>%
  # Group by Crop to calculate the ratios
  group_by(Crop) %>%
  # Calculate the average for each variable divided by Total_disposed
  summarize(across(all_of(variables), ~ mean(.x / Total_disposed, na.rm = TRUE) * 100),
            .groups = 'drop') %>%
  # Format the variables as percentages with 0 decimal places
  mutate(across(all_of(variables), ~ sprintf("%.0f", .)))

# View the summary table
#View(scottish_average2)


#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
str5 <- " - Data - Raw Data - June Disposals final results Scottish averages - "
str6 <- ".csv"
outputname <- paste(
  str4,
  format(Sys.Date(), format="%Y"),
  str5,
  format(Sys.time(), format="%d %B"),
  str6,
  sep = "")

write.csv(scottish_average2, outputname, row.names = FALSE)



#------------------------------------------------------------------------------#





