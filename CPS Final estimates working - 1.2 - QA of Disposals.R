## QA ##
### Needs further development ###
# Possibly have 2 in the future, 1 with 3 main flags for email QA and a separate that QA's the proportions#

Disposals_QA <- disposals %>%
  mutate(Total_Disposals = rowSums(across(variables))) %>% 
  pivot_wider(
    id_cols = all_of(id_cols),
    names_from = Month,
    values_from = all_of(column_names),
    names_sep = "_"
  )

Disposals_QA_reordered <- Disposals_QA %>%
  select(
    CPH, parish, holding, Region, Crop, Crop_general,
    ends_with("_oct"),
    ends_with("_jun"),
    everything()
  )%>%
  relocate(Total_Disposals_Oct, .before = Closing_stock_Oct) %>%
  relocate(Total_Disposals_Jun, .before = Closing_stock_Jun)

Disposals_QA_Flagged <- Disposals_QA_reordered %>%
  mutate(
    stock_mismatch_flag = Closing_stock_Oct != Opening_stock_Jun, #Flags if Opening stock in June is different to the October closing stock
    stock_difference = if_else(
      stock_mismatch_flag,
      Closing_stock_Oct - Opening_stock_Jun, #Returns the value of the difference
      0
    ),
    Stock_diff_over_50 = if_else(
      (Closing_stock_Oct - Opening_stock_Jun) > 50,
      1,
      0
    ),
    Closing_left_opening_zero = if_else(
      Closing_stock_Oct > 50 & Opening_stock_Jun == 0,
      1,
      0
    ),
    June_open_greaterthan_oct_closing = if_else(
      Opening_stock_Jun > Closing_stock_Oct,
      1,
      0
    ),
    stock_difference_pct = if_else(
      stock_mismatch_flag & !is.na(Opening_stock_Oct) & Opening_stock_Oct != 0, 
      (stock_difference / Opening_stock_Oct) * 100,#Returns the value of the difference as a percentage of overall production for that item
      0
    ),
    stock_difference_pct_fmt = paste0(round(stock_difference_pct, 0), "%"), # formats to a percentage with 0 decimal places.
    
    #Flag those where the opening stocks are the same but there has been disposals in period 1
    Opening_Jun_eq_Oct_and_greater_than_Closing_Oct = if_else(
      Opening_stock_Oct == Opening_stock_Jun & Opening_stock_Jun > Closing_stock_Oct,
      1,
      0
    ),
    
    
    # Difference between June opening and (June closing + disposals)
    June_Stock_Discrepancy_Flag = ifelse(
      Opening_stock_Jun != (Closing_stock_Jun + Total_Disposals_Jun), 1, 0
    ),
    Difference_June_Op_Clo_Stock = ifelse(
      June_Stock_Discrepancy_Flag == 1, 
      Opening_stock_Jun - (Closing_stock_Jun + Total_Disposals_Jun), 0
    ),
    
    # Flags for unusual usage by crop type
    Wheat_Milling_flag_Oct = if_else(
      Crop == "Wheat" & Merchants_for_Milling_Oct > 0.3 * Opening_stock_Oct,
      "Milling flag",
      NA_character_
    ),
    Wheat_Milling_flag_Jun = if_else(
      Crop == "Wheat" & Merchants_for_Milling_Jun > 0.3 * Opening_stock_Jun,
      "Milling flag",
      NA_character_
    ),
    Barley_Milling_flag_Oct = if_else(
      Crop == "Barley" & Merchants_for_Milling_Oct > 0.10 * Opening_stock_Oct,
      "Milling flag",
      NA_character_
    ),
    Barley_Milling_flag_Jun = if_else(
      Crop == "Barley" & Merchants_for_Milling_Jun > 0.10 * Opening_stock_Jun,
      "Milling flag",
      NA_character_
    ),
    Oats_Malting_flag_Oct = if_else(
      Crop == "Oats" & Merchants_for_Malting_Oct > 0.2 * Opening_stock_Oct,
      "Merchants Seed flag",
      NA_character_
    ),
    Oats_Malting_flag_Jun = if_else(
      Crop == "Oats" & Merchants_for_Malting_Jun > 0.2 * Opening_stock_Jun,
      "Merchants Seed flag",
      NA_character_
    )
  )

# Create data frame of those just with issues 
Disposals_QA_with_issues <- Disposals_QA_Flagged %>%
  filter(
    stock_mismatch_flag |
      June_Stock_Discrepancy_Flag == 1 |
      Stock_diff_over_50 == 1|
      Closing_left_opening_zero == 1|
      June_open_greaterthan_oct_closing == 1|
      !is.na(Wheat_Milling_flag_Oct) |
      !is.na(Wheat_Milling_flag_Jun) |
      !is.na(Barley_Milling_flag_Oct) |
      !is.na(Barley_Milling_flag_Jun) |
      !is.na(Oats_Malting_flag_Oct) |
      !is.na(Oats_Malting_flag_Jun)
  )%>%
  select(-stock_difference_pct)

# To view it in RStudio (optional)
View(Disposals_QA_with_issues)



#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
str5 <- " - Data - Raw Data - QA - June Disposals requiring QA - "
str6 <- ".csv"
outputname <- paste(
  str4, 
  format(Sys.Date(), format="%Y"), 
  str5, 
  format(Sys.time(), format="%d %B"), 
  str6, 
  sep = "") 
  
write.csv(Disposals_QA_with_issues, outputname, row.names = FALSE)



## Data set to be used in mail merge for stock mismatch.
data <- Disposals_QA_with_issues %>%
  filter(Stock_diff_over_50 == 1 | Closing_left_opening_zero == 1 | June_open_greaterthan_oct_closing == 1) %>%
  select(-c("stock_mismatch_flag","stock_difference","stock_difference_pct_fmt","June_Stock_Discrepancy_Flag","Difference_June_Op_Clo_Stock", 
            "Wheat_Milling_flag_Oct",
            "Wheat_Milling_flag_Jun","Barley_Milling_flag_Oct","Barley_Milling_flag_Jun","Oats_Malting_flag_Oct","Oats_Malting_flag_Jun"))

sample_QA <- Sample %>%
  select(parish, holding, primary_email)


# Then join to data by CPH
Qa_emails <- data %>%
  left_join(sample_QA, by = c("parish","holding"))%>%
  relocate(primary_email, .before = Region)


#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
str5 <- " - Data - Raw Data - QA - Emails for stock mis match - "
str6 <- ".csv"
outputname <- paste(
  str4,
  format(Sys.Date(), format="%Y"),
  str5,
  format(Sys.time(), format="%d %B"),
  str6,
  sep = "")
write.csv(Qa_emails, outputname, row.names = FALSE)



# ------------------------------------------------------------------------------------------------------------------##
## STEP 3 of QA checking the proportion of disposals against averages from previous years ###
#  Disposals per crop type as a subset of Disposals_ALL
Disposals_Crop_June_QA <- Disposals_QA %>%
  select(
    Crop, Opening_stock_Oct, Opening_stock_Jun, 
    Merchants_for_Malting_Jun, Merchants_for_Feed_Jun, Merchants_for_Milling_Jun,
    Merchants_for_Seed_Jun, Merchants_for_Industrial_Jun, Merchants_for_Other_Jun, 
    Farmers_in_Scotland_Jun, Farmers_outwith_Scotland_Jun, 
    Used_for_Seed_Jun, Used_for_Feed_Jun, Waste_Other_Jun, 
    Total_Disposals_Jun, Closing_stock_Jun
  ) %>%
  group_by(Crop) %>%
  summarise(across(everything(), sum, na.rm = TRUE))


# Calculate percentages and retain only the percentage columns
Disposals_Crop_June_percentages <- Disposals_Crop_June_QA %>%
  # Calculate percentages
  mutate(across(
    c(Merchants_for_Malting_Jun, Merchants_for_Feed_Jun, Merchants_for_Milling_Jun,
      Merchants_for_Seed_Jun, Merchants_for_Industrial_Jun, Merchants_for_Other_Jun,
      Farmers_in_Scotland_Jun, Farmers_outwith_Scotland_Jun,
      Used_for_Seed_Jun, Used_for_Feed_Jun, Waste_Other_Jun),
    ~ round(. / Total_Disposals_Jun * 100, 1),
    .names = "{.col}_percent"
  )) %>%
  select(Crop, ends_with("_percent"))

# Print the result to check
#view(Disposals_Crop_June_percentages)


Disposals_Crop_June_QA_flags <- Disposals_QA %>%
  mutate(
    ## --- WHEAT FLAGS ---
    Wheat_Milling_flag = if_else(Crop == "Wheat" & Merchants_for_Milling_Jun > 0.2 * Opening_stock_Oct, "Milling flag", NA_character_),
    Wheat_Seed_flag    = if_else(Crop == "Wheat" & Merchants_for_Seed_Jun > 0.1 * Opening_stock_Oct, "Merchants Seed flag", NA_character_),
    Wheat_Industrial_flag = if_else(Crop == "Wheat" & Merchants_for_Industrial_Jun > 0.1 * Opening_stock_Oct, "Industrial flag", NA_character_),
    Wheat_Other_flag = if_else(Crop == "Wheat" & Merchants_for_Other_Jun > 0.1 * Opening_stock_Oct, "Other flag", NA_character_),
    Wheat_Scotland_flag = if_else(Crop == "Wheat" & Farmers_in_Scotland_Jun > 0.1 * Opening_stock_Oct, "Farmers Scotland flag", NA_character_),
    Wheat_OutScotland_flag = if_else(Crop == "Wheat" & Farmers_outwith_Scotland_Jun > 0.05 * Opening_stock_Oct, "Farmers Outwith flag", NA_character_),
    Wheat_Seed_Use_flag = if_else(Crop == "Wheat" & Used_for_Seed_Jun > 0.05 * Opening_stock_Oct, "Used Seed flag", NA_character_),
    Wheat_Feed_Use_flag = if_else(Crop == "Wheat" & Used_for_Feed_Jun > 0.2 * Opening_stock_Oct, "Used Feed flag", NA_character_),
    Wheat_Waste_flag = if_else(Crop == "Wheat" & Waste_Other_Jun > 0.1 * Opening_stock_Oct, "Waste flag", NA_character_),
    
    ## --- BARLEY FLAGS ---
    Barley_Milling_flag = if_else(Crop == "Barley" & Merchants_for_Milling_Jun > 0.1 * Opening_stock_Oct, "Milling flag", NA_character_),
    Barley_Seed_flag = if_else(Crop == "Barley" & Merchants_for_Seed_Jun > 0.1 * Opening_stock_Oct, "Seed flag", NA_character_),
    Barley_Industrial_flag = if_else(Crop == "Barley" & Merchants_for_Industrial_Jun > 0.1 * Opening_stock_Oct, "Industrial flag", NA_character_),
    Barley_Other_flag = if_else(Crop == "Barley" & Merchants_for_Other_Jun > 0.1 * Opening_stock_Oct, "Other flag", NA_character_),
    Barley_Scotland_flag = if_else(Crop == "Barley" & Farmers_in_Scotland_Jun > 0.1 * Opening_stock_Oct, "Farmers Scotland flag", NA_character_),
    Barley_OutScotland_flag = if_else(Crop == "Barley" & Farmers_outwith_Scotland_Jun > 0.05 * Opening_stock_Oct, "Farmers Outwith flag", NA_character_),
    Barley_Seed_Use_flag = if_else(Crop == "Barley" & Used_for_Seed_Jun > 0.05 * Opening_stock_Oct, "Used Seed flag", NA_character_),
    Barley_Waste_flag = if_else(Crop == "Barley" & Waste_Other_Jun > 0.05 * Opening_stock_Oct, "Waste flag", NA_character_),
    
    ## --- OATS FLAGS ---
    Oats_Seed_flag = if_else(Crop == "Oats" & Merchants_for_Seed_Jun > 0.1 * Opening_stock_Oct, "Seed flag", NA_character_),
    Oats_Industrial_flag = if_else(Crop == "Oats" & Merchants_for_Industrial_Jun > 0.1 * Opening_stock_Oct, "Industrial flag", NA_character_),
    Oats_Other_flag = if_else(Crop == "Oats" & Merchants_for_Other_Jun > 0.2 * Opening_stock_Oct, "Other flag", NA_character_),
    Oats_Scotland_flag = if_else(Crop == "Oats" & Farmers_in_Scotland_Jun > 0.1 * Opening_stock_Oct, "Farmers Scotland flag", NA_character_),
    Oats_OutScotland_flag = if_else(Crop == "Oats" & Farmers_outwith_Scotland_Jun > 0.1 * Opening_stock_Oct, "Farmers Outwith flag", NA_character_),
    Oats_Seed_Use_flag = if_else(Crop == "Oats" & Used_for_Seed_Jun > 0.1 * Opening_stock_Oct, "Used Seed flag", NA_character_),
    Oats_Feed_Use_flag = if_else(Crop == "Oats" & Used_for_Feed_Jun > 0.3 * Opening_stock_Oct, "Used Feed flag", NA_character_),
    Oats_Waste_flag = if_else(Crop == "Oats" & Waste_Other_Jun > 0.1 * Opening_stock_Oct, "Waste flag", NA_character_)
  )


#view(Disposals_Crop_June_QA_flags)

# Identify all columns that end with "_flag"
flag_cols <- grep("_flag$", names(Disposals_Crop_June_QA_flags), value = TRUE)

# Filter to keep only rows with at least one non-NA flag
Disposals_Crop_June_QA_flags <- Disposals_Crop_June_QA_flags %>%
  filter(if_any(all_of(flag_cols), ~ !is.na(.)))

## If flags have a look at individual data for each crop ##

#export csv
# filename appropriate for data upload to erdm
# Export CSV without row numbers
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
str5 <- " - Data - Raw Data - QA - June Disposals Crop proportions flagged data - "
str6 <- ".csv"

outputname <- paste(
  str4, 
  format(Sys.Date(), format="%Y"), 
  str5, 
  format(Sys.time(), format="%d %B"), 
  str6, 
  sep = ""
) 

write.csv(Disposals_Crop_June_QA_flags, outputname, row.names = FALSE)






