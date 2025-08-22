## Disposal patterns tab ###
## Position in order: 5 / 11

#Packages can be found and run on tab 1/11


## This tab creates the proportional results for June to be used as an estimate for any partial returns ## 

Disposals_patterns <- final_subset_DRT %>%
  filter(Month == "Jun") %>% 
  group_by(Crop, Region) %>%
  summarise(
    across(
      all_of(column_names_pattern),
      ~ sum(.x, na.rm = TRUE),
      .names = "{.col}_sum"
    ),
    Total_disposed = sum(Total_disposed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      ends_with("_sum") & !starts_with("Total_disposed"),
      ~ round((.x / Total_disposed) * 100, 1),
      .names = "{.col}_pct"
    ),
    total_disposals_sum_pct = round((Total_disposed / Start_stock_sum) * 100, 1)  # new column
  ) %>%
  select(Crop, Region, ends_with("_pct"), -c(Start_stock_sum_pct, Closing_Stock_sum_pct))

#### using list of partial returns calculated previously make jun disposal estimates

# Columns to update
partial_returns_updated <- partial_returns %>%
  left_join(
    Disposals_QA
  ) %>% 
  left_join(Disposals_patterns, by = c("Crop", "Region"))

# Step 1: Set Opening_stock_Jun = Closing_stock_Oct
partial_returns_updated <- partial_returns_updated %>%
  mutate(
    Opening_stock_Jun = Closing_stock_Oct,
    # Step 2: Total_Disposals_Jun = Opening_stock_Jun × total_disposals_sum_pct
    Total_Disposals_Jun = Opening_stock_Jun * (total_disposals_sum_pct / 100)
  )

# Step 3: Fill _Jun columns by multiplying Total_Disposals_Jun × _sum_pct
for (col in variables) {
  pct_col <- paste0(col, "_sum_pct")
  jun_col <- paste0(col, "_Jun")
  
  partial_returns_updated[[jun_col]] <- partial_returns_updated$Total_Disposals_Jun * (partial_returns_updated[[pct_col]] / 100)
}

# Step 4: Compute Closing_stock_Jun
partial_returns_updated <- partial_returns_updated %>%
  mutate(Closing_stock_Jun = Opening_stock_Jun - Total_Disposals_Jun)

# Step 5: Clean up for final dataset
partial_returns_with_estimates <- partial_returns_updated %>%
  select(
    -matches("_sum_pct$"),
    -matches("_Oct$")
  ) %>%
  mutate(Month = "Jun",
         estimates_flag = 1) %>%
  relocate(Month, .before = Region) %>% 
  rename_with(
    .fn = ~ gsub("_Jun$", "", .),
    .cols = ends_with("_Jun")
  ) %>%
  relocate(Total_Disposals, .before = Closing_stock)

#View(partial_returns_with_estimates)



# #export csv
# # filename appropriate for data upload to erdm
# str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - July - "
# str5 <- " - Data - Raw Data - June Disposals estimates for partial returns - "
# str6 <- ".csv"
# outputname <- paste(
#   str4,
#   format(Sys.Date(), format="%Y"),
#   str5,
#   format(Sys.time(), format="%d %B"),
#   str6,
#   sep = "")
# 
# write.csv(partial_returns_with_estimates, outputname, row.names = FALSE)
# #outputname



