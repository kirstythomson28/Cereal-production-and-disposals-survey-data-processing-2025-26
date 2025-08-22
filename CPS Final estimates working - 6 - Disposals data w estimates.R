## Disposals data ##
## Position in order: 6 / 11


# This page is loading the disposals data set with the estimates for partial returns appended
# This will need developed in the future as this year the estimates were created in excel 

#Packages can be found and run on tab 1/11

# Read in raw data from October and June surveys 
disposals.w.est <- bind_rows(disposals, partial_returns_with_estimates)
view(disposals.w.est)

#export xlsx
# filename appropriate for data upload to erdm
str4 <- "Cereal Production and Disposal Survey - 2024-25 - Disposals - Nov and Jul - "
str5 <- " - Data - Raw Data - Disposals with estimates - "
str6 <- ".csv"
outputname <- paste(
  str4,
  format(Sys.Date(), format="%Y"),
  str5,
  format(Sys.time(), format="%d %B"),
  str6,
  sep = "")

write.csv(disposals.w.est, outputname, row.names = FALSE)
