## Script for reading Input-datasets from IMF download
## GVersteeg, April 12th, 2019
##
## DESCRIPTION
## This temporary script reads a downloaded  csv file (tab delimited) from
## IMF, containing gdp, population, oil-imports, unemployment, inflation
## data per country (184). 
##
## IN/OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : WEO_Data.csv                      / IMF download          -- 
## -- LUT's :                                                           -- 
## -- Output: EconData.csv                      (920  / 8 )             -- 
## --       : IMF_countries.csv                 (184  / 7 )             -- 
## ----------------------------------------------------------------------- 
##
## ----------------------------------------------------------------------- 
## --- ORGANIZE PROCESSING ENVIRONMENT ----------------------------------- 
## ----------------------------------------------------------------------- 
## Loading libraries
## Note: require is used inside functions, as it outputs a warning and 
## continues if the package is not found, whereas library will throw an error
library(tidyverse)
library(stringr)


## Setup locations
insite <- ""
rawdir <- "datalake/raw_data/"
lutdir <- "datalake/luts/"
clndir <- "datalake/clean_data/"
valdir <- "datalake/valid_data/"
inddir <- "datalake/indicators/"
dptdir <- "datalake/data_points/"
laydir <- "datalake/infoproducts/GIS_layers/"
qlkdir <- "datalake/infoproducts/Qlik_cubes/"
sopdir <- "datalake/infoproducts/sopact_sheets/"

## Setup parameters
version <- ""
indate = "2019-04-11"
lutdate = ""
outdate = "2019-03-31"

## Setup filenames
fname_in_raw <- paste0(rawdir,"WEO_Data.csv")
fname_in_lutC <- paste0(lutdir,"CountriesExt", "_", indate,".xlsx")
fname_out_IMF <- paste0(lutdir,"IMF_countries", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- IMF ---- IMF General Economic Data --------------------------------
## --- GDP in billion of US-Dollars
## --- Inflation (avg.cust prices) as an index
## --- Value of oil imports in billion of US-Dollars
## --- Perc. unemployment in percentage of total labor force
## --- Population in millions
## ----------------------------------------------------------------------- 
## 1 - inlezen bestand, remove unusable rows and columns
raw <- read_tsv(fname_in_raw, locale = locale(encoding = 'ISO-8859-1'))
raw <- raw[-(nrow(raw)),]
raw <- select(raw, c(3,4,7))

## 2 - spread subjects that are now in column 'Subject Descriptor"
raw <- spread(raw, key = `Subject Descriptor`, value = `2016`)
colnames(raw)[2] <- "GDP"
colnames(raw)[3] <- "Inflation"
colnames(raw)[5] <- "Unemployment"
colnames(raw)[6] <- "OilImport"

## 3 - erase all thousand-separators (","), leave decimal points
raw$GDP <- as.numeric(gsub(",", "", raw$GDP))
raw$Inflation <- as.numeric(gsub(",", "", raw$Inflation))
raw$Population <- as.numeric(gsub(",", "", raw$Population))
raw$Unemployment <- as.numeric(gsub(",", "", raw$Unemployment))
raw$OilImport <- as.numeric(gsub(",", "", raw$OilImport))

## 3a - remove outliers for inflation
for (i in seq_along(raw$Country)) {
  raw$Inflation[i] <- ifelse(raw$Inflation[i] > 10000, NA, raw$Inflation[i])
}

## 4 - map countries to Countries_Mapped_Model using lutC
lutC <- read_xlsx(fname_in_lutC, range = "Sheet1!A1:B221")
ix <- raw$Country %in% lutC$Countries
raw <- mutate(raw, GHG_Country = NA)
for (i in seq_along(raw$Country)) {
  if(ix[i]) {
    raw$GHG_Country[i] <- lutC$GHG_Countries[
      lutC$Countries == raw$Country[i]]
  }
}
raw <- raw %>%
  select(-Country) %>%
  group_by(GHG_Country) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE),
            Inflation = mean(Inflation, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE), 
            Unemployment = mean(Unemployment, na.rm = TRUE),
            OilImport = sum(OilImport, na.rm = TRUE)) %>%
  ungroup
  
## 5 - wegschrijven CSV-file in datalake/clean_data
IMF <- raw
write.csv2(IMF, fname_out_IMF, row.names = FALSE,
           fileEncoding = "UTF-8")
rm(raw)

