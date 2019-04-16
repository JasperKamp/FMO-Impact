## Script for plotting GHG-emissions
## GVersteeg, April 10th, 2019
##
## IN/OUTPUT
## ----------------------------------------------------------------------- 
## -- Input : KPI_aGHG.csv                    (247 / 7 )                -- 
## ----------------------------------------------------------------------- 
##
## ----------------------------------------------------------------------- 
## --- ORGANIZE PROCESSING ENVIRONMENT ----------------------------------- 
## ----------------------------------------------------------------------- 
## Loading libraries
## Note: require is used inside functions, as it outputs a warning and 
## continues if the package is not found, whereas library will throw an error
library(tidyverse)

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
indate = "2019-03-31"
lutdate = ""
outdate = "2019-04-10"

## Setup filenames
fname_in_AE <- paste0(inddir,"KPI_aGHG", "_", indate,".csv")
fname_in_PWA <- paste0(clndir,"Pathway_Allow", "_", indate,".csv")
fname_out_custPF <- paste0(clndir,"CustPortfolio", "_", outdate,".csv")

## ----------------------------------------------------------------------- 
## --- AE/AEyr ------ Creating and Plotting GHG Emission data ------------
## ----------------------------------------------------------------------- 
## 1 - read file from xlsx-tab, remove last line (totals)
AE <- read.csv2(fname_in_AE,
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
AE <- AE[!is.na(AE$Ems),]
hist(AE$Ems)

## 2 - read pathways and allowances over the years 2018 to 2025
PWA <- read.csv2(fname_in_PWA,
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8")
colnames(PWA) <- gsub("X", "", colnames(PWA))
pw15 <- PWA[5,c(4:11)]
pw15 <- gather(pw15, `2018`, `2019`,`2020`,`2021`,`2022`,
               `2023`,`2024`,`2025`,
               key = Year, value = ems)
pw15$Year <- as.numeric(pw15$Year)
pw20 <- PWA[6,c(4:11)]
pw20 <- gather(pw20, `2018`, `2019`,`2020`,`2021`,`2022`,
               `2023`,`2024`,`2025`,
               key = Year, value = ems)
pw20$Year <- as.numeric(pw20$Year)
alw15 <- PWA[3,c(4:11)]
alw15 <- gather(alw15, `2018`, `2019`,`2020`,`2021`,`2022`,
                `2023`,`2024`,`2025`,
                key = Year, value = ems)
alw15$Year <- as.numeric(alw15$Year)
alw20 <- PWA[4,c(4:11)]
alw20 <- gather(alw20, `2018`, `2019`,`2020`,`2021`,`2022`,
                `2023`,`2024`,`2025`,
                key = Year, value = ems)
alw20$Year <- as.numeric(alw20$Year)

## 3 - shift means to 3.38 per 2018 by using a factor
##     to simulate a sum with a confidence interval
# oldmean <- mean(AE$Ems, na.rm = TRUE)
# newtot <- 3.37862339163655
# shift <- newtot/oldmean
# AEs <- mutate(AE, Ems = Ems * shift)

## 4 - generate emissions previous years, considering (1.5C pathway): 
# perc <- 1.5
# peru <- perc/100
# sdev <- abs(peru)
# AEyr <- AEs %>%
#   select(Ems) %>%
#   rename(Yr_2018 = Ems) %>%
#   mutate(Yr_2017 = (Yr_2018 * (1 + rnorm(1, mean=peru, sd=sdev)))) %>%
#   mutate(Yr_2016 = (Yr_2017 * (1 + rnorm(1, mean=peru, sd=sdev)))) %>%
#   mutate(Yr_2015 = (Yr_2016 * (1 + rnorm(1, mean=peru, sd=sdev))))
# check_4 <- round(100-(sum(AEyr$Yr_2015)/sum(AEyr$Yr_2018)*100), 2)
# check_4

## 5 - gathering the resulting emissions
AEg <- AEyr
colnames(AEg) <- gsub("Yr_", "", colnames(AEg))
AEg <- gather(AEg, `2018`, `2017`,`2016`,`2015`,
              key = "Year", value = "ems")
AEg$Year <- as.numeric(AEg$Year)


## 6 - plotting the emissions and adding the allowances
g <- ggplot(data=AEg, aes(x=Year, y=ems)) +
  ggtitle("Projected GHG emissions on FMO pathway") +
  xlab("Year") + ylab("Absolute emissions (BtCO2e/yr)") +
  theme(
    plot.title = element_text(color="darkgray", size=14, face="bold", hjust = 0.5),
    axis.title.x = element_text(color="#993333", size=11),
    axis.title.y = element_text(color="#993333", size=11)) +
  scale_x_continuous(breaks = seq(2015,2025,1),
                     lim = c(2015,2025)) +
  coord_cartesian(ylim = c(2.5,4)) +
  geom_point(size=0.2 , color="Blue") +
  stat_smooth(method = "lm", formula = y ~ x, color="Blue",
              fullrange = TRUE, level = 0.03) +
  geom_line(data=pw15, aes(x=Year, y=ems), color="Yellow") +
  geom_line(data=pw20, aes(x=Year, y=ems), color="Brown")
g

# mdl <- lm(AEg, formula = ems ~ as.vector(Year))
# summary(mdl)
# newyr <- data.frame(Year = as.numeric(c(2019, 2020, 2021, 2022,
#                                         2023, 2024, 2025)))
# z <- predict(mdl, as.vector(newyr), interval = "confidence")


## 4 - write source data
## write.csv2(GenInd, fname_out_GenInd, row.names = FALSE,
##            fileEncoding = "UTF-8")
