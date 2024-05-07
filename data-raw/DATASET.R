## code to prepare `DATASET` dataset goes here
library(readr)
library(dplyr)
sushi <- read_csv("./data-raw/sushi3a.csv")[,-c(1,2)]
gdpdata <- read_csv("./data-raw/gdpData.csv")
bggtoptens <- read_csv("./data-raw/toptens.csv")
bggsoloranks <- read_csv("./data-raw/UserRankings.csv")

gdpdata <- gdpdata %>%
  pivot_longer(c("2000", "2005", "2010", "2015", "2020", "2022"), names_to = "year", values_to = "gdp") %>%
  rename(code = "Country Code", "country" = "Country Name") %>%
  filter(code %in% c("USA", "CHN", "JPN", "DEU", "IND", "GBR", "FRA", "RUS", "CAN", "ITA")) %>%
  mutate(code = case_when(code == "USA" ~ "us",
                          code == "CHN" ~ "cn",
                          code == "JPN" ~ "jp",
                          code == "DEU" ~ "de",
                          code == "IND" ~ "in",
                          code == "GBR" ~ "gb",
                          code == "FRA" ~ "fr",
                          code == "RUS" ~ "ru",
                          code == "CAN" ~ "ca",
                          code == "ITA" ~ "it"))


usethis::use_data(gdpdata, overwrite = TRUE)
usethis::use_data(sushi, overwrite = TRUE)
usethis::use_data(bggtoptens, overwrite = TRUE)
usethis::use_data(bggsoloranks, overwrite = TRUE)
