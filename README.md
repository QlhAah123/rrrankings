

# rrrankings

<!-- badges: start -->
<!-- badges: end -->

`rrrankings` and the associated thesis introduce two types of visualizations: a modified bump plot that preserves and clearly presents information about how far apart each rank is over time, and a plot for displaying the confidence intervals of paired comparisons between each object in a ranking.

## Installation

The development version of `rrrankings` is available from [GitHub](https://github.com/) with:

```{r}
#install.packages("devtools")
#devtools::install_github("QlhAah123/rrrankings")
#library(rrrankings)
```
## Example


```{r examples, message= FALSE}
#Import dataset
data("gdpdata")
gdpdata <- gdpdata %>%
  filter(!(year %in% c(2000, 2022))) %>%
  mutate(year = as.numeric(year)) %>%
  select(!c(country)) %>%
  rename(x = year, y = gdp, group = code)

#Simple example - background_bump()
background_bump(gdpdata, n.breaks = 6) +
  labs(x="Year", y="GDP Rank")

#Example of customization - background_bump()
#Subset of data for flags
flagdat <- gdpdata %>%
  group_by(x) %>%
  filter(x==2005|x==2020) %>%
  mutate(rank = rank(y, ties.method = "random")) %>%
  ungroup()

bcc <- scales::div_gradient_pal(low = "#f7faff", mid = "#6baed6", high = "#072f6b", "Lab")(seq(0,1,length.out=6))
background_bump(gdpdata, breaks = rev(c(2.11*10^13,1.77*10^13,1.43*10^13,1.09*10^13,7.54*10^12,
                              7.54*10^12, 4.15*10^12, 0)), repel.yn = FALSE) +
  geom_point(data = flagdat, mapping = aes(x=x, y=rank), size = 11,  fill = "black", stroke=NA)+
  geom_flag(data = flagdat, mapping = aes(x=x, y=rank, country=group), size = 8) +
  labs(x="Year", y="GDP Rank") +
  scale_fill_manual(values = bcc, name = "GDP <=") + 
  coord_cartesian(xlim = c(2004.6,2020.4), ylim = c(1,10))

#Import dataset
data("bggsoloranks")
pivoted <- bggsoloranks %>%
  pivot_wider(names_from = Game, values_from = Rank) %>%
  select(c("User", "Spirit Island","Mage Knight Board Game", "Marvel Champions: The Card Game",
           "Arkham Horror: The Card Game", "Lord of the Rings: The Card Game", "Too Many Bones",
           "Dune: Imperium", "Ark Nova", "Terraforming Mars", "Imperium: Classics")) 
pivoted <- pivoted[rowSums(is.na(pivoted)) != ncol(pivoted)-1, ] %>%
  group_by(User) %>%
  filter(n() >= 5) %>% #only users with at least 3 of the selected games ranked are included. 567 w/ 3 of top 10, 199 w/ 5
  summarise(across(,~sum(., na.rm = TRUE)))

#Example - BradTerrTiles()
BradTerrTiles(pivoted, conf.int = 0.95, reps = 100)

```