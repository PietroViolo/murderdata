#---------------------------------------------------------------------------#
# Nom : murderdata.R                                        			          #
# Description : Data viz on murderdata                                      #
# Auteur : Pietro Violo                                                     #
# Date : April 24  2023                                                    #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)
rm(list = ls())

#---------------------------------------------------------------------------#
# Library and data                                                          #
#---------------------------------------------------------------------------#
library(tidyverse)
library(ggthemes)
library(urbnmapr)
library(viridis)
library(ggrepel)

df <- read.csv("./Data/UCR65_21.csv") %>% 
  group_by(State, County) %>% 
  summarise(MRD = sum(MRD),
            CLR = sum(CLR)) %>% 
  mutate(clearance_rate = CLR/MRD * 100,
         clearance_rate = ifelse(MRD < 10, NA, clearance_rate),
         County = str_remove(County, regex(",...")),
         County = ifelse(County == "De Kalb", "DeKalb", County),
         County = ifelse(State == "Louisiana", paste(County, "Parish"), County)) %>% 
  filter(State != "Alaska") %>% 
  mutate(clearance_rate = ifelse(clearance_rate <= 100, clearance_rate, NA),
         clearance_rate = round(clearance_rate,0),
         clearance_rate = case_when(clearance_rate %in% 0:19 ~ "0 - 19%",
                                    clearance_rate %in% 20:29 ~ "20 - 29%",
                                    clearance_rate %in% 30:39 ~ "30 - 39%",
                                    clearance_rate %in% 40:49 ~ "40 - 49%",
                                    clearance_rate %in% 50:59 ~ "50 - 59%",
                                    clearance_rate %in% 60:69 ~ "60 - 69%",
                                    clearance_rate %in% 70:79 ~ "70 - 79%",
                                    clearance_rate %in% 80:89 ~ "80 - 89%",
                                    clearance_rate %in% 90:110 ~ "90% +",
                                    ))
clearance_factors <- c("0 - 19%",
                       "20 - 29%",
                       "30 - 39%",
                       "40 - 49%",
                       "50 - 59%",
                       "60 - 69%",
                       "70 - 79%",
                       "80 - 89%",
                       "90% +")

df <- df %>% mutate(clearance_rate = factor(clearance_rate, levels = clearance_factors))

df_years <- read.csv("./Data/UCR65_21.csv") %>% 
  group_by(YEAR, State) %>% 
  summarise(MRD = sum(MRD),
            CLR = sum(CLR)) %>%
  mutate(Year = case_when(YEAR %in% 1965:1969 ~ 1967,
                          YEAR %in% 1970:1974 ~ 1972,
                          YEAR %in% 1975:1979 ~ 1977,
                          YEAR %in% 1980:1984 ~ 1982,
                          YEAR %in% 1984:1989 ~ 1987,
                          YEAR %in% 1990:1994 ~ 1992,
                          YEAR %in% 1995:1999 ~ 1997,
                          YEAR %in% 2000:2004 ~ 2002,
                          YEAR %in% 2005:2009 ~ 2007,
                          YEAR %in% 2010:2014 ~ 2012,
                          YEAR %in% 2015:2019 ~ 2017,
                          YEAR %in% 2020:2021 ~ 2021)) %>% 
  group_by(Year, State) %>% 
  summarise(MRD = sum(MRD),
            CLR = sum(CLR)) %>% 
  mutate(clearance_rate = CLR/MRD * 100) %>% 
  filter(clearance_rate <= 100)

df_usa <- read.csv("./Data/UCR65_21.csv") %>% 
  group_by(YEAR) %>% 
  summarise(MRD = sum(MRD),
            CLR = sum(CLR)) %>% 
  mutate(UnitedStates = CLR/MRD * 100)

#---------------------------------------------------------------------------#
# Data wrangling                                                            #
#---------------------------------------------------------------------------#

# Fip look up function by
# https://github.com/cran/usmap/blob/master/R/fips.R
fips <- function(state, county = c()) {
  if (missing(state) & missing(county)) {
    df <- utils::read.csv(system.file("extdata", "state_fips.csv", package = "usmap"))
    return(sprintf("%02d", df$fips))
  }
  
  state_ <- tolower(state)
  county_ <- tolower(county)
  
  if (length(county_) == 0) {
    df <- utils::read.csv(system.file("extdata", "state_fips.csv", package = "usmap"))
    abbr <- tolower(df$abbr)
    full <- tolower(df$full)
    fips2 <- c(df$fips, df$fips)
    
    result <- fips2[match(state_, c(abbr, full))]
    
    formatted_result <- sprintf("%02d", result)
    formatted_result[formatted_result == "NA"] <- NA
    formatted_result
  } else {
    if (length(state_) > 1) {
      stop("`county` parameter cannot be used with multiple states.")
    }
    
    df <- utils::read.csv(system.file("extdata", "county_fips.csv", package = "usmap"))
    name <- tolower(df$county)
    state_abbr <- tolower(df$abbr)
    state_full <- tolower(df$full)
    
    result <- c()
    
    for (county_i in county_) {
      result <- c(
        result,
        df$fips[which(
          (name %in% county_i | name %in% paste(county_i, "county"))
          &
            (state_abbr %in% state_ | state_full %in% state_)
        )]
      )
    }
    
    if (length(result) == 0) {
      if (length(county) == 1) {
        stop(paste0(county, " is not a valid county in ", state, ".\n"))
      } else {
        stop(paste0(county, " are not valid counties in ", state, ".\n"))
      }
    } else {
      sprintf("%05d", result)
    }
  }
}

n = 1
fips_code <- c()

for(i in df$State){
  fips_code <- c(fips_code, tryCatch(fips(i,df[n,"County"]),
                                      error = function(e){
                                        NA
                                      }))
  n = n+1
}

df$county_fips <- fips_code

rm(fips_code, i, n, fips)

# Left join
counties_plot <- left_join(counties, df) %>% 
  filter(!(state_abbv %in% c("AK","HI"))) # Remove Hawaii and Alaska

#---------------------------------------------------------------------------#
# Visualization                                                             #
#---------------------------------------------------------------------------#

data_ends <- df_years %>% filter(Year == 2021)

x <- df_years %>% ggplot(aes(x = Year, y = clearance_rate, group = State)) +
  geom_line(alpha = 0.3, size = 1, color = "white") +
  geom_line(data = df_usa, aes(x = YEAR, y = UnitedStates, color = "#f77f00"), inherit.aes = F, size = 1.5) +
  theme_dark()+
  theme(legend.position="bottom",
        panel.grid.major.y = element_line(color = "white"),
        plot.background = element_blank(),
        panel.background = element_blank(), 
        legend.background = element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank()) 

table <- data_ends %>% arrange(desc(clearance_rate)) %>% ungroup() %>% 
  select(State, MRD, CLR, clearance_rate) %>% 
  mutate(CLR = as.integer(CLR)) %>% 
  filter(!(State %in% c("Alaska", "Hawaii")))


colnames(table) <- c("State", "Homicides", "Solved", "(%)")

table <- table %>% mutate(Stateabb = state.abb[match(State,state.name)]) %>% 
  select(State,Homicides, `(%)`)

t <- xtable::xtable(table)
print(t, include.rownames = F)


# MAP
my_palette <- rev(magma(9))

pp <- ggplot(data = counties_plot,
             aes(x = long, y = lat, group = group, fill = clearance_rate)) +
  geom_polygon() +
  ggtitle("How likely would a murder get solved?",
          subtitle = "Clearance rate by county")+ 
  coord_sf(crs = 4326) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(), 
        legend.background = element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size= 18, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 10, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.text = element_text(colour = "white"))  +
  borders("state", colour = "white") +
  scale_size_area() +
  coord_quickmap() +
  scale_fill_manual( 
    values = my_palette, 
    name="Clearance rate", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
    na.value = "grey35")


ggsave("graph.png", plot = pp, bg = "transparent", 
       device = "png", 
       height = 3182, 
       width = 3774, 
       units = "px",
       dpi = 300)


ggsave("spaghetti_plot.png", plot = x, bg = "transparent", 
       device = "png", 
       height = 1800, 
       width = 3774, 
       units = "px",
       dpi = 300)
