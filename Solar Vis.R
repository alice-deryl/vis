# BOM Solar Vises
# inspiration from: https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html

install.packages("tidyverse")
install.packages("ggridges")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("gridextra")
install.packages("grid")


library(tidyverse)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(tidyr)
library(dplyr)
library(lubridate)
library(viridis)
library(gridExtra)
library(grid)

solar <- read.csv(choose.files())

solar <- na.omit(solar)

#format a date column
solar$Month <- as.integer(solar$Month)
solar$Year <- as.character(solar$Year)
solar$Day <- as.character(solar$Day)
solar$Date <- paste(solar$Day, solar$Month, solar$Year, sep = "-")
solar$Date <- as.Date(solar$Date, format = "%d-%m-%Y")


# add Seasons for grouping
getSeason_SH <- function(DATES) {
  SS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Summer Solstice
  AE <- as.Date("2012-3-20",  format = "%Y-%m-%d") # Autumn Equinox
  WS <- as.Date("2012-6-20",  format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-9-22",  format = "%Y-%m-%d") # Spring Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= SS | d < AE, "Summer",
          ifelse (d >= AE & d < WS, "Autumn",
                  ifelse (d >= WS & d < SE, "Winter", "Spring")))
}

solar$Season <- getSeason_SH(solar$Date)

#format Month, Year as factors

solar$Month <- as.factor(solar$Month)
solar$Year <- as.factor(solar$Year)

#make a Week & Day_Num column for more granular data
solar$Week <- factor(paste(format(solar$Date, format = "%W")))
solar$Day_Num <- as.factor(format(solar$Date, "%j"))

#rename solar exposure variable
solar <- rename(solar, dailySolarExposure = Daily.global.solar.exposure..MJ.m.m.)


#create an Aggregate dataframe
solar_agg_week <- aggregate(dailySolarExposure ~ WeekNum + Season, data=solar_agg_week, FUN = "mean")


# ------ Circular BarChart ------------


# favourite one so far
df <- aggregate(dailySolarExposure ~ Day_Num, data = solar, mean)
df$Seasons <- factor(getSeason_SH(as.Date(df$Day_Num, format = "%j")), levels = c("Summer", "Autumn", "Winter", "Spring"))
df <- na.omit(df)

df <- NULL

r <- ggplot(df, aes(x = Day_Num, y = dailySolarExposure, fill = dailySolarExposure))
r+ geom_bar(stat = "identity")+
  scale_fill_viridis(aesthetics = "fill")+
  theme(axis.text = element_text(angle = 90)) +
  ylim(-15, 25) +
  theme_minimal() +
  labs(title = "Average Daily Solar Exposure Level in Sydney Botanic Gardens", subtitle = "The circle represents one full year | Units are 'Megajoules per square metre'", fill = "MJ/m*m")+
# remove all the labels and text
  theme(
    plot.title = element_text(family = "serif", vjust = -3),
    plot.subtitle = element_text(vjust = -3),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm"), # remove excessive margins
  )+
  coord_polar(start = 0)


# ------- Set empty bars to separate groups -----------
empty_bar <- 10 # there will be 10 empty bars

#create a matrix with same columns and the right number of empty bars for each season
to_add <- data.frame(matrix(NA, nrow = empty_bar*nlevels(df$Seasons), ncol = ncol(df))) 
colnames(to_add) <- colnames(df)
to_add$Seasons <- rep(levels(df$Seasons), each = empty_bar) # give 10 empty bars to each season

df <- rbind(df, to_add) # add the empties into the original df
df <- df %>% arrange(Seasons)

df$ID <- seq(1, nrow(df)) # create a column of unique IDs for each row incl. empties

#patch up to take the last days of the year to the beginning of 'summer'
df$ID[74:90] <- c(-16:0)




# ------------- Prepare a data frame for base lines --------------
base_data <- df %>%
  group_by(Seasons) %>%
  summarize(start = min(ID), end = max(ID) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end))) # sets the position along the x axis of the title (ie in the middle/'mean')
  


# -------- Prepare a data frame for base grid(scales) --------------

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1: nrow(grid_data) -1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]



# ----------- New vis with grid lines, scale and baseline ------------
g <- ggplot(df, aes(x = as.factor(ID), y = dailySolarExposure, fill = dailySolarExposure))
g + geom_bar(stat = "identity")+
  scale_fill_viridis(aesthetics = "fill")+
  
  # Add a val = 25/20/15/10 to match scale. Do it at the beginning so that the barplots are OVER it
  geom_segment(data = grid_data, aes(x = end, y = 25, xend = start, yend = 25), colour = "grey", size = 0.3, inherit.aes = FALSE)+
  geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 15, xend = start, yend = 15), colour = "grey", size = 0.3, inherit.aes = FALSE) +
  geom_segment(data = grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", size = 0.3, inherit.aes = FALSE)+

  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(min(df$ID), 4), y = c(10, 15, 20, 25), label = c("10", "15", "20", "25"), colour = "grey", size = 3, angle=0, fontface="bold", hjust=0) +
  
  ylim(-15, 40) +
  theme_minimal() +
  labs(title = "Average Daily Solar Exposure Level in Sydney Botanic Gardens", subtitle = "Seasons are meteorological | Units are 'Megajoules per square metre'", fill = "MJ/m*m")+
  # remove all the labels and text
  theme(
    plot.title = element_text(family = "serif", vjust = -3),
    plot.subtitle = element_text(vjust = -3),
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    plot.margin = unit(rep(0, 4), "cm"), # remove excessive margins
  )+
  coord_polar(start = 0) +
  
  # add the base line and information
  geom_segment(data = base_data, aes(x = start, y = -2, xend = end, yend = -2), colour = "grey", alpha = 0.8, size = 0.6, inherit.aes = FALSE)+
  geom_text(data = base_data, aes(x = title, y = c(35, 30, 30, 35), label = Seasons), vjust = 0.5, hjust = 0.5, colour = "grey", alpha = 0.8, size = 4, inherit.aes = FALSE)


# --------- Other options -----------

# option b
s <- ggplot(solar, aes(x=Week, y=dailySolarExposure, fill = Season))
s + geom_bar(stat = "summary", fun = "mean") +
  # make a blank spot in the middle of the chart
  ylim(-15, 25) +
  theme_minimal() +
  # remove all the labels and text
  theme( 
    axis.text = element_blank(),
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2, 4), "cm") # remove excessive margins
  ) +
  # make the chart circular
  coord_polar(start = 0) 


  #geom_text(data = label_data, aes(x = week_nums, y = 30, label = paste("Week", week_nums), hjust = hjust), angle = label_data$angle, inherit.aes = FALSE)

# option c
q <- ggplot(solar, aes(x = Day_Num, y = dailySolarExposure, fill = Season))
q + geom_bar(stat = "summary", fun = "mean") +
  theme(axis.text = element_text(angle = 90)) +
  ylim(-15, 25) +
  theme_minimal() +
  # remove all the labels and text
  theme( 
    axis.text = element_blank(),
    axis.title = element_blank(), 
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2, 4), "cm") # remove excessive margins
  )+
  coord_polar(start = 0)


# option d | Circular barchart using the aggregated (week) df
t <- ggplot(solar_agg_week, aes(x = WeekNum, y=dailySolarExposure, fill = Season))
t + geom_bar(stat = "identity") +
  coord_polar(start = 0)+
  ylim(-15, 28)+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) 



# ------------ Label attempts ------------
week_nums <- as.integer(levels(label_data$Week))
number_of_bars <- length(week_nums)
angle <-  90 - 360 * (week_nums-0.5) /number_of_bars # generate list of angles around the circle

label_data <- data.frame(week_nums)

#calculate the alignment of the labelsL right or left
label_data$hjust <- ifelse(angle < -90, 1, 0) # if the angle is less than -90, hjust = 1, else 0
label_data$angle <- ifelse(angle < -90, angle+180, angle) # flip the angle if it's less than 90


ggplot()+ coord_polar(start = 0) + geom_text(data = label_data, aes(x = week_nums, y = 30, label = paste("Week", week_nums), hjust = hjust), angle = label_data$angle)
# can't get the labels to show on the plot



# -------------- Multiple plots for different years ------------------
filter_year <- function(df, x){
  filter <- df$Year == x
  new_df <- df[filter,]
}

filter <- solar$Year == 1990


par(mfrow = c(4, 2))
years <- c(1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018)
years

plotlist = list()

for (x in 1:length(years)){
  df <- filter_year(solar,years[x])
  pname <- paste("Plot-", years[x])
  p <- ggplot(df, aes(x = Day_Num, y = dailySolarExposure, fill = dailySolarExposure)) +
    ggtitle(as.character(years[x]))+
    geom_bar(stat = "identity")+
    scale_fill_viridis(aesthetics = "fill")+
    theme(axis.text = element_text(angle = 90)) +
    ylim(-15, 25) +
    theme_minimal() +
    labs(title = as.character(years[x]), fill = "MJ/m*m")+
    # remove all the labels and text
    theme(
      plot.title = element_text(family = "serif", vjust = -3),
      plot.subtitle = element_text(vjust = -3),
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(), 
      panel.grid = element_blank(),
      plot.margin = unit(rep(0, 4), "cm"), # remove excessive margins
    )+
    coord_polar(start = 0)
 ggsave(paste(pname, ".png"), p, path = "C:/Users/alice/OneDrive/Documents/R")
 
 plotlist[[x]] = p
  
}


p <- grid.arrange(grobs=plotlist, ncol=4, 
                  top = textGrob("Solar exposure in selected years\nSydney Botanic Gardens",
                                 gp = gpar(fontsize = 16, fontfamily = "serif"))) 

