library(ggplot2)
library(reshape2)
library(RCurl)

#--------------------------------------------------------------------------------------------------
# 1. Seat Share for BJP/NDA in major six states 

# Read the csv file containing states and their vote share
state_vote <- read.csv(text=getURL("https://raw.githubusercontent.com/HardikLGupta/Indian_Election/master/State-wise-votes.csv"))

#Plot the data, per state
state_vote_plot <-
  ggplot(state_vote, aes(x = Party, y = Seats, fill = factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge()) + facet_wrap( ~State) +
  geom_bar(width = 0.7,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(
    paste("Year"),
    values = c("yellow3", "red3", "green3")
  ) +
  labs(title = "Seat Share for BJP/NDA in major six states", x = "", y = "Seats") +
  scale_y_continuous(breaks = seq(0, max(state_vote$Seats), 5)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

state_vote_plot


#----------------------------------------------

# 2. Google Trend Searches, June 13 to June 14 (Top 10 Regions, constituency seat wise)

geoMap1 <- read.csv(text=getURL("https://raw.githubusercontent.com/HardikLGupta/Indian_Election/master/geoMap.csv"))
geoMap1.m <- melt(geoMap1, id.vars = 'Region')

geoMap1_plot <- ggplot(geoMap1.m, aes(x = Region, y = value, fill = variable)) +
  geom_bar(width = 0.7,
           stat = "identity",
           position = position_dodge()) +
  scale_fill_manual(
    paste("Political Leader"),
    values = c("yellow3", "red2", "green3"),
    labels = c("Narendra Modi", "Rahul Gandhi", "Modi")
  ) +
  labs(title = "Google Trend Searches, June 13 to June 14 (Top 10 Regions, constituency seat wise)", x = "Region", y = "Unit") +
  scale_y_continuous(breaks = seq(0, max(geoMap1.m$value), 5)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

geoMap1_plot


#-----------------------------------------------------------------------------

# Google Trend Searches for Election Manifesto, June 13 to June 14 (Top 10 Regions, constituency seat wise)

geoMap2 <- read.csv(text=getURL("https://raw.githubusercontent.com/HardikLGupta/Indian_Election/master/geoMap_manifesto.csv"))
geoMap2.m <- melt(geoMap2, id.vars = 'Region')

geoMap2_plot <-
  ggplot(geoMap2.m, aes(x = Region, y = value, fill = variable)) +
  geom_bar(width = 0.7,
           stat = "identity",
           position = position_dodge()) + coord_flip() +
  scale_fill_manual(
    paste("Political Leader"),
    values = c("yellow3", "red2", "green3"),
    labels = c("BJP Manifesto 2014", "Congress Manifesto 2014")
  ) +
  labs(title = "Google Trend Searches for Election Manifesto, June 13 to June 14 (Top 10 Regions, constituency seat wise)", x = "Region", y = "Unit") +
  scale_y_continuous(breaks = seq(0, max(geoMap1.m$value), 5)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

geoMap2_plot


#-----------------------------------------------------------------------------

## Wikipedia Trends - Modi vs Rahul Gandhi (June 13 to June 14) ##

library("wikipediatrend")
library("ggplot2")
options(scipen = 1000000)

NM = wp_trend("Narendra Modi", from = "2013-06-01", to = "2014-06-01")
RG = wp_trend("Rahul Gandhi", from = "2013-06-01", to = "2014-06-01")

dcomp = rbind(NM, RG)

ggplot(dcomp) +
  geom_line(aes(x = date, y = count, colour = title), size = 1.1) +
  scale_colour_manual(
    values = c("red", "blue"),
    labels = c("Narendra Modi", "Rahul Gandhi"),
    "Political Leader"
  ) +
  scale_y_continuous(breaks = seq(0, max(dcomp$count), 20000)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 12)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) + scale_x_date(date_breaks = "1 month", date_labels = "%d %b %y") +
  labs(title = "Wikipedia Trends - Modi vs Rahul Gandhi (June 13 to June 14)", x = "", y = "Search Count")


#----------------------------------------------------------------------
## Wikipedia Trends - Modi vs Rahul Gandhi vs BJP vs Congress (June 13 to June 14) ##

library("wikipediatrend")
library("ggplot2")
options(scipen = 1000000)

BJP = wp_trend("Bharatiya Janata Party", from = "2013-06-01", to = "2014-06-01")
INC = wp_trend("Indian National Congress", from = "2013-06-01", to = "2014-06-01")
NM = wp_trend("Narendra Modi", from = "2013-06-01", to = "2014-06-01")
RG = wp_trend("Rahul Gandhi", from = "2013-06-01", to = "2014-06-01")

dcomp = rbind(BJP, INC, NM, RG)

ggplot(dcomp) +
  geom_line(aes(x = date, y = count, colour = title), size = 1.05) +       # and color aesthetics
  scale_colour_manual(
    values = c("yellow4", "blue4", "red4", "green3"),
    labels = c(
      "Bharatiya Janata Party",
      "Indian National Congress",
      "Narendra Modi",
      "Rahul Gandhi"
    ),
    "Searched Identity"
  ) +
  scale_y_continuous(breaks = seq(0, max(dcomp$count), 20000)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 12)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d %b %y") +
  labs(title = "Wikipedia Trends - Modi vs Rahul Gandhi vs BJP vs Congress (June 13 to June 14)",
       x = "", y = "Search Count")


#------------------------------------------------------------------------------------------------------------

## Wikipedia Trends - Scams and Corruption ##

corruption = wp_trend("Corruption in India", from = "2011-06-01", to = "2016-06-01")
scandals = wp_trend("List of scandals in India", from = "2011-06-01", to = "2016-06-01")

dcomp = rbind(corruption, scandals)

ggplot(dcomp) +
  geom_line(aes(x = date, y = count, colour = title), size = 1.1)
scale_colour_manual(
  values = c("red3", "blue4"),
  labels = c("Corruption in India", "List of Scandals in India"),
  "Legend"
) +
  scale_y_continuous(breaks = seq(0, max(dcomp$count), 1000)) +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text("Legend"),
    legend.background = element_rect(colour = "black"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted"),
    axis.title.x = element_text(margin = margin(r = 12)),
    axis.title.y = element_text(margin = margin(r = 10))
  )  +
  labs(title = "Wikipedia Trends for Scams and Corruption Queries, 2011 to 2016", x = "", y = "Search Count")
