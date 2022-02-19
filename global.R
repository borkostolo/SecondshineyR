library(DT)
library(shiny)
library(googleVis)

library(shinydashboard)
library(tidyverse)
library(leaflet)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)
library(shinyWidgets)
# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice <- colnames(state_stat)[-1]

#Other Feature selecting choices

attr_df  = read.csv("./csv_outputs/attr_lista_toltott.csv", stringsAsFactors = F)
financedf = read.csv("./csv_outputs/financials.csv", stringsAsFactors = F)
financedf %>% filter(., financedf$crefoid == 9317474664 & financedf$financialValue != 0 & financedf$valueText == "ADÓZÁS ELŐTTI EREDMÉNY") %>% select(., endDate, valueText, financialValue)
