# Brian Baez
# Project 2: Raw Power
# CS 424 - UIC Spring 2021

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

################################################################################
# 2018 DATA
################################################################################

# read PLNT18 tab from egrid2018_data_v2.xlsx file
dataPLNT18 <- read_excel("egrid2018_data_v2.xlsx", "PLNT18")

# select the columns needed
dataPLNT18 <- select(dataPLNT18, 3, 4, 25, 20, 21, 108:118)

# rename columns
colnames(dataPLNT18) <- c("STATE", "NAME", "TYPE", "LAT", "LONG", "COAL", "OIL", "GAS","NUCLEAR",
                                     "HYDRO", "BIO", "WIND", "SOLAR", "GEO", "OTHER1", "OTHER2")

# remove row 1
dataPLNT18 <- dataPLNT18[-1,]

# combine OTHER1 and OTHER2 columns into just OTHER1
dataPLNT18$OTHER1 <- as.numeric(dataPLNT18$OTHER1)
dataPLNT18$OTHER2 <- as.numeric(dataPLNT18$OTHER2)

dataPLNT18$OTHER1 <- dataPLNT18$OTHER1 + dataPLNT18$OTHER2

# remove OTHER2 source
dataPLNT18 <- select(dataPLNT18, -"OTHER2")

# rename OTHER1
colnames(dataPLNT18)[15] <- "OTHER"

# remove any rows with NA values
dataPLNT18 <- na.omit(dataPLNT18)

# remove any rows with negative energy values
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$COAL >= 0)
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$OIL >= 0)
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$GAS >= 0)
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$HYDRO >= 0)
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$BIO >= 0)
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$OTHER >= 0)

# make source values numeric
dataPLNT18$COAL <- as.numeric(dataPLNT18$COAL)
dataPLNT18$OIL <- as.numeric(dataPLNT18$OIL)
dataPLNT18$GAS <- as.numeric(dataPLNT18$GAS)
dataPLNT18$NUCLEAR <- as.numeric(dataPLNT18$NUCLEAR)
dataPLNT18$HYDRO <- as.numeric(dataPLNT18$HYDRO)
dataPLNT18$BIO <- as.numeric(dataPLNT18$BIO)
dataPLNT18$WIND <- as.numeric(dataPLNT18$WIND)
dataPLNT18$SOLAR <- as.numeric(dataPLNT18$SOLAR)
dataPLNT18$GEO <- as.numeric(dataPLNT18$GEO)

# make latitude and longitude values numeric
dataPLNT18$LAT <- as.numeric(dataPLNT18$LAT)
dataPLNT18$LONG <- as.numeric(dataPLNT18$LONG)

# rename values in type column
dataPLNT18$TYPE <- gsub("BIOMASS", "BIO", dataPLNT18$TYPE)
dataPLNT18$TYPE <- gsub("GEOTHERMAL", "GEO", dataPLNT18$TYPE)
dataPLNT18$TYPE <- gsub("OTHF", "OTHER", dataPLNT18$TYPE)
dataPLNT18$TYPE <- gsub("OFSL", "OTHER", dataPLNT18$TYPE)

# change all names to uppercase
dataPLNT18$NAME <- toupper(dataPLNT18$NAME)

# create a TOTAL column and add energy values
dataPLNT18$TOTAL <- (dataPLNT18$COAL + dataPLNT18$OIL + dataPLNT18$GAS + dataPLNT18$NUCLEAR
                    + dataPLNT18$HYDRO + dataPLNT18$BIO + dataPLNT18$WIND + dataPLNT18$SOLAR 
                    + dataPLNT18$GEO + dataPLNT18$OTHER)

# remove any rows that have a TOTAL of zero
dataPLNT18 <- subset(dataPLNT18, dataPLNT18$TOTAL > 0)

# create columns for percentages of each energy source
dataPLNT18$COAL_PER <- (dataPLNT18$COAL / dataPLNT18$TOTAL) * 100
dataPLNT18$OIL_PER <- (dataPLNT18$OIL / dataPLNT18$TOTAL) * 100
dataPLNT18$GAS_PER <- (dataPLNT18$GAS / dataPLNT18$TOTAL) * 100
dataPLNT18$NUCLEAR_PER <- (dataPLNT18$NUCLEAR / dataPLNT18$TOTAL) * 100
dataPLNT18$HYDRO_PER <- (dataPLNT18$HYDRO / dataPLNT18$TOTAL) * 100
dataPLNT18$BIO_PER <- (dataPLNT18$BIO / dataPLNT18$TOTAL) * 100
dataPLNT18$WIND_PER <- (dataPLNT18$WIND / dataPLNT18$TOTAL) * 100
dataPLNT18$SOLAR_PER <- (dataPLNT18$SOLAR / dataPLNT18$TOTAL) * 100
dataPLNT18$GEO_PER <- (dataPLNT18$GEO / dataPLNT18$TOTAL) * 100
dataPLNT18$OTHER_PER <- (dataPLNT18$OTHER / dataPLNT18$TOTAL) * 100

# sum all renewable energy sources
dataPLNT18$TOTAL_RENEWABLE <- (dataPLNT18$HYDRO + dataPLNT18$BIO + dataPLNT18$WIND + dataPLNT18$SOLAR + dataPLNT18$GEO)

# sum all other energy sources
dataPLNT18$TOTAL_NON_RENEWABLE <- (dataPLNT18$TOTAL - dataPLNT18$TOTAL_RENEWABLE)

# create columns for percentages of renewable and non-renewables
dataPLNT18$RENEWABLE_PER <- (dataPLNT18$TOTAL_RENEWABLE / dataPLNT18$TOTAL) * 100
dataPLNT18$NON_RENEWABLE_PER <- (dataPLNT18$TOTAL_NON_RENEWABLE / dataPLNT18$TOTAL) * 100

################################################################################
# 2000 DATA
################################################################################

# read EGRDPLNT00 from eGRID2000_plant.xls file
dataPLNT00 <- read_excel("eGRID2000_plant.xls", "EGRDPLNT00")

# select the columns needed
dataPLNT00 <- select(dataPLNT00, 3, 4, 29, 24, 25, 69:78)

# rename columns
colnames(dataPLNT00) <- c("STATE", "NAME", "TYPE", "LAT", "LONG", "COAL", "OIL", "GAS","NUCLEAR",
                          "HYDRO", "BIO", "WIND", "SOLAR", "GEO", "OTHER")

# remove rows 1, 2, and 3
dataPLNT00 <- dataPLNT00[-(1:3),]

# remove any rows with NA values
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$LAT != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$LONG != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$COAL != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$OIL != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$GAS != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$NUCLEAR != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$HYDRO != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$BIO != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$WIND != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$SOLAR != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$GEO != 'N/A')
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$OTHER != 'N/A')

# remove any rows with negative energy values
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$COAL >= 0)
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$OIL >= 0)
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$GAS >= 0)
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$HYDRO >= 0)
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$GEO >= 0)

# make source values numeric
dataPLNT00$COAL <- as.numeric(dataPLNT00$COAL)
dataPLNT00$OIL <- as.numeric(dataPLNT00$OIL)
dataPLNT00$GAS <- as.numeric(dataPLNT00$GAS)
dataPLNT00$NUCLEAR <- as.numeric(dataPLNT00$NUCLEAR)
dataPLNT00$HYDRO <- as.numeric(dataPLNT00$HYDRO)
dataPLNT00$BIO <- as.numeric(dataPLNT00$BIO)
dataPLNT00$WIND <- as.numeric(dataPLNT00$WIND)
dataPLNT00$SOLAR <- as.numeric(dataPLNT00$SOLAR)
dataPLNT00$GEO <- as.numeric(dataPLNT00$GEO)
dataPLNT00$OTHER <- as.numeric(dataPLNT00$OTHER)

# make latitude and longitude values numeric
dataPLNT00$LAT <- as.numeric(dataPLNT00$LAT)
dataPLNT00$LONG <- as.numeric(dataPLNT00$LONG)

# make longitude values negative
dataPLNT00$LONG <- -dataPLNT00$LONG

# rename values in type column
dataPLNT00$TYPE <- gsub("NG", "GAS", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("AB", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("BFG", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("BIOMASS", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("BIT", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("BL", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("COL", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("DFO", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("EF", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("GE", "GAS", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("JF", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("KER", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("LFG", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("GASO", "GEO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("LIG", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("MSW", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("MWC", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("NUC", "NUCLEAR", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("OBG", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("OG", "OTHER", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("OO", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("OTG", "OTHER", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("OTS", "OTHER", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("PC", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("RFO", "OIL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("SL", "SOLAR", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("SUB", "COAL", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("SUN", "SOLAR", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("TDF", "OTHER", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("UR", "NUCLEAR", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WAT", "HYDRO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WDL", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WDS", "BIO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WH", "OTHER", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WN", "WIND", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WINDD", "WIND", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WT", "HYDRO", dataPLNT00$TYPE)
dataPLNT00$TYPE <- gsub("WOC", "COAL", dataPLNT00$TYPE)

# change all names to uppercase
dataPLNT00$NAME <- toupper(dataPLNT00$NAME)

# create a TOTAL column and add energy values
dataPLNT00$TOTAL <- (dataPLNT00$COAL + dataPLNT00$OIL + dataPLNT00$GAS + dataPLNT00$NUCLEAR
                     + dataPLNT00$HYDRO + dataPLNT00$BIO + dataPLNT00$WIND + dataPLNT00$SOLAR 
                     + dataPLNT00$GEO + dataPLNT00$OTHER)

# remove any rows that have a TOTAL of zero
dataPLNT00 <- subset(dataPLNT00, dataPLNT00$TOTAL > 0)

# create columns for percentages of each energy source
dataPLNT00$COAL_PER <- (dataPLNT00$COAL / dataPLNT00$TOTAL) * 100
dataPLNT00$OIL_PER <- (dataPLNT00$OIL / dataPLNT00$TOTAL) * 100
dataPLNT00$GAS_PER <- (dataPLNT00$GAS / dataPLNT00$TOTAL) * 100
dataPLNT00$NUCLEAR_PER <- (dataPLNT00$NUCLEAR / dataPLNT00$TOTAL) * 100
dataPLNT00$HYDRO_PER <- (dataPLNT00$HYDRO / dataPLNT00$TOTAL) * 100
dataPLNT00$BIO_PER <- (dataPLNT00$BIO / dataPLNT00$TOTAL) * 100
dataPLNT00$WIND_PER <- (dataPLNT00$WIND / dataPLNT00$TOTAL) * 100
dataPLNT00$SOLAR_PER <- (dataPLNT00$SOLAR / dataPLNT00$TOTAL) * 100
dataPLNT00$GEO_PER <- (dataPLNT00$GEO / dataPLNT00$TOTAL) * 100
dataPLNT00$OTHER_PER <- (dataPLNT00$OTHER / dataPLNT00$TOTAL) * 100

# sum all renewable energy sources
dataPLNT00$TOTAL_RENEWABLE <- (dataPLNT00$HYDRO + dataPLNT00$BIO + dataPLNT00$WIND + dataPLNT00$SOLAR + dataPLNT00$GEO)

# sum all other energy sources
dataPLNT00$TOTAL_NON_RENEWABLE <- (dataPLNT00$TOTAL - dataPLNT00$TOTAL_RENEWABLE)

# create columns for percentages of renewable and non-renewables
dataPLNT00$RENEWABLE_PER <- (dataPLNT00$TOTAL_RENEWABLE / dataPLNT00$TOTAL) * 100
dataPLNT00$NON_RENEWABLE_PER <- (dataPLNT00$TOTAL_NON_RENEWABLE / dataPLNT00$TOTAL) * 100

################################################################################
# 2010 DATA
################################################################################

# read PLNT10 from eGRID2010_Data.xls file
dataPLNT10 <- read_excel("eGRID2010_Data.xls", "PLNT10")

# select the columns needed
dataPLNT10 <- select(dataPLNT10, 2, 3, 30, 21, 22, 82:92)

# rename columns
colnames(dataPLNT10) <- c("STATE", "NAME", "TYPE", "LAT", "LONG", "COAL", "OIL", "GAS","NUCLEAR",
                          "HYDRO", "BIO", "WIND", "SOLAR", "GEO", "OTHER1", "OTHER2")

# remove row 1, 2, and 3
dataPLNT10 <- dataPLNT10[-(1:4),]

# combine OTHER1 and OTHER2 columns into just OTHER1
dataPLNT10$OTHER1 <- as.numeric(dataPLNT10$OTHER1)
dataPLNT10$OTHER2 <- as.numeric(dataPLNT10$OTHER2)

dataPLNT10$OTHER1 <- dataPLNT10$OTHER1 + dataPLNT10$OTHER2

# remove OTHER2 source
dataPLNT10 <- select(dataPLNT10, -"OTHER2")

# rename OTHER1
colnames(dataPLNT10)[15] <- "OTHER"

# remove any rows with NA values
dataPLNT10 <- na.omit(dataPLNT10)

# remove any rows with negative energy values
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$COAL >= 0)
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$OIL >= 0)
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$GAS >= 0)
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$HYDRO >= 0)
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$BIO >= 0)

# make source values numeric
dataPLNT10$COAL <- as.numeric(dataPLNT10$COAL)
dataPLNT10$OIL <- as.numeric(dataPLNT10$OIL)
dataPLNT10$GAS <- as.numeric(dataPLNT10$GAS)
dataPLNT10$NUCLEAR <- as.numeric(dataPLNT10$NUCLEAR)
dataPLNT10$HYDRO <- as.numeric(dataPLNT10$HYDRO)
dataPLNT10$BIO <- as.numeric(dataPLNT10$BIO)
dataPLNT10$WIND <- as.numeric(dataPLNT10$WIND)
dataPLNT10$SOLAR <- as.numeric(dataPLNT10$SOLAR)
dataPLNT10$GEO <- as.numeric(dataPLNT10$GEO)

# make latitude and longitude values numeric
dataPLNT10$LAT <- as.numeric(dataPLNT10$LAT)
dataPLNT10$LONG <- as.numeric(dataPLNT10$LONG)

# rename values in type column
dataPLNT10$TYPE <- gsub("BIOMASS", "BIO", dataPLNT10$TYPE)
dataPLNT10$TYPE <- gsub("GEOTHERMAL", "GEO", dataPLNT10$TYPE)
dataPLNT10$TYPE <- gsub("OTHRFOSL", "OTHER", dataPLNT10$TYPE)
dataPLNT10$TYPE <- gsub("WSTHTOTPUR", "OTHER", dataPLNT10$TYPE)

# change all names to uppercase
dataPLNT00$NAME <- toupper(dataPLNT00$NAME)

# create a TOTAL column and add energy values
dataPLNT10$TOTAL <- (dataPLNT10$COAL + dataPLNT10$OIL + dataPLNT10$GAS + dataPLNT10$NUCLEAR
                     + dataPLNT10$HYDRO + dataPLNT10$BIO + dataPLNT10$WIND + dataPLNT10$SOLAR 
                     + dataPLNT10$GEO + dataPLNT10$OTHER)

# remove any rows that have a TOTAL of zero
dataPLNT10 <- subset(dataPLNT10, dataPLNT10$TOTAL > 0)

# create columns for percentages of each energy source
dataPLNT10$COAL_PER <- (dataPLNT10$COAL / dataPLNT10$TOTAL) * 100
dataPLNT10$OIL_PER <- (dataPLNT10$OIL / dataPLNT10$TOTAL) * 100
dataPLNT10$GAS_PER <- (dataPLNT10$GAS / dataPLNT10$TOTAL) * 100
dataPLNT10$NUCLEAR_PER <- (dataPLNT10$NUCLEAR / dataPLNT10$TOTAL) * 100
dataPLNT10$HYDRO_PER <- (dataPLNT10$HYDRO / dataPLNT10$TOTAL) * 100
dataPLNT10$BIO_PER <- (dataPLNT10$BIO / dataPLNT10$TOTAL) * 100
dataPLNT10$WIND_PER <- (dataPLNT10$WIND / dataPLNT10$TOTAL) * 100
dataPLNT10$SOLAR_PER <- (dataPLNT10$SOLAR / dataPLNT10$TOTAL) * 100
dataPLNT10$GEO_PER <- (dataPLNT10$GEO / dataPLNT10$TOTAL) * 100
dataPLNT10$OTHER_PER <- (dataPLNT10$OTHER / dataPLNT10$TOTAL) * 100

# sum all renewable energy sources
dataPLNT10$TOTAL_RENEWABLE <- (dataPLNT10$HYDRO + dataPLNT10$BIO + dataPLNT10$WIND + dataPLNT10$SOLAR + dataPLNT10$GEO)

# sum all other energy sources
dataPLNT10$TOTAL_NON_RENEWABLE <- (dataPLNT10$TOTAL - dataPLNT10$TOTAL_RENEWABLE)

# create columns for percentages of renewable and non-renewables
dataPLNT10$RENEWABLE_PER <- (dataPLNT10$TOTAL_RENEWABLE / dataPLNT10$TOTAL) * 100
dataPLNT10$NON_RENEWABLE_PER <- (dataPLNT10$TOTAL_NON_RENEWABLE / dataPLNT10$TOTAL) * 100

################################################################################
# VALUES NEEDED FOR APPLICATION
################################################################################

# subsets for illinois data
dataIllinois18 <- subset(dataPLNT18, dataPLNT18$STATE == "IL")
dataIllinois00 <- subset(dataPLNT00, dataPLNT00$STATE == "IL")
dataIllinois10 <- subset(dataPLNT10, dataPLNT10$STATE == "IL")

# since original data is too large to display, only getting samples of 500
sampleDataPLNT00 <- sample_n(dataPLNT00, 500)
sampleDataPLNT10 <- sample_n(dataPLNT10, 500)
sampleDataPLNT18 <- sample_n(dataPLNT18, 500)

sources <- c("Coal", "Biomass", "Hydro", "Geothermal","Natural Gas",
             "Nuclear", "Oil", "Other", "Solar", "Wind")

sourceNames <- c("COAL", "BIO", "HYDRO", "GEO","GAS",
             "NUCLEAR", "OIL", "OTHER", "SOLAR", "WIND")

sourcesColors <- c("black", "lightgreen", "lightblue", "gray", "white",
                   "purple", "red", "orange", "green", "darkblue")

################################################################################
# UI
################################################################################

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title="CS 424 Project 2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Illinois Power Plants", tabName = "illinoisPage", icon = icon("bolt")),
      menuItem("State Power Plants", tabName = "statePage", icon = icon("lightbulb")),
      menuItem("U.S. Power Plants", tabName = "usPage", icon = icon("flag-usa")),
      menuItem("About", tabName = "aboutPage", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "illinoisPage",
                box(title="Locations of Power Plants in Illinois in 2018", solidHeader = TRUE,
                    leafletOutput("illinoisMap18", height = 550, width = 800), width = 9
                ), # end box with map
              
                box(title = "Select the energy sources you would like to see:", solidHeader = TRUE, width = 3,
                  
                  checkboxInput("selectAll", label="Select/Deselect All", value = TRUE),
                  
                  checkboxInput("renewableChoice", label="Renewables", value = TRUE),
                  
                  checkboxInput("nonRenewableChoice", label="Non-Renewables", value = TRUE),
                  
                  checkboxGroupInput("sourcesChoice", label=NULL, inline=FALSE, 
                                     choices = sources)
              ) # end box with choice selections
      ), # end illinois 2018 page
      
      tabItem(tabName = "statePage",
              box(title = "Illinois Power Plants in 2000", solidHeader = TRUE,
                  width = 6,
                  leafletOutput("illinoisMap00", height = 500, width = 500)
              ),
              
              box(title = "Illinois Power Plants in 2010", solidHeader = TRUE,
                  width = 6,
                  leafletOutput("illinoisMap10", height = 500, width = 500)
              )
              
      ), # end illinois 2000 and illinois 2010 comparison page
      
      tabItem(tabName = "usPage",
              box(title = "U.S. Power Plants", solidHeader = TRUE,
                  tabBox(selected="2000",
                         tabPanel("2000",
                                leafletOutput("usMap00", height = 500, width = 1000)  
                          ),
                         tabPanel("2010",
                                  leafletOutput("usMap10", height = 500, width = 1000)
                          ),
                         tabPanel("2018",
                                  leafletOutput("usMap18", height = 500, width = 1000)  
                          )
                  ), width = 12
              )
      ), # end us 2000, 2010, and 2018 page
      
      tabItem(tabName = "aboutPage",
              box(title = "Application created by Brian Baez", solidHeader = TRUE,
                  HTML("The data used for this application was acquired from https://www.epa.gov/egrid/download-data"))
      ) # end about page
    ) # end tabItems
  ) 
)

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  prevAllState <- TRUE
  
  # updates checkboxes accordingly
  observe({
    updateCheckboxInput(session, "renewableChoice",
                        label = "Renewables",
                        value = input$selectAll)
  })
  
  observe({
    updateCheckboxInput(session, "nonRenewableChoice",
                        label = "Non-Renewables",
                        value = input$selectAll)
  })
  
  observe({
    updateCheckboxGroupInput(session, "sourcesChoice",
                            choices = sources,
                            selected = if(input$selectAll) sources)
  })
  
  # renders map for Illinois in 2018
  output$illinoisMap18 <- renderLeaflet({
    leaflet(dataIllinois18) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = dataIllinois18$LONG, 
                 lat = dataIllinois18$LAT,
                 label = dataIllinois18$TYPE,
                 popup = dataIllinois18$NAME
      )
  })
  
  # renders map for Illinois in 2000
  output$illinoisMap00 <- renderLeaflet({
    leaflet(dataIllinois00) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = dataIllinois00$LONG, 
                        lat = dataIllinois00$LAT,
                        label = dataIllinois00$TYPE,
                        popup = dataIllinois00$NAME
      )
  })
  
  # renders map for Illinois in 2010
  output$illinoisMap10 <- renderLeaflet({
    leaflet(dataIllinois10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = dataIllinois10$LONG, 
                        lat = dataIllinois10$LAT,
                        label = dataIllinois10$TYPE,
                        popup = dataIllinois10$NAME
      )
  })
  
  # renders map for US in 2000
  output$usMap00 <- renderLeaflet({
    leaflet(sampleDataPLNT00) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = sampleDataPLNT00$LONG, 
                 lat = sampleDataPLNT00$LAT,
                 label = sampleDataPLNT00$TYPE,
                 popup = sampleDataPLNT00$NAME
      )
  })
  
  # renders map for US in 2010
  output$usMap10 <- renderLeaflet({
    leaflet(sampleDataPLNT10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = sampleDataPLNT10$LONG, 
                 lat = sampleDataPLNT10$LAT,
                 label = sampleDataPLNT10$TYPE,
                 popup = sampleDataPLNT10$NAME
      )
  })
  
  # renders map for US in 2018
  output$usMap18 <- renderLeaflet({
    leaflet(sampleDataPLNT18) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMarkers(lng = sampleDataPLNT18$LONG, 
                 lat = sampleDataPLNT18$LAT,
                 label = sampleDataPLNT18$TYPE,
                 popup = sampleDataPLNT18$NAME
      )
  })
  
}

################################################################################
################################################################################

shinyApp(ui, server)  
