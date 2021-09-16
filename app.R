library(ggplot2)
library(rvest)
library(dplyr)
library(leaflet)
library(geojsonio)
library(rgdal)
library(RColorBrewer)
library(tidyverse)
library(shiny)

sakuradata <- read_csv("sakura_first_bloom_dates.csv")
japanmap <- geojson_read("japan_prefectures.geojson", what = "sp")
japanmap@data <- japanmap@data %>% select(name_1) %>% rename(name = name_1)

###Modify the sakuradata so that place names are prefectures 
japanmap$name <- japanmap$name %>%
    replace(33, "Hyogo")

sakuradatacopy <- read_csv("sakura.csv")
japanmap@data <- japanmap@data %>% inner_join(sakuradatacopy, by = c("name" = "Site Name"))
sakuradata <- japanmap@data

###Animate map

#cOLOR
date<-seq(as.Date("2020-03-10"), as.Date("2020-5-10"), by="day")
sakuracolor <- c("grey95", "thistle1", "plum1", "darkorchid", "plum1", "thistle1", "grey95")
color2 <- colorBin(palette = sakuracolor,
                   bins = c(-100, -7, -3, 0, 3, 7, 100))

###Part 2 graph
summary(sakuradata)
sakuradata <- sakuradata %>%
    select(-`Currently Being Observed`, -Notes, -`30 Year Average 1981-2010`)

sakuradatalong <- sakuradata %>%
    pivot_longer(!name, names_to = "year",
                 values_to = "date")

sakuradatalong$year <- sakuradatalong$year %>%
    as.numeric()

sakuradatalong$date <- sakuradatalong$date %>%
    as.Date(format = "%m/%d/%Y")

sakuradatalong$date <-str_sub(sakuradatalong$date, start = 6, end = -1)

#Scraping temperature Data
tempurl <- "https://pkgstore.datahub.io/core/global-temp/annual_csv/data/a26b154688b061cdd04f1df36e4408be/annual_csv.csv"

temp <- read_csv(tempurl) %>%
    filter(Source == "GCAG") %>%
    filter(Year > 1952)


shinyApp(
    
    ui = fluidPage(
        bootstrapPage(tags$style(type = "text/css", "html, body, .leaflet {width:100%; height:100%}"),
                      leafletOutput("map", width = "100%", height = "100%"),
                      selectizeInput("Year", "Choose the year of the animated map!",
                                     choices = 1953:2020),
                      uiOutput(outputId = "slider1")),
        
        leafletOutput("japanmap"),
        
        selectizeInput("Prefecture", "Choose the prefecture to plot on graph1", 
                       choices = unique(sakuradatalong$name)),
        
        plotOutput("graph"),
        plotOutput("graph1"),
        
        textOutput("Text")
    ),
    
    server = function(input, output) {
        
        
        output$slider1 <- renderUI({
            sliderInput("date", "Date:",
                        as.Date(paste(c(input$Year, "03-10"), collapse = "-")), 
                        as.Date(paste(c(input$Year, "05-10"), collapse = "-")),
                        value = as.Date(paste(c(input$Year, "05-10"), collapse = "-")),
                        step = 2,
                        animate = T)
        })
        
        output$japanmap <- renderLeaflet({
            print(input$leaflet1_shape_click$id)
            
            japanmap %>%
                leaflet() %>%
                addPolygons(color = "black",
                            #fillColor = ~color2(japanmap$days),
                            fillOpacity = 0.7,
                            opacity = 1,
                            weight = 1)
        })
        
        observeEvent(input$date, {
            japanmap@data[ ,input$Year] <-   japanmap@data[ ,input$Year] %>%
                as.Date(format = "%m/%d/%Y")
            
            japanmap@data <- japanmap@data %>%
                mutate(days = japanmap@data[ ,input$Year] - input$date)
            
            japanmap@data$days <- japanmap@data$days %>% as.numeric()
            
            leafletProxy("japanmap", data = japanmap) %>%
                #clearShapes() %>%
                addPolygons(fillColor = ~color2(japanmap@data$days),
                            color = "black",
                            #fillColor = ~color2(japanmap$days),
                            fillOpacity = 0.7,
                            opacity = 1,
                            weight = 1)
            
        })
        
        
        output$graph <- renderPlot({
            
            sakuradatalong %>%
                filter(name == input$Prefecture) %>%
                mutate(date = as.Date(date, format = "%m-%d")) %>%
                ggplot(aes(x = year,
                           y = date)) +
                geom_point() +
                geom_smooth(
                    method = "lm",
                    se = FALSE) +
                geom_line() +
                theme_bw() + xlab("Date") + ylab("Year")
            
        })
        
        output$graph1 <- renderPlot(
            ggplot(temp) + 
                geom_line(mapping = aes(x = Year, 
                                        y = Mean)) +
                theme_bw() + ylab("Annual mean temperature") +
                ggtitle("Annual global mean temperature") +
                geom_smooth(mapping = aes(x = Year, 
                                          y = Mean),
                            method = "lm",
                            se = FALSE)
        )
        
        output$Text <- renderPrint(
            cat("The research question I wanted to explore was: does the rise in global annual temperature over the past century has an effect on the 
            blooming date of cherry blossoms in Japan?",
                "For Data Scraping,", 
                "I tried to scrape the cherry blossom data from Japan's Meteorological Website, but I realized that I did not 
            know how to make the single text file into a table. Thanks to Kaggle, someone already did it for me using a combination of R and 
            Python's Google translate. Then, I scraped a geojson polygon of all the prefectures of Japan, as well as global annual temperature data
            in the form of CSV from some other websites. I choose these data because I wanted to create a concise but informative interactive app
            that allows the user to explore, to some flexibility, the data provided by Japan's Meteorological Agency and to see the changes over the year
            to the cherry blossoms in an interactive and aesthetically pleasing way", 
                "There are a few limtations to this project.",
                "For one, I couldn't find an effective way to combine the temperture graph and the cherry blossom graph in one single visualization. 
            I also realized after creating the animation that while it looks really cool to see how the cherry blossom is affected by latitude (temperature), 
            it does not tell a convincing visualization of the change over the years. While the user can choose which year to animate, I couldn't put 
            everything I wanted to show in one single visualization. The graph complements the animation by showing temperature change over the years,
             while allowing the user to choose whichever prefecture to graph. A more efficient way I could have explored is using the shape click function 
            in Shiny.",
                "To this end, I do think this research question is quite effectively answered by the negative correlation between sakura blooming date and 
            global annual temperture rise.",
                sep="\n")
        )
    }
)



