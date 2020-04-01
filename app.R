#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(dplyr)
library(readr)
library(leaflet) # for interactive maps
library(tigris)
library(shinydashboard)

# NYT County level data
urlfile="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
nyt_counties <- read_csv(url(urlfile))
nyt_counties[nyt_counties$county == 'New York City','fips'] <- 99999

#NYT state level data
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
nyt_states <- read_csv(url(urlfile))

# load county boundaries
load(file="us_counties.Rdata")

covid_counties_map <- geo_join(us_counties, nyt_counties, by_sp='GEOID', by_df = 'fips', how='inner')
covid_counties_map$death_rate <- covid_counties_map$deaths/ covid_counties_map$cases
# covid_counties_map$log_cases <- log(covid_counties_map$cases)

# Define UI for application that draws a histogram
# ui <- fluidPage(
ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        selectInput(inputId = 'date_select', 
                    label='Select Date',
                    choices = unique(nyt_states$date),
                    selected = max(nyt_states$date)
                    ),
        radioButtons(inputId = 'metrics_select',
                     label = 'Select Metric',
                     choices = c('cases','deaths', 'death_rate'),
                     selected = "cases")
        ),
    dashboardBody(
        fluidPage(
            box(width = 8, 
                leafletOutput("county_map")
                )
        )
    )
)
# ui <- shinyUI(
        # fluidPage(
        #     box(leafletOutput("county_map", width = "100%", height = "100%"))
        #           )
        #)
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
    chosen_date <- reactive({input$date_select})
    metric <- reactive({input$metrics_select})
    chosen_data <- reactive({
        covid_counties_map[covid_counties_map$date == input$date_select,c("state","county","cases","deaths","geometry")]
    })
     output$county_map <- 
       renderLeaflet({
           # pal <- colorNumeric(
           #     # domain = log(covid_counties_map[covid_counties_map$date == max(nyt_states$date),]$cases),
           #     # domain=c(0,50000),
           #     domain = NULL,
           #     palette = heating(7),
           #     # palette = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
           #                                  # "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), space = "Lab", bias = 1.5),
           #     # probs = c(0, 25, 50, 60, 75, 80, 85, 90, 95, 100)/100,
           #     # n = 5,
           #     # bins=7,
           #     # bins = unique(quantile(covid_counties_map[covid_counties_map$date == max(nyt_states$date),]$cases, probs = c(.5,.94,.98,.99,.995,1))),
           #     # bins = c(0,5,10,20,30,40,50,100,1000,5000, 500000),
           #     # reverse=TRUE
           #     reverse=FALSE
           # )
           # heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
           # qpal <- 
           #     colorNumeric(
           #         # domain = c(0,log(max(covid_counties_map[covid_counties_map$date == max(nyt_states$date),]$cases))),
           #         # domain = c(0, 100000),
           #         domain = NULL,
           #         # na.color = 'Red',
           #         # palette = "Spectral",
           #         palette = heating(100),
           #         reverse=FALSE
           #     )
           # 
           # pal = c("red","white","blue", "green")
           
           # chosen_data = covid_counties_map[covid_counties_map$date == max(nyt_states$date),c("state","county","cases","deaths","geometry")]
           markers <- eventReactive(input$metrics_select,{
               chosen_data()
           })
           chosen_data[,'select_values'] <- c(chosen_date[,input$metrics_select])
           leaflet(data=chosen_data()) %>%
               addTiles() %>%
               # addPolygons(group = 'base_map', data = covid_counties_map[covid_counties_map$date == max(nyt_states$date),], weight = .5, fillOpacity=0.2) %>%
               # addPolygons(group = 'metric', 
               #             data= covid_counties_map[covid_counties_map$date == max(nyt_states$date),], 
               #             weight = 1, 
               #             fillOpacity=1,
               #             color = ~pal(log(cases))
               #             ) %>%
               addMarkers(group="metric",
                          data=st_point_on_surface(chosen_data()),
                          options = markerOptions(select_values = ~select_values),
                          popup = ~ as.character(paste0("Cases: ",cases, " |  Deaths: ",deaths)),
                          label = ~ as.character(paste0(county,", ", state)),
                          clusterOptions = markerClusterOptions(
                              iconCreateFunction=JS("function (cluster) {    
                                                    var markers = cluster.getAllChildMarkers();
                                                    var marker_sum = 0; 
                                                    for (i = 0; i < markers.length; i++) {
                                                      marker_sum += Number(markers[i].options.select_values);
                                                //      marker_sum += 1;
                                                    }

                                                    var c = ' marker-cluster-';
                                                    if (marker_sum < 100) {
                                                    c += 'small';
                                                    } else if (marker_sum < 10000) {
                                                    c += 'medium';
                                                    } else {
                                                    c += 'large';
                                                    }
                                                    
                                                    return new L.DivIcon({ html: '<div><span>' + marker_sum + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) })
                                                    ;
                                                    }")
                              ),
                          clusterId = "casesCluster") #%>%
               # # addMarkers(label = htmlEscape(cases))
               # addLegend("bottomright",
               #           # pal = pal,
               #           # select_values = ~rexp(1000000),
               #           # colors = pal(c(0,1000,10000,100000)),
               #           # colors = pal(c(log(0),log(1),log(10),log(100),log(1000),log(10000),log(100000))),
               #           colors = heating(7),
               #           # select_values = ~ log(covid_counties_map[covid_counties_map$date == max(nyt_states$date),]$cases),
               #           # select_values = ~ (seq(0,max(covid_counties_map[covid_counties_map$date == max(nyt_states$date),]$cases), 10)),
               #           title = "Covid Cases",
               #           labels = c("0","1","10","100","1,000","10,000","10,001+"),
               #           # bins = 7,
               #           # labFormat = labelFormat(prefix = "$"),
               #           opacity = 1
               # )
               #
       })
     
     # observeEvent(input$date_select, {
     #     selected_metric = metric()
     #     # chosen_data$select_values = c(chosen_data[,selected_metric])
     #     leafletProxy("county_map") %>%
     #         clearMarkers() %>%
     #         clearMarkerClusters()
     #         addMarkers(#group="metric",
     #                    data=st_point_on_surface(chosen_data()),
     #                    options = markerOptions(cases = ~cases),
     #                    popup = ~ as.character(paste0("Cases: ",cases, " |  Deaths: ",deaths)),
     #                    label = ~as.character(paste0(county,", ", state)),
     #                    clusterOptions = markerClusterOptions(
     #                        iconCreateFunction=JS("function (cluster) {
     #                                              var markers = cluster.getAllChildMarkers();
     #                                              var marker_sum = 0;
     #                                              for (i = 0; i < markers.length; i++) {
     #                                              marker_sum += Number(markers[i].options.cases);
     #                                              //      marker_sum += 1;
     #                                              }
     # 
     #                                              var c = ' marker-cluster-';
     #                                              if (marker_sum < 100) {
     #                                              c += 'small';
     #                                              } else if (marker_sum < 10000) {
     #                                              c += 'medium';
     #                                              } else {
     #                                              c += 'large';
     #                                              }
     # 
     #                                              return new L.DivIcon({ html: '<div><span>' + marker_sum + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) })
     #                                              ;
     # }")
     #                          ),
     #                    clusterId = "casesCluster")
     # })
}

# Run the application 
shinyApp(ui = ui, server = server)

