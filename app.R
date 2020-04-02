# COVIDXIX dashboard base on NYT data that they compile daily. Will always pull latest data set
# County data geometry modified to account for NYT merging of all buroughs.

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
nyt_counties <- nyt_counties %>% 
    select(c('date', 'fips', 'date',"state","county","cases","deaths")) %>%
    group_by(state,county) %>%
    arrange(date) %>%
    mutate(new_cases = cases - lag(cases, default = first(cases), order_by = date),
           new_deaths = deaths - lag(deaths),
           death_rate = deaths/cases) %>%
    ungroup %>%
    data.frame

#NYT state level data
urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
nyt_states <- read_csv(url(urlfile))

# load county boundaries
load(file="us_counties.Rdata")

covid_counties_map <- geo_join(us_counties, nyt_counties, by_sp='GEOID', by_df = 'fips', how='inner')
covid_counties_map <- covid_counties_map 

ui <- dashboardPage( skin = "red",
    dashboardHeader(title = "Covid 19 Metrics"),
    dashboardSidebar(
        
            selectInput(inputId = 'date_select', 
                    label='Select Date',
                    choices = unique(nyt_states$date),
                    selected = max(nyt_states$date)
                    )
        ),
    dashboardBody(
        fluidPage(
                  fluidRow(
                    tabBox(selected = 'Cases and Deaths',
                           width = 12, 
                        tabPanel(title = 'Cases and Deaths',
                                 leafletOutput("county_map"),
                                 radioButtons(inputId = 'metrics_select',
                                              label = 'Select Metric',
                                              choiceNames = c('Cases','Deaths', 'New Cases', 'New Deaths'),
                                              choiceValues = c('cases','deaths', 'new_cases', 'new_deaths'),
                                              selected = "cases"),
                                 checkboxInput('county_select',
                                               label = "Show county map?*",
                                               value = FALSE),
                                 "*Takes a little bit to load"
                                 ) 
                        #Not quite there yet. Gonna work on scale later.
                        #,
                        # tabPanel(title = 'Death Rate',
                        #          leafletOutput('county_dr')
                        #          )
                        )
                    ),
                  fluidRow(
                      box(
                          width = 6,
                          "This map will contain updated data from the New York Times investigations into the spread of Covid-19.",
                          "Data collection and methods can be found on their github: ", tags$a(href="https://github.com/nytimes/covid-19-data", "https://github.com/nytimes/covid-19-data")
                      ),
                  # ),
                  # fluidRow(
                      box(
                          width = 6,
                          "The code for this project can be found on my github: ", tags$a(href="https://github.com/walshj/COVIDXIX", "https://github.com/walshj/COVIDXIX")
                      )
                  )
                  )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    chosen_date <- reactive({input$date_select})
    metric <- reactive({input$metrics_select})
    chosen_data <- reactive({
        covid_counties_map[covid_counties_map$date == input$date_select,c("state","county","cases","deaths", "new_cases", "new_deaths","death_rate","geometry")] %>%
            mutate(select_values = eval(parse(text=input$metrics_select)))
    })
    eventReactive(input$heatmap,{
            leafletProxy(county_map) %>%
                clearMarkers() %>%
                addHeatmap(data=st_point_on_surface(chosen_data()),
                           intensity = ~ select_values)
    })


     output$county_map <- 
       renderLeaflet({
           heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
           pal <- colorNumeric(
               domain = NULL,
               palette = heating(7),
               reverse=FALSE
           )
            if(input$county_select){
                leaflet(data=chosen_data()) %>%
                    addTiles() %>%
                    addPolygons(group = 'base_map', data = covid_counties_map[covid_counties_map$date == chosen_date(),], weight = .5, fillOpacity=0.2) %>%
                    addPolygons(group = 'metric',
                                data= covid_counties_map[covid_counties_map$date == chosen_date(),],
                                weight = 1,
                                fillOpacity=1,
                                color = ~pal(log(cases))
                                ) %>%
                addLegend("bottomright",
                          colors = heating(7),
                          title = "Covid Cases",
                          labels = c("0","1","10","100","1,000","10,000","10,001+"),
                          opacity = 1
                )
                    
            } else {
           leaflet(data = chosen_data()) %>%
               addTiles() %>%
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
                          clusterId = "casesCluster")
                }
       })
     output$county_dr <- 
         renderLeaflet({
             heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
             pal <- colorNumeric(
                 domain = NULL,
                 palette = heating(7),
                 reverse=FALSE
             )
             leaflet(data=chosen_data()) %>%
                 addTiles() %>%
                 addPolygons(group = 'base_map', data = covid_counties_map[covid_counties_map$date == chosen_date(),], weight = .5, fillOpacity=0.2) %>%
                 addPolygons(group = 'metric',
                             data= covid_counties_map[covid_counties_map$date == chosen_date(),],
                             weight = 1,
                             fillOpacity=1,
                             color = ~pal(log(cases))
                 ) %>%
                 addLegend("bottomright",
                           colors = heating(7),
                           title = "Covid Cases",
                           labels = c("0","1","10","100","1,000","10,000","10,001+"),
                           opacity = 1
                 )
         })
}

# Run the application 
shinyApp(ui = ui, server = server)

