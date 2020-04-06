# COVIDXIX dashboard base on NYT data that they compile daily. Will always pull latest data set
# County data geometry modified to account for NYT merging of all buroughs.

library(shiny)
library(sf)
library(dplyr)
library(readr)
library(leaflet) # for interactive maps
library(tigris)
library(shinydashboard)


nyt_states <- read_csv(file = "nyt_states.csv") %>% data.frame
if(max(nyt_states$date) < Sys.Date() - 1){
    print('fail')
    urlfile="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
    nyt_counties <- read_csv(urlfile)
    save(file="nyt_counties.Rdata", nyt_counties)
    
    urlfile = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    nyt_states <- read_csv(urlfile)
    write.csv(nyt_states, "nyt_states.csv",
              row.names=FALSE)
} else {
    load(file = "nyt_counties.Rdata")
}

nyt_counties[nyt_counties$county == 'New York City','fips'] <- 99999
nyt_counties <-  nyt_counties %>% 
    select(c('date', 'fips', 'date',"state","county","cases","deaths")) %>%
    group_by(state,county) %>%
    arrange(date) %>%
    mutate(new_cases = cases - lag(cases, default = first(cases), order_by = date),
           new_deaths = deaths - lag(deaths),
           death_rate = deaths/cases) %>%
    ungroup %>%
    data.frame


# load county boundaries
load(file = "us_counties.Rdata")

covid_counties_map <- geo_join(us_counties, nyt_counties, by_sp='GEOID', by_df = 'fips', how='inner')
# covid_counties_map <- covid_counties_map 

ui <- dashboardPage( skin = "red",
    dashboardHeader(title = "Covid 19 Metrics"),
    dashboardSidebar(
            selectInput(inputId = 'date_select', 
                    label='Select Date',
                    choices = unique(nyt_states$date),
                    selected = max(nyt_states$date)
                    ),
            radioButtons(inputId = 'metrics_select',
                         label = 'Select Metric',
                         choiceNames = c('Cases','Deaths', 'New Cases', 'New Deaths'),
                         choiceValues = c('cases','deaths', 'new_cases', 'new_deaths'),
                         selected = "cases"),
            checkboxInput('county_select',
                          label = "Show county map?*",
                          value = FALSE),
            "*Takes a little bit to load"
        ),
    dashboardBody(
        fluidPage(
                  fluidRow(
                    tabBox(selected = 'Cases and Deaths',
                           width = 12, 
                        tabPanel(title = 'Cases and Deaths',
                                 leafletOutput("county_map")
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
        covid_counties_map[covid_counties_map$date == input$date_select,] %>%
            select(c("state","county","cases","deaths", "new_cases", "new_deaths", "geometry"))
    })

     output$county_map <- renderLeaflet({
         heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
             pal <- colorNumeric(
                 domain = NULL,
                 palette = heating(7),
                 reverse=FALSE
             )
             qpal <- colorQuantile(
                 domain = NULL,
                 palette = heating(11),
                 n = 11,
                 reverse=FALSE
             )


           leaflet(data = st_point_on_surface(chosen_data())) %>%
               addTiles() %>%
             setView(-100,45, zoom = 2.5) %>%
         # clearMarkers %>%
         # clearMarkerClusters() %>%
         addMarkers(group="metric",
                    data=st_point_on_surface(chosen_data()),
                    options = markerOptions(metric = ~ eval(as.symbol(input$metrics_select))),
                    popup = ~ as.character(paste0("Cases: ",cases, " |  Deaths: ",deaths)),
                    label = ~ as.character(paste0(county,", ", state)),
                    clusterOptions = markerClusterOptions(
                        iconCreateFunction=JS("function (cluster) {
                                              var markers = cluster.getAllChildMarkers();
                                              var marker_sum = 0;
                                              for (i = 0; i < markers.length; i++) {
                                              marker_sum += Number(markers[i].options.metric);
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
                                              ;}")
                        ),
                    clusterId = "casesCluster"
                        ) %>%
             addPolygons(group = 'county_metric', data = chosen_data(), weight = .5, fillOpacity=0.2) %>%
             addPolygons(group = 'county_metric',
                         data= chosen_data(),
                         weight = 1,
                         fillOpacity=.6,
                         color = ~pal((eval(as.symbol(input$metrics_select))))
             ) %>%
             addLegend("bottomright",
                       group = 'county_metric',
                       pal = pal,
                       title = paste0(input$metrics_select),
                       values = ~eval(as.symbol(input$metrics_select)),
                       # labels =  ~eval(as.symbol(input$metrics_select)),
                       opacity = 1
             ) %>%
             addLayersControl(
                    overlayGroups = c('county_metric', 'metric'),
                    options = layersControlOptions(collapsed = FALSE)
                )
            
           })
     # observe({
     #    metrics <- input$metrics_select
     #     leafletProxy("county_map", data = st_point_on_surface(chosen_data())) %>%
     #         # clearMarkers() %>%
     #         # clearMarkerClusters() %>%
     #         addMarkers(
     #                # data=st_point_on_surface(chosen_data()),
     #                options = markerOptions(select_values = ~ select_values),
     #                popup = ~ as.character(paste0("Cases: ",cases, " |  Deaths: ",deaths)),
     #                label = ~ as.character(paste0(county,", ", state)),
     #                clusterOptions = markerClusterOptions(
     #                    iconCreateFunction=JS("function (cluster) {
     #                                                var markers = cluster.getAllChildMarkers();
     #                                                var marker_sum = 0;
     #                                                for (i = 0; i < markers.length; i++) {
     #                                                  marker_sum += Number(markers[i].options.select_values);
     #                                            //      marker_sum += 1;
     #                                                }
     # 
     #                                                var c = ' marker-cluster-';
     #                                                if (marker_sum < 100) {
     #                                                c += 'small';
     #                                                } else if (marker_sum < 10000) {
     #                                                c += 'medium';
     #                                                } else {
     #                                                c += 'large';
     #                                                }
     # 
     #                                                return new L.DivIcon({ html: '<div><span>' + marker_sum + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) })
     #                                                ;
     #                                                }")
     #                ),
     #                clusterId = "casesCluster")
     # 
     #              })
               
                
           # if(input$county_select){
           #     leaflet(data=chosen_data()) %>%
           #         addTiles() %>%
           #         addPolygons(group = 'base_map', data = covid_counties_map[covid_counties_map$date == chosen_date(),], weight = .5, fillOpacity=0.2) %>%
           #         addPolygons(group = 'metric',
           #                     data= covid_counties_map[covid_counties_map$date == chosen_date(),],
           #                     weight = 1,
           #                     fillOpacity=.6,
           #                     color = ~pal(log(cases))
           #         ) %>%
           #         addLegend("bottomright",
           #                   colors = heating(7),
           #                   title = "Covid Cases",
           #                   labels = c("0","1","10","100","1,000","10,000","10,001+"),
           #                   opacity = 1
           #         )
           #     
           # } else {
           

     # observe({
     #     proxy <- leafletProxy("county_map", data = chosen_data())
     #     heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
     #     pal <- colorNumeric(
     #         domain = NULL,
     #         palette = heating(7),
     #         reverse=FALSE
     #     )      
     #     qpal <- colorQuantile(
     #         domain = NULL,
     #         palette = heating(11),
     #         n = 11,
     #         reverse=FALSE
     #     )
     #     
     #     # Remove any existing legend, and only if the legend is
     #     # enabled, create a new one.
     #     if (input$county_select) {
     #         if(input$metrics_select == 'cases'){
     #             proxy %>%
     #                 clearControls() %>%
     #                 clearMarkers() %>%
     #                 clearMarkerClusters() %>%
     #                 addPolygons(group = 'base_map', data = chosen_data(), weight = .5, fillOpacity=0.2) %>%
     #                 addPolygons(group = 'metric',
     #                             data= chosen_data(),
     #                             weight = 1,
     #                             fillOpacity=.6,
     #                             color = ~pal(log(eval(as.symbol(input$metrics_select))))
     #                 ) %>%
     #                 addLegend("bottomright",
     #                           colors = heating(7),
     #                           title = "Covid Cases",
     #                           labels = c("0","1","10","100","1,000","10,000","10,001+"),
     #                           opacity = 1
     #                 ) %>%
     #                 # setView(lng = input$county_map_center$lng, lat = input$county_map_center$lat, zoom = input$county_map_zoom) 
     #             print(data.frame(chosen_data())[,input$metrics_select])
     #         } else{
     #             color_values <- unique(as.data.frame(chosen_data())[,metric()])
     #             proxy %>%
     #                 # clearControls() %>%
     #                 # clearMarkers() %>%
     #                 # clearMarkerClusters() %>%
     #                     addPolygons(group = 'base_map', data = chosen_data(), weight = .5, fillOpacity=0.2) %>%
     #                     addPolygons(group = 'county_fill',
     #                                 data= chosen_data(),
     #                                 weight = 1,
     #                                 fillOpacity=.6,
     #                                 color = ~pal(eval(as.symbol(input$metrics_select)))
     #                     ) %>%
     #                     addLegend("bottomright",
     #                               # colors = heating(7),
     #                               pal = pal,
     #                               title = "Covid Cases",
     #                               values = ~eval(as.symbol(input$metrics_select)),
     #                               # labels = c("0","1","10","100","1,000","10,000","10,001+"),
     #                               opacity = 1,
     #                               group = 'county_fill'
     #                     ) #%>%
     #             # setView(lng = input$county_map_center$lng, lat = input$county_map_center$lat, zoom = input$county_map_zoom) 
     #             print(input$county_map_center$lng)
     #             print(input$county_map_center$lat)
     #         
     #         }
     #         } else if(!input$county_select) {
     #         print(names(chosen_data()))
     #         proxy %>%
     #             # clearShapes() %>%
     #             # clearControls() %>%
     #             clearMarkers() %>%
     #             addMarkers(
     #                    data=st_point_on_surface(chosen_data()),
     #                    options = markerOptions(metric = ~ eval(as.symbol(input$metrics_select))),
     #                    popup = ~ as.character(paste0("Cases: ",cases, " |  Deaths: ",deaths)),
     #                    label = ~ as.character(paste0(county,", ", state)),
     #                    clusterOptions = markerClusterOptions(
     #                        iconCreateFunction=JS(paste0("function (cluster) {
     #                                                    var markers = cluster.getAllChildMarkers();
     #                                                    var marker_sum = 0;
     #                                                    for (i = 0; i < markers.length; i++) {
     #                                                      marker_sum += Number(markers[i].options.metric);
     #                                                //      marker_sum += 1;
     #                                                    }
     # 
     #                                                    var c = ' marker-cluster-';
     #                                                    if (marker_sum < 100) {
     #                                                    c += 'small';
     #                                                    } else if (marker_sum < 10000) {
     #                                                    c += 'medium';
     #                                                    } else {
     #                                                    c += 'large';
     #                                                    }
     # 
     #                                                    return new L.DivIcon({ html: '<div><span>' + marker_sum + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) })
     #                                                    ;
     #                                                    }"))
     #                    ),
     #                        clusterId = "casesCluster") #%>%
     #             # setView(lng = input$county_map_center$lng, lat = input$county_map_center$lat, zoom = input$county_map_zoom) 
     #     }
     # 
     #     # } else {
     #     #     proxy %>%
     #     #         clearControls() %>%
     #     #         clearMarkers() %>%
     #     #         clearMarkerClusters() %>%
     #     #         setView(lng = input$county_map_center$lng, lat = input$county_map_center$lat, zoom = input$county_map_center_zoom) %>%
     #     #         addPolygons(group = 'base_map', data = chosen_data(), weight = .5, fillOpacity=0.2) %>%
     #     #         addPolygons(group = 'metric',
     #     #                     data= chosen_data(),
     #     #                     weight = 1,
     #     #                     fillOpacity=.6,
     #     #                     color = ~pal(eval(as.symbol(input$metrics_select)))
     #     #         ) %>%
     #     #         addLegend("bottomright",
     #     #                   # colors = heating(7),
     #     #                   palette = ~ pal(eval(as.symbol(input$metrics_select))),
     #     #                   title = paste0("Number of ", input$metrics_select),
     #     #                   # labels = c("0","1","10","100","1,000","10,000","10,001+"),
     #     #                   opacity = 1
     #     #         )
     #     # }
     # })
     #    observe({
     #        print(input$county_map_zoom)
     #        print(input$county_map_center)
     #        })
     # Haven't added access to this yet. It still ends up looking off. Need to fix scale first
     # output$county_dr <- 
     #     renderLeaflet({
     #         heating <- colorRampPalette(c("blue","yellow","orange","red"), bias=.8)
     #         pal <- colorNumeric(
     #             domain = NULL,
     #             palette = heating(7),
     #             reverse=FALSE
     #         )
     #         leaflet(data=chosen_data()) %>%
     #             addTiles() %>%
     #             addPolygons(group = 'base_map', data = covid_counties_map[covid_counties_map$date == chosen_date(),], weight = .5, fillOpacity=0.2) %>%
     #             addPolygons(group = 'metric',
     #                         data= covid_counties_map[covid_counties_map$date == chosen_date(),],
     #                         weight = 1,
     #                         fillOpacity=1,
     #                         color = ~pal(log(cases))
     #             ) %>%
     #             addLegend("bottomright",
     #                       colors = heating(7),
     #                       title = "Covid Cases",
     #                       labels = c("0","1","10","100","1,000","10,000","10,001+"),
     #                       opacity = 1
     #             )
     #     })
}

# Run the application 
shinyApp(ui = ui, server = server)

