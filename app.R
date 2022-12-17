
library(tidyverse)
library(magrittr)
library(plotly)
library(ggplot2)
library(leaflet)
library(mapsapi)
library(shiny)
library(shinythemes)

trips_stopstimes <- read.csv("trips_stopstimes.csv")
subway_4m <- read.csv("subway_4m.csv")
commuter_bus_4m <- read.csv("commuter_bus_4m.csv")
Ferry_stop_time <- read.csv("Ferry_stop_time.csv")
red_stoptime <- trips_stopstimes %>% filter(route_id=="Red")
greenb_stoptime <- trips_stopstimes %>% filter(route_id=="Green-B")
greenc_stoptime <- trips_stopstimes %>% filter(route_id=="Green-C")
greend_stoptime <- trips_stopstimes %>% filter(route_id=="Green-D")
greene_stoptime <- trips_stopstimes %>% filter(route_id=="Green-E")
blue_stoptime <- trips_stopstimes %>% filter(route_id=="Blue")
orange_stoptime <- trips_stopstimes %>% filter(route_id=="Orange")
red_stop <- red_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
greenb_stop <- greenb_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
greenc_stop <- greenc_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
greend_stop <- greend_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
greene_stop <- greene_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
blue_stop <- blue_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()
orange_stop <- orange_stoptime %>% select(stop_name, stop_lat, stop_lon) %>% unique()

key <- "AIzaSyDLprD-qNkDzpPf4I5KuDzARTwKrlsLzIM"

ui <- navbarPage(theme = shinytheme('lumen'), "MBTA MAP",
            navbarMenu("Rapid Transit",
                       
                       tabPanel("Total Routes Map",
                                
                                mainPanel(leafletOutput("m1"))
                       ),
                       
                       tabPanel("direction Route Map",
                fluidPage(  tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          
                          
                sidebarLayout(
                            
                sidebarPanel(selectInput("line","Line",unique(subway_4m$route_id)),
                              selectInput("datetime", "Select Datetime:", unique(subway_4m$service_date)),
                              uiOutput("SelectProc1"),
                              uiOutput("SelectProc2"),  
                                        
                              
                            ),
                            mainPanel(
                              leafletOutput("bbmap", height=300),
                              plotOutput("t1") 
                              
                            )
                            
                          )
                          
                          )
                 )
                ),

tabPanel("Commuter BUS MAP",
         sidebarLayout(
           sidebarPanel(
             selectInput("date", "Select Date:",  
                         commuter_bus_4m$service_date, selected = "2022-01-03"),
             uiOutput("SelectProc3"),
             uiOutput("SelectProc4"),
             uiOutput("SelectProc5"),
             uiOutput("SelectProc6"),
             uiOutput("SelectProc7")
             
           ),
           mainPanel(
             leafletOutput("t2") 
           )
         )
),

tabPanel("Ferry Map",
         sidebarLayout(      
           sidebarPanel(
             selectInput("date1", "Select Date:", choices = unique(Ferry_stop_time$service_date)),
             uiOutput("SelectProc8"),
             uiOutput("SelectProc9")
             
           ),
           mainPanel(
             leafletOutput("t3", height = 300),
             plotOutput("t4")
             
           )
         )
) 

)
server <- function(input, output){
  
  output$m1 <- renderLeaflet(
    leaflet() %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addPolylines(as.numeric(red_stop[1:17,]$stop_lon),as.numeric(red_stop[1:17,]$stop_lat),color="red") %>%
      addCircleMarkers(as.numeric(red_stop[1:17,]$stop_lon),as.numeric(red_stop[1:17,]$stop_lat),radius=1,popup = red_stop[1:17,]$stop_name) %>%
      
      addPolylines(as.numeric(red_stop[18:23,]$stop_lon),as.numeric(red_stop[18:23,]$stop_lat),color="red") %>%
      addCircleMarkers(as.numeric(red_stop[18:23,]$stop_lon),as.numeric(red_stop[18:23,]$stop_lat),radius=1,popup = red_stop[18:23,]$stop_name)%>%
      
      addPolylines(as.numeric(greenb_stop[1:23,]$stop_lon),as.numeric(greenb_stop[1:23,]$stop_lat),color="green") %>%
      addCircleMarkers(as.numeric(greenb_stop[1:23,]$stop_lon),as.numeric(greenb_stop[1:23,]$stop_lat),radius=1,popup = greenb_stop[1:23,]$stop_name)%>%
      
      addPolylines(as.numeric(greenc_stop[7:20,]$stop_lon),as.numeric(greenc_stop[7:20,]$stop_lat),color="green") %>%
      addCircleMarkers(as.numeric(greenc_stop[7:20,]$stop_lon),as.numeric(greenc_stop[7:20,]$stop_lat),radius=1,popup = greenc_stop[7:20,]$stop_name)%>%
      
      addPolylines(as.numeric(greend_stop[1:25,]$stop_lon),as.numeric(greend_stop[1:25,]$stop_lat),color="green") %>%
      addCircleMarkers(as.numeric(greend_stop[1:25,]$stop_lon),as.numeric(greend_stop[1:25,]$stop_lat),radius=1,popup = greend_stop[1:25,]$stop_name)%>%
      
      addPolylines(as.numeric(greene_stop[1:20,]$stop_lon),as.numeric(greene_stop[1:20,]$stop_lat),color="green") %>%
      addCircleMarkers(as.numeric(greene_stop[1:20,]$stop_lon),as.numeric(greene_stop[1:20,]$stop_lat),radius=1,popup = greene_stop[1:20,]$stop_name)%>%
      
      addPolylines(as.numeric(blue_stop[1:12,]$stop_lon),as.numeric(blue_stop[1:12,]$stop_lat),color="blue") %>%
      addCircleMarkers(as.numeric(blue_stop[1:12,]$stop_lon),as.numeric(blue_stop[1:12,]$stop_lat),radius=1,popup = blue_stop[1:12,]$stop_name)%>%
      
      addPolylines(as.numeric(orange_stop[1:20,]$stop_lon),as.numeric(orange_stop[1:20,]$stop_lat),color="orange") %>%
      addCircleMarkers(as.numeric(orange_stop[1:20,]$stop_lon),as.numeric(orange_stop[1:20,]$stop_lat),radius=1,popup = orange_stop[1:20,]$stop_name)
    
  )
  
  select <- reactive(subway_4m %>% filter(route_id==input$line&service_date==input$datetime))
  output$SelectProc1 <- renderUI(
    selectInput('origin', 'Origin', 
                choices =  select()$from_stop_name )
  )   
  
  s1 <- reactive(select()%>%filter(from_stop_name==input$origin))
  output$SelectProc2 <- renderUI(
    selectInput('destination', 'Destination', 
                choices =  s1()$stop_name
    )
  )
  
  
  s2 <- reactive(s1()%>% filter(stop_name==input$destination))
  pred_time <- reactive(round(mean(s2()$travel_time_sec)) ) 
  doc <- reactive( mp_directions(
    origin = c(s2()$from_stop_lon[1], s2()$from_stop_lat[1]),
    destination = c(s2()$stop_lon[1], s2()$stop_lat[1]),
    alternatives = FALSE,
    mode = "transit",
    key = key,
    transit_mode = "subway",
    quiet = TRUE
  ))
  routes <-  reactive(mp_get_routes(doc())) 
  palette <- reactive(colorFactor(palette = "Set2", domain = routes()$alternative_id))  
  output$bbmap <- renderLeaflet(
    leaflet() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolylines(data = routes(), 
                   opacity = 1, 
                   weight = 7, 
                   color = ~palette()(alternative_id),
                   label = paste(pred_time(),"sec"),
                   labelOptions = labelOptions(noHide = TRUE))
    
    
  )
  
  
  output$t1 <- renderPlot(
    s2() %>% ggplot(aes(as.numeric(travel_time_sec))) +
      geom_histogram( colour="black",fill="white")+
      xlab("travel time")+ggtitle("Travel Time distribution"))
  
  select3 <- reactive(commuter_bus_4m %>% filter(service_date==input$date))
  output$SelectProc3 <- renderUI(
    selectInput('route', 'Select route', 
                choices =  select3()$route_id )
  )
  select4 <- reactive(select3()%>% filter(route_id==input$route))
  output$SelectProc4 <- renderUI(
    selectInput('trip', 'Select Trip:', 
                choices =  select4()$half_trip_id )
  )
  select5 <- reactive(select4()%>% filter(half_trip_id==input$trip))
  output$SelectProc5 <- renderUI(
    selectInput('direction', 'Select direction:', 
                choices =  select5()$direction_id )
  )
  select6 <- reactive(select5()%>% filter(direction_id==input$direction))
  output$SelectProc6 <- renderUI(
    selectInput('origion1', 'Select Origin:', 
                choices =  select6()$stop_name)
  )
  
  output$SelectProc7 <- renderUI(
    selectInput('destination1', 'Select Destination:', 
                choices =  select6()$stop_name)
  )
  
  origin2 <- reactive(select6()%>% filter(stop_name==input$origion1))
  
  destination2 <- reactive(select6()%>% filter(stop_name==input$destination1))
  order1 <- reactive(origin2()$time_point_order)
  order2 <- reactive(destination2()$time_point_order)
  time1 <- reactive(strptime(origin2()$scheduled, "%Y-%m-%d %H:%M:%OS"))
  time2 <- reactive(strptime(destination2()$scheduled, "%Y-%m-%d %H:%M:%OS"))
  diff1 <- reactive(abs(as.double(time2()-time1())))
  
  output$t2 <- renderLeaflet( 
    
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
    addPolylines(as.numeric(select6()[order1():order2(),]$stop_lon),
                 as.numeric(select3()[order1():order2(),]$stop_lat),
                 color="red", label = paste(diff1(),"min"),
                 labelOptions = labelOptions(noHide = TRUE))%>%
      addCircleMarkers(as.numeric(select6()[order1():order2(),]$stop_lon),
                       as.numeric(select3()[order1():order2(),]$stop_lat),
                       radius=1,popup = select6()[order1():order2(),]$stop_name)
    
    
    
  )
 
  u1 <- reactive(Ferry_stop_time %>% filter(service_date==input$date1))
  output$SelectProc8 <- renderUI(selectInput("departure","Select Departure: ", choices = u1()$departure_terminal))
  u3 <- reactive(u1() %>% filter(departure_terminal==input$departure))
  output$SelectProc9 <- renderUI(selectInput("arrival","Select Arrival: ", choices = u3()$arrival_terminal))
  u2 <- reactive(u1() %>% filter(departure_terminal==input$departure & arrival_terminal==input$arrival))   
  lon <- reactive(c(unique(u2()$stop_lon_depa), unique(u2()$stop_lon_arrival)))
  lat <- reactive(c(unique(u2()$stop_lat_depa), unique(u2()$stop_lat_arrival)))
  Ferry_time <- reactive(as.numeric(u2()$travel_time)) 
  
  output$t3 <- renderLeaflet(
    leaflet() %>% 
      setView(lng = -71.0589, lat = 42.3601, zoom = 10) %>% 
      addProviderTiles("CartoDB.Positron")%>%
      addCircleMarkers(lon(),lat(),radius=1,popup = c(unique(u2()$departure_terminal), unique(u2()$arrival_terminal))) %>%
      addPolylines(lon(),lat(),color="orange", label = paste(round(mean(Ferry_time())),"min"), labelOptions = labelOptions(noHide = TRUE)
      )
    
  )
  tim <- reactive(data.frame(x=Ferry_time()))
  output$t4 <- renderPlot(
    
    hist(tim()$x, col="grey", main="Travel Time Distribution",
         xlab= "Travel Time in min"
    )
  )   
  
}

shinyApp(ui, server)
