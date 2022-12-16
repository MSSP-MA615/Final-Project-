
library(tidyverse)
library(magrittr)
library(plotly)
library(ggplot2)
library(leaflet)
library(mapsapi)

library(shiny)
library(devtools)
install_github("cran/MBTAr")
library(MBTAr)
RT_weekday_trips <- read.csv("RT_weekday_trips.csv")
commuter_bus_4m <- read.csv("commuter_bus_4m.csv")
Ferry_stop_time <- read.csv("Ferry_stop_time.csv")

key <- "AIzaSyDLprD-qNkDzpPf4I5KuDzARTwKrlsLzIM"
api_key <- "wX9NwuHnZU2ToO7GmGR9uw"
ui <- navbarPage("MBTA MAP",
   tabPanel("Rapid Transit Map",
  fluidPage(

  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("line","Line", RT_weekday_trips$route_id),
      uiOutput("SelectProc1"),
      uiOutput("SelectProc2"),
      textInput("Datetime", "Enter Date from 2022 10 to 2022 12:", value = "2022-11-01 12:05:00" )      
    ),
    mainPanel(
      leafletOutput("bbmap", height=300),
      plotOutput("t1", height=300)
      
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
  select <- reactive(RT_weekday_trips%>%filter(route_id==input$line))
  
  output$SelectProc1 <- renderUI(
    selectInput('origin', 'Origin', 
                choices =  select()$stop_name )
  )   
  output$SelectProc2 <- renderUI(
    selectInput('destination', 'Destination', 
                choices =  select()$stop_name
    )
  )     
  select1 <- reactive(select()[select()$stop_name==input$origin
                               |select()$stop_name==input$destination,] ) 
  
  time <- reactive(strptime(input$Datetime, "%Y-%m-%d %H:%M:%OS")) 
  output$bbmap <- renderLeaflet({
    
    origin1 <- reactive(select1()[select1()$stop_name==input$origin,])
    destination1 <- reactive(select1()[select1()$stop_name==input$destination,])
    
    travel_time <- Ttraveltimes(as.numeric(origin1()$stop_id[1]) , 
                                as.numeric(destination1()$stop_id[1]), 
                                route_id = input$line, time() - 600, time()+600, api_key ) 
    pred_time <- as.numeric(travel_time$travel_time_sec[1]) 
    
    doc <- mp_directions(
      origin = c(origin1()$stop_lon[1], origin1()$stop_lat[1]),
      destination = c(destination1()$stop_lon[1], destination1()$stop_lat[1]),
      alternatives = FALSE,
      mode = "transit",
      key = key,
      transit_mode = "subway",
      quiet = TRUE
    ) 
    routes <- mp_get_routes(doc)
    
    
    palette = colorFactor(palette = "Set2", domain = routes$alternative_id) 
    leaflet() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolylines(data = routes, 
                   opacity = 1, 
                   weight = 7, 
                   color = ~palette(alternative_id),
                   label = paste(pred_time,"sec"),
                   labelOptions = labelOptions(noHide = TRUE))
    
  }) 
  
  origin1 <- reactive(select1()[select1()$stop_name==input$origin,])
  destination1 <- reactive(select1()[select1()$stop_name==input$destination,])
  output$t1 <- renderPlot(
    

    print(Ttraveltimes(as.numeric(origin1()$stop_id[1]) , 
                                as.numeric(destination1()$stop_id[1]), 
                                route_id = input$line, time() - 3600, time()+3600, api_key ) %>%
      ggplot(aes(as.numeric(travel_time_sec))) +
      geom_histogram(bins = 10, colour="black",fill="white")+
        xlab("travel time")+ggtitle("Travel Time Counts in Two Hours")))
  
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
