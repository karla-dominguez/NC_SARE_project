#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load necessary libraries
library(shiny)
library(shinythemes)
library(apsimx)
library(leaflet)
library(leaflet.providers)
library(lubridate)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(dplyr)
library(sf)
library(soilDB)
library(aqp)
library(sp)
library(spData)
library(rnassqs)
library(tidygeocoder)
library(nasapower)


nassqs_auth(key = "1F35C383-BD90-319B-8AA2-6D92BBF4C9A4")

ui <- navbarPage(
  title = "OPTICORN",
  
  # Home tab
  tabPanel("Home",
           fluidPage(
             h4("Home Section"),
             p("This section contains information about this app.")
           )
  ),
  
  # Tools tab
  tabPanel("Tools",
           fluidPage(
             tags$head(
               tags$style(HTML("
          .well { 
            width: 100%; 
            height: auto; 
            margin: 0;
          }
          .input-field {
            width: 50%;
          }
          .multicol {
            column-count: 2;
            column-gap: 10px;
          }
          .section-heading {
            font-size: 16px;
            font-weight: bold;
          }
          .input-label {
            font-size: 14px;
            font-weight: bold;
          }
        "))
             ),
             
             # title
             titlePanel(h3("OPTICORN: Optimum Performance Tool for Informed Corn Nitrogen Recommendation")),
             
             # Sidebar layout with map and inputs
             sidebarLayout(
               mainPanel(
                 # Input section for geographic coordinates (horizontal layout)
                 wellPanel(
                   tags$h6(class="section-heading", "Geographic Coordinates"),
                   fluidRow(
                     column(6, div(class="input-field", numericInput("latitude", tags$span(class="input-label", "Latitude (degrees):"), value=40.47, min = -90, max = 90))),
                     column(6, div(class="input-field", numericInput("longitude", tags$span(class="input-label", "Longitude (degrees):"), value=-86.99, min = -180, max = 180)))
                   )
                 ),
                 
                 # Input section for selecting corn hybrid type (horizontal layout)
                 wellPanel(
                   tags$h6(class="section-heading", "Corn Hybrid Type"),
                   fluidRow(
                     column(12, radioButtons("cultivarType", tags$span(class="input-label", "Select Cultivar Type:"),
                                             choices = c("Short Season Hybrid", "Long Season Hybrid"), inline = TRUE))
                   )
                 ),
                 
                 # Input section for past year agricultural information (horizontal layout)
                 wellPanel(
                   tags$h6(class="section-heading", "Previous Year's Agricultural Information"),
                   fluidRow(
                     column(6, radioButtons("previousCashCrop", tags$span(class="input-label", "Select Previous Cash Crop:"),
                                            choices = c("Corn", "Soybean"), inline = FALSE)),
                     column(6, div(class="input-field", numericInput("preSeasonNitrogen", 
                                                                     tags$span(class="input-label", "Pre-Season Nitrogen Applied (lbs/acre):"),
                                                                     value = 190)))
                   )
                 ),
                 
                 # Input section for current year agricultural practices (horizontal layout)
                 wellPanel(
                   tags$h6(class="section-heading", "Current Year's Agricultural Practices"),
                   fluidRow(
                     column(6, div(class="input-field", selectInput("tillageType", tags$span(class="input-label", "Tillage Practice:"),
                                                                    choices = c("Conventional", "Strip", "No-till")))),
                     column(6, div(class="input-field", dateInput("plantingDate", tags$span(class="input-label", "Planting Date:"), value = Sys.Date())))
                   ),
                   fluidRow(
                     column(6, div(class="input-field", numericInput("seedRate", tags$span(class="input-label", "Seed Rate (seeds/acre):"), value = 32000)))
                   )
                 ),
                 
                 # Input section for nitrogen application details (horizontal layout)
                 wellPanel(
                   tags$h6(class="section-heading", "Nitrogen Application Details"),
                   fluidRow(
                     column(6, div(class="input-field", dateInput("firstNitrogenAppDate", tags$span(class="input-label", "First Application Date:"), value = Sys.Date()))),
                     column(6, div(class="input-field", numericInput("firstNitrogenAppAmount", tags$span(class="input-label", "First Application Amount (lbs/acre):"), value = 190)))
                   ),
                   fluidRow(
                     column(6, div(class="input-field", dateInput("secondNitrogenAppDate", tags$span(class="input-label", "Second Application Date (optional):"), value = Sys.Date()))),
                     column(6, div(class="input-field", numericInput("secondNitrogenAppAmount", tags$span(class="input-label", "Second Application Amount (lbs/acre) (optional):"), value = 30)))
                   )
                 ),
                 
                 # output selection for crop-related metrics
                 wellPanel(
                   tags$h6(class="section-heading", "Crop Metrics"),
                   div(class="multicol", 
                       checkboxGroupInput("cropOutput", tags$span(class="input-label", "Select Output Metric:"),
                                          choices = c("Corn Yield" = "cyield", 
                                                      "Corn Growth Stage" = "cgrowthstage", 
                                                      "Corn Nitrogen Uptake" = "cnuptake",
                                                      "Nitrate Leaching" = "no3leaching",
                                                      "Nitrous Oxide Emissions" = "n2oemissions"))
                   )
                 ),
                 
                 # output selection for soil and weather conditions
                 wellPanel(
                   tags$h6(class="section-heading", "Soil and Weather Conditions"),
                   fluidRow(
                     column(6, checkboxGroupInput("soilConditions", tags$span(class="input-label", "Select Soil Conditions:"),
                                                  choices = c("Soil Type", "Drainage Class"))),
                     column(6, checkboxGroupInput("weatherConditions", tags$span(class="input-label", "Select Weather Conditions:"),
                                                  choices = c("Cumulative Growing Degree Days", "Cumulative Precipitation")))
                   )
                 ),
                 actionButton("submit", "Submit"), width = 5),
               sidebarPanel(
                 # map output for selecting geographic coordinates
                 leafletOutput("map", width = "75%", height = "300px"),
                 # plot output for yield vs. days
                 tabsetPanel(
                   tabPanel("Crop Metrics",
                            fluidRow(
                              column(6, plotOutput("yieldPlot1", width="100%", height="250px")),
                              column(6, plotOutput("yieldPlot2", width="100%", height="250px"))
                            ),
                            fluidRow(
                              column(6, plotOutput("yieldPlot3", width="100%", height="250px")),
                              column(6, plotOutput("yieldPlot4", width="100%", height="250px"))
                            ),
                            fluidRow(
                              column(6, plotOutput("yieldPlot5", width="100%", height="250px")),
                              column(6, plotOutput("yieldPlot6", width="100%", height="250px"))
                            )
                   ),
                   tabPanel("Soil and Weather Conditions",
                            fluidRow(
                              column(6, plotOutput("yieldPlot7", width="100%", height="250px")),
                              column(6, plotOutput("yieldPlot8", width="100%", height="250px"))
                            ),
                            fluidRow(
                              column(6, plotOutput("yieldPlot9", width="100%", height="250px")),
                              column(6, plotOutput("yieldPlot10", width="100%", height="250px"))
                            )
                   )
                 )
                 , width = 7)
             )
           )
  ),
  
  # Feedback tab
  tabPanel("Feedback",
           fluidPage(
             h4("Feedback"),
             
             # Input fields for feedback form
             textInput("name", "Name:", value = ""),
             textInput("email", "Email:", value = ""),
             textAreaInput("comments", "Comments/Feedback:", value = "", width = "100%", height = "150px"),
             
             # Submit button
             actionButton("submitFeedback", "Submit")
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  # for updating the pin on map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(lat = input$latitude, lng = input$longitude)
    
  })
  
  # for updating the input boxes for lat/long
  observeEvent(input$map_click,{
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    updateTextInput(session, "latitude", value = lat)
    updateTextInput(session, "longitude", value = lng)
    
    # Update map with marker
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = lng, lat = lat)
  })
  #============================================================================map interaction ends here
  observeEvent(input$submit, {
    
    #=======================================================================================get the historical yield records
    # get location from coordinates
    coords_df <- data.frame(latitude=input$latitude, longitude=input$longitude)
    location_info <- reverse_geocode(coords_df, lat = latitude, long = longitude, method = 'osm', full_results = TRUE)
    
    # format county and state abbreviation to pass onto nassrq
    county_location <- location_info$county
    cleaned_county <- toupper(gsub(" County", "", county_location))
    state_name <- location_info$state
    state_abb <- state.abb[match(state_name, state.name)]
    
    # retrieve corn yield data
    params <- list(commodity_desc = "CORN", year__GE = 2000, state_alpha = state_abb,
                   county_name = cleaned_county, statisticcat_desc = "YIELD")
    corn_yield <- nassqs(params)
    years <- corn_yield$year
    yields_value <- corn_yield$Value
    
    # data frame with year and yield
    yield_df <- data.frame(Year = years, "Yield" = yields_value) #in bushel per acre
    print(county_location)
    print(yield_df)
    max_yield_yr <- yield_df[which.max(yield_df$Yield),]
    min_yield_yr <- yield_df[which.min(yield_df$Yield),]
    mean_yield_yr <- yield_df[which.min(abs(yield_df$Yield - mean(yield_df$Yield))),]
    #==========================================================================================yield record section done
    
    
    #========================================================================== weather data
    current_date <- Sys.Date()
    current_year <- format(current_date,"%Y")
    start_date <- paste("01-01",current_year, sep ="-")
    start_date <- as.Date(start_date, format = "%d-%m-%Y")
    en_date <- current_date - 1  # numbers can be changws based on situation
    en_date <- as.Date(en_date,format = "%d-%m-%Y")
    field.present <-get_power_apsim_met(lonlat = c(input$longitude, input$latitude), dates = c(start_date, en_date))
    print(field.present)
    
    end_date_day <- day(en_date)
    end_date_month <- month(en_date)
    end_date_year <- year(en_date)
    date_end <- as.Date(paste(end_date_year,end_date_month,end_date_day, sep = "-"))
    
    #integrating 
    st_day_past <- day(en_date) + 1 # numbers can be changws based on situation
    st_day_past <- as.character(st_day_past)
    st_month_past <- month(en_date)
    end_day_past <- "31"
    end_month_past <- "12"
    
    #******************************************************************************worst year
    worst_year <- as.character(min_yield_yr$Year)
    worst_year_stdate <- as.Date(paste(worst_year,st_month_past,st_day_past, sep = "-"), format = "%Y-%m-%d")
    worst_year_edate <- as.Date(paste(worst_year,end_month_past,end_day_past,sep = "-"), format = "%Y-%m-%d")
    field.worst <- get_power_apsim_met(lonlat = c(input$longitude, input$latitude), dates = c(worst_year_stdate, worst_year_edate))
    print("*************worst year weather*******************************")
    print(field.worst)
    
    #****************************************************************************best year
    best_year <- as.character(max_yield_yr$Year)
    best_year_stdate <- as.Date(paste(best_year,st_month_past,st_day_past, sep = "-"), format = "%Y-%m-%d")
    best_year_edate <- as.Date(paste(best_year,end_month_past,end_day_past,sep = "-"), format = "%Y-%m-%d")
    field.best <- get_power_apsim_met(lonlat = c(input$longitude, input$latitude), dates = c(best_year_stdate, best_year_edate))
    print("*************best year weather*******************************")
    print(field.best)
    
    #****************************************************************************avg year
    avg_year <- as.character(mean_yield_yr$Year)
    avg_year_stdate <- as.Date(paste(avg_year,st_month_past,st_day_past, sep = "-"), format = "%Y-%m-%d")
    avg_year_edate <- as.Date(paste(avg_year,end_month_past,end_day_past,sep = "-"), format = "%Y-%m-%d")
    field.avg <- get_power_apsim_met(lonlat = c(input$longitude, input$latitude), dates = c(avg_year_stdate, avg_year_edate))
    print("*************best year weather*******************************")
    print(field.avg)
    
    weat_present_df <- data.frame(field.present)
    #************************************************************************* weather file for worst year
    weat_wlist <- list(field.present,field.worst)
    weat_wdf <- data.frame(field.worst)
    weat_worst <- rbind(weat_present_df,weat_wdf)
    weat_worst[weat_worst==worst_year] <- end_date_year
    weat_worst <- weat_worst %>% mutate(across(everything(), ~ifelse(is.na(.), mean(.,na.rm = TRUE),.)))
    weat_worst <- weat_worst %>% distinct(day, .keep_all = TRUE)
    weat_worst_prep <- as.list(weat_worst)
    names(weat_worst) <- c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
    units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
    attr(weat_worst_prep,"colnames") <- names(weat_worst)
    attr(weat_worst_prep, "units") <- units
    attr(weat_worst_prep,"latitude") <- paste("latitude =",  input$latitude)
    attr(weat_worst_prep,"longitude") <- paste("longitude =", input$longitude)
    attr(weat_worst_prep,"tav") <- paste("tav =", mean(colMeans(weat_worst[,c("maxt","mint")], na.rm = TRUE), na.rm = TRUE))
    attributes(weat_worst_prep)
    wdate <- as.Date(paste(weat_worst$year, weat_worst$day, sep = "-"), format = "%Y-%j")
    wmnth <- as.numeric(format(wdate,"%m"))
    weat_worst$month <- wmnth
    wmtemp <- (weat_worst$maxt + weat_worst$mint)/2
    weat_worst$meantemp <- wmtemp
    met.agg<- aggregate(meantemp~wmnth, data = weat_worst, FUN = mean)
    wans <- round(max(met.agg$meantemp) - min(met.agg$meantemp),2)
    attr(weat_worst_prep,"amp") <- paste("amp", wans)
    print("here is the prepared data")
    print(weat_worst_prep, max =369999996)
    write_apsim_met(weat_worst_prep,"C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",filename = "weather_worst.met")
    
    #**************************************************************************************weather file for best year
    weat_blist <- list(field.present,field.best)
    weat_bdf <- data.frame(field.best)
    weat_best <- rbind(weat_present_df,weat_bdf)
    weat_best[weat_best==best_year] <- end_date_year
    weat_best <- weat_best %>% mutate(across(everything(), ~ifelse(is.na(.), mean(.,na.rm = TRUE),.)))
    weat_best <- weat_best %>% distinct(day, .keep_all = TRUE)
    weat_best_prep <- as.list(weat_best)
    names(weat_best) <- c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
    units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
    attr(weat_best_prep,"colnames") <- names(weat_best)
    attr(weat_best_prep, "units") <- units
    attr(weat_best_prep,"latitude") <- paste("latitude =",  input$latitude)
    attr(weat_best_prep,"longitude") <- paste("longitude =", input$longitude)
    attr(weat_best_prep,"tav") <- paste("tav =", mean(colMeans(weat_best[,c("maxt","mint")], na.rm = TRUE), na.rm = TRUE))
    attributes(weat_best_prep)
    wdate2 <- as.Date(paste(weat_best$year, weat_best$day, sep = "-"), format = "%Y-%j")
    wmnth2 <- as.numeric(format(wdate2,"%m"))
    weat_best$month <- wmnth2
    wmtemp2 <- (weat_best$maxt + weat_best$mint)/2
    weat_best$meantemp <- wmtemp2
    met.agg2<- aggregate(meantemp~wmnth2, data = weat_best, FUN = mean)
    wans2 <- round(max(met.agg2$meantemp) - min(met.agg2$meantemp),2)
    attr(weat_best_prep,"amp") <- paste("amp", wans2)
    print("here is the prepared data")
    print(weat_best_prep, max =369999996)
    write_apsim_met(weat_best_prep,"C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",filename = "weather_best.met")
    
    #**************************************************************************************weather file for average year
    weat_avglist <- list(field.present,field.avg)
    weat_avgdf <- data.frame(field.avg)
    weat_avg <- rbind(weat_present_df,weat_avgdf)
    weat_avg[weat_avg==avg_year] <- end_date_year
    weat_avg <- weat_avg %>% mutate(across(everything(), ~ifelse(is.na(.), mean(.,na.rm = TRUE),.)))
    weat_avg <- weat_avg %>% distinct(day, .keep_all = TRUE)
    weat_avg_prep <- as.list(weat_avg)
    names(weat_avg) <- c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed")
    units <- c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)", "(%)", "(m/s)")
    attr(weat_avg_prep,"colnames") <- names(weat_avg)
    attr(weat_avg_prep, "units") <- units
    attr(weat_avg_prep,"latitude") <- paste("latitude =",  input$latitude)
    attr(weat_avg_prep,"longitude") <- paste("longitude =", input$longitude)
    attr(weat_avg_prep,"tav") <- paste("tav =", mean(colMeans(weat_avg[,c("maxt","mint")], na.rm = TRUE), na.rm = TRUE))
    attributes(weat_avg_prep)
    wdate <- as.Date(paste(weat_avg$year, weat_avg$day, sep = "-"), format = "%Y-%j")
    wmnth <- as.numeric(format(wdate,"%m"))
    weat_avg$month <- wmnth
    wmtemp <- (weat_avg$maxt + weat_avg$mint)/2
    weat_avg$meantemp <- wmtemp
    met.agg<- aggregate(meantemp~wmnth, data = weat_avg, FUN = mean)
    wans <- round(max(met.agg$meantemp) - min(met.agg$meantemp),2)
    attr(weat_avg_prep,"amp") <- paste("amp", wans)
    print("here is the prepared data")
    print(weat_avg_prep, max =369999996)
    write_apsim_met(weat_avg_prep,"C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",filename = "weather_avg.met")
    
    #==============================================================================done creating weather data; three files completed
    
    #==============================================================================setting up the start and end date of simulation
    st_simulation <- gsub("-", "/",start_date)
    ed_sim <- as.Date(paste(current_year,12,30,sep = "-"), format = "%Y-%m-%d")
    end_simulation <- gsub("-", "/",ed_sim)
    print(st_simulation)
    print(end_simulation)
    
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++fetching inputs
    planting_date <- format(input$plantingDate, "%d-%b")
    first_Ndate <- format(input$firstNitrogenAppDate, "%d-%b")
    first_Namount <- input$firstNitrogenAppAmount
    
    edit_apsimx("optimum_N_rate.apsimx",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",node = "Clock",
                parm = "Start", value = st_simulation, overwrite = TRUE)
    edit_apsimx("optimum_N_rate.apsimx",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",node = "Clock",
                parm = "End", value = end_simulation, overwrite = TRUE)
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", 
                node = "Manager", manager.child = "SowMaize", parm = "SowDate", value = planting_date, overwrite = TRUE)
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", 
                node = "Manager", manager.child = "FertilizeMaize", parm = "FertiliserDates", value = first_Ndate, overwrite = TRUE)
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", 
                node = "Manager", manager.child = "FertilizeMaize", parm = "Amount", value = first_Namount, overwrite = TRUE)
    
    
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", node = "Weather",
                value = "weather_best.met", overwrite = TRUE)
    
    #================================================running the file
    
    sim_best <- apsimx("optimum_N_rate","C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                       value = "report")
    df_best <- data.frame(sim_best)
    print(df_best)
    
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", node = "Weather",
                value = "weather_worst.met", overwrite = TRUE)
    sim_worst <- apsimx("optimum_N_rate","C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                        value = "report")
    df_worst <- data.frame(sim_worst)
    print(df_worst)
    
    edit_apsimx("optimum_N_rate",src.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                wrt.dir = "C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//", node = "Weather",
                value = "weather_avg.met", overwrite = TRUE)
    sim_avg <- apsimx("optimum_N_rate","C://Users//pathak19//Desktop//Purdue_research//APSIM_purdue_Napplication//APSIM_on_R//apsimfile//",
                      value = "report")
    df_avg <- data.frame(sim_avg)
    print(df_avg)
    
    
    df_best$category <- "best"
    df_worst$category <- "worst"
    df_avg$category <- "average"
    df_final <- rbind(df_best, df_worst, df_avg)
    
    p1 <- ggplot(df_final) +
      geom_line(aes(x = df_final$Date, y = df_final$CornBuac, color = category)) + theme(text = element_text(size = 18))+ theme_bw() + labs(x = "Date", y = "Crop yield (bu/ac)")
    
    if("cyield" %in% input$cropOutput){
      output$yieldPlot1 <- renderPlot({p1})}
    
    p2 <- ggplot(df_final) +
      geom_line(aes(x = df_final$Date, y = df_final$CropBiomass, color = category)) + theme(text = element_text(size = 18))+ theme_bw() + labs(x = "Date", y = "Crop biomass and growth stage()")
    
    if("cgrowthstage" %in% input$cropOutput){
      output$yieldPlot2 <- renderPlot({p2})}
    
    p3 <- ggplot(df_final) +
      geom_line(aes(x = df_final$Date, y = df_final$CropNUptake, color = category)) + theme(text = element_text(size = 18))+ theme_bw() + labs(x = "Date", y = "Crop N uptake")
    
    if("cnuptake" %in% input$cropOutput){
      output$yieldPlot3 <- renderPlot({p3})}
    
    p4 <- ggplot(df_final) +
      geom_line(aes(x = df_final$Date, y = df_final$NO3Leaching, color = category)) + theme(text = element_text(size = 18))+ theme_bw() + labs(x = "Date", y = "NO3 leaching")
    
    if("no3leaching" %in% input$cropOutput){
      output$yieldPlot4 <- renderPlot({p4})}
    
    p5 <- ggplot(df_final) +
      geom_line(aes(x = df_final$Date, y = df_final$N2OEmissions, color = category)) + theme(text = element_text(size = 18))+ theme_bw() + labs(x = "Date", y = "N2O Emissions")
    
    
    if("n2oemissions" %in% input$cropOutput){
      output$yieldPlot5 <- renderPlot({p5})}
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
