library("shiny")
library("dplyr")
library("ggplot2")
library("lubridate")
library("mapproj")
library("rsconnect")

df_crime <- read.csv(file = "data/crime.csv", sep = ",", stringsAsFactors = FALSE)
df_911 <- read.csv(file = "data/SEAfire911.csv", sep = ",", stringsAsFactors = FALSE)
df_911_3 <- df_911
  

server <- function(input, output) {
  
  ## overview plots
  # data preparation of 911 dataset
  df_911$Datetime <- as.POSIXct(df_911$Datetime, format = "%m/%d/%Y")
  df_911$Year <- as.character(year(df_911$Datetime))
  df_911$Month <- month(df_911$Datetime)
  df_911$Month <- month.abb[df_911$Month]
  
  df_911 %>% select(Incident.Number, Year, Month) -> data_911
  data_911 %>% 
    group_by(Year) %>%
    summarise(freq = n()) -> data_911_year
  
  
  # data preparation of crime dataset
  df_crime$Occurred.Date <- as.Date(df_crime$Occurred.Date, format = "%m/%d/%Y")
  df_crime$Year <- as.character(year(df_crime$Occurred.Date))
  df_crime$Month <- month(df_crime$Occurred.Date)
  df_crime$Month <- month.abb[df_crime$Month]
  df_crime %>% 
    select(Report.Number, Year, Month) %>%
    filter(Year == "2017" | Year == "2018")-> data_crime
  data_crime %>% 
    group_by(Year) %>%
    summarise(freq = n())  -> data_crime_year
  
  
  # plot of firecalls
  
  output$firecall <- renderPlot({
    # overview of 2017 and 2018
    if(input$firecall == 1){
      ggplot(data_911_year) +
        geom_col(width = 0.85) +
        aes(Year, freq, fill = Year) +
        geom_text(label = data_911_year$freq) +
        ggtitle("Fire Calls Overview of 2017 and 2018") +
        labs(x = "Year", y = "Reports") +
        scale_fill_manual(
          breaks = data_911_year$Year,
          values = c("indianred3", "sienna2"),
          labels = c("2017", "2018")
        ) +
        theme(panel.background = element_rect(fill = "grey98")) +
        coord_fixed(ratio = 1/40000)
    }
    
    # months in the given year
    else {
      data_911 %>%
        filter(Year == input$firecall) %>%
        group_by(Month) %>% 
        summarise(freq = n()) %>% 
        arrange(Month) -> data_911_month
      data_911_month$Month <- as.factor(data_911_month$Month)
      color_vec <- c("indianred3", "sienna2")
      if(input$firecall == "2017"){
        color <- color_vec[1]
      }
      else{
        color <- color_vec[2]
      }
      ggplot(data_911_month) + 
        geom_col(fill = color, width = 0.85) + 
        aes(x = Month, y = freq, fill = Month) +
        geom_text(label = data_911_month$freq) +
        ggtitle(paste("911 Fire Calls in", input$firecall, "from January to December")) +
        labs(x = "Month", y = "Reports") +
        theme(panel.background = element_rect(fill = "grey98")) +
        coord_fixed(ratio = 1/1000)
      
    }
  })
  
  # plot of crime
  output$crime <- renderPlot({
    if(input$crime == 1){
      ggplot(data_crime_year) +
        geom_col(width = 0.85) +
        aes(Year, freq, fill = Year) +
        geom_text(label = data_crime_year$freq) +
        ggtitle("Crime Overview of 2017 and 2018") +
        labs(x = "Year", y = "Reports") +
        scale_fill_manual(
          breaks = data_crime_year$Year,
          values = c("dodgerblue3", "steelblue3"),
          labels = c("2017", "2018")
        ) +
        theme(panel.background = element_rect(fill = "grey98"))+
        coord_fixed(ratio = 1/25000)
    }
    
    # months in the given year
    else {
      data_crime %>%
        filter(Year == input$crime) %>%
        group_by(Month) %>% 
        summarise(freq = n()) %>% 
        arrange(Month) -> data_crime_month
      data_crime_month$Month <- as.factor(data_crime_month$Month)
      color_vec <- c("dodgerblue3", "steelblue3")
      if(input$crime == "2017"){
        color <- color_vec[1]
      }
      else{
        color <- color_vec[2]
      }
      ggplot(data_crime_month) + 
        geom_col(fill = color, width = 0.85) + 
        aes(x = Month, y = freq, fill = Month) +
        geom_text(label = data_crime_month$freq) +
        ggtitle(paste("Crimes in", input$firecall, "from January to December")) +
        labs(x = "Month", y = "Reports") +
        theme(panel.background = element_rect(fill = "grey98")) +
        coord_fixed(ratio = 1/500)
      
    }
    
    
  })

  ## 2 crime dataset exploration plots
  
  ## 2.1 pie chart
 
  # data preparation
  
  uniqueNeighborhoods <- unique(df_crime$Neighborhood)
  
  fullvector <- c()
  for (one in uniqueNeighborhoods) {
    one <- nrow(df_crime %>% 
                  filter(Neighborhood == one))
    fullvector <- c(fullvector, one)
  } 
  final <- data.frame("Neighborhood Name" = uniqueNeighborhoods, "Crime Amount" = fullvector)
  final <- final[order(-(final$Crime.Amount)),]
  top10<- slice(final, 1:10)
  
  final2 <- final[order(final$Crime.Amount),]
  least10<- slice(final2, 1:10)
  
  # pie chart
  
  output$pie <- renderPlot({
    if (input$rank == "top10") {
      
      pie(top10$Crime.Amount, 
          col = c("#ccffb2", "#feff9e", "#aaebf9", "#f5c1ff", "#ffa7dc"),
          labels = top10$Neighborhood.Name, 
          main = paste0("Top 10 Crime Neighborhoods"),
          cex = .6
          )
    }
    else {
      
      pie(least10$Crime.Amount, 
          col = c("#fc3232", "#13d604", "#ffe74d", "#ff8c4a", "#2b8fef"),
          labels = least10$Neighborhood.Name, 
          main = paste0("Last 10 Crime Neighborhoods"),
          cex = .6
          )
    }
  })
  
  
  # 2.2 bar chart
  
  # data preparation
  input_data <- reactive({ 
    #filters the crime dataset by crime type 
    type.data <- filter(df_crime, df_crime$Crime.Subcategory == input$type)
    time.category <- c("morning", "afternoon", "night", "midnight")
    #count the filteded dataset by the frequency of a specific crime that took place in the 
    #four time periods. Morning indicates from 8:00 to 11:49, afternoon indicates from 12:00 to 16:59
    #night indicates from 17:00 to 23:59 and midnight implies from 00:00 to 07:59. 
    #makes a vector composed of frequency of four time periods
      morning.freq <- nrow(type.data) - 
        count(type.data, 
              type.data$Occurred.Time >= 800 & type.data$Occurred.Time < 1200)[[1,2]]
      afternoon.freq <- nrow(type.data) - 
        count(type.data, 
              type.data$Occurred.Time >= 1200 & type.data$Occurred.Time < 1700)[[1,2]]
      night.freq <- nrow(type.data) - 
        count(type.data, 
              type.data$Occurred.Time >= 1700 & type.data$Occurred.Time <= 2359)[[1,2]]
      midnight.freq <- nrow(type.data) - 
        count(type.data, 
              type.data$Occurred.Time >= 0 & type.data$Occurred.Time < 800)[[1,2]]
      #makes a vector out of the four time period
      freq.crime <- c(morning.freq, afternoon.freq, night.freq, midnight.freq)
      #make a data frame table out of four time periods 
      time.table <- data.frame(time.category, freq.crime)
      return(time.table)
      
  
    
    
  })
  
  # bar chart
  output$barPlot_state <- renderPlot({
    #input from the filtered dataset according the type of crime 
    time.table <- input_data()
    barplot(
      time.table$freq.crime, 
      horiz = TRUE,
      main = "Time and frequency of crimes",
      las = 0,
      names.arg = c("Morning", "Afternoon", "Night", "Midnight"),
      xlab = "Frequency of crimes",
      col= c("mistyrose1", "mistyrose2", "mistyrose3", "mistyrose4")
      )
    
  })
  
  
  
  ## 3 911 dataset plot(map)
  
  # data preparation
  pick_data <- reactive({
    map_data <- df_911_3
    #Finds year
    map_data$Datetime = substr(map_data$Datetime, start = 1, stop = 10)
    year <- year(as.POSIXlt(map_data$Datetime, format="%m/%d/%Y"))
    
    #Maniplutes the data to a more condensed, efficent form
    map_data <- map_data %>% 
      mutate(Year = year) %>%
      #Includes only selected year
      filter(Year == input$year) %>%
      add_count(Address) %>%
      mutate(Calls = n) %>%
      select(Year, Latitude, Longitude, Calls) %>%
      #Removes a couple extreme outliers
      filter(Latitude > 47.45, Latitude < 47.75, Longitude < -122.225)
    
    #Removes incomplete data
    map_data[complete.cases(map_data), ]
  }) 
  
  # map plot
  output$map <- renderPlot({
    map_data = pick_data()
    
    #Picks and prints color
    if (input$color == "blue"|input$color == "green") {
      map_colors = c(paste0("light",input$color), input$color, paste0("dark",input$color), "black")
    } 
    else if (input$color == "red"){
      map_colors = c("pink",input$color, input$color, paste0("dark",input$color), "black")
    } 
    else if (input$color == "grey"){
      map_colors = c("white",input$color, input$color, paste0("dark",input$color), "black")
    } 
    else {
      map_colors = c("yellow", "orange", "red", "darkred")
    }
    
    #Plots
    ggplot(map_data, aes(x=Longitude, y=Latitude)) +
      geom_point(aes(color = Calls)) + ggtitle("Seattle") +
      scale_color_gradientn(colours = map_colors,
                            values = c(1.0,0.2,0.075,0.01,0)) + coord_map()
  })
  
  
  ## output text
  
  output$summary <- renderText({
    "The major theme that this project will be exploring is social safety in Seattle. 
    There are two sets of data that our group will be working with, Fire Data and Crime Data. 
    The first one is the collection of data from Fire 911 Last 24 Hours. 
    This data is found in the Public Safety section of Seattle Open Data Portal. 
    It provides us with 7 aspects of relevant information including address, type of fire call, date and time, Latitude, 
    Longitude, report location and Incident number. 
    The Crime Data represents crime reported to Seattle Police Department. 
    This data is also found in Seattle Open Data Portal. The Crime Data consists of information regarding to Report Number, 
    occured data, occured time, reported data, reported time, Crime Subcategory and Primary Offense Description."
  })

  # 1
  output$descrip_overview <- renderText({
    "The two plots above are displaying the crime and firecall recorded during the year 2017 and 2018. 
    The widget on the right side of the two plots allows users to choose the specific year they want to inspect.
    If users have intentions to dig deeper into the crime and firecall datasets, the information and plots below should fulfill their expectation."
    
  })
  
  output$wid_overview <- renderText({
    "The default choice summary is a barplot comparing the total reports between 2017 and 2018.  
    The rest of the choices left two users to pick a year and further explore the distribution in this year based on month."
    
  })

  # 2
  output$descrip_pie <- renderText({
    "The pie chart will provide you clear insight on which neighborhoods have greater chances of crime according to the dataset. 
    The computations we calculated was that the neighborhood with the most crime was Downtown Commercial with a total of 13715 reported crimes from 2017 to 2018. 
    The neighborhood with the least crime was Commercial Harbor Island with only 64 reported crimes."
  })
  
  output$wid_pie <- renderText({
    "The widget has two choices to select whether you want to view the Top 10 or Last 10 Crime Neighborhoods in Seattle. 
    If you select the Top 10 the top 10 neighborhoods with the most crime plot will be rendered.
    If you select the Last 10 there will be the Top 10 Neighborhoods with the least crime plot rendered. "
    
  })
  
  output$descrip_bar <- renderText({
    paste("The horizontal Bar Plot named 'Frequency of crimes during time of a day'
          illustrates the distribution of crimes during certain periods of a day. 
          The time frame for the morning period is from 8:00 to 11:59;
          afternoon period from 12:00 to 16:59; night period from 17:00 to 23:59 and midnight period from 00:00 to 7:59. 
          For instance, we can see that Homicides take place the most frequently at night and the least frequently in the morning.")
  })
  
  output$wid_bar <- renderText({
    "Users can select a type of crime (a total of 30 types) under 'Select a Type of Crime'. 
          The Bar Plot will show the frequency of crimes that occurs in the morning, 
    in the afternoon, at night, at midnight for the specific type of crime. "
    
  })
  
  # 3
  output$descrip_map <- renderText({
    "The map is created using longitude and latitude data and shows the amount of fires in the area via color.
    This map will allow people to see which areas in Seattle are have the most fires and also allow them to see the changes from year to year. "
  })
  
  output$wid_map <- renderText({
    "There are two widgets that affect this map: 
    One changes the color of the map (green, red, blue, grey, and 'fire colors' (red, yellow, orange)) 
    and with the second one you can change the year being displayed."
  })
  
  
  
  }
