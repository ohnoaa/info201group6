
## packages used:
library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(lubridate) ## this package provides the ability to modify the dates in the data

## loading data:
weather <- read_delim("seattle_weather_1948-2017.csv.xls")

## mutating data:
some_date <- weather$DATE

weather_edit <- weather %>%
  mutate("MONTH" = month(as.POSIXlt(some_date, format="%Y/%m/%d"))) %>% 
  mutate("YEAR" = year(as.POSIXct(some_date, format="%Y/%m/%d")))

  ## creating seasons column
weather_edit$SEASONS[weather_edit$MONTH == 12] <- "Winter"
weather_edit$SEASONS[weather_edit$MONTH == 1] <- "Winter"
weather_edit$SEASONS[weather_edit$MONTH == 2] <- "Winter"
weather_edit$SEASONS[weather_edit$MONTH == 3] <- "Spring"
weather_edit$SEASONS[weather_edit$MONTH == 4] <- "Spring"
weather_edit$SEASONS[weather_edit$MONTH == 5] <- "Spring"
weather_edit$SEASONS[weather_edit$MONTH == 6] <- "Summer"
weather_edit$SEASONS[weather_edit$MONTH == 7] <- "Summer"
weather_edit$SEASONS[weather_edit$MONTH == 8] <- "Summer"
weather_edit$SEASONS[weather_edit$MONTH == 9] <- "Fall"
weather_edit$SEASONS[weather_edit$MONTH == 10] <- "Fall"
weather_edit$SEASONS[weather_edit$MONTH == 11] <- "Fall"

  ## creating decades column
weather_edit <- weather_edit %>% 
      mutate("DECADE" = (YEAR %/% 10)*10)



## ui
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  
  ## navigation tabs
  navbarPage(title = strong(em("Seattle Weather Information")),
             tabPanel(strong("Introduction"),
                    ## Page includes additional (relevant) visual flare (e.g. image) beyond the summary paragraph
                      img(src='seattleskyline.jpeg', align = "center", height = "95%", width = "95%"),
                      br(),
                      em("The beautiful Seattle skyline (when the weather is nice)."),
                    ## The purpose/importance of the project
                      h3("The Purpose of Understanding Seattle Weather"),
                      p("Seattle weather is known for being difficult to understand.
                        One moment you're enjoying the sun and soon after you're being
                        drenched by the incoming rain storm. For all the Seattlites out
                        there, this webpage provides information about the different
                        weather patterns that Seattle has experienced over the past few 
                        decades, and helps you better understand if you might want to
                        bring out the umbrella for the day."),
                    ## The source of the data of the project
                      h3("The Source for the Data"),
                      p("For this project, we decided to use a dataset from a website
                        called Kaggle. The data set provides information about Seattle's 
                        weather from January 1, 1948 to December 14, 2017. The data 
                        includes information about the maximum and minimum daily
                        temperature along with information about whether or not it
                        rained that day and, if so, the amount of percepitation. There is 
                        potentially some missing values in the data, which we took into
                        account. Go to", a(href = "https://www.kaggle.com/datasets/malihachaity/heuristic-model?resource=download",
                        "Kaggle Seattle Weather"), "to find out more.")
             ),
             ## widget 1:
             tabPanel(strong("Widget 1 - Average Temperatures"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                          column(12, 
                            ## A description of what the chart attempts to answer/understand
                                h3("Average Seattle Temperatures"),
                                p("Here you can analyze and understand what the average daily 
                                  temperatures that Seattle has experienced over the past few
                                  decades. This tool is incredibly helpful if you aren't aware
                                  of the temperature trends that Seattle exhibits.")
                          ),
                          column(6,
                      ## Meaningful widget labels
                            dateRangeInput("daterange", label = h5("Determine Date
                                                                       Range:"),
                                           start = "1948-01-01",
                                           end = "2017-12-14",
                                           min = "1948-01-01",
                                           max = "2017-12-14",
                                           startview = "decade"),
                            p("Select a range of dates from January 1, 1948 to 
                                December 14, 2017 to learn more. Make sure to use a",
                                strong("later date"), "for the first input."),
                            p(em("The format for the date is Year-Month-Date."))
                      ## Setting an appropriate default value
                      ## Widget working (e.g., changing the desired output)
                          ),
                          column(6,
                            uiOutput("difftemp"),
                            p("Select the type of temperature value you'd like to see
                              for the range selected.")
                          )
                         )
                        ),
                        mainPanel(
                          ## Appropriate chart/table type based on the question of interest and the data type(s) of the features
                          plotOutput("scatterplot")
                        )
                      )
             ),
             ## widget 2:
             tabPanel(strong("Widget 2 - Total Precipitation"),
                    sidebarLayout(
                      sidebarPanel(
                        fluidRow(
                          ## A description of what the chart attempts to answer/understand
                          column(12,
                                 h3("Precipitation Throughout the Seasons"),
                                 p("Here you can analyze and understand how much it rains in Seattle
                            based on the different seasons. This will help you determine when
                            you'd like to travel to Seattle to have the most likely chance of clear skies.")
                          ),
                          ## Meaningful widget labels / ## Setting an appropriate default value /  ## Widget working (e.g., changing the desired output)
                          column(6, 
                                 uiOutput("decades")
                          ),
                          column(6,
                                 p(em("Please note that the 1940's and 2010's don't include
                                the complete data because of a lack of yearly information.
                                Therefore their sums may be depicted less compared to the 
                                other decades."))
                          )
                        )
                      ),
                      mainPanel(
                        ## Appropriate chart/table type based on the question of interest and the data type(s) of the features
                        plotOutput("barplot")
                      )
                    )
              ),
             ## widget 3:
             tabPanel(strong("Widget 3 - Temperature and Precipitation Correlation"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            column(12,
                                   h3("Correlation Between Maximum Temperature and Precipitation"),
                                   ## A description of what the chart attempts to answer/understand
                                   p("Here you can analyze and understand the correlation between the
                                     type of weather and the possiblity of it raining at a certain maximum
                                     temperature. This is especially helpful in understanding to be
                                     able to better coordinate outdoor activities.")
                                   ),
                            column(6,
                                   ## Widget working (e.g., changing the desired output) 
                                   uiOutput("seasonpick")
                                   ),
                            column(6,
                                   p("Please ", strong("select "), "a season to begin."),
                                   p(em("You can hover over any of the points on the graph to better
                                        understand when that piece of data was taken."))
                                   )
                          )
                        ),
                        mainPanel(
                          ## Appropriate chart/table type based on the question of interest and the data type(s) of the features
                          plotlyOutput("plot")
                        )
                      )
             ),
             ## conclusion:
             tabPanel(strong("Conclusion"),
                      fluidRow(
                        column(12, 
                      ## A description of the notable insight or pattern discovered in your project
                      h3("Notable Pattern"),
                      p("While further exploring the data, we've not only realized the consistency
                        that Seattle has among its weather patterns, but more specifically that the
                        ratio of precipitation across the different deasons also remains fairly
                        consistent throughout the decades."),
                      br()
                        ),
                        column(6,
                      ## A specific piece of data, table, or chart that demonstrates the pattern/insight
                      plotOutput(outputId = "barplot1", width = "100%")
                        ),
                        column(6,
                      plotOutput(outputId = "barplot2", width = "100%")
                        ),
                      column(12,
                      ## The broader implications of the insight
                      h3("Broader Implications"),
                      p("In a broader sense, we could consider the stability of the weather to potentially
                        remain constant in the coming decades, even with global warming. Because of the span
                        of the data and the amount of years that there is information about, the possibility 
                        of continuing to have a consistent amount of precipitation is likely because the data
                        has been taken across the decades where global warming started to have a significant impact
                        on the weather."),
                      br(),
                      ## A few sentences about data quality. Did you find your dataset of reasonable quality? Do you think it gives unbiased results? Do you see issues with potentially harming certain population groups?
                      h3("Data Quality"),
                      p("We would consider the dataset of pretty reasonable quality. While
                        the initial information that the dataset included is pretty sparce, with only 
                        a few variables for the data, but a lot of observations, there was room to expand
                        on and mutate the variables to be able to explore the data further. Relatively speaking,
                        there doesn't seem to be any immediate biases within the data. Considering that the data is 
                        all numerical, it does lean towards unbiased because of a lack of personal opinion that is
                        apparent and involved. However, there is a lack of information about how the data was collected, which could
                        lead to potential, hidden biases. The data itself only records information about Seattle specifically,
                        with no information about where in Seattle, which could lead to potential misinformation to certain
                        individuals that live in different parts of Seattle or outside of Seattle, that would use this data
                        but it wouldn't readily involve them."),
                      br(),
                      ## Future ideas about how to advance the project
                      h3("Future Advancements"),
                      p("In the future, we could see the potential of expanding this project to not only consider
                        past information about Seattle's weather, but to take that information to predict the 
                        future patterns of Seattle's temperature and precipitation. While that may be a larger 
                        aspiration, we could begin by increasing the amount of variables that we have in the data 
                        and begin to include specific information about the type of daily weather or wind patterns
                        to help people understand Seattle's chaotic weather.")
                      )
                 )
             )
  )
)

## server
server <- function(input, output) {
  ## widget 1 page
    ## scatter plot information
        years_filtered <- reactive({
          weather %>% 
            filter(DATE > input$daterange[1] & DATE <= input$daterange[2])
        })
        
        
        output$scatterplot <- renderPlot({
          want_trend <- FALSE
          p <- ggplot(years_filtered(), aes_string("DATE", input$selecttemp)) +
            geom_point(col = "darkblue", size = 3) +
            ylim(0, 105) +
            labs(x = "Date",
                 y = "Temperature (F)",
                 title = "Daily Temperature over the Decades") +
            theme(text = element_text(size = 15))
          if(input$daterange[2] < input$daterange[1]){
            p <- p +
              labs(title = "Note: Please ensure you chose the correct order for 
                     your dates.")
          }
          p
          if(want_trend) {
            p <- p +
              geom_smooth(method = "lm", se = FALSE)
          }
          p
        })
      ## select box information
        output$difftemp <- renderUI({
          selectInput("selecttemp", label = h5("Select Type of Temperature:"),
                      choices = list("Minimum Daily Temperature" ="TMIN",
                                     "Maximum Daily Temperature" = "TMAX"),
                      selected = "Minimum Daily Temperature") 
        })
  
  ## widget 2
    ## radio buttons / Meaningful widget labels
    output$decades <- renderUI({
      radioButtons("decadescheck",  label = h5("Choose a decade:"),
                   choices = unique(weather_edit$DECADE))
    })
    
    ## bar plot: / Charts/tables appropriately labeled, e.g. x/y labels and units, title, meaning of color codes that are not immediately clear
    decades_filtered <- reactive({
      weather_edit %>% 
        filter(DECADE %in% input$decadescheck) %>% 
        filter(!is.na(PRCP)) %>% 
        group_by(DECADE)
    })
    
    output$barplot <- renderPlot({
      decades_filtered() %>% 
      ggplot(aes(SEASONS, PRCP)) +
        geom_col() +
        ylim(0, 175) +
        labs(x = "Season",
             y = "Total Precipitation (Inches)",
             title = "Total Precipitation Every Season per Decade") +
             theme(text = element_text(size = 15))
    })
    
  ## widget 3
      ## checkbox group widget / Meaningful widget labels
        output$seasonpick <- renderUI({
          checkboxGroupInput("seasonchoice", label = h5("Choose a season(s):"), 
                             choices = unique(weather_edit$SEASONS))
        })
        
        seasonchosen <- reactive({
          weather_edit %>% 
            filter(SEASONS %in% input$seasonchoice) %>% 
            filter(!is.na(PRCP))
        })
        
        ## scatter plot / Charts/tables appropriately labeled, e.g. x/y labels and units, title, meaning of color codes that are not immediately clear
        output$plot <- renderPlotly({
          pl <- plot_ly(data = seasonchosen(),
                  x = ~TMAX, y = ~PRCP, color = ~SEASONS,
                  type = "scatter",
                  text = ~paste("Date: ", DATE, "Temp: ", TMAX, "Prec.: ", PRCP)) %>% 
            layout(xaxis = list(title = "Maximum Temperature (F)",
                                range = c(0, 110)),
                   yaxis = list(title = "Daily Precipitation (Inches)",
                                range = c(0, 5.5)),
                   title = "Correlation between Maximum Daily Temperature and Precipitation")
          pl
        })
  
  ## conclusion
    ## barplot1 / A specific piece of data, table, or chart that demonstrates the pattern/insight
        output$barplot1 <- renderPlot({
          weather_edit %>% 
            filter(DECADE == 1960) %>% 
            ggplot(aes(SEASONS, PRCP)) +
            geom_col() +
            ylim(0, 175) +
            labs(x = "Season",
                 y = "Total Precipitation (Inches)",
                 title = "Total Precipitation Every Season in 1960") +
            theme(text = element_text(size = 15))
        })
    
    ## barplot2:
        output$barplot2 <- renderPlot({
          weather_edit %>% 
            filter(DECADE == 1970) %>% 
            ggplot(aes(SEASONS, PRCP)) +
            geom_col() +
            ylim(0, 175) +
            labs(x = "Season",
                 y = "Total Precipitation (Inches)",
                 title = "Total Precipitation Every Season in 1970") +
            theme(text = element_text(size = 15))
        })
}


shinyApp(ui = ui, server = server)
