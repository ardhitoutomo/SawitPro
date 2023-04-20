# Library
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyquant)
library(ggplot2)
library(ggthemes)
library(plotly)
library(forecast)
library(dtw)
library(biclust)
library(DT)
library(dtwclust)
`%notin%` <- Negate(`%in%`)
set.seed(8)

# Data
# dat <- fread("owid-covid-data.csv") %>%
#   mutate(location = as.factor(location),
#          continent = as.factor(continent),
#          date = as.Date(date)) %>%
#   filter(location != "World") %>%
#   select(continent, location, date, total_cases, new_cases, total_deaths, new_deaths,
#          total_cases_per_million, new_cases_per_million, total_deaths_per_million,
#          total_vaccinations, people_vaccinated, people_fully_vaccinated, total_boosters,
#          population_density, median_age, gdp_per_capita)
# 
# sapply(dat, function(y) sum(length(which(is.na(y))))) %>%
#   as.data.frame() %>%
#   arrange(-.)
# 
# continent <- dat$continent %>% unique() %>% as.character()
# 
# dat <- dat %>%
#   group_by(date, location) %>%
#   arrange(location, date) %>%
#   mutate(weekday = weekdays(date, abbreviate = T) %>% as.factor(),
#          new_cases = ifelse(is.na(new_cases), 0, new_cases),
#          new_deaths = ifelse(is.na(new_deaths), 0, new_deaths)) %>%
#   ungroup() %>% 
#   filter(location %notin% continent) %>% 
#   filter(continent != "")
# 
# saveRDS(dat, "covid-data-cleaned.RDS")

dat <- readRDS("covid-data-cleaned.RDS") %>% 
  group_by(location) %>% 
  slice_tail(n = 109)

cts1 <- dat[,c(2,3,4)] %>% spread(location, total_cases)
cts2 <- as.data.frame(cts1) %>% 
  filter(date >= as.Date("2023-01-01"))
cts3 <- cts2 %>% 
  select_if(~ !any(is.na(.))) %>% 
  dplyr::select(-date) %>% 
  as.list()

# Define UI
ui <- dashboardPage(title = "...", skin = "purple",
                    
                    # Application title
                    dashboardHeader(title = "Covid Data"),
                    
                    # Sidebar with a slider input for number of bins 
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Summary", tabName ="1", icon = icon("paste")),
                        menuItem("Forecasting", tabName ="2", icon = icon("line-chart")),
                        menuItem("Dataset", tabName = "3", icon = icon("object-group"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "1", 
                                box(width=14,                 
                                    h1("Summary"),
                                    h4("This Shiny Dashboard is created for Sawit Pro Data Science Test. You can find some info related to Covid, 
                                        including cases, deaths, vaccinations, etc into a report like boxplot of weekday of new cases, and 
                                        forecast of new cases. The forecast and data used are from ourworldindata."),
                                    fluidRow(
                                      valueBoxOutput("outl"),
                                      valueBoxOutput("death"),
                                      valueBoxOutput("reve")
                                    )
                                ),
                                           selectInput("dv", label = "Choose an Outlet",
                                                       choices = dat$location, selected = ""),
                                           plotlyOutput("point")
                        ),
                        tabItem(tabName = "2",
                                box(width = 14,
                                    h1("Forecasting")
                                ),
                                h3(textOutput("desfore")),
                                selectInput("fc", label = "Choose an Outlet", 
                                            choices = dat$location, selected = ""),
                                plotOutput("fore")
                        ),
                        tabItem(tabName = "3",
                                box(width = 16,
                                    h3("Covid Data Set")
                                ),
                                h4("This data is obtained", a("here", href="https://ourworldindata.org/covid-cases"), "(filter to only 2023)."),
                                dataTableOutput("df")
                        )
                        )
                      )
                    )
# )


# Define server logic
server <- function(input, output){
  
  output$outl <- renderValueBox({
    valueBox(
      paste0(nlevels(dat$location)), "Countries", icon = icon("flag"),
      color = "aqua"
    )
  })
  
  output$death <- renderValueBox({
    valueBox(
      paste0(max(dat$new_deaths)), 
      paste0("Max New Death on 2023 (in ", dat[dat$new_deaths == max(dat$new_deaths), "location"] %>% pull(), 
             " at ", 
             dat[dat$new_deaths == max(dat$new_deaths), "date"] %>% pull(), ")"), 
      icon = icon("tombstone-blank"),
      color = "purple"
    )
  })
  
  output$reve <- renderValueBox({
    dollar <- paste(round(sum(dat$new_cases),2))
    valueBox(
      paste0(dollar), "Total Cases", icon = icon("virus"),
      color = "olive"
    )
  })
  
  
  output$point <- renderPlotly({
    set.seed(8)
    ggplot(dat[dat$location == input$dv,], aes(x = weekday, y = new_cases)) +
      geom_boxplot(aes(fill = weekday)) +
      geom_jitter()
  })
  
  
  output$fore <- renderPlot({
    receipt.hw <- HoltWinters(ts(data = dat[dat$location == input$fc,]$new_cases,
                                 start = c(1,5),
                                 frequency = 7), alpha = 0.1877, beta = F, gamma = 0.8123) 
    receipt.forecast <- predict(receipt.hw, n.ahead = 7)
    
    plot(receipt.hw, receipt.forecast)
    })
  
  
  output$desfore <- renderText({
    paste("Forecast of outlet ",input$fc," for the next 7 days")
  })
  
  
  output$cetees <- renderPlot({
    tscl <- tsclust(cts3, type = "partitional", k = input$kclu, 
                    distance = "dtw_basic", centroid = "pam", 
                    seed = 8L, trace = TRUE,
                    args = tsclust_args(dist = list(window.size = input$kclu)))
    plot(tscl)
  })
  
  output$df<-renderDataTable({
    datatable(as.data.frame(dat),options = list(scrollX = TRUE))
  })
  
  output$mx<-renderDataTable({
    datatable(cts1, options = list(scrollX = TRUE))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
