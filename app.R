if (!require(tidyverse)) install.packages('tidyverse')
if (!require(httr)) install.packages('httr')
if (!require(jsonlite)) install.packages('jsonlite')
if (!require(fitdistrplus)) install.packages('fitdistrplus')
if (!require(R0)) install.packages('R0')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(knitr)) install.packages('knitr')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(shiny)) install.packages('shiny')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(lubridate)) install.packages('lubridate')

library(tidyverse)
library(httr)
library(jsonlite)
library(fitdistrplus)
library(R0)
library(ggplot2)
library(knitr)
library(gridExtra)
library(shiny)
library(shinythemes)
library(lubridate)
library(plotly)

source("c3aidatalake.R")

mask_date <- read.csv('mask_date.csv') #read_data
stateslist <- c(state.abb , 'DC') #add DC in the states list
stateslist <- sort(stateslist)

result <- NULL
for(stateab in stateslist){
  if(mask_date$date[mask_date$geo_value == stateab] != ""){
    #generate statename
    if(stateab == 'DC'){
      statename = "DistrictofColumbia"
    }else{ 
      statename <- gsub(" ", "", state.name[state.abb == stateab], fixed = TRUE)}
    statename <- paste(statename, 'UnitedStates', sep = '_')
    
    #get data
    data <- evalmetrics("outbreaklocation",
                        list(
                          spec = list(
                            ids = list(statename),
                            expressions = list("CovidTrackingProject_ConfirmedCases"),
                            interval = "DAY",
                            start = "2020-03-01",
                            end  = "2020-11-13"
                          )
                        )
    )
    data <- data[, c('dates','data')]
    
    #change format of dates and data 
    #(cumulative to incident and positive case should be postive integer)
    data$dates <- as.Date(data$dates)
    data$data <- data$data - c(0, data$data[1:(length(data$data)-1)])
    data$data[data$data <= 1] <- 1
    data$data <- as.integer(data$data)
    
    #generate epic data and mask mandate effective date
    #(Here we have a 15 days delay due to the incubation period and test delay)
    epic <- data$data
    effectdate <- as.Date(mask_date$date[mask_date$geo_value == stateab], 
                          format = '%m/%d/%y') + 15
    names(epic) <- as.Date(data$dates)
    
    #generate generation time based on serial interval distribution
    mGT <- generation.time("gamma", c(3.96,4.75))
    
    #calculate the R0 before and after mask mandate effective date
    R0_before <- estimate.R(epid = epic, GT = mGT, begin = effectdate - 30, 
                            end = effectdate, methods=c("ML"))$estimates$ML$R
    R0_after <- estimate.R(epid = epic, GT = mGT, begin = effectdate+1, 
                           end = effectdate + 30, methods=c("ML"))$estimates$ML$R
    tmp <- data.frame(state = stateab, R0_before = R0_before, R0_after = R0_after)
    result <- rbind(result,tmp)
  }
}

R0_series <- NULL
startdate <- as.Date('2020-03-01')
i = 1:(Sys.Date() - as.Date('2020-03-01') - 30) 
date <- startdate + 29 + i
R0_series <- data.frame(date = date)

#calculate each state R0 series
for(stateab in stateslist){
  if(stateab == 'DC') statename = "DistrictofColumbia" else statename <- gsub(" ", "", state.name[state.abb == stateab], fixed = TRUE)
  statename <- paste(statename, 'UnitedStates', sep = '_')
  
  #get data
  data <- evalmetrics("outbreaklocation",
                      list(
                        spec = list(
                          ids = list(statename),
                          expressions = list("CovidTrackingProject_ConfirmedCases"),
                          interval = "DAY",
                          start = "2020-03-01",
                          end  = Sys.Date()
                        )
                      )
  )
  data <- data[, c('dates','data')]
  #change format of dates and data (cumulative to incident and positive case should be postive integer)
  data$dates <- as.Date(data$dates)
  data$data <- data$data - c(0, data$data[1:(length(data$data)-1)])
  data$data[data$data <= 1] <- 1
  data$data <- as.integer(data$data)
  
  #generate epic data
  epic <- data$data
  names(epic) <- as.Date(data$dates)
  
  #generate generation time based on serial interval distribution
  mGT <- generation.time("gamma", c(3.96,4.75))
  
  #calculating the R0 for each states by daily sliding window with size 30
  R0 <- c()
  for(i in 1:length(R0_series$date)){
    tmp <- estimate.R(epid = epic, GT = mGT, begin = startdate + i-1, end = startdate + i + 29, methods=c("ML"))$estimates$ML$R
    R0 <- c(R0,tmp)
  }
  R0_series <- transform(R0_series, tmp = R0 )
  colnames(R0_series)[ncol(R0_series)] <- stateab
}

stay_date <- read.csv('policy_date.csv', header = T)
politics <- read.csv('2016_election_result.csv')
politics <- politics[,c('State_ab','Hillary.Clinton.Democratic.percentage',
                        'Donald.Trump.Republican.percentage')]
colnames(politics) <- c('state','blue','red')
politics$red <- as.numeric(sub("%","",politics$red))/100
politics$blue <- as.numeric(sub("%","",politics$blue))/100
politics$group = "Swing"

# separate states with red blue and swing states with the election result
politics$group[(politics$red - politics$blue) > 0.1] = "Red"
politics$group[(politics$blue - politics$red) > 0.1] = "Blue"
politics <- politics[,c('state', 'group')]
politics <- politics[politics$state %in% c(state.abb,'DC'),]
redvsblue <- data.frame(red = politics$state[politics$group == 'Red'], 
                        blue = c(politics$state[politics$group == 'Blue'],
                                 rep("", sum(politics$group == 'Red') - 
                                       sum(politics$group == 'Blue'))), 
                        swing = c(politics$state[politics$group == 'Swing'], 
                                  rep("", sum(politics$group == 'Red') - 
                                        sum(politics$group == 'Swing'))))


myapp <- function(){
  ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel('Roling R0 by Team Ninja'),
    tabsetPanel(
      tabPanel("Rolling R0 curve",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("stateab", "Select one state",
                               stateslist),
                   sliderInput("date", "Select date range",
                               min = as.Date('2020-04-01'), max = Sys.Date(),
                               value = c(as.Date('2020-04-01'),min = as.Date('2020-11-01'))),
                   checkboxInput("policy","Including Dates of important policies"),
                   actionButton("Continue", "Continue")
                   ),
                 mainPanel(
                   plotOutput('R0plot')
                   )
                 )
               )
      ,
      tabPanel("R0 table (mask mandate)", 
               fluidRow(
                 column(4, tableOutput('R01')
                 ),
                 column(4, tableOutput('R02')
                 ),
                 column(4, tableOutput('R03'))
               ),
               textOutput('tablelegend')
      )
    ),
    htmlOutput('data')
  )
  
  server <- function(input, output, session) {
    state <- reactive(input$stateab)
    startdate <- reactive(input$date[1])
    enddate <- reactive(input$date[2]) 
    observeEvent(input$Continue,{
    output$R0plot <- renderPlot({
      stateab <- state()
      start <- startdate()
      end <- enddate()
      date <- as.Date(start, origin = '1970-01-01') + (0: (end-start))
      if(politics$group[politics$state == stateab] == 'Red') color = 'red'
      if(politics$group[politics$state == stateab] == 'Blue') color = 'blue'
      if(politics$group[politics$state == stateab] == 'Swing') color = 'green'
      p <- ggplot(data = R0_series[R0_series$date %in% date,],aes(x = date, y = eval(parse(text = stateab)), color = color)) + 
        geom_line(color = color)+
        labs(x = 'Date', y = 'R0', title =  stateab)+
        scale_x_continuous(breaks=seq(date[1], date[length(date)], "month"))+
        geom_hline(yintercept = 1, color = "black") +
        theme(legend.position = c(0.8,0.8))
      print(p)
      if(input$policy){
        output$R0plot <- renderPlot({
          stateab <- state()
          start <- startdate()
          end <- enddate()
          date <- as.Date(start, origin = '1970-01-01') + (0: (end-start))
          if(politics$group[politics$state == stateab] == 'Red') color = 'red'
          if(politics$group[politics$state == stateab] == 'Blue') color = 'blue'
          if(politics$group[politics$state == stateab] == 'Swing') color = 'green'
          p <- ggplot(data = R0_series[R0_series$date %in% date,],aes(x = date, y = eval(parse(text = stateab)), color = color)) + 
            geom_line(color = color)+
            labs(x = 'Date', y = 'R0', title =  stateab)+
            geom_vline(aes(xintercept = as.Date(mask_date$date[mask_date$geo_value == stateab], format ='%m/%d/%y'), color = 'Date of mask mandate')) +
            geom_vline(aes(xintercept = as.Date(stay_date$stay_at_home_date[stay_date$geo_value == stateab], format = '%m/%d/%y'),color = 'Date of stay at home order')) +
            geom_vline(aes(xintercept = as.Date(stay_date$stay_at_home_expire_date[stay_date$geo_value == stateab], format = '%m/%d/%y'), color = 'Expire date of stay at home order'))+
            geom_vline(aes(xintercept = as.Date(mask_date$date[mask_date$geo_value == stateab], format = '%m/%d/%y') + 15, color = 'Effective Date of mask mandate'))+
            scale_x_continuous(breaks=seq(date[1], date[length(date)], "month"))+
            geom_hline(yintercept = 1, color = "black") +
            scale_color_manual(name = 'Dates of Policies', values = c('Date of mask mandate' = 'darkred', 'Effective Date of mask mandate' = 'orange', 'Date of stay at home order' = 'darkblue', 'Expire date of stay at home order' = 'darkgreen'))+
            theme(legend.position = c(0.8,0.8))
          print(p)
        })
      }
    })
    updateActionButton(session, "Continue")
    }
    )
    observeEvent(input$date,{
      output$R0plot <- renderPlot({
        NULL
      })
    })
    observeEvent(input$state,{
      output$R0plot <- renderPlot({
        NULL
      })
    })
    observeEvent(input$policy,{
      output$R0plot <- renderPlot({
        NULL
      })
    })
    output$R01 <- renderTable(
      result[1:11,]
    )
    output$R02 <- renderTable(
      result[12:22,]
    )
    output$R03 <- renderTable(
      result[23:33,]
    )
    output$tablelegend <- renderText(
      'There are 33 states posting mask mandate totally, 
      we use the daily positive increase cases data in one month 
      before mask mandate to estimate the R0 before mask mandate effective date 
      (15 days after mask mandate)
      and the data in one month after mask mandate to estimate the R0 
      after mask mandate to estimate R0 after mask mandate.'
    )
    output$data <- renderText(
   HTML('Data Source:<br/>
      • The 2016 Presidential Election Results: New York Times <br/>
(https://www.nytimes.com/elections/2016/results/president) <br/>
• Stay-at-home order: USAToday <br/>
(https://www.usatoday.com/storytelling/coronavirus-reopening-america-map/caseload)<br/>
• Cumulative confirmed case: COVID tracking project (https://covidtracking.com) from c3.ai data lake<br/>
(https://c3.ai/customers/covid-19-data-lake/)<br/>
• Mask-mandate: AARP<br/>
(https://www.aarp.org/health/healthy-living/info-2020/states-mask-mandates-coronavirus.html)')
    )
  }
  shinyApp(ui, server)
}

myapp()
