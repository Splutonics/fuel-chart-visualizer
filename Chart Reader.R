
#these load the necessary add-ons for this code to function
library(tidyverse)
library(readxl)
library(shiny)

#sets the working drive to the specified folder
setwd('C:/Users/jonfr/Desktop/Coding/R')

#Define Aircraft Types
#This should be the ONLY change you have to make in this script
acft_list <- list('Airplane', 
                  'Car')
# ----

#suppresses the warnings produced by the changing data size
#options(warn = -1)


#this will pull all the data from the excel file, clean it, and assign it to the approp. name
for (x in 1:length(acft_list)) {
  
  #loads the data from the specified file excel file, makes it a tibble, and assigns it
  exampledata <- read_excel('Chart Reader Example Data.xlsx', sheet = x) %>% as_tibble()
  
  #takes the data imported from the excel sheet and deletes any rows
  #that dont have relevant data. the data is assigned based on the type of data
  #that it is (FF, max rng, end)
  f <- filter(exampledata, exampledata$type == 'ff') %>%
    
    #gets rid of the no longer necessary 'type' variable used for sorting
    select(-'type') %>% 
    #tidys the data by making weight a single variable, and making FF the value
    pivot_longer(-c('DI','DA','KIAS','KTAS'), names_to = 'weight', values_to = 'FF' ) %>%   
    #seperates engine speed from weight
    separate('weight', into = c("weight","engspeed"),  sep = "/") %>%
    #converts values to their appropriate data type
    type.convert()

  
  
  e <- filter(exampledata, exampledata$type == 'endkias') %>%
    select(-c('type','KIAS','KTAS')) %>%
    pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'KIAS' ) %>%
    separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
    
    cbind(filter(exampledata, exampledata$type == 'endktas') %>%
            select(-c('type','KIAS','KTAS')) %>%
            pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'KTAS' ) %>%
            separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
            select('KTAS')) %>%
    
    cbind(filter(exampledata, exampledata$type == 'endff') %>%
            select(-c('type','KIAS','KTAS')) %>%
            pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'FF' ) %>%
            separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
            select('FF')) %>%
    as_tibble() %>%
    type.convert()
  
  r <- filter(exampledata, exampledata$type == 'rngkias') %>%
    select(-c('type','KIAS','KTAS')) %>%
    pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'KIAS' ) %>%
    separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
    
    cbind(filter(exampledata, exampledata$type == 'rngktas') %>%
            select(-c('type','KIAS','KTAS')) %>%
            pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'KTAS' ) %>%
            separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
            select('KTAS')) %>%
    
    cbind(filter(exampledata, exampledata$type == 'rngff') %>%
            select(-c('type','KIAS','KTAS')) %>%
            pivot_longer(-c('DI','DA'), names_to = 'weight', values_to = 'FF' ) %>%
            separate('weight', into = c('weight','engspeed'),  sep = "/") %>%
            select('FF')) %>%
    as_tibble() %>%
    type.convert()

  assign(paste0(as.character(acft_list[x]),'FF'), f)
  assign(paste0(as.character(acft_list[x]),'end'), e)
  assign(paste0(as.character(acft_list[x]),'rng'), r)
  

}
  #removes extraneous data
  rm(exampledata, f, e, r)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Chart Reader"),   # App title 

  sidebarLayout(  # Sidebar layout with input and output definitions 
    sidebarPanel(     # Sidebar panel for inputs 
      #Input: Select for the Aircraft Type
      selectInput(inputId = "acft", 
                  label = "Aircraft Type",
                  choices = acft_list),
      #Input: Select Display of KIAS or KTAS
      selectInput(inputId = "speedtype", 
                  label = "Airspeed Type",
                  choices = list("KIAS",
                                 "KTAS")
                  ),
      uiOutput(outputId = "acftspecificUI")
      ),
    
    mainPanel(   # Main panel for displaying outputs 
      plotOutput(outputId = "dataPlot"))    # Output: Graph designated below
  )
) 

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  
  output$acftspecificUI <- renderUI({
    
    x1 <- eval(as.name(paste0(input$acft,'FF')))
                      
    tagList(
      sliderInput(inputId = "DI",
                  label = "DI",
                  min = min(x1$DI),
                  max = max(x1$DI),
                  value = min(x1$DI),
                  step = 1
      ),
      sliderInput(inputId = "DA",
                  label = "DA",
                  min = min(x1$DA),
                  max = max(x1$DA),
                  value = min(x1$DA),
                  step = 1
      ),
      
      sliderInput(inputId = "weight",
                  label = "Weight",
                  min = min(x1$weight),
                  max = max(x1$weight),
                  value = min(x1$weight),
                  step = 1000
      )
# 
#       slider  Input(inputId = "engspeed",
#                   label = "Engine Speed",
#                   choices = list("max",
#                                  "min")
#       )
    )
  })
  

  output$dataPlot <- renderPlot({

    x1 <- eval(as.name(paste0(input$acft,'FF')))
    x2 <- eval(as.name(paste0(input$acft,'end')))
    x3 <- eval(as.name(paste0(input$acft,'rng')))
    
    xff <- filter(x1,
                 x1$DI == input$DI,                #desired DI
                 x1$DA == input$DA,                #desired DA
               x1$weight == input$weight,          #desired weight
                 x1$engspeed == "max")        
    xend <- filter(x2,
                 x2$DI == input$DI,
                 x2$DA == input$DA,
                 x2$weight == input$weight,
                 x2$engspeed == "max")
    xrng <- filter(x3,
                 x3$DI == input$DI,
                 x3$DA == input$DA,
                 x3$weight == input$weight,
                 x3$engspeed == "max")
    
    
    
    ggplot(xff, aes(x = xff$KIAS, y = xff$FF)) +
      #assigns the data to a line plot
      geom_line() +
      
      #assigns a common x/y scale to the graph based on the min/max values for x/y inputs
      scale_x_continuous(limits = c(min(x1$KIAS),max(x1$KIAS))) +
      scale_y_continuous(limits = c(0,max(x1$FF))) +
      
      geom_point(data = xend, mapping = aes(x = xend$KIAS, y = xend$FF)) +
      geom_point(data = xrng, mapping = aes(x = xrng$KIAS, y = xrng$FF)) +
      
      geom_text(data = xend, label = "Max End", mapping = aes(x = xend$KIAS, y = xend$FF)) +
      geom_text(data = xrng, label = "Max Rng", mapping = aes(x = xrng$KIAS, y = xrng$FF)) +
      
      #labels the graph
      labs(x = input$speedtype, y = "Fuel Flow", title = paste(input$acft, "FF"))      
      
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
  
  #to do
  #create usable KTAS selectable/scale
  #Create usable Max/Min Eng Speed selector
  #plot rng
  #plot end
  #make step size variable (based on acft type)
  #guess data between FF values, AKA interpolation data
