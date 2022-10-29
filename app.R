library(shiny)
library(shinydashboard)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library("dplyr")
library("ggpubr")
library(plotly)
library(mapview)
library(leaflet)
library(leaflet.extras)

#setwd('C:\\Users\\mpr10\\OneDrive\\Documents\\R Shiny Project\\try2')

getwd()

Bus_Model <- read.csv('Bus_Model.csv',stringsAsFactors = T)
Route_Capacity <- read.csv('Route_Capacity.csv')
Market_Share <- read.csv('Market_Share.csv')
Annual_Ridership <- read.csv('Annual_Ridership.csv') 

summary(Bus_Model)
summary(Route_Capacity)
summary(Market_Share)
summary(Annual_Ridership)


Weighted_Average_Capacity_Per_Bus <- weighted.mean(Bus_Model$Capacity, Bus_Model$Units)
Route_Capacity$Daily.Capacity <- (Route_Capacity$Trips*Weighted_Average_Capacity_Per_Bus)
Total_Daily_Capacity <- as.integer(sum(Route_Capacity$Daily.Capacity))
Annual_Capacity <- Total_Daily_Capacity*365
Market_Share$Operator.Capacity <- Market_Share$Market.Share*Annual_Capacity

Annual_Ridership_Adjusted <- Annual_Ridership[-c(17,16),] #2020 and 2021 removed to remove COVID effect from forecast

Annual_Ridership_Adjusted$Average.Annual.Ridership.Mils = Annual_Ridership_Adjusted$Average.Annual.Ridership/1000000

Annual_Ridership$Average.Annual.Ridership.Mils = Annual_Ridership$Average.Annual.Ridership/1000000

#-------------------------------------------

ridership_fc <- lm(Average.Annual.Ridership.Mils ~ Year, data=Annual_Ridership_Adjusted)
print(ridership_fc)

summary(ridership_fc)

modelSummary <- summary(ridership_fc)
modelCoefficients <- modelSummary$coefficients
beta.estimate <- modelCoefficients["Year", "Estimate"]
std.error <- modelCoefficients["Year", "Std. Error"]  

#t statistic
t_value <- beta.estimate/std.error

#p Value
p_value <- 2*pt(-abs(t_value), df=nrow(Annual_Ridership_Adjusted)-ncol(Annual_Ridership_Adjusted))  

#f statistic
f_statistic <- modelSummary$fstatistic[1]

# parameters for model p-value calc
f <- summary(ridership_fc)$fstatistic

model_p <- pf(f[1], f[2], f[3], lower=FALSE)

new_years <- data.frame(Year=c(2022:2030))

result<- predict(ridership_fc,newdata= new_years)
print(result)

Year=c(2022:2030)

forecast <- data.frame(Year=Year, Average.Annual.Ridership=result*1000000, Average.Annual.Ridership.Mils=result)

forecast

colnames(forecast) <- c('Year', 'Average.Annual.Ridership','Average.Annual.Ridership.Mils')

Annual_Ridership_Forecast <- rbind(Annual_Ridership,forecast)

Annual_Ridership_Forecast

#------------------------------------------

Annual_Ridership_Forecast$Annual.Capacity = Annual_Capacity

Annual_Ridership_Forecast$Utilization = round(((Annual_Ridership_Forecast$Average.Annual.Ridership/Annual_Ridership_Forecast$Annual.Capacity)*100),1)

Annual_Ridership_Forecast

#------------------------------------------

SBST_Market_Share <- 0.60
SMRT_Market_Share <- 0.25
GAS_Market_Share <- 0.07
TTS_Market_Share <- 0.08

SBST_Annual_Capacity <- Annual_Capacity*SBST_Market_Share
SMRT_Annual_Capacity <- Annual_Capacity*SMRT_Market_Share
GAS_Annual_Capacity <- Annual_Capacity*GAS_Market_Share
TTS_Annual_Capacity <- Annual_Capacity*TTS_Market_Share

#Operator Wise Capacity

Annual_Ridership_Forecast$SBST.Annual.Capacity <- Annual_Ridership_Forecast$Annual.Capacity*SBST_Market_Share
Annual_Ridership_Forecast$SMRT.Annual.Capacity <- Annual_Ridership_Forecast$Annual.Capacity*SMRT_Market_Share
Annual_Ridership_Forecast$GAS.Annual.Capacity <- Annual_Ridership_Forecast$Annual.Capacity*GAS_Market_Share
Annual_Ridership_Forecast$TTS.Annual.Capacity <- Annual_Ridership_Forecast$Annual.Capacity*TTS_Market_Share

Operators <- c("SBST", "SMRT", "GAS", "TTS")
Op_Cap <- c((Annual_Capacity*SBST_Market_Share)/1000000, (Annual_Capacity*SMRT_Market_Share)/1000000, (Annual_Capacity*GAS_Market_Share)/1000000, (Annual_Capacity*TTS_Market_Share)/1000000)

Operator_Capacity <- data.frame(Operator = Operators, Capacity = Op_Cap)


#Operator Wise Ridership

Annual_Ridership_Forecast$SBST.Annual.Ridership.Mils <- Annual_Ridership_Forecast$Average.Annual.Ridership.Mils*SBST_Market_Share
Annual_Ridership_Forecast$SMRT.Annual.Ridership.Mils <- Annual_Ridership_Forecast$Average.Annual.Ridership.Mils*SMRT_Market_Share
Annual_Ridership_Forecast$GAS.Annual.Ridership.Mils <- Annual_Ridership_Forecast$Average.Annual.Ridership.Mils*GAS_Market_Share
Annual_Ridership_Forecast$TTS.Annual.Ridership.Mils <- Annual_Ridership_Forecast$Average.Annual.Ridership.Mils*TTS_Market_Share



#Operator Wise Utilization

Annual_Ridership_Forecast$SBST.Annual.Capacity <- 1553983850
Annual_Ridership_Forecast$SMRT.Annual.Capacity <- 600347620
Annual_Ridership_Forecast$GAS.Annual.Capacity <- 180666605
Annual_Ridership_Forecast$TTS.Annual.Capacity <- 208402225

Annual_Ridership_Forecast$SBST.Utilization <- (Annual_Ridership_Forecast$SBST.Annual.Ridership/(Annual_Ridership_Forecast$SBST.Annual.Capacity/1000000))*100
Annual_Ridership_Forecast$SMRT.Utilization <- (Annual_Ridership_Forecast$SMRT.Annual.Ridership/(Annual_Ridership_Forecast$SMRT.Annual.Capacity/1000000))*100
Annual_Ridership_Forecast$GAS.Utilization <- (Annual_Ridership_Forecast$GAS.Annual.Ridership/(Annual_Ridership_Forecast$GAS.Annual.Capacity/1000000))*100
Annual_Ridership_Forecast$TTS.Utilization <- (Annual_Ridership_Forecast$TTS.Annual.Ridership/(Annual_Ridership_Forecast$TTS.Annual.Capacity/1000000))*100

Capacity_Utilization_Forecast <- subset(Annual_Ridership_Forecast, Year>2019)

#print(Capacity_Utilization_Forecast)

print(Capacity_Utilization_Forecast$TTS.Utilization)
print(Capacity_Utilization_Forecast$SBST.Utilization)
print(Capacity_Utilization_Forecast$SMRT.Utilization)
print(Capacity_Utilization_Forecast$GAS.Utilization)

#-------------------------------------------

df_long1 <- read.csv("bus_age_line.csv")

#-------------------------------------------

#Bus_Age <- read.csv("bus_age_line.csv")

#---------------------------------------------

MRT <- read_csv("busstopr.csv")

#------------------------------------------------

buses <- read.csv("count.csv", header=TRUE, sep = ",")

#---------------------------------------------------

Bus_Fuel_Type <- read.csv("Bus_Fuel_Type_Data.csv", stringsAsFactors = T)

labs <- c('Electric', 'Diesel')

SBST_FUEL <- c(Bus_Fuel_Type$SBST)
SMRT_FUEL <- c(Bus_Fuel_Type$SMRT)
GAS_FUEL <- c(Bus_Fuel_Type$GAS)
TTS_FUEL <- c(Bus_Fuel_Type$TTS)

SBST_E_SHARE <- round(100*SBST_FUEL/sum(SBST_FUEL),1)
SMRT_E_SHARE <- round(100*SMRT_FUEL/sum(SMRT_FUEL),1)
GAS_E_SHARE <- round(100*GAS_FUEL/sum(GAS_FUEL),1)
TTS_E_SHARE <- round(100*TTS_FUEL/sum(TTS_FUEL),1)

Bus_Fuel_Type



#---------------------------------------------------

ui <- dashboardPage(
  skin='red',
  dashboardHeader(title='Singapore Bus Transport System', titleWidth = 450),
  dashboardSidebar( width = 450, sidebarMenu(id='sbm', 
                                             menuItem('Accessibility & Availability', tabName='Acc'),
                                             menuItem('Capacity', tabName='Cap'),
                                             menuItem('Challenges', tabName='Cha'))),
  dashboardBody(
    tabItems(
      
      #Capacity 
      tabItem(tabName='Cap',
              tags$div(HTML("<b> <span style=font-size:150%;> Descriptive Statistics </span> </b>")),
              fluidPage(
                fluidRow(
                  column(width=12,
                         height=500,
                         box(
                           width=6,
                           height=800,
                           
                           selectInput("Operator", "Operator", unique(Bus_Model$Operator)),
                          
                           tags$div(HTML("<b> <span style=font-size:130%> Bus Fleet </span> </b>")),
                           
                           tableOutput("tableBF")
                         ),
                         
                         box(
                           width=6,
                           height=800,
                           plotOutput("Plot5", height = "750")
                         ),
                         
                         
                         
                         tags$div(HTML("<b> <span style=font-size:150%;> Forecast </span> </b>")),
                         
                         box(width=6,
                             height=800,
                             
                             selectInput("operator", "Operator:",
                                         c("All" = "All",
                                           "SBST" = "SBST",
                                           "SMRT" = "SMRT",
                                           "GAS" = "GAS",
                                           "TTS" = "TTS")),
                             
                             
                             
                             plotOutput("Plot3", height = 700)
                             
                         ),
                         box(width=6,
                             height=800,
                             
                             selectInput("operatorX", "Operator:",
                                         c("All" = "All",
                                           "SBST" = "SBST",
                                           "SMRT" = "SMRT",
                                           "GAS" = "GAS",
                                           "TTS" = "TTS")),
                             
                             plotOutput("Plot4", height = "700")
                         ),
                         
                         
                  )
                )
              )
      ),
      
      #Accesibility
      tabItem(tabName='Acc',
              fluidPage(
                fluidRow(
                  column(width=12,
                         height=350,
                         
                         tags$div(HTML("<b> <span style=font-size:130%;> No. of Bus Stops within Radius (Km) </span> </b>")),
                         
                         box(
                           width=4,
                           height=350,
                         selectInput(inputId = 'distance',
                                     label = "Select The Radius (km)",
                                     list("1km", "2km", "3km", "4km", "5km")),
          
                         ),
                         
                         
                         box(
                           width=8,
                           height=350,
                         plotOutput("plot", height = "320"),
                         ),
                         
                         tags$div(HTML("<b> <span style=font-size:130%;> Bus Stops in Singapore </span> </b>")),
                         
                         box(
                           width=12,
                           height=600,
                           leafletOutput("PlotMap", height = "570")
                         ),
                  )
                )
              ),
      ),
      
      #Challenges
      tabItem(tabName='Cha',
              fluidPage(
                fluidRow(
                  column(width=12,
                         height=500,
                         
                         tags$div(HTML("<b> <span style=font-size:130%;> Bus Fuel Type Distribution </span> </b>")),
                         
                         box(width=3,
                             height=400,
                             tableOutput("tableFuel")
                         ),
                         
                         box(width=9,
                             height=400,
                             splitLayout(
                               style = "border: 1px solid silver;",
                               cellWidths = 247,
                               cellArgs = list(style = "padding: 6px"),
                               plotOutput("PlotPie1", height = 370),
                               plotOutput("PlotPie2", height = 370),
                               plotOutput("PlotPie3", height = 370),
                               plotOutput("PlotPie4", height = 370)
                             ),
                             
                         ),
                         
                         #Bus Age Graph
                         box(width=12,
                             height=600,
                             plotlyOutput("BusAgeGraph", height = "580")
                         ),
                  )
                )
              ),
      )
    ),
  ),
)

#-------------------------------------------------------------------

server <- function(input,output) {
  
  output$BusAgeGraph <- renderPlotly({
    q <- plot_ly(
      data = df_long1,
      x = ~Age,
      y = ~Count,
      color = ~Year,
      type = "scatter",
      mode = "lines+markers"
    )%>%
      layout(title = 'Bus Age Distribution', plot_bgcolor = "#e5ecf6")
    
    print(q)
  })
  
  output$Table1 <- renderTable(Annual_Ridership, rownames = TRUE, colnames = TRUE)
  
  output$Plot1 <- renderPlot({
    q <- ggplot(data=Annual_Ridership_Adjusted, aes(x=factor(Year), y=Average.Annual.Ridership.Mils)) + geom_line(group = 1) +geom_point(colour = "red") + labs(x="Year",y="Average Annual Ridership (Millions)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    print(q)
  })
  
  output$Table2 <- renderTable(Annual_Ridership, rownames = TRUE, colnames = TRUE)
  
  output$Plot2 <- renderPlot({
    q <- scatter.smooth(x=factor(Annual_Ridership_Adjusted$Year),y=Annual_Ridership_Adjusted$Average.Annual.Ridership.Mils, main = 'Annual Ridership Forecast',xlab = "Year", ylab = "Average Annual Ridership (Millions)")
    print(q)
  }) 
  
  #Plot 5
  output$Plot5 <- renderPlot({
    q <- ggplot(data=Operator_Capacity, aes(x=factor(Operator), y=Capacity)) + geom_bar(stat="identity", fill = "darkseagreen4") + ggtitle("Operator Wise Capacity") + labs(x="Operator", y="Capacity (Millions)")
    print(q)
  }) 
  
  #Bus Fleet Table
  datasetInput1 <- reactive({
    Bus_Model %>% filter(Operator == input$Operator) %>% select(-Total.Capacity)
  })
  output$tableBF <- renderTable({
    dataset <- datasetInput1()
  })  
  
  
  
  
  ## Plot 3
  output$Plot3 <- renderPlot({
    ARF = ''
    if (input$operator == "TTS") {
      ARF = Annual_Ridership_Forecast$TTS.Annual.Ridership.Mils}
    else if (input$operator == "SBST") {
      ARF = Annual_Ridership_Forecast$SBST.Annual.Ridership.Mils}
    else if (input$operator == "SMRT") {
      ARF = Annual_Ridership_Forecast$SMRT.Annual.Ridership.Mils}
    else if (input$operator == "GAS") {
      ARF = Annual_Ridership_Forecast$GAS.Annual.Ridership.Mils}
    else {
      ARF = Annual_Ridership_Forecast$Average.Annual.Ridership.Mils}
    
    print(ARF)
    #Average.Annual.Ridership.Mils
    q <- ggplot(data=Annual_Ridership_Forecast, aes(x=factor(Year), y=ARF)) + geom_line(group = 1) +geom_point(colour = "red") + labs(x="Year",y="Average Annual Ridership Forecast (Millions)") + ggtitle("Annual Ridership Forecast") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    print(q)
  })
  #print(ARF)
  print(Capacity_Utilization_Forecast$TTS.Utilization)
  print(Capacity_Utilization_Forecast$SBST.Utilization)
  print(Capacity_Utilization_Forecast$SMRT.Utilization)
  print(Capacity_Utilization_Forecast$GAS.Utilization)
  
  ## Plot 4
  output$Plot4 <- renderPlot({
    CUF = ''
    if (input$operatorX == "TTS") {
      CUF = Capacity_Utilization_Forecast$TTS.Utilization}
    else if (input$operatorX == "SBST") {
      CUF = Capacity_Utilization_Forecast$SBST.Utilization}
    else if (input$operatorX == "SMRT") {
      CUF = Capacity_Utilization_Forecast$SMRT.Utilization}
    else if (input$operatorX == "GAS") {
      CUF = Capacity_Utilization_Forecast$GAS.Utilization}
    else {
      CUF = Capacity_Utilization_Forecast$Utilization}
    
    print(CUF)
    
    q <- ggplot(data = Capacity_Utilization_Forecast ,aes(x=factor(Year), y=CUF)) + geom_line(group = 1) +geom_point(colour = "red") + labs(x="Year",y="Annual Utilization Forecast %") + ggtitle("Capacity Utilization Forecast") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    print(q)
  })
  ##
  
  output$PlotMap <- renderLeaflet({
    q <- mapview(MRT, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE, cex = -30)@map
    print(q)
  })
  
  output$plot <- renderPlot({
    data <- reactive({
      req(input$distance)
      df <- buses %>% filter(dis %in% input$distance) %>% group_by(MRT) %>% summarise(Value = sum(Value))
    })
    g <- ggplot(data(), aes(y = Value ,x = MRT ))
    g+ geom_bar(stat = "sum")
    
    
    
  }) 
  
  output$tableFuel <- renderTable({
    q <- Bus_Fuel_Type
  })
  
  output$PlotPie1 <- renderPlot({
    
    q <- pie(SBST_FUEL, labels = SBST_E_SHARE, col = (rainbow(2)), main = "SBST Bus Fuel Type Share (%)")
    print(q)
    
  })

  output$PlotPie2 <- renderPlot({
    q <- pie(SMRT_FUEL, labels = SMRT_E_SHARE, col = (rainbow(2)), main = "SMRT Bus Fuel Type Share (%)")
  })
  
  output$PlotPie3 <- renderPlot({
  pie(GAS_FUEL, labels = GAS_E_SHARE, col = (rainbow(2)), main = "GAS Bus Fuel Type Share (%)")
  })
  
  output$PlotPie4 <- renderPlot({
  pie(TTS_FUEL, labels = TTS_E_SHARE, col = (rainbow(2)), main = "TTS Bus Fuel Type Share (%)")
  })
  
}

shinyApp(ui = ui, server = server)
