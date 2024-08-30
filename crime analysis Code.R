#install.packages("shiny")
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("rvest")
#install.packages("plotly")
#install.packages("treemapify")
#install.packages("corrplot")

library(shiny)
library(readr)
library(rvest)
library(tidyverse)
library(ggplot2)
library(treemapify)
library(dplyr)
library(plotly)
library(corrplot )


##fetching data 
data<- read_html("https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_crime_rate") %>% html_table()
table1 <- data[[3]]
tablel1=data[[4]]

## data cleaning

names <- colnames(table1)
names[2]<-"Total IPC & SLL Crimes (State/UT-wise)\n2018"
names[3]<-"Total IPC & SLL Crimes (State/UT-wise)\n2019"
names[4]<-"Total IPC & SLL Crimes (State/UT-wise)\n2020"
names[5]<-"Total IPC & SLL Crimes (State/UT-wise)\n2021"
names[6]<-"% share of state/UT\n2018"
names[7]<-"% share of state/UT\n2019"
names[8]<-"Crime Rate 2018"
names[9]<-"Crime Rate 2019"
names[10]<-"Crime Rate 2020"
names[11]<-"Crime Rate 2021"
colnames(table1) <-names
table1<- table1 %>% select(-6)
table1<- table1 %>% select(-6,-11)
table1<- table1 %>% slice(-32)
table1<- table1 %>% slice(-1)
table1<- table1 %>% slice(-2)

tablel1=tablel1 %>% slice(-2,-31)
table1 <- table1 %>%
  arrange(ifelse(row_number() == 1, 37, ifelse(row_number() == 37, 1, row_number())))

tablel1<- tablel1 %>%
  arrange(ifelse(row_number() == 1, 37, ifelse(row_number() == 37, 1, row_number())))
table1 <- table1[1:36,]

year <- rep(c(2018,2019,2020,2021),c(36,36,36,36))
new_df = data.frame(year)
new_df$Crime <- c(table1$`Crime Rate 2018`,table1$`Crime Rate 2019`,table1$`Crime Rate 2020`,table1$`Crime Rate 2021`)
new_df$`State/UT` <- rep(table1$`State/UT`,4)
as_tibble(new_df)
new_df$Crime=as.numeric(new_df$Crime)


table1$`Total IPC & SLL Crimes (State/UT-wise)
2018`=as.numeric(table1$`Total IPC & SLL Crimes (State/UT-wise)
2018`)
table1$`Total IPC & SLL Crimes (State/UT-wise)
2019`=as.numeric(table1$`Total IPC & SLL Crimes (State/UT-wise)
2019`)
table1$`Total IPC & SLL Crimes (State/UT-wise)
2020`=as.numeric(table1$`Total IPC & SLL Crimes (State/UT-wise)
2020`)
table1$`Total IPC & SLL Crimes (State/UT-wise)
2021`=as.numeric(table1$`Total IPC & SLL Crimes (State/UT-wise)
2021`)
table1$`Crime Rate 2018`=as.numeric(table1$`Crime Rate 2018`)
table1$`Crime Rate 2019`=as.numeric(table1$`Crime Rate 2019`)
table1$`Crime Rate 2020`=as.numeric(table1$`Crime Rate 2020`)
table1$`Crime Rate 2021`=as.numeric(table1$`Crime Rate 2021`)
table1$`Investigation Rate % (IPC)`=as.numeric(table1$`Investigation Rate % (IPC)`)





tablel1$`Violent Crimes`=as.numeric(tablel1$`Violent Crimes`)
tablel1$Murder=as.numeric(tablel1$Murder)
tablel1$Rape=as.numeric(tablel1$Rape)
tablel1$Kidnapping=as.numeric(tablel1$Kidnapping)
tablel1$`Crimes Against Children`=as.numeric(tablel1$`Crimes Against Children`)
tablel1$Extortion=as.numeric(tablel1$Extortion)
tablel1$Robbery=as.numeric(tablel1$Robbery)
tablel1$`Hit & Run`=as.numeric(tablel1$`Hit & Run`)
tablel1$`Drugs Trafficking`=as.numeric(tablel1$`Drugs Trafficking`)
tablel1$`Illegal Arms`=as.numeric(tablel1$`Illegal Arms`)

table1=table1[,c(-11,-12,-10)]
write.csv(table1, "table1.csv")
write.csv(tablel1, "tablel1.csv")


## making of r shiny



# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #sidebarPanel {
        margin-top: 100px;  /* Adjust the margin-top value as needed */
      }
    "))
  ),
  tags$head(
    tags$style(HTML("
      #distPlot {
        margin-top: 50px;  /* Adjust the margin-top value as needed */
      }
    "))
  ),
  
  titlePanel("Crime Analysis of India"),
  
  navbarPage("Crime Analysis of India",
             
             tabPanel("Summary",
                      sidebarPanel(id="sidebarPanel14",
                                   selectInput(inputId = "sum",
                                               label="Choose the dataset",
                                               choices=c("Timewise"=1, "Crimewise"=2))
                                   
                                   
                      ),
                      mainPanel
                      (
                        verbatimTextOutput("Sum")
                      )),
             
             tabPanel("Timewise",
                      sidebarPanel(id = "sidebarPanel",
                                   selectInput(inputId = "Yearwisecolumn",
                                               label = "Choose Year:",
                                               choices = c("Crime Rate 2018", "Crime Rate 2019", "Crime Rate 2020", "Crime Rate 2021")),
                      ),
                      mainPanel(
                        plotOutput("distPlot"),
                      )
             ),
             
             
             
             tabPanel("crimewise",
                      sidebarPanel(id = "sidebarPanel2",
                                   selectInput(inputId = "crime",
                                               label= "choose the crime",
                                               choices = names(tablel1)[-1]
                                   ),
                      ),
                      
                      mainPanel(
                        
                        plotOutput("mapPlot"),
                      )
             ),
             tabPanel("Statewise",
                      sidebarPanel(id="sidebarPanel5",
                                   selectInput(inputId = "statewise",
                                               label = "Choose the state",
                                               choices = tablel1$`State / UT`[-37],
                                               selected="Goa")),
                      mainPanel(
                        plotlyOutput("piechart"),
                        
                      )
             ),
             tabPanel("Timewiseanlysis",
                      sidebarPanel(id = "sidebarPanel3",
                                   selectInput(inputId ="ta",
                                               label="select states",
                                               choices =table1$`State/UT`,
                                               multiple=T),
                                   
                      ),
                      
                      mainPanel(
                        plotOutput("statePlots"),
                      )
             ),
             tabPanel("Correlation",
                      mainPanel(
                        plotOutput("corr")
                      )
                      ),
             
  )
)

# Shiny app server
server <- function(input, output) {
  
  sub <- reactive({
    year <- switch(input$Yearwisecolumn,
                   "Crime Rate 2018" = 6,
                   "Crime Rate 2019" = 7,
                   "Crime Rate 2020" = 8,
                   "Crime Rate 2021" = 9)
    table1 <- table1[c(1, as.numeric(year))]
    colnames(table1) <- c('State/UT', 'Crime Rate')
    return(table1)
  })
  
  output$distPlot <- renderPlot({
    ggplot(sub(), aes(x = `State/UT`, y = `Crime Rate`,fill=`State/UT`),na.rm=T) +
      geom_bar(stat = "identity", width = 0.5, position = "dodge" ) +
      xlab("States") + ylab("Crime Rate") + 
      ggtitle("Crime Rate Along Different ") +
      theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(angle = 30, hjust = 0.5))+ coord_flip()
  })
  
  sub1 <- reactive({
    c = switch(input$crime,
               "Violent Crimes" = 2,
               "Murder" = 3,
               "Rape" = 4,
               "Kidnapping" = 5,
               "Crimes Against Children" = 6,
               "Extortion" = 7,
               "Robbery" = 8,
               "Hit & Run" = 9,
               "Drugs Trafficking" = 10,
               "Illegal Arms" = 11
    )
    tablel1=tablel1[,c(1,c)]
    colnames(tablel1)=c("State/UT","bb")
    tablel1$bb=as.numeric(tablel1$bb)
    return(tablel1)
  })
  
  output$mapPlot <- renderPlot({
    
    
    ggplot(sub1(), aes(area = bb, fill =`State/UT`, label =`State/UT` )) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre", reflow = TRUE) +
      theme_minimal() +
      labs(title = "Statewise division of registered crime cases in 2021")
    
  })
  
  
  
  output$Sum <- renderPrint({
    if(input$sum==1)
      summary(table1)
    else
      summary(tablel1)
  })
  output$state<-renderPlot({
    
  })
  
  output$piechart <- renderPlotly({
    
    
    crime <- tablel1 %>%
      filter( tablel1$`State / UT` ==input$statewise)
    # Extract numeric values for the pie chart
    # Filter out negative or zero values
    crime<- crime[-c(1,2)]
    class(crime)
    crime <- data.frame(t(crime))  
    rownames(crime)
    crime[2] = rownames(crime)
    rownames(crime) = NULL
    colnames(crime) = c("Freq", "Type_crime")
    crime <- crime %>% relocate(Type_crime, Freq)
    
    plot_ly(
      crime,
      labels= ~Type_crime,
      values= ~Freq,
      type="pie",
      textinfo= "label+percent"
      
    )
    
   
    
  })
  s <- reactive({input$ta
    
    
  })
  sub2=reactive({
    T=new_df %>% filter(`State/UT` %in% s())
    T$Crime=as.numeric(T$Crime)
    return(T)
  })
  output$statePlots=renderPlot({
    ggplot(sub2(), aes(x=year)) + 
      geom_line(aes(y=Crime,col=`State/UT`))
    
  })
  output$corr=renderPlot({
    # Calculate the correlation matrix
    correlation_matrix <- cor(tablel1[-1])
    
    # Create the correlation matrix heatmap
    corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45)
    
    
  })
}

shinyApp(ui=ui, server=server)






