library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(survival)
library(survminer)
library(waterfalls)


##################################
ui <- fluidPage(
  headerPanel("Statistical analysis"), 
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Data Source", fileInput('file1', 'Select your file', 
                                                   accept = c( 
                                                     'text/csv', 
                                                     'text/comma-separated-values','.csv', 
                                                     '.sas7bdat') 
                )),
                tabPanel("scatterplot", plotOutput("splot"), click = "plot_click"),
                tabPanel("boxplot", plotOutput("bplot")),
                tabPanel("histogram", plotOutput("hist")), 
                tabPanel("Pie Chart", plotOutput("pie")),
                tabPanel("Bar Chart", plotOutput("bar")),
                tabPanel("KM plot", plotOutput("surv")),
                tabPanel("Waterfall Plot", plotOutput("wf"))
    )))

server <- function(input, output, session) {
  # Import data 
  data1 <- reactive({ 
    inFile <- input$file1 
    if(is.null(file)){return()} 
    read.csv(inFile$datapath) 
  }) 
  #plotting Box plot
  #output$sum <- renderPrint({summary(alltte <- read_csv("alltte.csv")) })
  
  #plotting scatter plot
  output$splot <- renderPlot({ alltte <- read_csv("alltte.csv")
  ggplot(alltte, aes(x=AGE, y=AVAL, color=COHORT)) + 
    geom_point(size=6) 
  })
  
  #plotting Box plot
  output$bplot <- renderPlot({alltte<-read_csv("alltte.csv") 
  ggplot(alltte, aes(x=COHORT, y=AVAL, color=COHORT)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)})
  
  #Plotting histogram
  output$hist <- renderPlot({alltte<-read_csv("alltte.csv") 
  hist(alltte$AVAL, 
       main="Histogram of OS Duration", 
       xlab="OS Duration", 
       border="blue", 
       col="lightgreen",
       xlim=c(0,100),
       las=1, 
       breaks=5)

  
  # Piechart
  output$pie <- renderPlot({alltte<-read_csv("alltte.csv") 
  ggplot(alltte, aes(x="", y=MORPHO, fill=MORPHO,main="Pie chart of morphological characteristics")) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) })
  
  # Bar chart
  output$bar <- renderPlot({alltte<-read_csv("alltte.csv") 
  barplot(height=alltte$AVAL, names=alltte$SUBJID, color=alltte$subjid,  xlab="Subject", 
          ylab="OS Duration ", 
          main="Bar chart of Overall Survival Durations", 
          ylim=c(0,60)) })
  
  #plotting PFS KM ESTIMATES 
  output$surv <- renderPlot({alltte<-read_csv("alltte.csv") 
  ggsurvplot(survfit(Surv(AVAL , CNSR) ~ COHORT , data = alltte) , risk.table = TRUE, pval = TRUE, data = alltte, main = "KM estimates of Overall Survival") })
  
  })
  
  # waterfall plot
  output$wf <- renderPlot({alltte<-read_csv("alltte.csv") 
  col <- ifelse(alltte$BOR == "CR", "green", 
                ifelse(alltte$BOR == "PR", "steelblue", 
                       ifelse(alltte$BOR == "PD", "red", 
                              ifelse(alltte$BOR == "SD", "cadetblue", 
                                     ifelse(alltte$BOR == "NE", "yellow", 
                                     "") 
                       ))))
  
  barplot(alltte$PCHG, 
          col=col, 
          border=col, 
          space=0.5, ylim=c(-100,100), 
          main = "Waterfall plot for Target Lesion Tumor Size", 
          ylab="Percent Change from baseline (%)",
          cex.axis=1.5, cex.lab=1.5, 
          legend.text= c( "CR", "PR", "SD", "PD", "NE"),args.legend=list(title="Best Overall Response", fill=c("green","steelblue",  "cadetblue", "red", "yellow"), border=NA, cex=1.0))
  abline(h=20, col = "black", lwd=0.5) # The "PD" line
  abline(h=-30, col = "black", lwd=0.5) # This "PR" line
  
  })
  
}

shinyApp(ui = ui, server = server)





