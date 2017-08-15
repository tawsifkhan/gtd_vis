library(shiny)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$head(includeScript("google-analytics.js")),
    tags$style(
    HTML('
         #sidebar {
         background-color: #FFFFFF;
         }
         body, label, input, button, select { 
         font-family: "Georgia";
         }')
  )),
  pageWithSidebar(
    headerPanel(''),
    sidebarPanel(id='sidebar',
      tags$head(tags$style(type="text/css", "
                           #loadmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #ffffff;
                           background-color: #000000;
                           z-index: 105;
                           opacity: 0.7;
                           }
                           ")),
      h3('Drag the end-points to select your timeframe',style = "font-size:14px;
         font-weight: bold; top:0px; text-align:center;"),
      sliderInput("yearInput", "Year", 
                  min=1970, 
                  max=2016,
                  value=c(1990, 2000),
                  sep = ""),
      selectInput("analysis", "Summaries:", 
                 choices = c('Deaths by Regions'='1','Weapons'='2',
                 'Weapons by Regions'='3','Avg. Deaths by Countries (Highest 10) '='4',
                 'Countries with most events (Highest 10)'='5',
                 'Average Deaths vs Number of Events'='6')
                 ),
      
      conditionalPanel(condition="input.analysis=='6'",
      plotlyOutput(outputId="chart2",width = '100%',height='350px')),
      conditionalPanel(condition="input.analysis!='6'",
      plotOutput(outputId="chart1",width = '100%',height='350px')),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage"))
      ),
    mainPanel(h3('Exploring the Global Terrorism Database',style="
                 text-align:center;font-weight:Bold"),
              h3(textOutput("text1"),style = "
                 position:absolute;
                 top:70px;
                 width:100%;
                 color: black;
                 padding: 10px;
                 z-index: 1000;"),
              plotlyOutput(outputId="plot", width="100%",height="600px"),
              h6(textOutput("text2"),
                 style = "
                 position:absolute;
                 top:120px;
                 text-align:left;
                 width:100%;
                 color: grey;
                 padding: 10px;"),
              h6(a('Data: Global Terrorism Database by UMD START downloaded from in Kaggle',
                 href="https://www.kaggle.com/START-UMD/gtd"),
                 style = "
                 position:absolute;
                 bottom:20px;
                 width:100%;
                 text-align:right;
                 color: grey;
                 padding: 10px;"),
              h6( a("Get the code",href="https://github.com/tawsifkhan/gtd_vis"),
                 style = "
                 position:absolute;
                 bottom:40px;
                 width:100%;
                 text-align:right;
                 color: grey;
                 padding: 10px;")
                 )
    
)
)