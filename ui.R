library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)

mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: 1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
"
ml <- c(1970,1975,1980,1985,1990,1995,2000,2005,2010,2016)
ui <- fluidPage(
  tags$head(tags$style(
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
              h6('Source: Global Terrorism Database found in Kaggle',
                 style = "
                 position:absolute;
                 bottom:40px;
                 width:100%;
                 text-align:right;
                 color: grey;
                 padding: 10px;"),
              h6( a("Github",href="http://github.com/tawsifkhan"),
                 style = "
                 position:absolute;
                 bottom:20px;
                 width:100%;
                 text-align:right;
                 color: grey;
                 padding: 10px;")
                 )
    
)
)