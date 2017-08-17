library(plotly)
library(magrittr)
library(ggplot2)
library(dplyr)
library(shiny)
library(data.table)
library(RColorBrewer)
library(scales)


gtd <- fread("Data/mygtd.csv")
gtd <- data.frame(gtd)

server <- function(input, output) {
interpolate <- data.frame(op=c(3,3.2,3.4))#,4,5))
nkillgroup <- function(nkill){
  if (is.numeric(nkill)==FALSE) {
    return('Unknown')
  }
  nkill <- as.numeric(nkill)
  if (is.na(nkill)==TRUE) {
    return('Unknown')
  }
  if (nkill == 0) {
    return('Only casualties')
  }
  
  if (nkill > 0 & nkill <= 5) {
    return('1-5')
  }
  else { 
    if (nkill > 5 & nkill <= 10){
      return('5-10') 
    }
    else {
      if (nkill > 10 & nkill <= 50){
        return('10-50')
      }
      else {
        if (nkill > 50 ){
          return('50+')
        }
        else {
          return('Unknown')
        }
      }
    }
  }
}

ordermyaxis <- function(axis_labels,reorder_elements) {
  j <- length(axis_labels)
  new_i <- which(reorder_elements %in% reorder_elements)
  new_labels <- reorder_elements
  new_labels<-append(new_labels,axis_labels[!axis_labels %in% reorder_elements],after = length(reorder_elements))
  return(new_labels)
}

##Below is the list of functions that create each plot

make_map <- function (df) {
  x_1<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(nkill=sum(nkill,na.rm=TRUE))
  x_2<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(wound=sum(nwound,na.rm=TRUE))
 # x_3<-df %>% group_by(iyear,country_txt,country,longitude,latitude,region_txt,region) %>% summarise(weaptype1=max(weaptype1,na.rm=TRUE))
  
  df<-Reduce(function(...) merge(..., all=TRUE), list(x_1, x_2))
  
  df<-merge(df,interpolate,all=TRUE)
  
  df <- df[with(df, order(iyear,country,longitude,latitude,op)), ]
  
  df$myframe <- paste(df$iyear,df$op)
  
  g <- list(
    #scope = 'world',
    showframe = F,
    showland = T,
    landcolor = 'rgb(54,66,71)'
  )
  
  g1 <- c(
    g,
    resolution = 0,
    projection = list(type = 'Mercator'),
    list(lataxis = list(range = c(-50, 100)))
  )
  
  
  g2 <- c(
    g,
    showcountries = F,
    bgcolor = toRGB("white", alpha = 0)
    #list(domain = list(x = c(0, .6), y = c(0, .6))
  )
  df$Deaths <- unlist(lapply(df$nkill,FUN = nkillgroup))
  p<- df %>%
    plot_geo(locationmode='country names',colors=colfunc(5))%>% 
    add_markers(x=~longitude,y=~latitude,size=~op,frame=~myframe,
                sizes=c(3,6),color=~Deaths,opacity = 0.5,
                text = ~paste('Country:',df$country_txt, '</br>Deaths:',df$nkill)
    )%>% 
    layout(geo=g1,geo2=g2) %>%
    animation_opts(frame = 300, transition = 200, easing = "elastic",
                   redraw = FALSE, mode = "afterall") %>%
    animation_button(
      x = 0, xanchor = "left", y = .1, yanchor = "bottom") %>%
    animation_slider(value = ~iyear,hide=TRUE)
  
  p <- p %>% layout(legend=list(x = 0, y = 0.2))
  
  p<-ggplotly(p)
  
  return(p)
}

deaths_by_regions <- function (df){
  df <- subset(df,df$nkill!=0)
  df$Deaths <- unlist(lapply(df$nkill,FUN = nkillgroup))
  x<-df %>% group_by(region_txt) %>% count()
  x<-arrange(x,n)
  g <- ggplot(df,aes(region_txt))+
    geom_bar(aes(fill=Deaths),width = 0.5) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=.95),
          legend.position = c(.15, .8),
          panel.background = element_rect(fill = '#FFFFFF'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks = element_blank()) +
    scale_fill_brewer(palette = "Reds",direction = 1)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=x$region_txt)
  return(g)
}

weapons<- function (df) {
  df <- subset(df,df$nkill!=0)
  x<-df %>% group_by(weaptype1_txt) %>% count()
  x<-arrange(x,n)
  x$weaptype1_txt<-ordermyaxis(x$weaptype1_txt,c('Unknown','Other'))
  g <- ggplot(df,aes(weaptype1_txt))+
    geom_bar(width = 0.5,fill = "#dd470b",alpha=.7) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=.95),
          legend.position = c(.15, .8),
          panel.background = element_rect(fill = '#FFFFFF'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks = element_blank()
    ) + 
    #scale_fill_brewer(palette = "Red",direction = -1)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=x$weaptype1_txt)
  return(g) 
}

weapons_by_regions <- function(df) {
  df <- subset(df,df$nkill!=0)
  x<-df %>% group_by(weaptype1_txt) %>% count()
  x<-arrange(x,n)
  x$weaptype1_txt<-ordermyaxis(x$weaptype1_txt,c('Unknown','Other'))
  g <- ggplot(df,aes(weaptype1_txt))+
    geom_bar(aes(fill=region_txt),width = 0.5) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=.95),
          legend.position = c(.35, .6),
          legend.title = element_blank(),
          panel.background = element_rect(fill = '#FFFFFF'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks = element_blank()
    ) + 
    scale_fill_brewer(palette = 'Spectral',direction = -1)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=x$weaptype1_txt)
  return(g) 
}

avg_deaths_by_country <- function(df) {
  df <- subset(df,df$nkill!=0)
  x<-df %>% group_by(country_txt) %>% summarise(avg = mean(nkill,na.rm=TRUE))
  x<-arrange(x,avg)
  x<-subset(x,x$country_txt != 'NA')
  
  x$country_txt <- factor(x$country_txt, levels = x$country_txt)
  x<-tail(x,10)
  g <- ggplot(x,aes(country_txt,avg))+
    geom_bar(width = 0.5,stat = 'Identity',fill = "#dd470b",alpha=.7)+
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=.95),
          legend.position = "none",
          panel.background = element_rect(fill = '#FFFFFF'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks = element_blank()) +
    #scale_fill_brewer(palette = "Reds",direction = 1)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=x$country_txt)
  return(g)
}

countries_with_most_events <- function (df) {
  df <- subset(df,df$nkill!=0)
  x<-df %>% group_by(country_txt) %>% summarise(n = sum(nkill,na.rm=TRUE))
  x<-arrange(x,(n))
  x<-subset(x,x$country_txt != 'NA')
  x<-tail(x,10)
  g <- ggplot(x,aes(country_txt,n))+
    geom_bar(width = 0.5,stat = 'Identity',fill = "#dd470b",alpha=.7) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=.95),
          legend.position = "none",
          panel.background = element_rect(fill = '#FFFFFF'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks = element_blank()) + 
    #scale_fill_brewer(palette = "Reds",direction = 1)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=x$country_txt)
  return(g) 
  
}

avg_deaths_events <- function(df) {
  df <- subset(df,df$nkill!=0)
  x_1<-df %>% group_by(country_txt) %>% summarise(avg = mean(nkill,na.rm=TRUE))
  x_2<-df %>% group_by(country_txt) %>% count()
  x <- merge(x_1,x_2,by = 'country_txt')
  x<-subset(x,x$country_txt != 'NA')
  
  myfont <- list(
    family='Helvetica',
    wieght='Bold',
    size=9)
  
  g <- plot_ly(data=x,x=~n,y=~avg,color=~country_txt,colors='RdYlGn',mode='markers',
               alpha = 0.7,
               text = ~paste('Country: ', x$country_txt,
                             '</br>Events: ', x$n,
                             '</br>Avg. Deaths: ', round(x$avg,2))
  ) %>%
    layout(showlegend=FALSE,
           yaxis=list(
             title = 'Avg. Deaths',
             showline = FALSE,
             showgrid = FALSE,
             zeroline = FALSE,
             titlefont = myfont,
             tickfont = myfont
           ),
           xaxis=list(
             title = 'Number of Events',
             showline = FALSE,
             showgrid = FALSE,
             zeroline = FALSE,
             titlefont = myfont,
             tickfont = myfont
             
           ),
           autosize = T, margin = list(
             l = 40,
             r = 0,
             b = 40,
             t = 0,
             pad = 4
           )
    ) %>% config(displayModeBar = F)
  return(g)
}

colfunc <- colorRampPalette(c("orange", "royalblue","springgreen"))


output$plot <- renderPlotly({
    df <- subset(gtd,gtd$iyear>=input$yearInput[1] & gtd$iyear <= input$yearInput[2])
    plot <- make_map(df)
  })
  output$text1 <- renderText({paste("Year: ",input$yearInput[1],"-",input$yearInput[2])})
  output$text2 <- renderText({'(Events with no deaths and casualties were removed)'})
  
  output$chart1 <- renderPlot({
    df <- subset(gtd,gtd$iyear>=input$yearInput[1] & gtd$iyear <= input$yearInput[2])
    if(input$analysis=='1'){
      plot <- deaths_by_regions(df)
    } else {
      if(input$analysis=='2'){
        plot <- weapons(df)
      }
      else {
        if(input$analysis=='3'){
          plot <- weapons_by_regions(df)
        }
        else {
          if(input$analysis=='4'){
            plot <- avg_deaths_by_country(df)
          } else {
            if(input$analysis=='5'){
              plot <- countries_with_most_events(df)
            }
          }
        }
      }
    }
    return(plot)
  })
  
  output$chart2 <- renderPlotly({
    df <- subset(gtd,gtd$iyear>=input$yearInput[1] & gtd$iyear <= input$yearInput[2])
    plot <- avg_deaths_events(df)
  })

}