library(maptools)
require(rgdal)
library(ggplot2)
library(maps)
library(rworldmap)
library(shiny)
library(leaflet)
library(plotly)
library(geojsonio)
library(rmapshaper)
library(data.table)
library(crosstalk)
# Loading package
library(reshape2)
library(magrittr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tidyr)
library(purrr)
library(pipeR)
library(stringi)
library(stringr)
library(lazyeval)
library(rCharts)
library(xml2, warn.conflicts = FALSE)
library(rlist, warn.conflicts = FALSE)
library(fasttime, warn.conflicts = FALSE)
library(mongolite, warn.conflicts = FALSE)
library(ggplot2)
library(parallel)
library(doSNOW)
library(foreach)
library(sf)
library(maptools)
library(ncdf4)
require(rgdal)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(oce)
library(raster)
library(shinydashboard)
library(crosstalk)
library(gapminder)
library(randomForest)
# geography data
df_shp <- readShapeSpatial('./data/ref-nuts-2016-03m/NUTS_RG_03M_2016_4326_LEVL_3.shp')
df_shp <- st_as_sf(df_shp) %>>% filter(CNTR_CODE=='UK')
st_crs(df_shp) <- 4326

# employment data
df_employment <- fread('./data/employment_data_nuts.csv')

select_emp_cols <- c("Economic activity rate - aged 16-64", "Employment rate - aged 16-64",                
"Unemployment rate - aged 16-64", "% who are economically inactive - aged 16-64", 
"% of economically inactive who want a job","% of economically inactive who do not want a job")

names(select_emp_cols) <- c(
  'Economic activity rate',
  'Employment rate',
  'Unemployment rate',
  '% who are economically inactive',
  '% of economically inactive who want a job',
  '% of economically inactive who do not want a job'
)


# educational data
df_education <- fread('./data/education_data_nuts.csv')
select_edu_cols <- c('achieve_rate_GCSE', 'FSM', 'GRANTFUNDING', 'PERCTOT')
names(select_edu_cols) <- c('Predicted GCSE pass rate', 'Percentage of free-school meals','Public funding (pounds)', 'Overall absence')





dash_header <- dashboardHeader(
                title = "KCL_Koders_"
               )
dash_sidebar <- dashboardSidebar(width = NULL,
                                 sidebarMenu(
                                   sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
                                   menuItem("Main", tabName = "main", icon = icon("dashboard")),
                                   menuItem("Analysis", tabName='analysis', icon = icon('bar-chart'))

                                 
                                 )
                )
dash_body <- dashboardBody(
              tags$head(tags$style(HTML('
                  .main-header .logo {
                                        font-family: "Georgia", Times, "Times New Roman", serif;
                                        font-weight: bold;
                                        font-size: 24px;
                  } 
                .small-box {height: 600px; wiehgt:1200px;font-size:50px;}
            

                                        '))),
              tags$style("#predicted_value{color: black;font-size: 200px; font-style:oblique}", 
                         "#predicted_variable{color: black;font-size: 50px; font-style:oblique}"),
              tabItems(
                      tabItem(tabName='main',
                              fluidRow(
                                #actionButton("submit", label = "Submit", icon = icon("play"), class="btn-info", width=NULL),
                                tabBox(id='main_parameters', height=300, title='Parameters', side='right', width = 4,
                                       tabPanel('Year', 
                                                fluidRow(
                                                  column(width=8, selectInput("select_eudcation_year", h5("Education"), choices = unique(df_education$year), selected = 2014) ,
                                                         selectInput("select_employment_year", h5("Employment"), choices = unique(df_employment$Year), selected = 2017)),
                                                  column(width=4, br(), br(), br(), 
                                                         actionButton("submit_year", label = "Submit", icon = icon("play-circle", 'fa-3x')))
                                                ),
                                                icon=shiny::icon('calendar')
                                                ),
                                       tabPanel('Measurement', 
                                                column(width=8, 
                                                       selectInput('select_education_measurement', h5('Education'),choices=select_edu_cols, selected='achieve_rate_GCSE'),
                                                       selectInput('select_employment_measurement', h5('Employment'),choices=select_emp_cols, selected = 'Economic activity rate - aged 16-64')
                                                       ),
                                                column(width=4, br(), br(), br(), 
                                                       actionButton("submit_measurement", label = "Submit", icon = icon("play-circle", 'fa-3x'))
                                                       ),
                                                icon=shiny::icon('calculator'))
                                ),
                                box(width=8, height=300, title='Data Scoure', status = 'warning',
                                    solidHeader = T,
                                    tagList(tags$h3("Educational Data:"), 
                                            tags$h4("https://www.compare-school-performance.service.gov.uk/download-data"),
                                            tags$h3("Geography Data:"),
                                            tags$h4("http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts#nuts16"),
                                            tags$h3("Employment Data:"),
                                            tags$h4("http://www.nomisweb.co.uk")
                                            )
                                    )
                                
                    
                                
                              ),
                              fluidRow(
                                    column(width=6,
                                           box(width=NULL,
                                               height=700,
                                               status = 'primary',
                                               solidHeader = TRUE,
                                               title=tagList(shiny::icon('graduation-cap'),  'Education'),
                                               leafletOutput('education_map', height=600)
                                               )
                                           ),
                                    column(width=6,
                                           box(width=NULL,
                                               height=700,
                                               status = 'primary',
                                               solidHeader = TRUE,
                                               title=tagList(shiny::icon('briefcase'),  'Employment'),
                                               leafletOutput('employment_map', height=600)
                                               )
                                           )
                                    
                                    
                                       )
                              
                              
                              
                              ),
                      tabItem(tabName='analysis',
                              fluidRow(
                                    column(width=4,
                                           box(height=500,width=NULL,status='primary',solidHeader = T,
                                               title=tagList(shiny::icon('exclamation'), 'Factor Importance'),
                                               plotlyOutput('plot_imp')
                                               ),
                                           box(height=400,width=NULL,status='info',solidHeader = T,
                                               title=tagList(shiny::icon('info'), 'Factor Definition'),
                                               tagList(tags$h3("achieve_rate_GCSE:"), 
                                                       tags$h4("Predicted GCSE pass rate"),
                                                       tags$h3("FSM:"),
                                                       tags$h4("Percentage of free-school meals"),
                                                       tags$h3("GANTRUNDING:"),
                                                       tags$h4("Public funding (pounds)"),
                                                       tags$h3("PERCTOT:"),
                                                       tags$h4("Overall absence"))
                                           )
                                           ),
                                    
                                    column(width=8,
                                           box(height=500, width=NULL, status='primary',solidHeader = T,
                                               title=tagList(shiny::icon('list-ul'), 'Features'),
                                               sliderInput("slider_gcse", 'Predicted GCSE pass rate',
                                                           min = round(min(df_education$achieve_rate_GCSE), 2), 
                                                           max = round(max(df_education$achieve_rate_GCSE), 2), 
                                                           value = round(median(df_education$achieve_rate_GCSE), 2),
                                                           step=0.1),
                                               sliderInput("slider_fsm", 'Percentage of free-school meals',
                                                           min = round(min(df_education$FSM), 2), 
                                                           max = round(max(df_education$FSM), 2), 
                                                           value = round(median(df_education$FSM), 2),
                                                           step=0.1),
                                               sliderInput("slider_pubf", 'Public funding (pounds)',
                                                           min = round(min(df_education$GRANTFUNDING), 0), 
                                                           max = round(max(df_education$GRANTFUNDING), 0), 
                                                           value = round(median(df_education$GRANTFUNDING), 0),
                                                           step=10),
                                               sliderInput("slider_perc", 'Overall absence',
                                                           min = round(min(df_education$PERCTOT), 2), 
                                                           max = round(max(df_education$PERCTOT), 2), 
                                                           value = round(median(df_education$PERCTOT), 2),
                                                           step=0.1)
                                               
                                               ),
      
                                           box(height=400, width=NULL, status='primary', solidHeader=T,
                                               title=tagList(shiny::icon('line-chart'), 'Predicted Value'),
                                               br(),
                                               textOutput('predicted_variable'),
                                               textOutput('predicted_value')
                                               )
                                           
                                           )
                                      )
                              )
                      )
)
            
ui <- dashboardPage(
  dash_header,
  dash_sidebar,
  dash_body
)

server <- function(input, output) {
  
  df_employment_map <- eventReactive(input$submit_year | input$submit_measurement, {
    
    df_map <- left_join(df_shp, 
                        df_employment[Year==input$select_employment_year, c('mnemonic', input$select_employment_measurement, 'Year'), with=F]
                        , by=c('NUTS_ID'='mnemonic'))
    
    
    colnames(df_map)[colnames(df_map) == input$select_employment_measurement] <- 'value'
    
    df_map$tooltip <-  paste("Country: <b>", df_map$CNTR_CODE,"</b><br/>",
                         "Region: <b>", df_map$NUTS_NAME, "</b><br/>",
                         str_replace(input$select_employment_measurement, ' *- aged.*', ''), ": <b>", df_map$value, "%</b>",
                         sep="") %>>% 
                       lapply(htmltools::HTML)
    
    return(list(data=df_map, legend_title=str_replace(input$select_employment_measurement, ' *- aged.*', '')))
    
  })
  output$employment_map <- renderLeaflet({
      df_map <- df_employment_map()
      
      legend_title <- df_map$legend_title
      df_map <- df_map$data
      
      mypalette <- colorNumeric(palette="viridis", domain=df_map$value, na.color="transparent")
      leaflet(df_map) %>>% addTiles() %>>%
        addPolygons(opacity = 0,
                    color = 'white',
                    weight = .25,
                    fillOpacity = 0.5,
                    fillColor = ~mypalette(value),
                    label=~tooltip,
                    smoothFactor = 0) %>>%
        addLegend(pal=mypalette, values=~value, opacity=0.2, 
                  title = legend_title, position = "bottomleft",
                  labFormat = labelFormat(
                    suffix = "%"
                  ))

    })
    
  
  
  
  
  df_education_map <- eventReactive(input$submit_year | input$submit_measurement, {
    
    df_map <- left_join(df_shp, 
                        df_education[year==input$select_eudcation_year, c('NUTS_ID', input$select_education_measurement, 'year'), with=F]
                        , by='NUTS_ID')
    
    colnames(df_map)[colnames(df_map) == input$select_education_measurement] <- 'value'
    
    if(input$select_education_measurement=='GRANTFUNDING'){
      df_map$tooltip <-  paste("Country: <b>", df_map$CNTR_CODE,"</b><br/>",
                               "Region: <b>", df_map$NUTS_NAME, "</b><br/>",
                               str_replace(input$select_education_measurement, ' *- aged.*', ''), ": <b>", df_map$value, " £</b>",
                               sep="") %>>% 
        lapply(htmltools::HTML)
    } else {
      df_map$tooltip <-  paste("Country: <b>", df_map$CNTR_CODE,"</b><br/>",
                               "Region: <b>", df_map$NUTS_NAME, "</b><br/>",
                               str_replace(input$select_education_measurement, ' *- aged.*', ''), ": <b>", round(df_map$value, 2), "%</b>",
                               sep="") %>>% 
        lapply(htmltools::HTML)
    }
      
   
    
    return(list(data=df_map, legend_title=input$select_education_measurement))
    
  })
  output$education_map <- renderLeaflet({
    df_map <- df_education_map()
    
    legend_title <- df_map$legend_title
    df_map <- df_map$data
    
    mypalette <- colorNumeric(palette="viridis", domain=df_map$value, na.color="transparent")
    
    if(legend_title!='GRANTFUNDING'){
      
      leaflet(df_map) %>>% addTiles() %>>%
        addPolygons(opacity = 0,
                    color = 'white',
                    weight = .25,
                    fillOpacity = 0.5,
                    fillColor = ~mypalette(value),
                    label=~tooltip,
                    smoothFactor = 0) %>>%
        addLegend(pal=mypalette, values=~value, opacity=0.2, 
                  title = legend_title, position = "bottomleft",
                  labFormat = labelFormat(
                    suffix = "%"
                  ))
    } else{
      leaflet(df_map) %>>% addTiles() %>>%
        addPolygons(opacity = 0,
                    color = 'white',
                    weight = .25,
                    fillOpacity = 0.5,
                    fillColor = ~mypalette(value),
                    label=~tooltip,
                    smoothFactor = 0) %>>%
        addLegend(pal=mypalette, values=~value, opacity=0.2, 
                  title = legend_title, position = "bottomleft",
                  labFormat = labelFormat(
                    suffix = " £"
                  ))
    }
    
    
  })
  
  model_rf <- eventReactive(input$submit_year | input$submit_measurement,{
    df_emp <- df_employment[Year==input$select_employment_year, c('mnemonic', input$select_employment_measurement, 'Year'), with=F]
    colnames(df_emp)[colnames(df_emp) == input$select_employment_measurement] <- 'value'
    
    setnames(df_emp, 'mnemonic', 'NUTS_ID')
    
    df_emp <- merge(df_education[year==input$select_eudcation_year],df_emp,by='NUTS_ID', all.x=F, all.y=F)
    
    
    df_emp <- df_emp[, c('achieve_rate_GCSE','FSM','GRANTFUNDING','PERCTOT','value'), with=F] %>>%
      `[`(!is.na(value))
    
    model_rf <- randomForest(value ~ ., data=df_emp, ntree=500, keep.forest=T, importance=TRUE)
    
    return(list(model=model_rf, legend_title=str_replace(input$select_employment_measurement, ' *- aged.*', ''), data=df_emp))
  })
  
  
  output$plot_imp <- renderPlotly({
    
    model_rf <- model_rf()
    model_rf <- model_rf$model
    
    ay <- list(
      title = "",
      zeroline = F,
      showline = F,
      showticklabels = T,
      showgrid = F,
      tickangle = 45
    )
    
    ax <- list(
      title = "",
      zeroline = F,
      showline = F,
      showticklabels = F,
      showgrid = F
    )
    
    variable_name <- rownames(importance(model_rf))
    df_imp <- importance(model_rf) %>>% as.data.table %>>%
      `[`(, variable:=variable_name) %>>% setorderv('IncNodePurity', 1)
    
    plot_ly(df_imp, y = ~factor(variable, df_imp$variable), x = ~IncNodePurity) %>% 
      add_bars(orientation = 'h') %>>%
      layout(showlegend = FALSE, xaxis=ax, yaxis=ay, margin = list(l = 100))    
  })
  
  output$predicted_value <- renderText({ 
    model_rf <- model_rf()
    model_df <- model_rf$data
    legend_title <- model_rf$legend_title
    model_rf <- model_rf$model
    
    df <- data.table(achieve_rate_GCSE=input$slider_gcse,
               FSM=input$slider_fsm,
               GRANTFUNDING=input$slider_pubf,
               PERCTOT=input$slider_perc
               )
    
    predicted_value <- predict(model_rf, newdata=df)
    if(predicted_value > max(model_df$value)){
      predicted_value <- max(model_df$value)
      return(paste(round(predicted_value, 2), '%', sep=''))
    } else if(predicted_value < min(model_df$value)){
      predicted_value <- min(model_df$value)
      return(paste(round(predicted_value, 2), '%', sep=''))
    } else {
      return(paste(round(predicted_value, 2), '%', sep=''))
    }

    
    })
  output$predicted_variable <- renderText({
    df <- df_employment_map()
    return(df$legend_title)
  })
  
}

shinyApp(ui, server)



