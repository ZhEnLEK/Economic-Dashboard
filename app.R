
library(shiny)
library(plotly)
library(odbc)
#library(RODBC)
library(DBI)
library(dplyr)
library(lubridate)
library(zoo)
library(RSQLite)


#setwd("C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\")
#Working directory must be set whenever database needed to be reaccessed after updating
sqlite <- dbDriver("SQLite")
conn <- dbConnect(sqlite, "economic.db")

manufacturing <- dbGetQuery(conn = conn, "select * from manufacturing")
rubber <- dbGetQuery(conn =conn, "select * from rubber")
GDP <- dbGetQuery(conn =conn, "select * from GDP")
Sector_ID <- dbGetQuery(conn = conn, "select * from Sector_ID")
CPI <- dbGetQuery(conn = conn, "select * from CPI")
CPI_ID <- dbGetQuery(conn = conn, "select * from CPI_ID")
df_com <- dbGetQuery(conn = conn, "select * from Commodities")
Commodities_ID <- dbGetQuery(conn = conn, "select * from Commodities_ID")
External_Trade <- dbGetQuery(conn = conn, "select * from ExternalTrade")
Trade_ID <- dbGetQuery(conn = conn, "select * from Trade_ID")
df_lf <- dbGetQuery(conn = conn, "select * from LabourForce")

#data
# Define UI for application that draws a histogram

my.month.name <- Vectorize(function(n) c("January", "February", "March", 
                                         "April", "May", "June", "July", 
                                         "August", "September", "October",
                                         "November", "December")[n])

months_list<- c("January", "February", "March", 
           "April", "May", "June", "July", 
           "August", "September", "October",
           "November", "December")

cpi_item_list<- c("Food & Non-Alcoholic Beverages"                                  
                  , "Alcoholic Beverages & Tobacco"                                   
                  , "Clothing & Footwear"                                             
                  , "Housing, Water, Electricity, Gas & Other Fuels"                  
                  , "Furnishings, Household Equipment & Routine Household Maintenance"
                  , "Health"                                                          
                  , "Transport"                                                       
                  , "Communication"                                                   
                  , "Recreation Services & Culture"                                   
                  , "Education"                                                       
                  , "Restaurants & Hotels"                                            
                  , "Miscellaneous Goods & Services"                                  
                  , "Total")

timeline<-c("Year-to-Year", "Month-to-Month")
timeline_q2q <- c("Year-to-Year", "Quarter-to-Quarter")
value <- c("Value (RM billion)", "Change (%)")

 # con <- dbConnect(odbc(), Driver = "SQL Server", Server = "DESKTOP-U753JSI", 
 #                  Database = "Economic", Trusted_Connection = TRUE)
# 
# 
# CPI<-dbGetQuery(conn = con, "SELECT * FROM Economy.CPI")
# CPI_ID<- dbGetQuery(conn = con, "SELECT * FROM Economy.CPI_ID")
# 
# Sector_ID <- dbGetQuery(conn = con, "sELECT * FROM Economy.Sector")
# Commodities_ID <- dbGetQuery(conn = con, "SELECT * FROM Economy.Commodities_ID")
# Trade_ID <- dbGetQuery(conn = con, "SELECT * FROM Economy.Trade_ID")
# 
# df<- left_join(CPI, CPI_ID, by =c("Items_ID" = "ID"))
# write.csv(df, "C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\df.csv")
# 
# 


# 
#  df_gdp <- dbGetQuery(conn = con, "SELECT * FROM Economy.GDPRecord")
  df_gdp<- left_join(GDP, Sector_ID, by = c("Sector_ID" = "ID"))
  df_cpi<- left_join(CPI, CPI_ID, by = c("Items_ID" = "ID") )
#  write.csv(df_gdp, "C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\df_gdp.csv")
# 
# 
# df_com<- dbGetQuery(conn = con, "SELECT * FROM Economy.Commodities")
 df_com<- left_join(df_com, Commodities_ID, by = c("Commodities_ID" = "ID"))
# write.csv(df_com, "C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\df_com.csv")
# 
# 
# df_xt <- dbGetQuery(conn = con, "SELECT * FROM Economy.ExternalTrade")
 df_xt <- left_join(External_Trade, Trade_ID, by = c("Trade_ID" = "ID"), suffix = c("_x", "_y") )
# write.csv(df_xt, "C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\df_xt.csv")
# 
# 
# df_lf <- dbGetQuery(conn = con, "SELECT * FROM Economy.LabourForce")
# write.csv(df_lf, "C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\df_lf.csv")

#setwd("C:\\Users\\zhenl\\Desktop\\New Shiny App\\new\\")

#df <- read.csv("df.csv")
df_cpi$date <- as.Date(paste(df_cpi$Year,"-", df_cpi$Month,"-01", sep = ""))

#df_gdp <- read.csv("df_gdp.csv")
df_gdp$date <- as.yearqtr(paste(df_gdp$Year, df_gdp$Quarter, sep = ""), format = "%Y%q")

#df_com <- read.csv("df_com.csv")
df_com$date <- as.Date(paste(df_com$Year,"-01-01", sep = ""))

#df_xt <- read.csv("df_xt.csv")
df_xt$date <- as.Date(paste(df_xt$Year,"-", df_xt$Month,"-01", sep = ""))

#df_lf <- read.csv("df_lf.csv")
df_lf$date <- as.Date(paste(df_lf$Year, "-", df_lf$Month, "-01", sep = ""))

manufacturing$date <- as.Date(paste(manufacturing$Year, "-", manufacturing$Month, "-01", sep = ""))
rubber$date <- as.Date(paste(rubber$Year, "-", rubber$Month, "-01", sep = ""))

manufacturing_value <- manufacturing %>%
  filter(Is_percentage == 0)

manufacturing_m2m <- manufacturing %>%
  filter(Is_M2M == 1)

manufacturing_y2y <- manufacturing %>%
  filter(Is_Y2Y == 1)


rubber_value <- rubber %>%
  filter(Is_percentage == 0)

rubber_m2m <- rubber %>%
  filter(Is_M2M == 1)

rubber_y2y <- rubber %>%
  filter(Is_Y2Y == 1)


ui <- fluidPage(
  
navbarPage(#fluid = TRUE,
  "Dashboard",
 # id = "mainnavbar",
  tabPanel("CPI",
           sidebarLayout(
             sidebarPanel(
             tabsetPanel(id = "tab_CPI",
                      
                      tabPanel("Timeline",
                               br(),
                               selectInput("Timeline_timeline", "Timeline", c("Year-to-Year","Month-to-Month"), selected = "Month-to-Month"),
                               selectInput("Item", "Item", choices = unique(df_cpi$CPI_items), selected = "Total")
                               #selectInput("Timeline_timeline", "Timeline", c("Year-to-Year","Month-to-Month"), selected = "Month-to-Month"),
                               
                      ),
                      tabPanel("Items",
                               br(),
                               selectInput("Timeline_items", "Timeline", c("Year-to-Year","Month-to-Month"), selected = "Year-to-Year"),
                               selectInput("Year_items", "Year", choices = c(2020, 2021), selected = 2020),
                               selectInput("Month_items", "Month", choices = months_list, selected = NULL)
                      )
                      
          )
             ),
        mainPanel(
        fluidRow(plotlyOutput("Plot_CPI")),
        fluidRow(textOutput("total_CPI"))
     
        )
        )
),
tabPanel("GDP",
         sidebarLayout(
           sidebarPanel(
             tabsetPanel(id = "tab_GDP",
                         
                         tabPanel("GDP timeline",
                                  br(),
                                  selectInput("Timeline_comparison_timeline", "Comparison", choices = c("Year-to-Year", "Quarter-to-Quarter"), selected = NULL),
                                  selectInput("Timeline_GDP_value", "Timeline", choices = value, selected = "Value (RM billion)"),
                                  helpText("Year-to-Year with Value refers to annual GDP value"),
                                  selectInput("Timeline_sectors", "Sectors", choices = unique(df_gdp$Sector), selected = "Total")
                         ),
                         tabPanel("Sectors",
                                  br(),
                                  selectInput("Timeline_comparison_sectors", "Comparison", choices = c("Year-to-Year", "Quarter-to-Quarter"), selected = NULL),
                                  selectInput("Year_GDP", "Year", choices = unique(df_gdp$Year), selected = 2020),
                                  selectInput("Quarter_GDP", "Quarter", choices = unique(df_gdp$Quarter), selected = NULL),
                                  helpText("-1 is Annual"),
                                  selectInput("Value_type", "Value", choices = value, selected = "Value (RM billion)")
                                  
                         )
                         
             )

           ) ,
           mainPanel(
            fluidRow(  plotlyOutput("Plot_GDP")),
            fluidRow( textOutput("total_GDP"))
           )
         )

         ),
tabPanel("Commodities",
         sidebarLayout(
           sidebarPanel(
             tabsetPanel(id = "tab_COM",
                         # tabPanel("Commodity",
                         #          br(),
                         #          selectInput(inputId = "Com_year", label =  "Year", choices = unique(df_com$Year), selected = NULL)
                         #        ),
                         tabPanel("Timeline",
                                  br(),
                                  selectInput(inputId = "Com_subsector", label = "Commodity", choices = unique(df_com$Commodity_items), selected = NULL)
                                ),
                         tabPanel("Commodity",
                                  br(),
                                  selectInput(inputId = "Com_year", label =  "Year", choices = unique(df_com$Year), selected = NULL)
                         )
           )
           ) ,
           mainPanel(plotlyOutput("Plot_COM"))
         )
         ),
tabPanel("External Trade",
         sidebarLayout(
           sidebarPanel(
             tabsetPanel(id = "tab_XT",
                         tabPanel("Timeline",
                                  br(),
                                  selectInput(inputId = "xt_cat", label =  "Category", choices = unique(df_xt$Trade_ID_y), selected = NULL),
                                  selectInput(inputId = "xt_time", label =  "Timeline", choices = c("Value (RM million)", "Change (%)"), selected = NULL),
                                  selectInput(inputId = "xt_comp", label = "Comparison", choices = timeline, selected = NULL),
                                  helpText("Year-to-Year with Value refers to the monthly trade volume")

                                  ),
                         tabPanel("Category",
                                  br(),
                                  selectInput(inputId = "xt_cat_time", label = "Timeline", choices = c("Value (RM million)", "Change (%)"), selected = NULL),
                                  selectInput(inputId = "xt_cat_comp", label = "Comparison", choices = timeline, selected = NULL),
                                  helpText("Year-to-Year with Value refers to the monthly trade volume"),
                                  selectInput(inputId = "xt_cat_year", label = "Year", choices = unique(df_xt$Year), selected = 2020),
                                  selectInput(inputId = "xt_cat_month", label = "Month", choices = my.month.name(unique(df_xt$Month)), selected = NULL)

                         )
             )

           ) ,
           mainPanel(
             plotlyOutput("Plot_XT")
           )
         )
         ),
tabPanel("Labour Force",
        # sidebarLayout(
           # sidebarPanel(
           #   tabsetPanel(id = "tab_LF",
           #               tabPanel("Unemployment Rate" ),
           #               tabPanel("Unemployed Figure" )
           #   )
           # 
           # ) ,
           mainPanel(
             plotlyOutput("Plot_LF")
           )
         #)
         ),
tabPanel("Manufacturing",
         # sidebarLayout(
         # sidebarPanel(
         #   tabsetPanel(id = "tab_LF",
         #               tabPanel("Unemployment Rate" ),
         #               tabPanel("Unemployed Figure" )
         #   )
         # 
         # ) ,
         mainPanel(
           plotlyOutput("Plot_M")
         )
         #)
),

tabPanel("Rubber",
         mainPanel(
           plotlyOutput("Plot_R")
         )
         )


)
)

    

server <- function(input, output, session) {
  
 
  
  


  observe({

    if(req(input$tab_CPI) == "Items")

         output$Plot_CPI <- renderPlotly({

            df_cpi %>%
            filter(Items_ID<13, Is_M2M == match(input$Timeline_items, timeline)-1, Year == input$Year_items, Month == match(input$Month_items, month.name)) %>%
                plot_ly(
                 x = ~CPI_items,
                y = ~Changes_percentage,
               type = "bar",
               text = ~Changes_percentage,
               textposition = "auto") %>%
             layout(title = paste(input$Year_items, input$Month_items,input$Timeline_items, sep = " "),
                    yaxis = list(title = "Changes (%)"),
                    xaxis = list(title = "CPI items"))
        })

         output$total_CPI <- renderText({
           df_cpi_total <- df_cpi %>%
             filter(Items_ID ==13, Is_M2M == match(input$Timeline_items, timeline)-1, Year == input$Year_items, Month == match(input$Month_items, month.name))

            paste("Total CPI: ", df_cpi_total$Changes_percentage,"%")
         })




    if(req(input$tab_CPI) == "Timeline")
      output$Plot_CPI <- renderPlotly({
        df_cpi %>%
          filter( Is_M2M == match(input$Timeline_timeline, timeline)-1, CPI_items == input$Item) %>%
          plot_ly(
            x = ~date,
            y = ~Changes_percentage,
            type = "scatter",
            mode = "lines+markers",
            text = ~Changes_percentage,
            textposition = "auto")%>%
          layout(title = paste(input$Item,input$Timeline_timeline, sep = " "),
                 yaxis = list(title = "Changes (%)"),
                 xaxis = list(title = "CPI items"))
      })

    if(req(input$tab_GDP) == "Sectors")
      output$Plot_GDP <- renderPlotly({

        df_gdp %>%
          #select(CPI_items, Year, Month, Changes_percentage, Is_M2M) %>%
          filter(Sector_ID<6, Is_percentage == match(input$Value_type, value)-1, Year == input$Year_GDP, Quarter == input$Quarter_GDP, Is_Q2Q== match(input$Timeline_comparison_sectors, timeline_q2q)-1) %>%
          plot_ly(
            x = ~Sector,
            y = ~Value_RM_billions,# %>% filter(Is_Q2Q == 0),
            type = "bar",
            text = ~Value_RM_billions,
            textposition = "auto") %>%
          layout(title = paste(input$Value_type, input$Year_GDP, "Quarter:", input$Quarter_GDP,input$Timeline_comparison_sectors, sep = " ")  ,
                 yaxis = list(title = input$Value_type),
                 xaxis = list(title = "Sector"))

      })

      output$total_GDP <- renderText({

        df_gdp_total <- df_gdp %>%
          filter(Sector_ID == 6, Is_percentage == match(input$Value_type, value)-1, Year == input$Year_GDP, Quarter == input$Quarter_GDP, Is_Q2Q== match(input$Timeline_comparison_sectors, timeline_q2q)-1)

        paste(input$Value_type,"Total: ", df_gdp_total$Value_RM_billions)


      })

    if(req(input$tab_GDP) == "GDP timeline")
      output$Plot_GDP <- renderPlotly({

        df_gdp %>%
          filter(Quarter >0,Is_percentage == match(input$Timeline_GDP_value, value)-1, Sector == input$Timeline_sectors, Is_Q2Q == match(input$Timeline_comparison_timeline, timeline_q2q)-1) %>%
          plot_ly(
            x = ~date,
            y = ~Value_RM_billions,
            type = "scatter",
            mode = "lines+markers",
            text = ~Value_RM_billions,
            textposition = "auto") %>%
          layout(title = paste(input$Timeline_sectors,input$Timeline_comparison_timeline, input$Timeline_GDP_value, sep = " ")  ,
                 yaxis = list(title = input$Timeline_GDP_value ),
                 xaxis = list(title = "Date"))
      })

    if(req(input$tab_COM) == "Commodity")
      output$Plot_COM <- renderPlotly({

        df_com %>%
          filter(Year == input$Com_year) %>%
          plot_ly(
            x = ~Commodity_items,
            y = ~Value_tonnes,
            type = "bar",
            text = ~Value_tonnes,
            textposition = "auto") %>%
          layout(title = paste("Commodity tonnage volume for year:",input$Com_year)  ,
                 yaxis = list(title = "Volume (Tonnes)" ),
                 xaxis = list(title = "Commodity items"))
      })

    if(req(input$tab_COM) == "Timeline")
      output$Plot_COM <- renderPlotly({

        df_com %>%
          filter(Commodity_items == input$Com_subsector, Is_percentage == 0) %>%
          plot_ly(
            x = ~Year,
            y = ~Value_tonnes,
            type = "scatter",
            mode = "lines+markers",
            text = ~Value_tonnes,
            textposition = "auto") %>%
          layout(title = paste(input$Com_subsector)  ,
                 yaxis = list(title = "Volume (Tonnes)" ),
                 xaxis = list(title = "Year"))
      })

    if(req(input$tab_XT) == "Timeline")
      output$Plot_XT <- renderPlotly({

        df_xt %>%
          filter(Trade_ID_y == input$xt_cat, Is_percentage == match(input$xt_time, c("Value (RM million)", "Change (%)"))-1,  Is_M2M == match(input$xt_comp, timeline)-1) %>%
          plot_ly(
            x = ~date,
            y = ~Value_RM_millions,
            type = "scatter",
            mode = "lines+markers",
            text = ~Value_RM_millions,
            textposition = "auto")%>%
          layout(title = paste(input$xt_cat,input$xt_time, input$xt_comp, sep = " ")  ,
                 yaxis = list(title = "Value (RM millions)" ),
                 xaxis = list(title = "Date"))
      })


    if(req(input$tab_XT) == "Category")
      output$Plot_XT <- renderPlotly({
        df_xt %>%
          filter(Year == input$xt_cat_year, Month == match(input$xt_cat_month, month.name),Is_percentage == match(input$xt_cat_time, c("Value (RM million)", "Change (%)"))-1, Is_M2M == match(input$xt_cat_comp, timeline)-1, Trade_ID < 3) %>%
          plot_ly(
            x = ~Trade_ID_y,
            y = ~Value_RM_millions,
            type = "bar",
            text = ~Value_RM_millions,
            textposition = "auto"
          )%>%
          layout(title = paste(input$xt_cat_comp,input$xt_cat_time, input$xt_cat_year, input$xt_cat_month,  sep = " " )  ,
                 yaxis = list(title = input$xt_cat_time ),
                 xaxis = list(title = "Trade categories"))
      })

   # if(req(input$tab_LF) == "Unemployment Rate")
      output$Plot_LF <- renderPlotly({
        
       df_lf %>%
          plot_ly(width = 1200) %>%

            add_trace(
            x = ~date,
            y = ~Unemployment_rate,
            name = "Unemployment Rate (%)",
            type = "scatter",
            mode = "lines+markers",
            text = ~Unemployment_rate,
            textposition = "auto"
          ) %>%
          add_trace(
            x = ~date,
            y = ~Unemployed_figure,
            name = "Unemployment Figure",
            yaxis = "y2",
            type = "bar",
            text = ~Unemployed_figure,
            textposition = "auto",
            opacity = 0.3
          ) %>%
          layout(
            title = "Unemployment Rate and Figure",
            yaxis2 = list(tickfont = list(color = "orange"),
                          overlaying = "y",
                          side = "right",
                          title = "Unemployment Figure"),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Unemployment Rate",
                         tickfont = list(color = "blue"))
          )
          })
      
      output$Plot_M <-renderPlotly({
        
        y2 <- list(
        #  tickfont = list(color = 'orange'),
         # titlefont = list(color = 'orange'),
          overlaying = "y",
          side = "right",
       #   anchor = 'free',
       #   position = 0.15,
          title = "Percentage changes (%)"
        )
        
         plot_ly(width = 1200) %>%
          add_trace(
            x = ~manufacturing_value$date,
            y = ~manufacturing_value$Value_RM_millions,
            name = "Manufacturing value (RM millions)",
            type = "bar",
            text = ~manufacturing_value$Value_RM_millions,
            textposition = "auto",
            opacity = 0.3
          ) %>%
          add_trace(
            x = ~manufacturing_m2m$date,
            y = ~manufacturing_m2m$Value_RM_millions,
            name = "Month to Month changes (%)",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            text = ~manufacturing_m2m$Value_RM_millions,
            textposition = "auto"
           ) %>%
          add_trace(
            x = ~manufacturing_y2y$date,
            y = ~manufacturing_y2y$Value_RM_millions,
            name = "Year to Year changes (%)",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            text = ~manufacturing_y2y$Value_RM_millions,
            textposition = "auto"
          ) %>%
          layout(
            title = "Manufacturing output and percentage changes (%)",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Manufacturing output value (RM millions)"),
            yaxis2 = y2, 
            yaxis3 = y2
          )
      })
      
      output$Plot_R <- renderPlotly({
        
        y2 <- list(
          overlaying = "y",
          side = "right",
          title = "Percentage changes (%)"
        )

        plot_ly(width = 1200) %>%
          add_trace(
            x = ~rubber_value$date,
            y = ~rubber_value$Value_tonnes,
            name = "Rubber production (tonnes)",
            type = "bar",
            text = ~rubber_value$Value_tonnes,
            textposition = "auto",
            opacity = 0.3
          ) %>%
          add_trace(
            x = ~rubber_m2m$date,
            y = ~rubber_m2m$Value_tonnes,
            name = "Month to Month changes (%)",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            text = ~rubber_m2m$Value_tonnes,
            textposition = "auto"
          ) %>%
          add_trace(
            x = ~rubber_y2y$date,
            y = ~rubber_y2y$Value_tonnes,
            name = "Year to Year changes (%)",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            text = ~rubber_y2y$Value_tonnes,
            textposition = "auto"
          ) %>%
          layout(
            title = "Rubber production and percentage changes (%)",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Rubber production (tonnes)"),
            yaxis2 = y2,
            yaxis3 = y2
          )

      })



})

}

# Run the application 
shinyApp(ui = ui, server = server)
