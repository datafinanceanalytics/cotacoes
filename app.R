library(shiny)
library(tidyverse)
library(shinydashboard)
library(BatchGetSymbols)
library(magrittr)
library(shinyWidgets)
library(plotly)
library(scales)
library(tibbletime)


 tickers<-GetIbovStocks(do.cache = FALSE) %>%
  select(tickers) %>%
  mutate(tickers = paste0(tickers,".SA")) %>%
  pull()

 acoes<-BatchGetSymbols(tickers = tickers,
                       first.date = "2019-01-01",
                       last.date = Sys.time(),
                       do.cache = FALSE) %>%
  extract2(2) %>%
  drop_na()


# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Ações"),


  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráfico", tabName = "graf", icon = icon("chart-bar"))
    )
  ),


  dashboardBody(
    tabItems(
      tabItem(
        "graf",
        fluidRow(
            column(4,
            pickerInput(width = "100%",
            inputId = "ticker_graf",
            label = "Escolha os tickers:",
            choices = (
              acoes %>%
                select(ticker) %>%
                distinct(ticker) %>%
                arrange(ticker) %>%
                pull()
            ),
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              `actions-box` = TRUE
            )
          )
          ),
        column(4,
          valueBoxOutput(outputId = "vol",width = "100%")
          ),
        column(4,
           valueBoxOutput(outputId = "ret_cum",width = "100%")   
               )
        ),
        fluidRow(
          column(4,
                 valueBoxOutput(outputId = "dist_mm50",width = "100%"),
                 valueBoxOutput(outputId = "dist_mm200",width = "100%")
                 ),
          column(8,
          box(width = "100%",
              plotlyOutput("candle",height = "500px")  
        )
          )
        )
      )
    )
)
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {

  output$candle<-renderPlotly({
    acoes %>% 
      filter(ticker == input$ticker_graf) %>% 
      select(ref.date,price.open,price.high,price.close,price.low) %>% 
      plot_ly(x = ~ref.date, type="candlestick",
              open = ~price.open, close = ~price.close,
              high = ~price.high, low = ~price.low) %>%
      layout(title = input$ticker_graf,
             xaxis = list(rangeslider = list(visible = F), title = "Data"))
  })
  
  output$vol<-renderValueBox({
    acoes %>% 
      filter(ticker == input$ticker_graf) %>% 
      select(ret.closing.prices) %>% 
      pull() %>% 
      sd() %>% 
      enframe(name = NULL) %>% 
      mutate(value = (value*(sqrt(252)))) %>% 
      pull() %>% 
      round(digits = 1) %>% 
      percent() %>% 
      valueBox(icon = icon("chart-bar"),subtitle = "Volatilidade Anualizada")
  })
  
  output$ret_cum<-renderValueBox({
    acoes %>% 
      filter(ticker == input$ticker_graf) %>% 
      select(ret.closing.prices) %>% 
      mutate(ret.closing.prices = ret.closing.prices+1) %>% 
      pull() %>%
      prod() %>% 
      -1 %>% 
      round(digits = 5) %>% 
      percent() %>% 
      valueBox(icon = icon("chart-bar"),
               subtitle = "Retorno Acumulado no Ano",
               color = case_when(. > 0 ~ "green",
                                 TRUE ~ "red"))
  })
  
  output$dist_mm50<-renderValueBox({
    
  media_50<-rollify(mean,window = 50)  
    acoes %>% 
      filter(ticker == input$ticker_graf) %>%
      mutate(mean_5 = media_50(price.close)) %>% 
      select(price.close,mean_5) %>% 
      drop_na() %>% 
      slice(n()) %>% 
      mutate(diferenca = ((price.close-mean_5)/price.close)) %>% 
      select(diferenca) %>% 
      pull() %>% 
      percent() %>%
      valueBox(icon = icon("chart-bar"),
               subtitle = "Distância Média Móvel 50 dias",
               color = case_when(. > 0 ~ "green",
                                 TRUE ~ "red")
                 )
    
    
    
  })
  
  
  output$dist_mm200<-renderValueBox({
    
    media_200<-rollify(mean,window = 200)  
    acoes %>% 
      filter(ticker == input$ticker_graf) %>%
      mutate(mean_5 = media_200(price.close)) %>% 
      select(price.close,mean_5) %>% 
      drop_na() %>% 
      slice(n()) %>% 
      mutate(diferenca = ((price.close-mean_5)/price.close)) %>% 
      select(diferenca) %>% 
      pull() %>% 
      percent() %>%
      valueBox(icon = icon("chart-bar"),
               subtitle = "Distância Média Móvel 200 dias",
               color = case_when(. > 0 ~ "green",
                                 TRUE ~ "red")
               )
    
    
    
  })
  
  
  
}

shinyApp(ui, server)

