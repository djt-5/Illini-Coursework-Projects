library(tidyverse)
library(shiny)
library(shinythemes)

names <- read_csv("companies.csv")
names <- arrange(names[!duplicated(names$Company), ])

SP_500 <- function(symbol) {
  av_key = "TRZA2BLTLEAWY5MA"
  url = str_c("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",symbol,"&apikey=",av_key,"&datatype=csv")
  d = read_csv(url)
  d$Symbol = symbol
  d = d |> left_join(names, by = "Symbol") |>
    rename(Date = timestamp, Open = open, High = high, Low = low, Close = close, 
           Volume = volume)
  return(d)
}

ui <- fluidPage(

    shinythemes::themeSelector(),
    titlePanel("S&P 500 Visualization"),
    sidebarLayout(
      sidebarPanel( 
        selectInput("input1", "Company 1", choices = c(" ", as.vector(names$Company))),
        selectInput("input2", "Company 2", choices = c(" ", as.vector(names$Company))),
        selectInput("input3", "Company 3", choices = c(" ", as.vector(names$Company))),
        selectInput("input4", "Company 4", choices = c(" ", as.vector(names$Company))),
        selectInput("input5", "Company 5", choices = c(" ", as.vector(names$Company))),
        selectInput("dropdown", "Dropdown Input", 
                    choices = c("High", "Low", "Open", "Close", "Volume")),
        actionButton("submit", "Submit")
        ),
      mainPanel(
        verbatimTextOutput('text1'),
        plotOutput('graph'),
        tableOutput('table')
      )
    )
)
server <- function(input, output) {
  observeEvent(input$submit, {
    companies <- c(input$input1, input$input2, input$input3, 
                                  input$input4,input$input5)
    companies <- unique(companies[nzchar(companies)])
    companies <- names |> filter(Company %in% companies)
    companies <- bind_rows(lapply(as.vector(companies$Symbol), FUN = SP_500), 
                           .id = "column_label")
    if(input$dropdown == "High") {
      company_plot <- companies |>
        ggplot(aes(x = Date, y = High)) +
        ggtitle("Daily Maximum Stock Prices Over the Last 100 Business Days (US Dollars)") +
        geom_line(aes(color = Company), size = 1.5) +
        theme_bw()
        frame <- companies |> select(Date, High, Symbol, Company)
        frame$Date <- as.character(frame$Date) 
        frame <- frame |> group_by(Symbol) |>
          summarise(Symbol = Symbol,
                    Company,
                    `Mean High` = mean(High), 
                    `Max High` = max(High),
                    `Min High` = min(High))
        frame <- frame[!duplicated(frame), ]
    }
    if(input$dropdown == "Low") {
      company_plot <- companies |>
        ggplot(aes(x = Date, y = Low)) +
        ggtitle("Daily Minimum Stock Prices Over the Last 100 Business Days (US Dollars)") +
        geom_line(aes(color = Company), size = 1.5) +
        theme_bw()
        frame <- companies |> select(Date, Low, Symbol, Company)
        frame$Date <- as.character(frame$Date)
        frame <- frame |> group_by(Symbol) |>
          summarise(Symbol = Symbol,
                    Company,
                    `Mean Low` = mean(Low), 
                    `Max Low` = max(Low),
                    `Min Low` = min(Low))
        frame <- frame[!duplicated(frame), ]
    }
    if(input$dropdown == "Open") {
      company_plot <- companies |>
        ggplot(aes(x = Date, y = Open)) +
        ggtitle("Daily Opening Stock Prices Over the Last 100 Business Days (US Dollars)") +
        geom_line(aes(color = Company), size = 1.5) +
        theme_bw()
        frame <- companies |> select(Date, Open, Symbol, Company)
        frame$Date <- as.character(frame$Date)
        frame <- frame |> group_by(Symbol) |>
          summarise(Symbol = Symbol,
                    Company,
                    `Mean Open` = mean(Open), 
                    `Max Open` = max(Open),
                    `Min Open` = min(Open))
        frame <- frame[!duplicated(frame), ]
    }
    if(input$dropdown == "Close") {
      company_plot <- companies |>
        ggplot(aes(x = Date, y = Close)) +
        ggtitle("Daily Closing Stock Prices Over the Last 100 Business Days (US Dollars)") +
        geom_line(aes(color = Company), size = 1.5) +
        theme_bw()
        frame <- companies |> select(Date, Close, Symbol, Company)
        frame$Date <- as.character(frame$Date)
        frame <- frame |>group_by(Symbol) |>
          summarise(Symbol = Symbol,
                    Company,
                    `Mean Close` = mean(Close),
                    `Max Close` = max(Close),
                    `Min Close` = min(Close))
        frame <- frame[!duplicated(frame), ]
    }
    if(input$dropdown == "Volume") {
      company_plot <- companies |>
        ggplot(aes(x = Date, y = Volume)) +
        ggtitle("Daily Stock Trading Volume Over Last 100 Business Days (US Dollars)") +
        geom_line(aes(color = Company), size = 1.5) +
        theme_bw()
        frame <- companies |> select(Date, Volume, Symbol, Company)
        frame$Date <- as.character(frame$Date)
        frame <- frame |>group_by(Symbol) |>
          summarise(Symbol = Symbol,
                    Company,
                    `Mean Volume` = mean(Volume), 
                    `Max Volume` = max(Volume),
                    `Min Volume` = min(Volume))
        frame <- frame[!duplicated(frame), ]
    }

    output$graph <- renderPlot(company_plot)
    output$table <- renderTable(frame)
  })

  
}
shinyApp(ui, server)

