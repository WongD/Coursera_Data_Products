library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
# title panel visible accross all tabls
      titlePanel(title=div("Retirement Calculator",
                           img(src = "squirrel.png", height = 50, width = 50,style="float:right"
                               ))),
          
          tabsetPanel(type = "tabs", 
                      tabPanel("Instructions",
                               tags$br(),
                               tags$p("This app is designed to give you an approximate idea
                                    of how much income you can expect when you retire."),
                               tags$br(),
                               tags$p("It uses the following information;"),
                               tags$ul(
                                     tags$li("Your age (in years)"), 
                                     tags$li("The age that you expect (or would like to) retire"), 
                                     tags$li("How much you have already saved for retirement"),
                                     tags$li("How much you expect to save in future"),
                                     tags$li("The interest rate you expect on your retirement savings"),
                                     tags$li("The rate of inflation you expect"),
                                     tags$li("How long you expect to live (in years)")
                               ),
                               tags$br(),
                               tags$p("From this it calculates;"),
                               tags$ul(
                                     tags$li("your actual balance of savings at different points of time"),
                                     tags$li("the real value you would expect to receive after retirement (i.e. the value at today's prices - taking into account expected inflation)")
                               ),
                               tags$p("Note that taxation is not taken into account, all values should be entered after tax."),
                               tags$br(),
                               tags$a(href="https://github.com/WongD/Coursera_Data_Products/tree/gh-pages", "The code for this application can be found at this link.", target="_blank")
                        ),

                      tabPanel("Annual View",
                               
                        sidebarLayout(
                              sidebarPanel(
                                    
                                    sliderInput("age", "Your age now:",
                                                min = 20, max = 80,
                                                value = 28),
                                    

                                    sliderInput("retirement_years", "Retirement Years (from, until):",
                                                min = 25, max = 130,
                                                value = c(50,95)),
                                    
                                    sliderInput("starting_bal", "Retirement savings now (thousands):",
                                                min = 0, max = 15000,
                                                value = 563),

                                    sliderInput("savings", "How much will you save per year (thousands):",
                                                min = 0, max = 1000,
                                                value = 50),

                                    sliderInput("savings_rate", "Will you increase the amount you save per year (annual % increase)?:",
                                                min = 0, max = 15, step = 0.5,
                                                value = 5),

                                    sliderInput("int_rate", "The net interest rate you expect to receive on your savings/investments (%)?:",
                                                min = 0, max = 20, step =0.5,
                                                value = 4),
                                    

                                    sliderInput("inf_rate", "The average inflation rate you expect (%)?:",
                                                min = 0, max = 15, step=0.5,
                                                value = 2.5),
                                    
                                    width = 4
                                    
                              ),
                              mainPanel(
                                    
                                    tags$br(),
                                    h4("In todays terms (adjusting for inflation), you could expect;"),
                                    h4(textOutput({"annual_pension"})),
                                    h4(textOutput({"lumpsum"})),
                                    
                                    # tableOutput({"table"})
                                    plotlyOutput({"plot1"})
                                    
                              )
                        )
                        
                       )
          )
    )
)
