library(shiny)
library(plotly)

shinyServer(function(input, output, session) {
      
      # Slider control
      observeEvent({input$age
                    input$retirement_years[1]}, {
            if (input$age>=input$retirement_years[1]) {
            slider_min <- input$age
            updateSliderInput(session, "retirement_years", value = slider_min,min = slider_min +2)
            } else {
                  slider_min <- input$age
                  slider_value <- input$retirement_years[1]
                  updateSliderInput(session, "retirement_years", value = slider_value, min = slider_min +2)      
            }
      })

      
      #create data frame
      create_table <- reactive({
      
      age <- input$age
      retire_start <- input$retirement_years[1]
      retire_end <- input$retirement_years[2]
      starting_bal<- input$starting_bal
      savings <- input$savings
      savings_rate <- input$savings_rate/100
      int_rate <- input$int_rate/100
      inf_rate <- input$inf_rate/100
            
            
      data <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), 
                       c("age",
                         "balance_open",
                         "savings_rate",
                         "income",
                         "int_rate",
                         "interest",
                         "pension",
                         "balance_close",
                         "inf_rate",
                         "balance_close_real"))
      
      
      data[1:(retire_end+1-age),1] <- seq.int(age,retire_end)
      data$savings_rate <- savings_rate
      data$int_rate <- int_rate
      if(int_rate==0) data$interest <- 0
      data$inf_rate <- inf_rate

      #populate initial values
      data$balance_open[1] <- starting_bal
      data$income[1] <- savings
      if(is.na(data$interest[1])) data$interest[1] <- (starting_bal * int_rate)
      data$balance_close[1] <-data$balance_open[1] + data$income[1] + data$interest[1]
      if((retire_start - age)> 0) data$income[1:(retire_start - age)] <-round(rep(savings,(retire_start - age))
                                                  *  c(1,cumprod(1+data$savings_rate[2:(retire_start - age)])),2)
      if((retire_start - age)> 0) data$income[(retire_start - age+1):nrow(data)] <-0
      
      for (i in 2:nrow(data)){
            data$balance_open[i] <- data$balance_close[i-1]
            if(is.na(data$interest[i])) data$interest[i] <- (data$balance_open[i] * data$int_rate[i])
            data$balance_close[i] <- data$balance_open[i] + data$income[i] + data$interest[i]
      }
      
      data$pension <- 0
      if(int_rate-inf_rate==0) {
            r <- 1.000000001
            inf_rate = inf_rate+0.000000001
            d <- 1-(1+inf_rate+0.0000001)^(retire_end-retire_start+1)*(1+int_rate)^-(retire_end-retire_start+1)
      }
            r <- (int_rate-inf_rate)
            d <- 1-(1+inf_rate)^(retire_end-retire_start+1)*(1+int_rate)^-(retire_end-retire_start+1)
      
      principal <- round((data$balance_open[(retire_start-age)+1]*r)/ d,2)
      data$pension[data$age==retire_start] <- principal
      data$pension[data$age>retire_start] <- principal * ((1+inf_rate)^seq(1:nrow(data[data$age>retire_start,])))
      
      # recalculate opening and closing balances
      for (i in 2:nrow(data)){
            data$balance_open[i] <- data$balance_close[i-1]
            data$interest[i] <- (data$balance_open[i] * data$int_rate[i])
            data$balance_close[i] <- data$balance_open[i] + data$income[i] + data$interest[i] - data$pension[i]
            }
      
      
      data$balance_close_real <- data$balance_close_real <- data$balance_close/((1+inf_rate)^as.numeric(rownames(data)))
      
      # text output 
      annual_pension <- round(principal/(1+inf_rate)^(retire_start+1-age) ,0)
      lumpsum<- round(data$balance_close_real[data$age==(retire_start-1)],0)
      
      output$annual_pension <- renderText(c("  - An annual pension of ",
                                     format(annual_pension,big.mark = ",")," thousand ",
                                    " (or around ",format(round(annual_pension/12,1),big.mark = ",")," thousand per month)"))
      
      output$lumpsum <- renderText(c("  - A lump sum on retirement of ",
                                            format(lumpsum,big.mark = ",")," thousand"))
      
      return(data)
      })     
      
      output$table <- renderTable(create_table())
      
     
     #plot output  
      output$plot1 <- renderPlotly({ 
                  
                  y_options <- list(  showline = FALSE,
                                      tozero = TRUE,
                                      gridcolor = toRGB("gray90"),
                                      gridwidth = 0.25,
                                      zerolinecolor = toRGB("grey50"),
                                      zerolinewidth = 0.5,
                                      linecolor = toRGB("black"),
                                      linewidth = 0.5,
                                      title = "savings balance (thousands)")
                  
                  x_options <- list(  showline = FALSE,
                                      tozero = TRUE,
                                      gridcolor = toRGB("gray90"),
                                      gridwidth = 0.25,
                                      zerolinecolor = toRGB("grey50"),
                                      zerolinewidth = 0.5
                                      )
                  
                  plot_ly(create_table(), x = ~age, y = ~balance_close, type = 'scatter', mode = 'lines', opacity=0.8,
                          name = "savings value",hoverinfo = 'text',
                          text = ~paste('age: ', age,'</br>',
                                        '</br> savings: ', round(balance_close,0),
                                        '</br> inflation adjusted savings: ', round(balance_close_real,0))) %>%
                  layout(yaxis=y_options, xaxis=x_options, 
                         legend = list(orientation = "h", xanchor = "center",x = 0.5, y=-.2),
                         margin =list(l=100,r=100, b=0, t=50, pad=3)) %>%      
                  add_trace(y = ~balance_close_real, mode = 'lines', name = "savings real value", line = list(color = "black")) %>% 
                  add_trace(x = input$retirement_years[1]-1, mode = 'lines', line = list(color = "black", width=1,dash="dot"), opacity=0.5, showlegend=FALSE) %>% 
                  style(hoverinfo="none", traces=3 ) %>%
                  config(displayModeBar = F) %>% 
                  config(showLink = F)

      })
      
             
})
