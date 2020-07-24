library(shiny)
library(tidyverse)

###### FUNCTIONS ######

disc.logistic <- function(n, p) { 
  r <- unname(p['r']) ## set the growth rate (dropping the parameter name with 'unname')
  K <-  unname(p['K']) ## set the carrying capacity
  n1 <- n + r * n * (1 - n / K) ## calculate the new population size
  return(n1) ## and return it 
}

disc.logistic.2 <- function(n, p, T) { ## T is the total number of time steps
  N <- vector(length = T + 1)
  N[1] <- n ## set the initial value
  for (t in 1:T) { 
    N[t+1] <- disc.logistic(N[t], p) ## we can call the function we wrote above to calculate the new population size
  }
  return(N) ## return the vector of population sizes
}

##### SHINY ######

ui <- fluidPage(
  sliderInput(inputId = "K", 
              label = "Choose carrying capacity", 
              value = 100, min = 10, max = 500), 
  
  sliderInput(inputId = "r", 
              label = "Choose intrinsic growth rate (r)", 
              value = 0.1, min = -1.0, max = 1.0, step = 0.1),
  
  plotOutput("timeseries")
)

server <- function(input, output) {
  output$timeseries <- renderPlot({
    n0 <- 1
    p <- c(r = input$r, K = input$K) 
    n_years <- 50
    n <- disc.logistic.2(n = n0, p = p, T = n_years)
    plot(x = 0:n_years, y = n, type = 'l', 
         xlab = 'Time', ylab = 'Population size', 
         main = 'Discrete time logistic model')
    
  })
}

shinyApp(ui = ui, server = server)
