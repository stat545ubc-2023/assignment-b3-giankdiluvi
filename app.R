library(shiny)
library(tidyverse)
library(mvtnorm)
set.seed(545)

# define target distribution
pmu <- c(0,0)
psigma <- matrix(data=c(2,0.9,0.9,10),nrow=2)

run_mcmc <- function(x0,sigma,steps){
  x <- matrix(data=x0,nrow=1)
  qsigma <- matrix(data=c(sigma**2,0,0,sigma**2),nrow=2)
  for(t in 1:steps){
    curr_x <- x[t,]
    new_x <- rmvnorm(1,mean=curr_x,sigma=qsigma) # draw from proposal
    l_ratio <- dmvnorm(new_x,pmu,psigma)/dmvnorm(curr_x,pmu,psigma) 
    alpha <- min(1,l_ratio) # acceptance prb is log ratio (topped to 1)
    if(alpha<runif(1)) new_x <- curr_x # if alpha is low, reject proposal
    x <- rbind(x,new_x)
  }
  row.names(x) <- NULL
  return(x)
}

ui <- fluidPage(
  plotOutput("contour")
)

server <- function(input, output){
  
  output$contour <- renderPlot({
    
  })
}

shinyApp(ui=ui, server=server)