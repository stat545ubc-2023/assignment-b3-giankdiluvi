library(shiny)
library(tidyverse)
library(mvtnorm)
ggplot2::theme_set(theme_classic(base_size=18))
set.seed(545)

### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
#  SETUP ######################################
### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
# define target distribution
pmu <- c(0,0)
psigma <- matrix(data=c(2,0.9,0.9,10),nrow=2)

# sample from target for contours
data_grid <- expand.grid(x1 = seq(-6, 6, length.out=200), x2 = seq(-12, 12, length.out=200))
psamp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = pmu, sigma = psigma)) %>% 
  as_tibble()

# the function below runs a random walk MCMC algorithm
# targeting a N(pmu,psigma) distribution as defined above
#
# Inputs: 
#   x0:    vector of size 2, initial value
#   sigma: positive scalar, scale of proposal distribution (isotropic Normal)
#   steps: int, number of MCMC iterations
# 
# Outputs:
#   x:    matrix of size (steps+1,2), samples
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
### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
#  USER INTERFACE #############################
### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
ui <- fluidPage(
  titlePanel("Markov chain Monte Carlo (MCMC) demo"),
  h4("Use this app to explore the behaviour of an MCMC random walk algorithm"),
  p("The MCMC algorithm targets a fixed bivariate Normal distribution 
    with mean at the origin, variances 4 and 100 (respectively) and covariance 0.9."),
  p("You can control the number of steps of the MCMC sampler,
    as well as the initial point and the variance of the proposal distribution"),
  p("The contour and scatter plot provides a qualitative assessment of convergence
    while the ergodic average plot provides a quantitative assesment thereof."),
  sidebarLayout(
    sidebarPanel(
      sliderInput("steps", "Select the number of MCMC iterations",
                  min=1,max=1000,value=100,step=1,round=TRUE),
      sliderInput("x1", "Select initial x1 value for sampler",
                  min=-20,max=20,value=10,step=0.01),
      sliderInput("x2", "Select initial x2 value for sampler",
                  min=-20,max=20,value=10,step=0.01),
      sliderInput("sigma", "Select proposal standard deviation for sampler",
                  min=0.1,max=5,value=3,step=0.01)
    ),
    mainPanel(
      plotOutput("contour"),
      plotOutput("ergodic")
    )
  )
)

### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
#  SERVER #####################################
### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ###
server <- function(input, output){
  # run mcmc algorithm with settings specified by user
  mcmc_x <- reactive({
    run_mcmc(x0=c(input$x1,input$x2),sigma=input$sigma,steps=input$steps) %>% 
    as_tibble() %>% 
    rename(x1=V1,x2=V2)
  })
  
  # create contour and scatter plots
  output$contour <- renderPlot({
    mcmc_x() %>% 
      ggplot() +
      geom_point(aes(x1,x2,color="#f98e09"), # scatter with sample
                 size=2.5) +
      geom_contour(data=psamp, 
                   aes(x=x1,y=x2,z=prob,color="black")) + # contour with target
      labs(x="x",y="y") +
      scale_colour_manual(name = "", 
                          values =c("#f98e09"="#f98e09","black"="black"), 
                          labels = c("MCMC","Target"))
  })
  
  # generate ergodic averages plot
  output$ergodic <- renderPlot({
    mcmc_x() %>% 
      mutate(iter = 1:n(),
             erg_mean1 = cummean(x1), # x erg avg
             erg_mean2 = cummean(x2)) %>%  # y erg avg
      ggplot() +
      geom_line(aes(iter,erg_mean1,color="#21918c")) + # x erg avg
      geom_line(aes(iter,erg_mean2,color="#3b528b")) + # y erg avg
      geom_hline(yintercept = 0,color="black", linetype="dashed") + #true
      labs(x="Iteration number",
           y="Ergodic mean") +
      scale_colour_manual(name = "", 
                          values =c("#21918c"="#21918c","#3b528b"="#3b528b"), 
                          labels = c("x","y"))
  })
}

shinyApp(ui=ui, server=server)