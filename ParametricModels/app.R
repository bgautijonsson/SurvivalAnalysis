#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(cowplot)

theme_set(theme_cowplot() +  
              background_grid(major = "xy", 
                              minor = "xy",
                              size.major = 0.25,
                              size.minor = 0.1) +
              theme(legend.position = "top",
                    plot.margin = margin(10, 10, 10, 10)))


expo <- list()
expo$hazard <- function(t, lambda) lambda
expo$cumhaz <- function(t, lambda) t * lambda
expo$surv <- function(t, lambda) exp(- t * lambda)

weib <- list()
weib$hazard <- function(t, lambda, gamma) lambda * gamma * t^(gamma - 1)
weib$cumhaz <- function(t, lambda, gamma) lambda * t^gamma
weib$surv <- function(t, lambda, gamma) exp(- lambda * t^gamma)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme(theme = "cerulean"),
                
                # Application title
                titlePanel("Parametric Survival Models"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("lambda",
                                    "Lambda",
                                    min = 0.1,
                                    max = 10,
                                    value = 1),
                        sliderInput("gamma",
                                    "Gamma",
                                    min = 0.1,
                                    max = 3,
                                    value = 1)
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("plot1", height = "400px")
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$plot1 <- renderPlot({
        lambda <- input$lambda
        gamma <- input$gamma
        
        upper <- qexp(0.99, rate = lambda) %>% ceiling
        
        expo$data <- crossing(lambda = lambda,
                              time = seq(0, upper, length.out = 100),
                              model = "Exponential") %>% 
            mutate(hazard = expo$hazard(time, lambda),
                   cumhaz = expo$cumhaz(time, lambda),
                   surv = expo$surv(time, lambda))
        
        weib$data <- crossing(lambda = lambda,
                              gamma = gamma,
                              time = seq(0, upper, length.out = 100),
                              model = "Weibull") %>% 
            mutate(hazard = weib$hazard(time, lambda, gamma),
                   cumhaz = weib$cumhaz(time, lambda, gamma),
                   surv = weib$surv(time, lambda, gamma))
        
        bind_rows(expo$data, weib$data) %>% 
            rename(Hazard = hazard, "Cumulative Hazard" = cumhaz, Survival = surv) %>% 
            pivot_longer(c(Hazard, "Cumulative Hazard", Survival))  %>% 
            mutate(name = fct_relevel(name, "Hazard", "Cumulative Hazard", "Survival")) %>% 
            ggplot(aes(time, value, col = model)) +
            geom_line() +
            facet_wrap("name", scales = "free") +
            scale_colour_brewer(type = "qual", palette = "Set1") +
            labs(x = "Time", colour = "Model",
                 title = "Comparison of Exponential and Weibull") +
            theme(axis.title.y = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
