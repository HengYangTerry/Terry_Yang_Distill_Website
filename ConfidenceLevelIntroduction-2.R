library(shiny)
library(ggplot2)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Confidence Levels Introduction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 750,
                  min = 1,
                  max = 1000),
      
      br(),
      
      sliderInput("size",
                  "Size of sampling:",
                  value = 500,
                  min = 1,
                  max = 1000),
      
      br(),
      
      sliderInput("sampling",
                  "Number of reapeated sampling",
                  value = 5000,
                  min = 50,
                  max = 10000),
      
      br(),
      
      sliderInput("confidencelevel",
                  "Confidence Level",
                  value = 90,
                  min = 1,
                  max = 99)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Original Data Plot", plotOutput("plot")),
                  tabPanel("Summary for Original Data", verbatimTextOutput("summary")),
                  tabPanel("Original Data", tableOutput("table")),
                  tabPanel("Reapeated Sampling from Original Data", tableOutput("sampling")),
                  tabPanel("Sampling Plot", plotOutput("samplingplot"), htmlOutput("repeatedsampling")),
                  tabPanel("Confidence Level", plotOutput("CL"),htmlOutput("confidencelevel")),
                  tabPanel("Confidence Intervals using the Sampling Means", plotOutput("CI"), htmlOutput("confidenceinterval")),
                  tabPanel("Intervals containing the true mean", tableOutput("havetruemean"), htmlOutput("numberintervals"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  rep_sampling <- reactive({
    size <- input$size
    sampling <- input$sampling
    
    rep <- function(size, sampling) {
      rep_sampling <- NULL
      for (i in 1:sampling) {
        rep_sampling[i] <- mean(sample(d(), size))
      }
      return(rep_sampling)
    }
    
    rep(size, sampling)
  })
  
  confidence_interval <- reactive({
    size <- input$size
    CL <- input$confidencelevel
    con_int <- function(CL, size) {
      con_interval <- NULL
      con_interval[1] <- mean(d()) - qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
      con_interval[2] <- mean(d())
      con_interval[3] <- mean(d()) + qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
      return(con_interval)
    }
    
    con_int(CL, size)
  })
  
  confidence_interval_sampling <- reactive({
    size <- input$size
    CL <- input$confidencelevel
    con_samp <- function(CL, size) {
      CI_list <- NULL
      for (i in 1:50) {
        lower <- rep_sampling()[i] - qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
        upper <- rep_sampling()[i] + qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
        CI_list[[i]] <- c(lower, upper)
      }
      CI_df <- data.frame(ci = 1:50, lower = sapply(CI_list, "[[", 1), upper = sapply(CI_list, "[[", 2))
      CI_df$color <- ifelse(CI_df$lower < mean(d()) & CI_df$upper > mean(d()), "green", "red")
      return(CI_df)
    }
    
    con_samp(CL, size)
  })
  
  confidence_interval_havetruemean <- reactive({
    size <- input$size
    sampling <- input$sampling
    CL <- input$confidencelevel
    con_samp <- function(CL, size, sampling) {
      lower <- NULL
      upper <- NULL
      for (i in 1:sampling) {
        lower[i] <- rep_sampling()[i] - qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
        upper[i] <- rep_sampling()[i] + qnorm((1 + CL/100)/2) * (sd(d()) / sqrt(size))
      }
      list <- c(paste("Predicted Number of Confidence Intervals containing the True Mean:", CL * sampling / 100, sep = " "), paste("Actual Number of Confidence Intervals containing the True Mean:", sum(lower <= mean(d()) & upper >= mean(d())), sep = " "))
      return(list)
    }
    
    con_samp(CL, size, sampling)
  })
  
  
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
  output$sampling <- renderTable({
    rep_sampling()
  })
  
  output$samplingplot <- renderPlot({
    ggplot() +
      geom_histogram(aes(x = rep_sampling(), y = ..density..), bins = 30, fill = "peachpuff", alpha = 0.75) + 
      geom_density(aes(x = rep_sampling()), col = "blue") +
      xlab("Samples") +
      ggtitle("Sampling Distribution") + 
      theme_classic()
  })
  
  output$repeatedsampling <- renderUI({
    HTML('<b>Reapeated Sampling:</b> <br>
- Now we have the means of the repeated sampling we make. And we have a graph for it. Be careful, when we have the sampling size equals to the number of observation, the graph appears wierd. Also, we can not have the sampling size greater to the number of observation<br>
- <b>Let’s observe the change in the graph while we make the sampling size bigger or the number of repeated sampling bigger:</b> After comparison, we can almost see that as they get bigger, this graph gets more similar to being normally distributed.'
    )
  })
  
  output$CL <- renderPlot({
    ggplot() +
      geom_segment(aes(x = confidence_interval()[1], y = input$confidencelevel, xend = confidence_interval()[3], yend = input$confidencelevel)) +
      scale_x_continuous(limits = c(-0.2, 0.2)) + 
      geom_vline(xintercept = mean(d()), color = "blue", size = .5) + 
      labs(x = "Confidence Interval",
           y = "Percent Confidence Level",
           title = "Confidence Interval with chosen Confidence Level") +
      theme_classic()
  })
  
  output$confidencelevel <- renderUI({
    HTML('<b>Confidence Intervals:</b> <br>
- Now since we have chosen a convidence level, we can calculate the confidence interval using the mean, the standard deviation and the z-value for the confidence level we chose. This interval has a lower bound being the mean minus the product of the z-value and the standard deviation over the squre root of the sampling size. We can also see that the blue line goes through the middle of the line segment, since this confidence interval is calculated using the true mean.<br>
- <b>Let’s observe the change in the line segment in the graph while we make the confidence level bigger:</b> After comparison, we can see that as the confidence level gets bigger, this line segment gets more and more bigger. And as the confidence level gets smaller, the line segment gets smaller. We can clearly see that if we look carefully at the two ends of the line segement in the graph.'
    )
  })
  
  output$CI <- renderPlot({
    ggplot(confidence_interval_sampling(), aes(x = ci, y = 1)) +
      geom_segment(aes(x = lower, y = ci, xend = upper, yend = ci), size = 1, color = confidence_interval_sampling()$color) +
      geom_vline(xintercept = mean(d()), color = "blue", size = .5) + 
      scale_y_continuous(breaks = NULL, limits = c(0.5, 50.5)) +
      labs(x = "Confidence Intervals", y = "Samples") +
      ggtitle(paste0(input$confidence, "50 Example Confidence Intervals")) + 
      theme_classic() 
  })
  
  output$confidenceinterval <- renderUI({
    HTML('<b>Confidence Intervals:</b> <br>
- Now here is a graph with 50 example sampling confidence interval. We use the sampling mean, the standard deviation and the z-value for the confidence level we chose to calculate 50 different confidence intervals. The blue vertical line is the true mean of the population, which is the mean of the original data. If the blue line intersect with the line segment, then the confidence interval contains the true mean. Also, if we look at the colors of the line segments, the confidence intervals colored in green contain the true mean and the confidence intervals colored in red does not cotain the true mean.<br>
- <b>Let’s observe the change in the line segment in the graph while we make the confidence level bigger:</b> After comparison, we can see that as the confidence level gets bigger, there are more confidence intervals that contains the true mean. And as the confidence level gets smaller, there are fewer intervals that contains the true mean. We can clearly see that from this graph. Now, we calculate exactly how many sampling confidence intervals contain the true mean.'
    )
  })
  
  output$havetruemean <- renderTable({
    confidence_interval_havetruemean()
  })
  
  output$numberintervals <- renderUI({
    HTML('<b>Confidence Intervals:</b> <br>
- Now here are the predicted number and actual number of confidence intervals containing the true mean. To calculate the predicted number, we use the number of sampling times the confidence level over 100. And comparing between these two numbers, we can see that there are actually more intervals containing the true mean than expected.<br>
- <b>Let’s observe the change in numbers as we make the sample size bigger and the confidence level bigger:</b> After comparison, we can see that as the confidence level gets bigger, there are more confidence intervals that contains the true mean. And as the sample size gets bigger, there are more intervals that contains the true mean. We can clearly see that from this graph. Now, we calculate exactly how many sampling confidence intervals contain the true mean.'
    )
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)