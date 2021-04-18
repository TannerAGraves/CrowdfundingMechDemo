library(tidyverse)
library(shiny)

generate <- function(n = 110, loc = 15, sd = 30){
  matches <- rnorm(3 * n, mean = loc, sd = sd)
  matches <- matches[matches >= 0][1:n]
  times <- runif(n, 0, 1)
  
  matches_df <- tibble(
    time = times,
    amount = matches
  )
  matches_df <- dplyr::arrange(matches_df, time)
  matches_df <- mutate(matches_df, pool = cumsum(matches_df$amount))
  matches_df <- mutate(matches_df, crowd = 1:nrow(matches_df))
  return(matches_df)
}

run_mdl_log <- function(matches_df, goal, power = 5, type='pool'){
  match_coeff <- NULL
  for(i in 1:nrow(matches_df)){
    match_coeff <- c(match_coeff, min(1, log(max(matches_df[i,type], 1), base = goal)^power))
  }
  results_df <- matches_df
  results_df$match_coeff <- match_coeff
  results_df <- mutate(results_df, applied = results_df$pool * results_df$match_coeff)
  results_df <- mutate(results_df, withheld = results_df$pool - results_df$applied)
  return(results_df)
}

run_mdl_std <- function(matches_df, goal, type='pool'){
  match_coeff <- NULL
  for(i in 1:nrow(matches_df)){
    match_coeff <- c(match_coeff, min(1, max(matches_df[i,type] / goal, 0)))
  }
  results_df <- matches_df
  results_df$match_coeff <- match_coeff
  results_df <- mutate(results_df, applied = results_df$pool * results_df$match_coeff)
  results_df <- mutate(results_df, withheld = results_df$pool - results_df$applied)
  return(results_df)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Donation Data",
                 sliderInput('match_n', "Number of Matches:", min = 0, max = 500, value = 120),
                 sliderInput('match_loc', "Location of Matches:", min = 0, max = 50, value = 10),
                 sliderInput('match_sd', "Standard Deviation of Matches:", min = 0, max = 100, value = 30),
                 actionButton('generate', "Generate Data")
    ),
    mainPanel(
      plotOutput(outputId = 'pool_plt'),
      selectInput('mechanism', "Select mechanism:", list('proportional' = 'std', 'logarithmic' = 'log'), selected = 'log'),
      selectInput('match_type', "Calculate matching with:", list('pool size' = 'pool', 'crowd size' = 'crowd', 'both' = 'both'),
                  selected = 'both'),
      conditionalPanel(condition = "input.match_type == 'pool' | input.match_type == 'both'",
                       sliderInput('pool_goal', "Pool Goal:", min = 0, max = 10000, value = 2000),
                       conditionalPanel(condition = "input.mechanism == 'log'", 
                                        sliderInput('pool_power', "Rate:", min = 0, max = 20, value = 5, round=FALSE)
                                        )
                       ),
      conditionalPanel(condition = "input.match_type == 'crowd' | input.match_type == 'both'",
                       sliderInput('crowd_goal', "Crowd-size Goal:", min = 0, max = 500, value = 100),
                       conditionalPanel(condition = "input.mechanism == 'log'",
                                        sliderInput('crowd_power', "Rate:", min = 0, max = 20, value = 5)
                                        )
                       ),
      checkboxInput('disp_withheld', "Show Witheld Matching", value = TRUE),
      checkboxInput('disp_goals', "Show Goals", value = TRUE),
      plotOutput('factor_plt'),
      tableOutput('table')
    )
  )
)

server <- function(input, output) {
  matches_df <- eventReactive(input$generate, {
    generate(n = input$match_n, 
             loc = input$match_loc, 
             sd = input$match_sd)
  }, ignoreNULL = FALSE)
  
  output$table <- renderTable({results_df_reactive()})
  #reactive({
  #  pool_results_df <- run_mdl(matches_df, goal = input$pool_goal, power = input$pool_power)
  #  crowd_results_df <- run_mdl(matches_df, goal = input$crowd_goal, power = input$crowd_power)
  #})
  
  results_df_reactive <- reactive({
    input_df <- matches_df()
    if(input$mechanism == 'log'){
      pool_results_df <- run_mdl_log(input_df, goal = input$pool_goal, power = input$pool_power, type = 'pool')
      crowd_results_df <- run_mdl_log(input_df, goal = input$crowd_goal, power = input$crowd_power, type = 'crowd')
    } else if(input$mechanism == 'std'){
      pool_results_df <- run_mdl_std(input_df, goal = input$pool_goal, type = 'pool')
      crowd_results_df <- run_mdl_std(input_df, goal = input$crowd_goal, type = 'crowd')
    }
    results_df <- full_join(pool_results_df, crowd_results_df, by = 'time', suffix = c('.p', '.c'))
    return(results_df)
  })
  
  output$pool_plt <- renderPlot({
    # method more flexaible than one DF, but if it stays this way it should be changed by combining
    results_df <- results_df_reactive()
    n <- nrow(results_df)
    p <- ggplot(results_df, aes(x=time))
    if(input$match_type == 'pool' | input$match_type == 'both'){#Plotting for pool data
      p <- p + geom_line(aes(y=applied.p), color='blue', size = 1.5)
      if(input$disp_withheld){
        p <- p + geom_line(aes(y=withheld.p), color='blue', linetype = 'dashed')
      }
      if(input$disp_goals){
        p <- p + geom_hline(yintercept = input$pool_goal, color='blue')
      }
    }
    if(input$match_type == 'crowd' | input$match_type == 'both'){#Plotting for crowd data
      p <- p + geom_line(aes(y=applied.c), color='red', size = 1.5)
      if(input$disp_withheld){
        p <- p + geom_line(aes(y=withheld.c), color='red', linetype = 'dashed')
      }
      if(input$disp_goals){
        p <- p + geom_vline(xintercept = input$crowd_goal / n, color='red')
      }
    }
    #if(input$disp_withheld){
    #  p <- p + geom_line(aes(y=withheld.p), color='blue', linetype = 'dashed') + 
    #    geom_line(aes(y=withheld.c), color='red', linetype = 'dashed')
    #}
    #if(input$disp_goals){
    #  p <- p + geom_vline(xintercept = input$crowd_goal / n, color='red') + 
    #    geom_hline(yintercept = input$pool_goal, color='blue')
    #}
    p <- p + scale_x_continuous(sec.axis = sec_axis(~. * n, name = "Number of Matchers"))
    p
  })
  
  #output$factor_plt <- renderplot({
  
  #})
  
  #output$crowd_plt <- renderPlot({
  #  input_df <- matches_df()
  #  crowd_results_df <- run_mdl(input_df, goal = input$crowd_goal, power = input$crowd_power)
  #  ggplot(crowd_results_df, aes(x=time)) + 
  #    geom_line(aes(y=applied)) + 
  #    geom_line(aes(y=withheld))
  #})
}

shinyApp(ui = ui, server = server)