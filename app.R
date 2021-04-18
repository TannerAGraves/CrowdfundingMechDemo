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
  titlePanel("SD Crowdmatching Mechanism Demo"),
  sidebarLayout(
    sidebarPanel("Donation Data",
                 sliderInput('match_n', "Number of Matches:", min = 0, max = 500, value = 120),
                 sliderInput('match_loc', "Location of Matches:", min = 0, max = 50, value = 10),
                 sliderInput('match_sd', "Standard Deviation of Matches:", min = 0, max = 100, value = 30),
                 actionButton('generate', "Generate Data"),
                 plotOutput('distro_plt'),
                 verbatimTextOutput('summary'),
                 textInput('state', "Preset code", value = "Enter or get parameter code..."),
                 actionButton('getState', "Get state"),
                 actionButton('setState', "Set state"),
                 #textOutput('genState')
    ),
    mainPanel(
      plotOutput(outputId = 'main_plt'),
      conditionalPanel(condition = "input.auxPlt_choice == 'matchFactorPlt'",{
        plotOutput('matchFactor_plt')
      }),
      conditionalPanel(condition = "input.auxPlt_choice == 'influencePlt'",{
        plotOutput('influence_plt')
      }),
      conditionalPanel(condition = "input.auxPlt_choice == 'influenceDiffPlt'",{
        plotOutput('influenceDiff_plt')
      }),
      selectInput('auxPlt_choice', "Auxiliary plot:", list('none'='none', "Match Factor Over Time" = 'matchFactorPlt', "Contribution Size vs Influence" = 'influencePlt', "Pool/Crowd Influence Difference vs Contribution Size" = 'influenceDiffPlt')),
      selectInput('mechanism', "Select mechanism:", list('proportional' = 'std', 'logarithmic' = 'log'), selected = 'std'),
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
      actionButton('syncGoals', "Sync Goals")
      #plotOutput('matchFactor_plt')
      #tableOutput('table')
    )
  )
)

server <- function(input, output, session) {
  lastDist <- reactiveValues()
  lastSetState <- reactiveVal(0) #remove after debug
  matches_df <- eventReactive(input$generate | input$setState, {
    lastVal = lastSetState()
    if(input$setState != lastVal){
      state = strsplit(input$state,',')[[1]]
      updateSliderInput(session, 'match_n', value = state[1])
      updateSliderInput(session, 'match_loc', value = state[2])
      updateSliderInput(session, 'match_sd', value = state[3])
      updateSliderInput(session, 'auxPlt_choice', value = state[4])
      updateSliderInput(session, 'mechanism', value = state[5])
      updateSliderInput(session, 'match_type', value = state[6])
      updateSliderInput(session, 'pool_goal', value = state[7])
      updateSliderInput(session, 'pool_power', value = state[8])
      updateSliderInput(session, 'crowd_goal', value = state[9])
      updateSliderInput(session, 'crowd_power', value = state[10])
      lastSetState(lastVal + 1)
    }
    lastDist$n <- input$match_n
    lastDist$loc <- input$match_loc
    lastDist$sd <- input$match_sd
    generate(n = input$match_n, 
             loc = input$match_loc, 
             sd = input$match_sd)
  }, ignoreNULL = FALSE)
  
  output$summary <- renderText({
    c("Summary Statistics\n", 
      sprintf("mean: %.2f\n", mean(matches_df()$amount)),
      sprintf("median: %.2f\n", median(matches_df()$amount)),
      sprintf("max: %.2f\n", max(matches_df()$amount))
    )
    })
  
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
  
  output$main_plt <- renderPlot({
    # method more flexible than one DF, but if it stays this way it should be changed by combining
    results_df <- results_df_reactive()
    n <- nrow(results_df)
    p <- ggplot(results_df, aes(x=time)) + 
      ylab("Matching (USD)") +
      xlab("Time")
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
    p <- p + scale_x_continuous(sec.axis = sec_axis(~. * n, name = "Number of Matchers"))
    p
  })
  
  output$distro_plt <- renderPlot({
    #curParams <- lastDist
    # crv_df <- as_tibble(seq(0,100,0.5))
    # crv_df$generated <- lastDist$n * dnorm(crv_df$value, mean = lastDist$loc, sd = lastDist$sd)
    # crv_df$preview <- input$match_n * dnorm(crv_df$value, mean = input$match_loc, sd = input$match_sd)
    # p <- ggplot(crv_df, aes(x = value)) +
    #   ggtitle("Distribution Summary") + 
    #   xlab("Match size (USD)") + 
    #   ylab("Scaled Probability")
    # if(!identical(c(input$match_n, input$match_loc, input$match_sd), c(lastDist$n, lastDist$loc, lastDist$sd))){
    #   p <- p + geom_line(aes(y = preview), linetype = 'dashed', color = 'red')
    # }
    # p <- p + geom_line(aes(y = generated))
    
    crv_df <- as_tibble(seq(0,100,0.5))
    crv_df$generated <- lastDist$n * dnorm(crv_df$value, mean = lastDist$loc, sd = lastDist$sd)
    crv_df$preview <- input$match_n * dnorm(crv_df$value, mean = input$match_loc, sd = input$match_sd)
    crv_df <- crv_df %>% gather(key = 'key', value = 'prob', generated, preview)
    p <- ggplot(crv_df, aes(x = value, y = prob, color = key, linetype = key)) +
      geom_line() + 
      theme(legend.position = 'bottom') + 
      ggtitle("Distribution Summary") +
      xlab("Match size (USD)") +
      ylab("Scaled Probability")
    p
  })
  
  output$matchFactor_plt <- renderPlot({
    df <- results_df_reactive()
    p <- ggplot(df, aes(x=time)) + 
      ylab("Portion of pool matched") +
      xlab("Time")
    if(input$match_type == 'pool' | input$match_type == 'both'){#Plotting for pool data
      p <- p + geom_line(aes(y=match_coeff.p), color='blue', size = 1.5)
      
    }
    if(input$match_type == 'crowd' | input$match_type == 'both'){#Plotting for crowd data
      p <- p + geom_line(aes(y=match_coeff.c), color='red', size = 1.5)
    }
    p
  })
  
  output$influence_plt <- renderPlot({
    df <- results_df_reactive()
    df <- mutate(df, delApplied.p = applied.p - lag(applied.p))
    df <- mutate(df, delApplied.c = applied.c - lag(applied.c))
    #df$delApplied.p = diff(df$applied.p)
    #f$delApplied.c = diff(df$applied.c)
    p <- ggplot(df, aes(x = amount.p)) +
      xlab("Size of individual contrabutions (USD)") +
      ylab("Change in total raised (USD)")
    if(input$match_type == 'pool' | input$match_type == 'both'){#Plotting for pool data
      p <- p + geom_point(aes(y=delApplied.p), color='blue', size = 1)
      
    }
    if(input$match_type == 'crowd' | input$match_type == 'both'){#Plotting for crowd data
      p <- p + geom_point(aes(y=delApplied.c), color='red', size = 1)
    }
    p
  })
  
  output$influenceDiff_plt <- renderPlot({
    df <- results_df_reactive()
    df <- mutate(df, delApplied.p = applied.p - lag(applied.p))
    df <- mutate(df, delApplied.c = applied.c - lag(applied.c))
    df <- mutate(df, cpDiff = delApplied.p - delApplied.c)
    meanMatch = mean(df[['amount.p']])
    p <- ggplot(df, aes(x = amount.p)) +
      xlab("Size of individual contrabutions (USD)") +
      ylab("Pool Influence - Crowd Influence")
    p <- p + geom_point(aes(y=cpDiff), color='purple', size = 1)
    #p <- p + geom_line(aes(y = amount.p - meanMatch))
    p
  })
  
  observeEvent(input$syncGoals, {
    poolGoal = input$pool_goal
    crowdGoal = input$crowd_goal
    df = results_df_reactive()
    poolGoal_row <- filter(df, match_coeff.p == 1.0) %>% slice_head()
    crowdGoal_row <- filter(df, match_coeff.c == 1.0) %>% slice_head()
    if(nrow(poolGoal_row) == 1 & nrow(crowdGoal_row) == 1){ #if both goals are met sync to the one met last
      if(poolGoal_row$time[1] > crowdGoal_row$time[1]){
        updateSliderInput(session, 'crowd_goal', value = poolGoal_row$crowd.p)
      } else{
        updateSliderInput(session, 'pool_goal', value = crowdGoal_row$pool.c)
      }
    } else if(dim(poolGoal_row)[1] == 1){# if the poolGoal is met, sync to that
      updateSliderInput(session, 'crowd_goal', value = poolGoal_row$crowd.p)
    } else if(dim(crowdGoal_row)[1] == 1){# if the crowdGoal is met, sync to that
      updateSliderInput(session, 'pool_goal', value = crowdGoal_row$pool.c)
    } else { # if neither goal is met, sync both sliders on the last value
      lastRow <- slice_tail(df)
      updateSliderInput(session, 'crowd_goal', value = lastRow$crowd.p)
      updateSliderInput(session, 'pool_goal', value = lastRow$pool.c)
    } # will not work if syncing requires slider to go out of range
    # TODO set limits on slider syncing
  })
  
  observeEvent(input$getState,{
    state = c(input$match_n,
              input$match_loc,
              input$match_sd,
              input$auxPlt_choice,
              input$mechanism,
              input$match_type,
              input$pool_goal,
              input$pool_power,
              input$crowd_goal,
              input$crowd_power
              )
    updateTextInput(session, 'state', value = state)
  })
  
  #output$genState <- renderText(c(input$generate,input$setState,lastSetState()))
  
  # matches_df <- eventReactive(input$setState, {
  #   state = strsplit(input$state,',')[[1]]
  #   updateSliderInput(session, 'match_n', value = state[1])
  #   updateSliderInput(session, 'match_loc', value = state[2])
  #   updateSliderInput(session, 'match_sd', value = state[3])
  #   updateSliderInput(session, 'auxPlt_choice', value = state[4])
  #   updateSliderInput(session, 'mechanism', value = state[5])
  #   updateSliderInput(session, 'match_type', value = state[6])
  #   updateSliderInput(session, 'pool_goal', value = state[7])
  #   updateSliderInput(session, 'pool_power', value = state[8])
  #   updateSliderInput(session, 'crowd_goal', value = state[9])
  #   updateSliderInput(session, 'crowd_power', value = state[10])
  #   generate(n = input$match_n, 
  #            loc = input$match_loc, 
  #            sd = input$match_sd)
  # })
  
}

shinyApp(ui = ui, server = server)