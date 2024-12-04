library(shiny)
library(shinyjs)
library(shinyWidgets)
library(fresh)
library(DT)

server <- function(input, output, session){
  
  winners_locked <<- FALSE
  output$locked_or_unlocked_text <- renderText({
    paste0('Conference winner selections <b>UNLOCKED</b> -- make picks')
  })
  
  # poss_matchups <- read.csv('possible_scenarios.csv', header = TRUE)
  win_probs <- read.csv('win_probs.csv', header = TRUE)[, 1:2]
  matchups <- read.csv('matchups.csv', header = TRUE, row.names = 1)
  
  team_bg_color_list <- list(
    'Oregon' = '#005740',
    'Penn State' = '#003366',
    'Texas' = '#BF5700',
    'Georgia' = '#9E1B32',
    'SMU' = '#9B1B30',
    'Clemson' = '#F66733',
    'Boise State' = '#1C77C3',
    'UNLV' = '#D40000',
    'Iowa State' = '#F2AA4C',
    'Arizona State' = '#9E1B32',
    'Indiana' = '#F5F1C8',
    'Alabama' = '#FFFFFF'
  )
  
  team_font_color_list <- list(
    'Oregon' = '#FEE100',
    'Penn State' = '#FFFFFF',
    'Texas' = '#FFFFFF',
    'Georgia' = '#000000',
    'SMU' = '#003B49',
    'Clemson' = '#522D80',
    'Boise State' = '#F26F30',
    'UNLV' = '#1D1D1B',
    'Iowa State' = '#9E1B32',
    'Arizona State' = '#F5A900',
    'Indiana' = '#9E1B32',
    'Alabama' = '#9E1B32'
  )
  
  prob_chosen_winners <- NULL
  
  output$matchups <- renderDT({
    datatable(
      matchups,
      selection = 'none'
    ) %>%
      formatStyle(
        columns = 1:ncol(matchups),
        backgroundColor = styleEqual(
          names(team_bg_color_list),
          unname(unlist(team_bg_color_list))
        ),
        color = styleEqual(
          names(team_font_color_list),
          unname(unlist(team_font_color_list))
        )
      )
  })
  
  selected_winners_list <- list()
    
  update_selected_winners <- function(conference, winner, winners_list){
    winners_list[[conference]] <- winner
    # include a message here that prints under the table "you have selected ____ to win ____"
    return(winners_list)
  }
  
  
  observeEvent(input$matchups_cell_clicked, {
    
    if(!winners_locked){
    
    winner_info <- input$matchups_cell_clicked
    
    if(!is.null(winner_info$row)){
      selected_conference <- rownames(matchups)[winner_info$row]
      selected_winner <- winner_info$value
      
      selected_winners_list <<- update_selected_winners(conference = selected_conference,
                                                        winner = selected_winner,
                                                        winners_list = selected_winners_list)
      
      # if we have a winner chosen for each conference, find the probability of that combo of winners
      if(length(selected_winners_list) == 5){
        running_win_prob <- 1
        for(team in unname(unlist(selected_winners_list))){
          team_win_prob <- win_probs$win_pct[win_probs$team == team]
          running_win_prob <- running_win_prob*team_win_prob
        }
        prob_chosen_winners <<- running_win_prob
      }
      else{
        prob_chosen_winners <<- NULL
      }
      
      output$just_clicked_winner <- renderText({
        paste0('You have chosen <b>', selected_winner, '</b> to win the <b>', selected_conference, '</b>')
      })
      
      
      output$all_chosen_winners <- renderText({
        # paste0('You have chosen <b>', selected_winner, '</b> to win the <b>', selected_conference, '</b>')
        splits <- sapply(seq(1, length(selected_winners_list)), function(x){
          paste0('<b>', names(selected_winners_list)[x], ': ', unname(unlist(selected_winners_list))[x])
        })
        paste(splits, collapse = '<br>')
      })
      
      if(!is.null(prob_chosen_winners)){
        output$prob_chosen_winners_combo <- renderText({
             
          paste0('The probability of this matchup occurring (according to FPI) is <b>', 
                 round(prob_chosen_winners, 3), '</b>')
        
        })
      }
      
    }
    
    }
  })
  
  observeEvent(input$lock_in_teams_button, {
    shinyjs::show('lock_in_assumptions_button')
    shinyjs::show('generate_matchup')
  
    winners_locked <<- TRUE
    output$locked_or_unlocked_text <- renderText({
      paste0('Conference winner selections <b>LOCKED</b> -- click Change Conference Winners to change')
    })
    
    output$assumptions <- renderUI({
      tagList(
        h3('What happens in the following cases?'),
        if(!('Boise State' %in% selected_winners_list)){
          selectInput(inputId = 'boise_state_lose_seed',
                      label = 'How far does Boise State drop after losing?',
                      choices = c(seq(10,12), 'Out of playoffs'),
                      selected = 'Out of playoffs')
        },
        if(!('SMU' %in% selected_winners_list)){
          selectInput(inputId = 'smu_lose_seed',
                      label = 'How far does SMU drop after losing?',
                      choices = c(seq(8,12), 'Out of playoffs'),
                      selected = 11)
        },
        if('Clemson' %in% selected_winners_list){
          selectInput(inputId = 'clemson_over_big12',
                      label = paste0('Does Clemson finish seeded higher than ', selected_winners_list[['Big 12']], '?'),
                      choices = c('Yes', 'No'),
                      selected = 'Yes')
        },
        if(!('Penn State' %in% selected_winners_list)){
          selectInput(inputId = 'psu_below_nd',
                      label = 'Does Penn State fall below ND after losing?',
                      choices = c('Yes', 'No'),
                      selected = 'Yes')
        },
        if(!('Texas' %in% selected_winners_list)){
          selectInput(inputId = 'texas_below_nd',
                      label = 'Does Texas fall below ND after losing?',
                      choices = c('Yes', 'No'),
                      selected = 'No')
        },
        if(!('Oregon' %in% selected_winners_list)){
          selectInput(inputId = 'oregon_below_nd',
                      label = 'Does Oregon fall below ND after losing?',
                      choices = c('Yes', 'No'),
                      selected = 'No')
        },
        
      )
    })
  
  })
  
  observeEvent(input$change_conference_winners_button, {
    winners_locked <<- FALSE
    output$locked_or_unlocked_text <- renderText({
      paste0('Conference winner selections <b>UNLOCKED</b> -- make picks')
    })
    output$assumptions <- renderUI({})
    shinyjs::hide('generate_matchup')
    reset('boise_state_lose_seed')
    reset('smu_lose_seed')
    reset('psu_below_nd')
    reset('texas_below_nd')
    reset('oregon_below_nd')
    reset('clemson_over_big12')
    
    # input$smu_lose_seed <- NULL
    # input$psu_below_nd <- NULL
    # input$texas_below_nd <- NULL
    # input$oregon_below_nd <- NULL
    # input$clemson_over_big12 <- NULL
    # shinyjs::hide('generate_matchup')
  })
  
  observeEvent(input$generate_matchup, {
    
    
    this_nd_seed <- get_nd_seed(psu_behind = input$psu_below_nd,
                           texas_behind = input$texas_below_nd,
                           oregon_behind = input$oregon_below_nd)
    
    opponent <- get_opponent(nd_seed = this_nd_seed,
                             winner_list = selected_winners_list,
                             boise_seed = input$boise_state_lose_seed,
                             smu_seed = input$smu_lose_seed,
                             clemson_above_big12 = input$clemson_over_big12)
    
    
    output$opp_text <- renderText({
      paste0('ND is projected to be the <b>', this_nd_seed,'</b> seed and play against <b>', opponent, '</b>')
    })
    
    
  })
  
  observeEvent(input$reset_button, {
    session$reload()
    
    
  })
  
  get_nd_seed <- function(psu_behind, texas_behind, oregon_behind){
    
    if(is.null(psu_behind)){
      psu_behind == 'default'
    }
    if(is.null(texas_behind)){
      texas_behind == 'default'
    }
    if(is.null(oregon_behind)){
      oregon_behind == 'default'
    }
    

    psu_tex_ore <- length(which(c(psu_behind, texas_behind, oregon_behind) == 'No'))

    
    teams_ahead <- 4 + psu_tex_ore
    
    return(teams_ahead + 1)
    
  }
  
  get_opponent <- function(nd_seed, winner_list, boise_seed, smu_seed, clemson_above_big12){
    
    if(is.null(boise_seed)){
      boise_seed = 0
    }
    if(boise_seed == 'Out of playoffs'){
      boise_seed = 0
    }
    if(is.null(smu_seed)){
      smu_seed = 0
    }
    if(smu_seed == 'Out of playoffs'){
      smu_seed = 0
    }
    # if(is.null(smu_seed) | (boise_seed == 'Out of playoffs')){
    #   smu_seed = 0
    # }
    
    boise_seed <- as.integer(boise_seed)
    smu_seed <- as.integer(smu_seed)
    nd_seed <- as.integer(nd_seed)
    
    
    if(is.null(clemson_above_big12)){
      clemson_above_big12 = 'Deafult'
    }
    
    
    if(!('Clemson' %in% winner_list)){
      clemson_above_big12 <- 'Deafult'
    }
    
    if(nd_seed + boise_seed == 17){
      return('Boise State')
    }
    
    if(nd_seed + smu_seed == 17){
      return('SMU')
    }
    
    
    
    if(nd_seed == 5){
      
    
      if('UNLV' %in% winner_list){
        return('UNLV')
      }
      else{ # if Boise State wins
        if(('Clemson' %in% winner_list) & (clemson_above_big12 == 'No')){
          return('Clemson')
        }
        else{
          return(winner_list[['Big 12']])
        }

          
        }
    }
    
    else if(nd_seed == 6){
      if('SMU' %in% winner_list){
        return('Alabama')
      }
      else{
        # if SMU or Boise State's seed is 11, it's already captured above
        if(smu_seed == 0){
          return('Miami')
        }
      }
    }
    
    else if(nd_seed == 7){
      
      return('Indiana')
      
    }
      
    
      
    
    
    
  }
  
  
  
}

ui <- fluidPage(
  titlePanel('ND Playoff Matchup Predictor'),
  
             h3('Click on the winner of each conference championship game'),
             HTML(strrep(br(), 2)),
             htmlOutput('locked_or_unlocked_text', container = div),
             HTML(strrep(br(), 2)),
             DTOutput('matchups'),
             htmlOutput('just_clicked_winner', container = div),
             HTML(strrep(br(), 2)),
             htmlOutput('all_chosen_winners', container = div),
             HTML(strrep(br(), 2)),
             htmlOutput('prob_chosen_winners_combo', container = div),
             HTML(strrep(br(), 2)),
             actionButton(inputId = 'lock_in_teams_button', label = 'Lock in Teams'),
             HTML(strrep(br(), 1)),
             actionButton(inputId = 'change_conference_winners_button', label = 'Change Conference Winners'),
        
             uiOutput('assumptions'),
             HTML(strrep(br(), 2)),
             actionButton(inputId = 'generate_matchup', label = 'Generate Matchup'),
             HTML(strrep(br(), 4)),
             htmlOutput('opp_text', container = div),
             HTML(strrep(br(), 2)),
             actionButton(inputId = 'reset_button', label = 'Reset All'),
             HTML(strrep(br(), 4))
  
)

shinyApp(ui = ui, server = server)