# app.R

library(shiny)
library(ggplot2)
library(tidyverse)

# user options:
#     1) select player 1 (selectbox 1 or more)
#     2) choose comparisons with all players, or specific (checkboxes)
#         2a) choose players to compare with (selectbox 1 or more)

# display:
#     1) table: P1, P2, Winner, Tourney
#         1a) provide some sorting or filtering to only a specific P1 (tabs for all/p1/p2)
#     2) table: P1, P2, set count
#         2a) provide some sorting or filtering to only a specific P1 (tabs for all/p1/p2?)
#     3) visualizations
#         3a) heatmap on win percent. legend = P1 win%, range = [0, 50, 100]
#         3b) heatmap on set differential. legend = P1 wins - P2 wins, range = [min, 0, max]

# user interface module
ui <- fluidPage(

    # add title to UI
    titlePanel("Snappy title, wow!!!!"),
    hr(), 

    # main area
    fluidRow(

        # user inputs
        column(width=3,

            # allow BCN file upload option
            fileInput('BCN_load', 'By default, data for North Carolina Smash 4 season 11 is displayed. If you have your own SkillKeeper file in .bcn format you would like to inspect, you can upload it here.', multiple = FALSE),
            br(),

            # select minimum number of sets to be included in widget
            numericInput('min_sets', 'Select the minimum number of sets required for a player to be included in analysis', value=15, min=1, step=1),
            br(),

            # select P1(s) for inspection
            selectInput('P1', 'Select one or more players for inspection. These will be referred to as "P1"', choices=c('ONE', 'TWO', 'THREE'), multiple = TRUE),
            br(),

            # select if comparisons are to all players or only some
            radioButtons('P2_focused', 'Do you want to compare P1 records against all other players, or only against players of your choosing?', choices=c('all players' = 'ALL', 'selected players (recommended)' = 'SUB')),

            # display P2 selector only if P2_focused == 'SUB'
            conditionalPanel(
                condition = "input.P2_focused == 'SUB'",
                br(),
                selectInput('P2', 'Select one or more players generate head to head records against. These will be referred to as "P2"', choices=c('ONE', 'TWO', 'THREE'), multiple = TRUE)
            )
        ),

        column(width=9,

            tabsetPanel(

                # first tab is set list
                tabPanel('Set list', textOutput('one')),

                # second tab is head to head
                tabPanel('Head to head table', textOutput('two')),

                # third tab are heatmaps
                tabPanel('Head to head heatmaps', textOutput('three'))
            )
        ) 
    )
)

server <- function(input, output) {
    output$one <- renderText({'nice job'})
    output$two <- renderText({'WOW'})
    output$three <- renderText({'u rule'})
}

# call the app to run it
shinyApp(ui = ui, server = server)
