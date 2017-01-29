# app.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(XML)

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

# global loads
parse_XML <- function(path) {
    full_xml <-xmlTreeParse(path, useInternal=TRUE)
    xml_root <- xmlRoot(full_xml)
    player_vec <- as.vector(unlist(xpathApply(xml_root, 'Players/Player/@Name')))
    # match_list <- getNodeSet(xml_root, 'Matches/Match') # slower than direct
    # p1_vec <- xmlSApply(match_list, xmlGetAttr, 'Player1') # slower than below
    p1_vec <- unlist(xpathApply(xml_root, 'Matches/Match/@Player1'))
    p2_vec <- unlist(xpathApply(xml_root, 'Matches/Match/@Player2'))
    win_vec <- unlist(xpathApply(xml_root, 'Matches/Match/@Winner'))
    tourney_vec <- unlist(xpathApply(xml_root, 'Matches/Match/@Description'))
    match_df <- data.frame(P1 = p1_vec, P2 = p2_vec, Winner = win_vec, Tournament = tourney_vec, stringsAsFactors=FALSE)
    return(list(player_vec, match_df))
}

# BCN_load<-parse_XML('data/season_11.bcn')

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
            uiOutput('P1select'),
            br(),

            # select if comparisons are to all players or only some
            radioButtons('P2_focused', 'Do you want to compare P1 records against all other players, or only against players of your choosing?', choices=c('all players' = 'ALL', 'selected players' = 'SUB'), selected='ALL'),

            # display P2 selector only if P2_focused == 'SUB'
            conditionalPanel(
                condition = "input.P2_focused == 'SUB'",
                br(),
                uiOutput('P2select')
                # actionButton('update_p2', 'Update')
            )
        ),

        column(width=9,

            tabsetPanel(

                # first tab is set list
                tabPanel('Set list', tableOutput('one')),

                # second tab is head to head
                tabPanel('Head to head table', textOutput('two')),

                # third tab are heatmaps
                tabPanel('Head to head heatmaps', textOutput('three'))
            )
        ) 
    )
)

server <- function(input, output) {

    # determine if data has been uploaded by user (if not, use NC default)
    loaded_data <- reactive({
        inFile <- input$BCN_load
        if (is.null(inFile)) {
            parse_XML('data/season_11.bcn')
        } else {
            parse_XML(inFile$datapath)
        }
    })

    # if they change the input data or the minimum set count, reactively change the players in the drop down list
    trim_player_list <- reactive({
        smin <- input$min_sets
        pnames <- loaded_data()[[1]]
        matches <- loaded_data()[[2]]
        set_counts <- rep(list(0), length(pnames))
        names(set_counts) <- pnames
        for (i in 1:nrow(matches)) {
            set_counts[[matches[i,1]]] <- set_counts[[matches[i,1]]] + 1
            set_counts[[matches[i,2]]] <- set_counts[[matches[i,2]]] + 1
        }
        flat_counts <- unlist(set_counts)
        sort(names(flat_counts[which(flat_counts >= smin)]))
    })

    # if the valid set of players changes, subset the matches table
    trim_matches <- reactive({
        matches <- loaded_data()[[2]] %>% filter(P1 %in% trim_player_list() & P2 %in% trim_player_list())
    })

    # create dynamic UI element for P1 selection
    output$P1select <- renderUI({
        selectInput('P1', 'Select one or more players for inspection. These will be referred to as "P1"', choices=trim_player_list(), multiple = TRUE)
    })

    # create dynamic UI element for P2 selection
    output$P2select <- renderUI({
        selectInput('P2', 'Select one or more players to generate head to head records against. These will be referred to as "P2"', choices=trim_player_list(), multiple = TRUE)
    })

    # generate subsetted match list based on P1 selections
    trim_matches_to_p1 <- reactive({
        trim_matches() %>% filter(P1 %in% input$P1 | P2 %in% input$P1)
    })

    # generate subsetted match list based on P2 selections
    trim_matches_to_p2 <- reactive({
        p1_matches <- trim_matches_to_p1()
        if (input$P2_focused == 'ALL') {
            p1_matches %>% mutate(Winner = ifelse(Winner == 1, P1, P2))
        } else {
            p1_v_p2 <- p1_matches %>% filter(P1 %in% input$P2 | P2 %in% input$P2) 
            # add direct head to head for multiple P1 selections
            p1_v_p1 <- p1_matches %>% filter(P1 %in% input$P1 & P2 %in% input$P1)
            bind_rows(p1_v_p1, p1_v_p2) %>% mutate(Winner = ifelse(Winner == 1, P1, P2))
            # if (input$update_p2 == 0) {
            #     trim_matches_to_p1() %>% mutate(Winner = ifelse(Winner == 1, P1, P2))
            # } else {
            #     delay_p2_sub()
            # }
        }
    })

    # delay_p2_sub <- eventReactive(input$update_p2, {
    #         trim_matches_to_p1() %>% filter(P1 %in% input$P2 | P2 %in% input$P2) %>% mutate(Winner = ifelse(Winner == 1, P1, P2))
    #     })


    # transform set list into set counts
    set_list_to_counts <- reactive({
        P1_list <- input$P1
        P2_list <- c(input$P1, input$P2)
        set_count_df <- as.data.frame(matrix(0, nrow=length(P1_list), ncol=length(P2_list)))
        dimnames(set_count_df) <- list(P1_list, P2_list)
        for (i in 1:length(P1_list)) { set_count_df[i,i]<-NA }

    })



    # generate set list view
    output$one <- renderTable({trim_matches_to_p2()})


    output$two <- renderText({'WOW'})
    output$three <- renderText({'u rule'})
}

# call the app to run it
shinyApp(ui = ui, server = server)


P1_list <- input$P1
P2_list <- c(input$P1, input$P2)
set_count_df <- as.data.frame(matrix(0, nrow=length(P2_list), ncol=length(P1_list)))
dimnames(set_count_df)<-list(P2_list, P1_list)
for (i in 1:length(P1_list)) { set_count_df[i,i]<-NA }



p1_list
p2_list
tim (p1 v p2)
tom (p1 v p1)
tam (rbind)

columns = p1_list
rows = c(p1_list, p2_list)






