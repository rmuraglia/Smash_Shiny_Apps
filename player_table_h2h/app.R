# app.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(XML)
library(RColorBrewer)

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

div_cols <- brewer.pal(11, 'Spectral')

# BCN_load<-parse_XML('data/season_11.bcn')

# user interface module
ui <- fluidPage(

    # add title to UI
    titlePanel("Player record inspector"),
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
                tabPanel('Head to head table', 
                    p('P1 selections are placed on the columns. P1 and P2 selections are placed on the rows to allow for easy viewing of P1 vs P1 head to head set counts.'),
                    p('Read set counts as "[Column wins] - [Row wins]" -- sorry if this is unintuitive if you are used to more typical matrix[row, col] notation, but this way it is easier to select a P1 and just scan down their column, seeing how they did against opponents from the P1 point of view.'),
                    p('If you are looking at a lot of players at a time, the next tab (heatmaps) might be a better view.'),
                    tableOutput('two')),

                # third tab are heatmaps
                tabPanel('Head to head heatmaps', 
                    p('Heatmap views for player match ups. W/L differential and win percentage are reported from the P1 point of view. Blues and greens mean P1 is doing well, and oranges and reds mean P1 is doing poorly against a given opponent.'),
                    p('If you are looking at a small number of players, the previous tab (table) might be a better view.'),
                    plotOutput('three'), 
                    br(),
                    plotOutput('four'))
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
        selectInput('P2', 'Select players to generate additional head to head records against. These will be referred to as "P2"', choices=trim_player_list(), multiple = TRUE)
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
        if (input$P2_focused == 'ALL') {
            P2_opponents <- sort(unique(c(trim_matches_to_p2()[,1], trim_matches_to_p2()[,2])))
            P2_opponents <- P2_opponents[-(which(P2_opponents %in% P1_list))]
            P2_list <- c(P1_list, P2_opponents)
        } else { P2_list <- c(input$P1, input$P2) }
        set_count_df <- as.data.frame(matrix(0, nrow=length(P2_list), ncol=length(P1_list)))
        dimnames(set_count_df) <- list(P2_list, P1_list)
        for (p1 in P1_list) {
            for (p2 in P2_list) {
                s_count <- trim_matches_to_p2() %>% filter(P1==p1 & P2==p2 | P2==p1 & P1==p2) %>% count(Winner) %>% as.data.frame(.)
                rownames(s_count) <- s_count[,1]
                if (is.na(s_count[p1,2])) { s_count[p1,2]<-0 }
            if (is.na(s_count[p2,2])) { s_count[p2,2]<-0 }
            set_count_df[p2, p1] <- paste(s_count[p1,2], s_count[p2,2], sep='-')
            }
        }
        for (i in 1:length(P1_list)) { set_count_df[i,i]<-NA }
        set_count_df
    })

    # transform set counts to W/L differential and W percent
    set_counts_to_diff_prc <- reactive({
        set_count_df <- set_list_to_counts()
        set_diff <- as.data.frame(matrix(NA, nrow=nrow(set_count_df), ncol=ncol(set_count_df)))
        dimnames(set_diff) <- dimnames(set_count_df)
        # set_diff <- set_count_df
        set_prc <- set_diff
        for (i in 1: nrow(set_count_df)) {
            for (j in 1:ncol(set_count_df)) {
                if (!is.na(set_count_df[i,j])) { 
                    s_count <- as.numeric(unlist(strsplit(set_count_df[i,j], '-')))
                    if (all(s_count==0)) {
                        set_diff[i,j] <- NA
                        set_prc[i,j] <- NA
                    } else { 
                        set_diff[i,j] <- round(s_count[1] - s_count[2], 0)
                        set_prc[i,j] <- round(100 * s_count[1] / (sum(s_count)), 2)
                    }
                } 
            }
        }
        list(set_diff, set_prc)
    })

    # create heatmap for W/L differential
    hmap_diff <- reactive({
        diff_df <- as.data.frame(t(set_counts_to_diff_prc()[[1]])) %>% mutate(P1=rownames(.)) %>% gather(key=P2, value=WL_diff, -P1)
        abs_max <- max(abs(diff_df[,3]), na.rm=TRUE)
        cbar_lims <- c(-abs_max, abs_max)
        ggplot(diff_df, aes(x=P2, y=P1)) + geom_tile(aes(fill=WL_diff), colour='grey50') + 
        scale_fill_gradientn(colours=div_cols, na.value='grey80', limits=cbar_lims) + 
        theme(axis.text.x=element_text(size=10, angle=45, hjust=1, vjust=1), axis.text.y=element_text(size=10)) +
        labs(title='Heatmap of P1 Set Win/Loss Differential')
    })

    # create heatmap for W percent
    hmap_prc <- reactive({
        prc_df <- as.data.frame(t(set_counts_to_diff_prc()[[2]])) %>% mutate(P1=rownames(.)) %>% gather(key=P2, value=Win_prc, -P1)
        cbar_lims <- c(0, 100)
        ggplot(prc_df, aes(x=P2, y=P1)) + geom_tile(aes(fill=Win_prc), colour='grey50') +
        scale_fill_gradientn(colours=div_cols, na.value='grey80', limits=cbar_lims) +
        theme(axis.text.x=element_text(size=10, angle=45, hjust=1, vjust=1), axis.text.y=element_text(size=10)) +
        labs(title='Heatmap of P1 Set Win Percentage')
    })

    # generate set list view
    output$one <- renderTable({trim_matches_to_p2()})

    # generate set count view
    output$two <- renderTable({set_list_to_counts()}, rownames=TRUE)

    # generate heatmaps view
    output$three <- renderPlot({hmap_diff()})
    output$four <- renderPlot({hmap_prc()})
}

# call the app to run it
shinyApp(ui = ui, server = server)
