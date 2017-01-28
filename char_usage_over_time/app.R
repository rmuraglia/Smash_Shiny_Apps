# app.R

library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
# library(gplots)

# global loads
dat <- read.table('data/frac-use_unique-users.txt', header=T, sep='\t', stringsAsFactors=F)
dat[,1] <- as.numeric(as.Date(dat[,1]))

char_list <- colnames(dat)[-1]
date_origin <- '1970-01-01'

# user interface module
ui <- fluidPage(

    # add title to UI
    titlePanel("Smash 4 character usage rates over time on Anther's Ladder by @Quappo_"),
    hr(), # horizontal line

    # split area horizontally
    # put control options at the top
    fluidRow(

        # time granularity
        column(width=3,
            numericInput('bin_size', 'First, input a number of weeks for time granularity. \nSmaller = wigglier line, captures short time scale variance. Larger = smoother line, captures long time scale trends.', value=3, min=1, max=16, step=1)
        ),

        # character select
        column(width=9,
            checkboxGroupInput('display_chars', 'Next, check the box to the left of the characters you want to include in the plot:', choices=char_list, inline=TRUE, selected=c('Diddy.Kong'))
        )
    ),

    # dedicate the majority of the area to the plot itself
    fluidRow(
        column(width=12,
            plotOutput('displayplot')
        )
    )
)

server <- function(input, output) {

    # bin data according to requested granularity
    bin_dat <- reactive({
        b_size <- input$bin_size
        binned_dat <- data.frame(matrix(nrow=ceiling(nrow(dat)/b_size), ncol=ncol(dat)))
        colnames(binned_dat) <- colnames(dat)
        for (i in 1:nrow(binned_dat)) {
            binned_dat[i,] <- apply(dat[((i-1)*b_size+1):(i*b_size),], 2, mean, na.rm=TRUE)
        }
        binned_dat
    })

    # subset data just to plotting data
    check_chars <- reactive({
        sub_df <- bin_dat() %>% mutate(Dates=as.Date(Dates, origin=date_origin)) %>% select(one_of(c('Dates', input$display_chars))) %>% gather(key=Character, value=Frac_Usage, -Dates)
        # char_colors <- rich.colors(length(input$display_chars))
        char_colors <- 'black'
        check_chars_out <- list(sub_df=sub_df, char_colors=char_colors)
        check_chars_out
    })

    # display reactive plot
    output$displayplot <- renderPlot({
        ggplot(check_chars()$sub_df, aes(x=Dates, y=Frac_Usage, colour=Character)) + 
            geom_line() + scale_x_date(breaks=pretty_breaks(10)) +
            # scale_colour_manual(values=check_chars()$char_colors) + 
            theme(text=element_text(size=17)) + labs(x='', y='Fractional Use')
    })

}

# call the app to run it
shinyApp(ui = ui, server = server)
