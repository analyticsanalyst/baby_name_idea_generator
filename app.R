library(shiny)
library(tidyverse)
library(babynames)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Baby Name Idea Generator"),
    sidebarLayout(
        sidebarPanel(
               p("Make input selections below then click 'Get Names Ideas'"),
               HTML("<p><a href='https://cran.r-project.org/web/packages/babynames/babynames.pdf'>Data source: R babynames package which gathers data from US SSA</a></p>"),
               p("Author: Brian Moore (@analyticsanalyst)"),
               HTML("<p><a href='https://github.com/analyticsanalyst'>Code on Github</a></p>"),
               p(""),
               selectInput("sex", "Gender", choices = c("F", "M"), selected = "F"),
               selectInput("year_filter", "Select names from what year?",
                            choices = seq(2017, 1907, -10), selected = 2017),
               selectInput("random", "Top 5 most popular or random selection of names?",
                           choices = c("Top 5 Most Popular", "Random Selection"),
                           selected = "Top 5 Most Popular"),
               actionButton("generate_sample", "Get Name Ideas")
        ),
        mainPanel(
            plotOutput('namechart', height = "800px")
        )
    )    
)

server <- function(input, output) {
    # simulate test data based on inputs
    df <- eventReactive(input$generate_sample, {
        bn2 <- babynames %>%
            filter(year==as.numeric(input$year_filter), 
                   sex==input$sex) %>%
            mutate(first_letter = substr(name,1,1))
        
        if(input$random=="Top 5 Most Popular") {
            bn2 %>%
                group_by(first_letter, name) %>%
                summarise(name_count = sum(n)) %>%
                group_by(first_letter) %>%
                arrange(-name_count) %>%
                filter(dplyr::row_number()<=5) %>%
                mutate(index = dplyr::row_number()) 
        } else {
            bn2 %>%
                group_by(first_letter, name) %>%
                summarise(name_count = sum(n)) %>%
                group_by(first_letter) %>%
                sample_n(5) %>%
                mutate(index = dplyr::row_number()) 
        }
    })
    
    output$namechart <- renderPlot({
        df() %>%
            ggplot(aes(x=first_letter,
                   y=index,
                   color=first_letter)) +
            geom_text(aes(label=name), 
                      size=5,
                      show.legend=F,
                      fontface="bold") +
            facet_wrap(. ~ first_letter, scale="free", ncol=9) +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank()) +
            labs(title="Name Ideas by First Letter") +
            scale_y_reverse(expand = expansion(mult = c(.2,.2)))
    }, res = 96)
}

shinyApp(ui = ui, server = server)



