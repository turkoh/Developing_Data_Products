require(shiny)

# Define UI for the application
shinyUI(pageWithSidebar(
        
        # Application title
        headerPanel(
                h1("Popular Music Top Hits - Song Titles Analysis", align = "center")
        ),
        
        # Sidebars
        sidebarPanel(
                strong(p("Word/Term Frequency Analysis", align = "center")),
                h5("Song Titles of yearly top hits from various pop music records and weekly top hit charts in the USA dating from 1890-2014."),
                h6("This dataset includes approximately 40,000 Song Titles, which consists of approximately 746,000 total Terms/Words."),
                radioButtons("instructions", "App Documentation:",
                        c("Show" = "show",
                          "Hide" = "hide"), selected = "hide", inline = TRUE),
                hr(),
                sliderInput("year_range",
                        "1st) Select Yearly Range:",
                        min = 1890, max = 2014, value = c(1995,2009), sep = ""),
                h6(uiOutput("year_text")),
                hr(),
                sliderInput("freq",
                        "2nd) Select Minimum Word Frequency:",
                        min = 1, max = 74, value = 15),
                h6(uiOutput("freq_text")),
                hr(),
                sliderInput("max",
                        "3rd) Select Maximum Number of Words:",
                        min = 3, max = 99, value = 50),
                hr(),
                sliderInput("clust",
                        "4th) Select Number of Clusters:",
                        min = 1, max = 9, value = 3),
                hr(),
                em("Word/Term removal from data analysis"),
                textInput("outlier", "Remove Word (extreme outlier):", value = "Love"),
                h6("The term 'Love' is use in an extreme number of Song Titles and therefore displays a visual dominance on the plots also creating a burden while analysing the other terms."),
                h6("This term or any term can be removed from the analysis by entering said term in this textbox."),
                hr(),
                h6(textOutput("sum_text")),
                tableOutput("summary")
        ),
        
        # Show the title and Outputs
        mainPanel(
                conditionalPanel("input.instructions == 'show'", (uiOutput("doc_text"))),
                hr(),
                strong(p('Song Title Terms (Wordcloud Plot)', align = "center")),
                plotOutput("wordcloud_plot"),
                hr(),
                plotOutput("dendrogram_plot"),
                hr(),
                plotOutput("kmeans_plot"),
                hr()
        )
))
