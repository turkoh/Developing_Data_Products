require(shiny)
require(tm)
require(wordcloud)
require(cluster)

# Read dataset into memory
dataset <- read.csv("pop_dataset.csv", header = TRUE, sep = ",", encoding = 'UTF-8')

# Define server logic required to generate and plot results
shinyServer(function(input, output, session) {
        
        # Display instructions and documentation on condition of radio button selection
        output$doc_text <- renderUI(
                HTML("<hr>
                <div style='color:navy; background-color:ghostwhite; padding:30px'>
                <h4>Exploring the public mood, sentiment, and social culture of an era through popular music can be fascinating.</h4>
                <strong>Music can be a reflection of the social experience in history.
                A way to analyze this historical experience is through the Song Titles of the popular music over time.</strong>
                <br><br>
                <span style='font-family:verdana'>Use this data app to analyze Pop Music Song Titles.  
                The dataset is composed of popular song titles grouped by the year of release and 
                then parsed into individual terms or words for frequency analysis.  The slider widgets give you control over the data and 
                allow you to explore the frequency of song-title words by different timeframes.</span>
                <br><br>
                <ol>
                <li>Select the beginning and ending years to define a yearly range grouping of song-title terms.</li>
                <li>Select the minimum threshold of term frequencies.  A higher minimum threshold will group fewer terms, but more frequently used terms.  
                A lower minimum threshold will group more terms, but also will include more infrequently use terms (possibly insignificant terms). </li>
                <li>Select the maximum number of terms to be displayed on the plots.</li>
                <li>Select the amount of clusters to visually analyze through the clustering algorithm plots.</li>
                <li>(Optional) Remove any term that over dominate the analysis as an outlier.</li>
                </ol>
                <br><br>
                <em>Plots:</em>
                <br>
                <span style='font-family:verdana'>Wordclouds are a visual representation presenting textural patterns, trends, and give greater prominence to words that appear more frequently.  
                The relationships between word/term correlations and similarity of frequencies can be analyzed in multiple visual cluster plots.  
                Hierarchical and K-means clustering are the two prevalent methods for analyzing distances between individual terms.</span>
                </div>")
        )
        
        # Display yearly info as bullet text after the yearly range slider input
        output$year_text <- renderUI(
                HTML("<ul>
                <li>Range spanning ", (input$year_range[2] - input$year_range[1]) + 1, " years</li>
                <li>Includes ", dim(dataset_range())[1], " Song Titles</li>
                <li>Analysing ", dim(dataset_tdm())[1], " unique terms</li>
                </ul>")
        )
        
        # Display min freq info as bullet text after the minimum word frequency slider input
        output$freq_text <- renderUI(
                HTML("<ul>
                <li>Filtered ", length(dataset_sorted_tdm()[dataset_sorted_tdm() >= input$freq]), " unique terms</li>
                <li>Minimum frequency set at ", input$freq, "</li>
                <li>Maximum frequency of ", dataset_sorted_tdm()[1], "</li>
                </ul>")
        )
        
        # Return the requested yearly range dataset based on input
        dataset_range <- reactive({
                # Parse data by yearly range.....
                dataset[dataset$Year >= input$year_range[1] & dataset$Year <= input$year_range[2],]
        })
        
        # Return the requested tdm (term document matrix) dataset
        dataset_tdm <- reactive({
                
                # Create Corpus
                corpus <- Corpus(VectorSource(paste(dataset_range()$Track, collapse=" ")))
                
                # Preprocessing - General Cleansing of Corpus
                outlier <- tolower(input$outlier) # Word removal (outlier input)
                corpus <- tm_map(corpus, content_transformer(tolower))
                corpus <- tm_map(corpus, removeWords, stopwords("english"))
                corpus <- tm_map(corpus, removePunctuation)
                corpus <- tm_map(corpus, removeNumbers) 
                corpus <- tm_map(corpus, removeWords, c("song", "partiro", outlier)) 
                corpus <- tm_map(corpus, stripWhitespace)
                corpus <- tm_map(corpus, PlainTextDocument)
                
                # Create the base term document matrix
                as.matrix(TermDocumentMatrix(corpus))
        })
        
        # Return the requested dataset of the sorted tdm
        dataset_sorted_tdm <- reactive({
                # Create the sorted (by frequency) term document matrix.....
                sort(rowSums(dataset_tdm()), decreasing = TRUE)
        })
        
        # Update the minimum frequency slider in response to the yearly range slider input calculations
        observe({
                updated_max <- as.numeric(dataset_sorted_tdm()[4])
                updated_val <- updated_max * .2
                updateSliderInput(session, "freq", max = updated_max, value = updated_val)
        })
        
        # Update the maximum number of words slider in response to the minimum frequency slider input calculations
        observe({
                updated_max <- ifelse(length(dataset_sorted_tdm()[dataset_sorted_tdm() >= input$freq]) < 100, 
                                      length(dataset_sorted_tdm()[dataset_sorted_tdm() >= input$freq]), 100)
                updated_val <- updated_max * .5
                updateSliderInput(session, "max", max = updated_max, value = updated_val)
        })
        
        # Update the number of clusters slider in response to the maximum number of words slider input calculations
        observe({
                updated_max <- ifelse(input$max > 9, 9, input$max - 1)
                updated_val <- ifelse(input$max < 9, 1, 3)
                updateSliderInput(session, "clust", max = updated_max, value = updated_val)
        })
        
        # Display text before the summary table
        output$sum_text <- renderText({ 
                paste("Table displaying terms with a Minimum Frequency of ", input$freq,
                      " -OR- a Maximum Number of ", input$max, " terms.")
        })
        
        # Display summary table
        output$summary <- renderTable({
                # Create a output table.....
                sum_tab <- as.data.frame.table(dataset_tdm())[,c(1,3)]
                sum_tab <- sum_tab[order(sum_tab$Freq, decreasing = TRUE),]
                rownames(sum_tab) <- seq(length=nrow(sum_tab)) 
                colnames(sum_tab)[2] <- "Frequency"
                head(sum_tab, input$max)
        })
        
        # Wordcloud Plot
        output$wordcloud_plot <- renderPlot({
                # Create a wordcloud plot.....
                wordcloud(names(dataset_sorted_tdm()), dataset_sorted_tdm(),
                          min.freq = input$freq, max.words = input$max,
                          colors = brewer.pal(9, "Set1"))
        })
        
        # Cluster Dendrogram Plot
        output$dendrogram_plot <- renderPlot({
                # Create a Cluster Dendrogram.....
                dist_matrix <- dist(scale(dataset_sorted_tdm()[1:input$max]))
                fit <- hclust(dist_matrix, method = "ward.D")
                plot(fit, main = "Hierarchical Clustering Algorithm (Dendrogram Plot)", xlab = "Dissimilarity Dendrogram Distribution Structure", sub ="")
                if(input$clust > 1) rect.hclust(fit, k = input$clust, border = brewer.pal(9, "Set1"))
        })
        
        # K-Means Cluster Plot
        output$kmeans_plot <- renderPlot({
                # Create a K-Means Cluster.....
                dist_matrix <- dist(scale(dataset_sorted_tdm()[1:input$max]))
                kfit <- kmeans(dist_matrix, input$clust)
                clusplot(as.matrix(dist_matrix), kfit$cluster,
                         color=T, shade=T, main = "K-means Clustering Algorithm (Term Cluster Plot)", labels=2, lines=0)
        })
})
