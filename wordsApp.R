
# Define server logic required to draw a histogram
server <- function(input, output) {
      
      # plot itself subsetted by input value
      output$phonePlot <- renderPlot({
            ggplot(word_freq.x[seq_len(input$words), ], aes(x = reorder(word.x, -n), y = n)) +
                  geom_bar(stat = "identity", fill = "mediumpurple2", colour = "darkgrey") +
                  labs(title = "#MotivationMonday: Top ten most frequent words (historic data)",
                       x = "",
                       y = "count\n") +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 75, hjust = 1))
      })
      
}


# Define UI for application that draws a histogram
ui<- fluidPage(
      
      # Sidebar with a slider input for the number of bins
      verticalLayout(
            titlePanel("Most Frequent Words #MotivationMonday"), # title
            plotOutput("phonePlot"), # output plot
            wellPanel(
                  sliderInput("words",   # slider
                              "Number of frequent words:", # slider title
                              min = 10,
                              max = 50,
                              value = 10) # starts at
            )
      )
)

# run
shinyApp(ui = ui, server = server)
      