library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggraph)

load("./data/processed/objects_for_plotting.rda")

ui <- dashboardPage(
    dashboardHeader(title = "LITCovid19 Text Analysis"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            box(sliderInput("slider", "Number of observations:", 25, 150, 50))
        ),
        fluidRow(
                    box(plotOutput("network", height = 700), height = 800, width = "90%")
        )
    )
)
server <- function(input, output) {
    
    set.seed(122)
    
    output$network <- renderPlot({
        
        bigram_graph <- litcovid_bigrams %>% 
            filter(n > input$slider) %>%
            graph_from_data_frame() 
        
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        bigram_graph %>% 
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
            geom_node_point(color = "#D8BFD8", size = 3) +
            geom_node_text(aes(label = name), check_overlap = TRUE, vjust = 1, hjust = 1) +
            theme_void()
    })
    
    output$tfidf <- renderPlot({
        
        top_by_tfidf <- entity_counts %>% 
            bind_tf_idf(term = term, document = entity_type_name, n = n) %>% 
            group_by(entity_type_name) %>% 
            top_n(15, tf_idf) %>% 
            ungroup() %>% 
            mutate(text = reorder_within(term, n, entity_type_name))
        
        ggplot(top_by_tfidf, aes(y = text, x = tf_idf, fill = entity_type_name)) + 
            geom_col() + 
            guides(fill = FALSE) +
            labs(x = "Tf-Idf", y = NULL, 
                 title = "15 most frequent entities",
                 subtitle = "Grouped by entity type") +
            facet_wrap(~ entity_type_name, scales = "free_y") +
            scale_y_reordered() +
            scale_fill_viridis_d(option = 'magma', end = 0.8) +
            theme(plot.title = element_text(face = "bold"),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank()) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
