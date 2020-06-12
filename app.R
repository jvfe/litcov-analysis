library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
theme_set(theme_bw(base_family = "Roboto condensed"))

load("./data/processed/objects_for_plotting.rda")

ui <- dashboardPage(
    dashboardHeader(title = "LITCovid19 Text Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Entities", tabName = "Entities", icon = icon("bar-chart")),
            menuItem("Network", tabName = "Network", icon = icon("arrows-alt"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Entities",
                    fluidRow(
                        box(title = "Filter data", status = "warning", 
                            collapsible = TRUE, solidHeader = TRUE,
                            sliderInput("slider_tf", "Number of observations:", 5, 25, 10)),
                        box(title = "Table", status = "warning", 
                            collapsible = TRUE, solidHeader = TRUE,
                            tableOutput("tfidf_data"))
                    ),
                    fluidRow(
                        box(title = "15 most frequent entities", status = "primary", solidHeader = TRUE,
                            HTML("<b>Grouped by entity type</b></br>"), br(),
                            plotOutput("tfidf", height = 700), height = 800, width = "90%")
                    )
            ),
            
            tabItem(tabName = "Network",
                fluidRow(
                    box(title = "Filter data", status = "warning", 
                        collapsible = TRUE, solidHeader = TRUE,
                        sliderInput("slider_net", "Number of observations:", 25, 150, 50)),
                    box(title = "Table", status = "warning", 
                        collapsible = TRUE, solidHeader = TRUE,
                        tableOutput("net_data"))
                ),
                fluidRow(
                    box(title = "Bigram Network", status = "primary", solidHeader = TRUE,
                        plotOutput("network", height = 700), height = 800, width = "90%")
                )
            )
        )
    )
)
server <- function(input, output) {
    
    set.seed(122)
    
    output$net_data <- renderTable({
        litcovid_bigrams %>% 
            filter(n > input$slider_net) %>%
            setNames(c("Word 1", "Word 2", "Count")) %>% 
            head(5)
    }, width = "100%")
    
    output$network <- renderPlot({
        
        bigram_graph <- litcovid_bigrams %>% 
            filter(n > input$slider_net) %>%
            graph_from_data_frame() 
        
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        
        bigram_graph %>% 
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
            geom_node_point(color = "#D8BFD8", size = 3) +
            geom_node_text(aes(label = name), check_overlap = TRUE, vjust = 1, hjust = 1) +
            theme_void()
    })
    
    output$tfidf_data <- renderTable({
        entity_counts %>% 
            bind_tf_idf(term = term, document = entity_type_name, n = n) %>% 
            group_by(entity_type_name) %>% 
            select(-c(tf, idf)) %>% 
            setNames(c("Entity Name", "Entity", "Count", "Tf-Idf")) %>% 
            top_n(input$slider_tf, `Tf-Idf`) %>% 
            ungroup() %>% 
            head(5)
    }, width = "100%")
    
    output$tfidf <- renderPlot({
        
        top_by_tfidf <- entity_counts %>% 
            bind_tf_idf(term = term, document = entity_type_name, n = n) %>% 
            group_by(entity_type_name) %>% 
            top_n(input$slider_tf, tf_idf) %>% 
            ungroup() %>% 
            mutate(text = reorder_within(term, n, entity_type_name))
        
        ggplot(top_by_tfidf, aes(y = text, x = tf_idf, fill = entity_type_name)) + 
            geom_col() + 
            guides(fill = FALSE) +
            labs(x = "Tf-Idf", y = NULL) +
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
