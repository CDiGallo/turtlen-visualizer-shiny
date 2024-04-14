source("shiny_functions.r")


## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    textAreaInput("contentinput", "create your turtle here (every line except the first needs to end with a '.' . Do not change the first line. " , "subject property object
<gianni> schema:children <Claudio>.
<mario> schema:children <gianni>.
<alberto> schema:children <mario>.", width = "1000px", height = "400px"),
    
    actionButton("ready", "Generate Graph"),
    visNetworkOutput("graph",width = "100%", height = "700px")
  )
  server <- function(input, output) {
   
    output$graph <- renderVisNetwork({
       visNetwork(nodes_function({ input$contentinput }) , edges_function({ input$contentinput }), main="Generated Network from turtle", background = "beige" )  %>%
        visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = TRUE)

      
      
         })|>
      bindEvent(input$ready)

    
        
    # output$graph <- renderPlot({ggraph(graph_creator_basic({ input$contentinput }) ) +
    #                      geom_node_label()+
    #                    geom_node_point()+
    #                    geom_edge_link(aes()) })|>
    #            bindEvent(input$ready)
  }
    
      

  shinyApp(ui, server)

  
  
    
}
?visNetworkOutput

visNetwork(nodes_function(df), edges_function(df))

# # function with base are below below ----------------------------------------------------------
# 
# library(shiny)
# source("shiny_functions.r")
# 
# 
# ## Only run examples in interactive R sessions
# if (interactive()) {
#   
#   ui <- fluidPage(
#     textAreaInput("contentinput", "create your turtle here", "subject property object
# <Claudio> is_son_of <gianni>.
# <gianni> is_son_of <mario>.
# <mario> is_son_of <albert>.", width = "1000px", height = "400px"),
#     
#     actionButton("ready", "filter data"),
#     plotOutput ("graph")
#   )
#   server <- function(input, output) {
#     
#     output$graph <- renderPlot({plot.igraph(graph_creator_basic({ input$contentinput }) )
#     })|>
#       bindEvent(input$ready)
#     
#     
#     
#     # output$graph <- renderPlot({ggraph(graph_creator_basic({ input$contentinput }) ) +
#     #                      geom_node_label()+
#     #                    geom_node_point()+
#     #                    geom_edge_link(aes()) })|>
#     #            bindEvent(input$ready)
#   }
#   
#   
#   
#   shinyApp(ui, server)
#   
#   
#   
#   
# }
# 
