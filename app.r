library(shiny)
library(ggplot2)
# UI ----
shinyUI <- fluidPage(
  
  tabsetPanel(
    # tab 1: Games ----
    tabPanel("Games", fluid = T,
             titlePanel(title = "Video Games with sales over 100,000 copies"),
             sidebarLayout(position = "left",
                           sidebarPanel(
                             h3("Variable selection"),
                             
                             # Console Selection
                             h4("Choose a console"),
                             selectInput(inputId = "consoleSelect",
                                         label = "Pick a Console",
                                         choices = consoles,
                                         selected = "All"),
                             
                             sliderInput(inputId = "slider",
                                         label = "Pick how many games to display in the bar chart",
                                         min = 3, 
                                         max = 10,
                                         value = 5,
                                         step = 1),
                             h6("Note: some consoles have fewer than 10 games.")
                             
                           ),
                           
                           # Main Panel
                           mainPanel(fluidPage(
                             plotOutput("barchart")
                             
                           )
                           )
             )
    ),
    
    # tab 2: Consoles ----
    tabPanel("Consoles", fluid = TRUE,
             titlePanel(title = "Video Game Sales by Console"),
             sidebarLayout(position = "left",
                           
                           sidebarPanel(
                             h3("Variable selection"),
                             
                             # Console Selection
                             h4("Which console's stats would you like to see?"),
                             selectInput(inputId = "marketChoice",
                                         label = "Pick a geographic market for sales",
                                         choices = markets,
                                         selected = "Overall")
                           ),
                           
                           # Main Panel
                           mainPanel(fluidPage(
                             plotOutput("consoleChart")
                           )
                           )
             )
    ),
    
    #tab 3: Franchises ----
    tabPanel("Franchises", fluid = TRUE,
             titlePanel(title = "Video Game Sales by Franchise"),
             sidebarLayout(position = "left",
                           
                           sidebarPanel(
                             h3("Variable selection"),
                             
                             # Console Selection
                             h4("Franchises"),
                             selectInput(inputId = "brandChoice",
                                         label = "Pick a console to view its exclusive games",
                                         choices = c("All", "Nintendo", "Sony"),
                                         selected = "All"),
                             h6("Note: not all consoles have exclusive games."),
                             
                             # Dynamic 
                             uiOutput("dynamicFranch")
                             
                             # selectInput(inputId = "franchChoice",
                             #             label = "Pick a franchise to label it in the chart",
                             #             choices = franchiseList,
                             #             selected = NULL,
                             #             multiple = T)
                             
                           ),
                           
                           # Main Panel
                           mainPanel(fluidPage(
                             plotOutput("franchiseChart")
                           )
                           )
             )
    )
  )
)





# Server ----
shinyServer <- function(input, output) {
  
  # Games Tab ----
  data <- reactive({
    if (input$consoleSelect == "All") {
      vgSales %>% 
        arrange(desc(Global_Sales))%>%
        slice(1:input$slider + 1)  
    } else {
      vgSales[vgSales$Platform == input$consoleSelect,] %>% 
        arrange(desc(Global_Sales))%>%
        slice(1:input$slider + 1) 
    }
  })
  title <- reactive({
    if (input$consoleSelect == "All") {
      paste0("Top ", input$slider,  " selling games - overall")
    }else {
      paste0("Top ", input$slider,  " selling games for the ",  input$consoleSelect)
    }
  })
  
  output$barchart <- renderPlot({
    
    ggplot(data(), 
           aes(x=reorder(Name, 
                         -Global_Sales),
               y=Global_Sales,
               fill = Franchise)
    )+
      
      geom_col(colour = "Black") +
      
      theme(
        legend.position = "top",
        legend.justification = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 20, 
                                  margin = margin(b = 30)),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, 
                                    size=1),
        axis.text.x = element_text(size = 14,
        ),
        axis.title.y = element_text(face = "bold", size = 14)
        
      ) +
      
      labs(title = title(),
           x = "",
           y = "Global Sales (Millions of Units)",
           fill = "Franchise"
      ) +
      
      scale_x_discrete(labels = function(Name) str_wrap(Name, width = 12))
    
  })
  
  # Console tab----
  market <- reactive({
    if (input$marketChoice == "US") {
      top10consoles$naSales
    } else if (input$marketChoice == "EU") {
      top10consoles$euSales
    } else if (input$marketChoice == "Japan") {
      top10consoles$jpSales
    } else if (input$marketChoice == "Other") {
      top10consoles$othSales
    } else {
      top10consoles$totalSales
    }
  })
  
  output$consoleChart <- renderPlot({
    ggplot(top10consoles,
           aes(x=reorder(Platform,-market()),
               y = market(),
               fill = Console_Family)) +
      
      geom_bar(stat = "identity",
               colour = "black")+
      
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 20, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, margin = margin(b = 25)),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(face = "bold", size = 14)
      ) +
      
      labs(
        title = "Number of games sold by console",
        subtitle = "The total number of games sold on each console - only games with over 100k sales",
        x = "",
        y = "Global Game Sales (Millions of Units)"
      ) +
      
      scale_x_discrete(labels = function(Name) str_wrap(Name, width = 5)) +
      
      scale_fill_manual(values = c("#107C10", "#FE0016","#F8F9FA","#2E6DB4"))
  })
  
  # Franchises tab ----
  
  plotData <- reactive({
    if (input$brandChoice == "All") {
      groupedFranchises2
    } else {
      exclusives1[exclusives1$Console_Family == input$brandChoice,]
    }
  })
  
  labelNum <- reactive({
    switch(input$brandChoice,
           "All" = 100,
           "Nintendo" = 32.6,
           "Sony" = 13
    )
  })
  
  franList <- reactive({
    switch (input$brandChoice,
            "All" = franchiseList,
            "Nintendo" = nintendoOnly,
            "Sony" = sonyOnly
    )
  })
  
  output$dynamicFranch <- renderUI({
    selectInput(inputId = "franchChoice",
                label = "Pick a franchise to change its shape in the chart",
                choices = franList()
    )
    
  })
  
  
  
  
  output$franchiseChart <- renderPlot({
    ggplot(plotData(),
           aes(
             x=sales, 
             y=count,
             size = consoleCount,
             colour = genre
           )
    ) +
      
      geom_point(
        aes(shape = ifelse(Franchise ==  input$franchChoice, as.integer(15), as.integer(16))), # Change shape of chosen fanchise
        alpha = 0.7
      )+
      
      scale_shape_identity() +
      
      theme(
        # legend.position = "top",
        # legend.direction = "horizontal",
        legend.background = element_blank(),
        plot.title = element_text(size = 16, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, margin = margin(b = 15)),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
      )+
      
      geom_text(
        colour = "Black",
        size = 5,
        aes(label = ifelse(sales > labelNum(), 
                           as.character(Franchise), 
                           '')),
        hjust = 1,
        vjust = 0
      )+
    
    labs(
      title = "Sales of different franchises",
      subtitle = "The amount of copies different franchises have sold, vs how many releases they have had.",
      x = "Worldwide Sales (Millions of units)",
      y = "Number of releases",
      fill = "Genre",
      size = "Number of different consoles \ngame is avilable on"
    ) +
      
      scale_size_continuous(range = c(5,9))
    
  })
  
}


# App ----
shinyApp(ui = shinyUI, server = shinyServer, options = list(height = 1080))