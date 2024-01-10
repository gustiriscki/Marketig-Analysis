library(shiny)
library(plotly)
library(DT)

# Initial data
data1 <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Additional data
data2 <- data.frame(
  Ad_Placement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 10),
  CTR = c(2.5, 3.8, 3.1, 2.7, 3.5, 2.9, 2.8, 4.0, 3.0, 2.6, 3.7, 3.2, 3.0, 3.9, 3.3, 2.4, 3.6, 2.8, 2.9, 4.1, 3.4, 2.5, 3.4, 3.1, 2.6, 3.8, 3.2, 2.7, 3.9, 3.5)
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #E0FFFF; /* Light Blue background color for the entire app */
        }
        .navbar {
          background-color: #2596be; /* Green background color */
        }
        .navbar-default .navbar-nav > li > a {
          color: #ffffff; /* White text color */
        }
        .navbar-default .navbar-nav > li > a:hover,
        .navbar-default .navbar-nav > li > a:focus {
          color: #000000; /* Black text color on hover/focus */
        }
        .navbar-default .navbar-brand {
          color: #ffffff; /* White text color for brand */
        }
        .navbar-default .navbar-brand:hover,
        .navbar-default .navbar-brand:focus {
          color: #000000; /* Black text color on hover/focus for brand */
        }
        .nav-tabs > li.active > a, .nav-tabs > li > a {
          background-color: #2596be; /* Green background color for active and inactive tabs */
          color: #024F55; /* White text color for active and inactive tabs */
        }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
          background-color: #024F55; /* Green background color for active tab */
          color: #ffffff; /* White text color for active tab */
        }
        .nav-tabs > li > a {
          background-color: #2596be; /* Green background color for inactive tabs */
          color: #ffffff; /* White text color for inactive tabs */
        }
        .nav-tabs > li > a:hover, .nav-tabs > li > a:focus {
          background-color: #00008B; /* Green background color on hover/focus for inactive tabs */
          color: #ffffff; /* White text color on hover/focus for inactive tabs */
        }
        .tab-content {
          padding: 15px; /* Add padding to tab content */
        }
        "
      )
    )
  ),
  titlePanel("Digital Marketing Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("day_input", "Day:", ""),
      numericInput("left_sidebar_input", "Left Sidebar RKT:", value = 0),
      numericInput("center_page_input", "Center Page RKT:", value = 0),
      numericInput("right_sidebar_input", "Right Sidebar RKT:", value = 0),
      actionButton("add_data_button", "Add Data", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data Table",
          DTOutput("data_table")
        ),
        tabPanel(
          "Graphs & Analysis",
          selectInput("plot_type", "Select Plot Type", choices = c("Scatterplot", "Boxplot", "Histogram", "Pie Chart"), selected = "Scatterplot"),
          plotlyOutput("selected_plot"),
          conditionalPanel(
            condition = "input.plot_type == 'Boxplot'",
            plotlyOutput("boxplot")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Histogram'",
            plotlyOutput("histogram")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Pie Chart'",
            plotlyOutput("pie_chart")
          )
        ),
        tabPanel(
          "Statistical Analysis",
          h3("ANOVA Analysis Results"),
          verbatimTextOutput("anova_output"),
          verbatimTextOutput("anova_conclusion"),
          HTML("<br><h4>When the p-value from the ANOVA results shows a value above alpha (0.05), it can be concluded that there is no difference in the ads for ZoomRunners placed in three different locations on the website: the left sidebar, the center (within the content), and the right sidebar. If the ANOVA results show a value below alpha (0.05), it can be concluded that there is at least one pair of differences in the ads for ZoomRunners placed in three different locations on the website: the left sidebar, the center (within the content), and the right sidebar.</h4>")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Store both data sets in reactiveValues
  rv <- reactiveValues(data1 = data1, data2 = data2)
  
  # Combine both datasets when button is pressed
  observeEvent(input$add_data_button, {
    day <- as.numeric(input$day_input)
    left_sidebar_rkt <- as.numeric(input$left_sidebar_input)
    center_page_rkt <- as.numeric(input$center_page_input)
    right_sidebar_rkt <- as.numeric(input$right_sidebar_input)
    
    new_data1 <- data.frame(Day = day, Left_Sidebar = left_sidebar_rkt, Center_Page = center_page_rkt, Right_Sidebar = right_sidebar_rkt)
    rv$data1 <- rbind(rv$data1, new_data1)
    
    new_data2 <- data.frame(Ad_Placement = rep(c("Left Sidebar", "Center Page", "Right Sidebar"), each = 1), CTR = c(left_sidebar_rkt, center_page_rkt, right_sidebar_rkt))
    rv$data2 <- rbind(rv$data2, new_data2)
  })
  
  # Display interactive table for data1
  output$data_table <- renderDT({
    datatable(rv$data1, options = list(pageLength = 10))
  })
  
  # Display selected plot based on user input
  output$selected_plot <- renderPlotly({
    if (input$plot_type == "Scatterplot") {
      plot_ly(rv$data1, x = ~Day, y = ~Left_Sidebar, type = 'scatter', mode = 'lines+markers', name = 'Left Sidebar') %>%
        add_trace(y = ~Center_Page, name = 'Center Page') %>%
        add_trace(y = ~Right_Sidebar, name = 'Right Sidebar') %>%
        layout(title = 'Scatter Plot of Day vs. RKT for Different Ad Placements', xaxis = list(title = 'Day'), yaxis = list(title = 'RKT'))
    } else if (input$plot_type == "Boxplot") {
      # Boxplot using all variables
      boxplot_data <- rv$data1 %>%
        gather(key = "Ad_Placement", value = "RKT", -Day) %>%
        filter(Ad_Placement %in% c("Left_Sidebar", "Center_Page", "Right_Sidebar"))
      
      plot_ly(boxplot_data, x = ~Ad_Placement, y = ~RKT, type = 'box') %>%
        layout(title = 'Boxplot of RKT by Ad Placement')
    } else if (input$plot_type == "Histogram") {
      # Histogram using all variables
      hist_data <- rv$data1 %>%
        gather(key = "Ad_Placement", value = "RKT", -Day) %>%
        filter(Ad_Placement %in% c("Left_Sidebar", "Center_Page", "Right_Sidebar"))
      
      plot_ly(hist_data, x = ~RKT, type = 'histogram', color = ~Ad_Placement) %>%
        layout(title = 'Histogram of RKT by Ad Placement', xaxis = list(title = 'RKT'), barmode = 'overlay')
    } else if (input$plot_type == "Pie Chart") {
      # Pie Chart using all variables
      pie_data <- rv$data1 %>%
        gather(key = "Ad_Placement", value = "RKT", -Day) %>%
        group_by(Ad_Placement) %>%
        summarize(Total_RKT = sum(RKT))
      
      plot_ly(pie_data, labels = ~Ad_Placement, values = ~Total_RKT, type = 'pie') %>%
        layout(title = 'Pie Chart of Total RKT by Ad Placement')
    }
  })
  
  # Adding statistical feature for data2
  observe({
    # Make sure there are at least two observations in each group
    if (nrow(rv$data2) >= 2 && length(unique(rv$data2$Ad_Placement)) >= 3) {
      # Anova test to compare mean RKT across different ad placement locations for data2
      anova_result <- aov(CTR ~ Ad_Placement, data = rv$data2)
      
      # Display Anova test results for data2
      output$anova_output <- renderPrint({
        summary(anova_result)
      })
      
      # Store the p-value for later use
      p_value <- summary(anova_result)$`Pr(>F)`[1]
      
      # Update reactiveValues with the p-value for data2
      rv$p_value_data2 <- p_value
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
