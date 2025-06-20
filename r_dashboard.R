# Packages
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)

# Loading data
data <- read.csv("C:\\Users\\hp\\OneDrive\\Desktop\\DataViz\\M.Wasil Fa23-bst-088\\user_behavior_dataset.csv",
                 stringsAsFactors = FALSE)


# Define UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "User Behavior Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview"),
      menuItem("Interactive Visuals", tabName = "interactive"),
      menuItem("Statistical Summary", tabName = "summary"),
      menuItem("Conclusion", tabName = "conclusion")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              fluidRow(
                box(width = 12, title = "General View of Data", verbatimTextOutput("data_structure")),
                box(width = 12, title = "Raw DataTable", DTOutput("raw_data"))
              )
      ),
      tabItem("interactive",
              # Cards row
              fluidRow(
                valueBoxOutput("average_drain"),
                valueBoxOutput("average_usage"),
                valueBoxOutput("number_apps")
              ),
              box(width = 12,
                  title = "Filter by Device Model",
                  checkboxGroupInput("selected_devices",
                                     "Select Device Model(s):",
                                     choices = unique(data$Device.Model),
                                     selected = unique(data$Device.Model))),
              fluidRow(
                box(width = 4, plotlyOutput("chart1")),
                box(width = 4, plotlyOutput("chart2")),
                box(width = 4, plotlyOutput("chart3"))
              ),
              fluidRow(
                box(width = 4, plotlyOutput("chart4")),
                box(width = 4, plotlyOutput("chart5")),
                box(width = 4, plotlyOutput("chart6"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("chart7")),
                box(width = 6, plotlyOutput("chart9"))
              ),
              fluidRow(
                box(width = 4, plotlyOutput("chart10")),
                box(width = 4, plotlyOutput("chart11")),
                box(width = 4, plotlyOutput("chart12"))
              ),
              fluidRow(
                box(width = 12, plotlyOutput("chart15"))
              )
      ),
      tabItem("summary",
              fluidRow(
                box(width = 6, title = "Numerical Summary", verbatimTextOutput("numerical_summary")),
                box(width = 6, title = "Categorical Summary", verbatimTextOutput("categorical_summary"))
              )
      ),
      tabItem("conclusion",
              fluidPage(
                h2("Key Findings and Conclusions"),
                fluidRow(
                  column(12,
                         h3("Data Insights"),
                         tags$div(
                           h4("Dataset Overview:"),
                           p("The dataset contains 700 user records with 11 variables including user demographics, device information, and usage patterns."),
                           h4("Key Findings:"),
                           tags$ul(
                             tags$li("The dataset shows diverse user behavior patterns across different age groups and genders."),
                             tags$li("Device models and operating systems show varying usage patterns and battery consumption."),
                             tags$li("There are clear correlations between screen time, app usage, and battery drain."),
                             tags$li("User behavior classes (1-5) represent different levels of device engagement.")
                           ),
                           h4("Recommendations:"),
                           tags$ul(
                             tags$li("Consider optimizing battery performance for high-usage devices."),
                             tags$li("Develop targeted features based on user behavior classes."),
                             tags$li("Consider age and gender demographics for personalized experiences."),
                             tags$li("Analyze usage patterns to optimize network performance.")
                           )
                         )
                  )
                )
              )
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # Filtered data reactive expression
  filtered_data <- reactive({ 
    req(input$selected_devices)
    data %>% filter(Device.Model %in% input$selected_devices)
  })
  
  # Filter by click on chart1 (Device Model)
  observeEvent(event_data("plotly_click", source = "chart1"), {
    click <- event_data("plotly_click", source = "chart1")
    if (!is.null(click)) {
      new_data <- data %>% filter(Device.Model == click$x)
      # Update selected_devices to match click
      updateCheckboxGroupInput(session, "selected_devices",
                               selected = click$x)
    }
  })
  
  # Filter by click on chart2 (Gender)
  observeEvent(event_data("plotly_click", source = "chart2"), {
    click <- event_data("plotly_click", source = "chart2")
    if (!is.null(click)) {
      new_data <- data %>% filter(Gender == click$label)
      # Currently, we do not filter by gender, but you can reuse this
      # if needed.
    }
  })
  
  output$raw_data <- renderDT({filtered_data()})
  
  output$data_structure <- renderPrint({str(filtered_data())})
  
  color_palette <- "Set2"
  
  # Average battery drain
  output$average_drain <- renderValueBox({ 
    avg_drain <- mean(filtered_data()$Battery.Drain..mAh.day., na.rm = TRUE)
    valueBox(format(round(avg_drain, 2), big.mark = ","), 
             subtitle = "Average Battery Drain (mAh/day)", 
             icon = icon("battery-half"), 
             color = "blue")
  })
  
  # Average data usage
  output$average_usage <- renderValueBox({ 
    avg_usage <- mean(filtered_data()$App.Usage.Time..min.day., na.rm = TRUE)
    valueBox(format(round(avg_usage, 2), big.mark = ","), 
             subtitle = "Average App Usage Time (minutes/day)", 
             icon = icon("mobile-alt"), 
             color = "green")
  })
  
  # Number of apps
  output$number_apps <- renderValueBox({ 
    number <- sum(filtered_data()$Number.of.Apps.Installed, na.rm = TRUE)
    valueBox(format(number, big.mark = ","), 
             subtitle = "Total Number of Apps Installed", 
             icon = icon("th-list"), 
             color = "yellow")
  })
  
  output$chart1 <- renderPlotly({ 
    df <- filtered_data() %>%
      group_by(Device.Model) %>%
      summarise(mean_screen = mean(Screen.On.Time..hours.day.), .groups = "drop")
    plot_ly(
      df, 
      x = ~Device.Model, 
      y = ~mean_screen,
      color = ~Device.Model, 
      colors = color_palette, 
      type = "bar",
      source = "chart1") %>%
      layout(title = "Average Screen Time by Device Model")
  })
  
  output$chart2 <- renderPlotly({ 
    df <- filtered_data() %>%
      group_by(Gender) %>%
      summarise(total_usage = sum(App.Usage.Time..min.day.), .groups = "drop")    
    
    plot_ly(
      df,
      labels = ~Gender,
      values = ~total_usage,
      type = "pie",
      hole = 0.4,
      source = "chart2") %>%
      layout(title = "Proportion of App Usage by Gender")
  })
  
  output$chart3 <- renderPlotly({ 
    df <- filtered_data() %>%
      group_by(Device.Model) %>%
      summarise(mean_drain = mean(Battery.Drain..mAh.day.), .groups = "drop") %>%
      arrange(mean_drain)
    
    plot_ly(
      df,
      y = ~Device.Model,
      x = ~mean_drain,
      type = "funnel") %>%
      layout(title = "Average Battery Drain by Device Model") 
  })
  
  output$chart4 <- renderPlotly({ 
    df <- filtered_data()
    df <- df %>%
      mutate(Age.Group = cut(Age,
                             breaks = c(0, 18, 25, 35, 45, 60, Inf),
                             labels = c("0-17", "18-24", "25-34", "35-44", "45-59", "60+"),
                             include.lowest = TRUE))
    plot_ly(data = df, y = ~Battery.Drain..mAh.day., color = ~Age.Group,
            colors = color_palette, 
            type = "violin") %>%
      layout(title = "Distribution of Battery Drain by Age Group") 
  })
  
  output$chart5 <- renderPlotly({ 
    df <- filtered_data()
    plot_ly(
      df, 
      x = ~Screen.On.Time..hours.day., 
      y = ~App.Usage.Time..min.day.,
      color = ~Device.Model,
      colors = color_palette,
      mode = "markers",
      hoverinfo = "x+y+color") %>%
      layout(title = "Screen Time vs App Usage (Scatter)", 
             xaxis = list(title = "Screen Time (Hours per Day)"), 
             yaxis = list(title = "App Usage Time (Minutes per Day)"))
  })
  
  output$chart6 <- renderPlotly({ 
    df <- data %>%
      group_by(Device.Model) %>%
      summarise(total_drain = sum(Battery.Drain..mAh.day.), .groups = "drop")    
    
    plot_ly(
      df, 
      labels = ~Device.Model, 
      values = ~total_drain,
      type = "pie") %>%
      layout(title = "Proportion of Battery Drain by Device Model (Static)") 
  })
  
  output$chart7 <- renderPlotly({ 
    df <- filtered_data()
    df <- df %>%
      mutate(Age.Group = cut(Age,
                             breaks = c(0, 18, 25, 35, 45, 60, Inf),
                             labels = c("0-17", "18-24", "25-34", "35-44", "45-59", "60+"),
                             include.lowest = TRUE)) %>%
      group_by(Device.Model, Age.Group) %>%
      summarise(mean_usage = mean(App.Usage.Time..min.day.), .groups = "drop")
    
    plot_ly(
      df,
      x = ~Device.Model,
      y = ~Age.Group,
      z = ~mean_usage,
      colorscale = "Viridis",
      type = "heatmap") %>%
      layout(title = "Average App Usage by Device Model and Age Group") 
  })
  
  output$chart9 <- renderPlotly({ 
    plot_ly(filtered_data(), x = ~Battery.Drain..mAh.day.,
            type = "histogram") %>%
      layout(title = "Distribution of Battery Drain") 
  })
  
  output$chart10 <- renderPlotly({ 
    plot_ly(filtered_data(), 
            x = ~Screen.On.Time..hours.day.,
            y = ~Battery.Drain..mAh.day.,
            type = "histogram2dcontour") %>%
      layout(title = "Screen Time vs Battery Drain (Contour)") 
  })
  
  output$chart11 <- renderPlotly({ 
    plot_ly(filtered_data(), 
            x = ~Screen.On.Time..hours.day.,
            y = ~App.Usage.Time..min.day.,
            size = ~Number.of.Apps.Installed,
            color = ~Device.Model,
            sizes = c(10, 100),
            mode = "markers",
            hoverinfo = "x+y+size+color") %>%
      layout(title = "Bubble Chart of App Usage vs. Battery Drain",
             xaxis = list(title = "Screen Time (Hours per Day)"),
             yaxis = list(title = "App Usage Time (Minutes per Day)"))
  })
  
  output$chart12 <- renderPlotly({ 
    plot_ly(filtered_data(), 
            x = ~Screen.On.Time..hours.day., 
            y = ~App.Usage.Time..min.day., 
            color = ~Gender, 
            symbol = ~Gender,
            mode = "markers") %>%
      layout(title = "Screen Time vs App Usage by Gender") 
  })
  
  output$chart15 <- renderPlotly({ 
    df <- filtered_data() %>%
      mutate(Age.Group = cut(Age,
                             breaks = c(0, 18, 25, 35, 45, 60, Inf),
                             labels = c("0-17", "18-24", "25-34", "35-44", "45-59", "60+"),
                             include.lowest = TRUE)) %>%
      group_by(Device.Model, Age.Group) %>%
      summarise(total = n(), .groups = "drop")
    
    # Combine all node names in a single vector
    nodes <- c(unique(as.character(df$Device.Model)),
               unique(as.character(df$Age.Group)))
    
    # Source and target IDs should match their index in this vector
    sources <- match(as.character(df$Device.Model), nodes) - 1
    targets <- match(as.character(df$Age.Group), nodes) - 1
    
    links <- data.frame(
      source = sources,
      target = targets,
      value = df$total
    )
    
    plot_ly(
      type = "sankey",
      node = list(
        pad = 10,
        thickness = 20,
        label = nodes
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value
      )
    ) %>%
      layout(title = "Sankey diagram of Device Model to Age Group") 
  })
  
  output$numerical_summary <- renderPrint({ 
    filtered_data() %>%
      select_if(is.numeric) %>%
      summary()
  })
  
  output$categorical_summary <- renderPrint({ 
    filtered_data() %>%
      select_if(is.character) %>%
      lapply(table)
  })
}

shinyApp(ui, server)
