library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)

options(shiny.maxRequestSize = 200 * 1024^2) # Increase file upload size

# UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Sales Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Dataset",
            status = "primary",
            solidHeader = TRUE,
            fileInput("file", "Choose CSV File", accept = c(".csv")),
            actionButton("load", "Load Data")
          ),
          box(
            title = "Preview Data",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("preview")
          )
        )
      ),
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            dateRangeInput(
              "date_range", 
              "Select Date Range:", 
              start = Sys.Date() - 30, 
              end = Sys.Date()
            ),
            selectInput("region", "Select Region:", choices = NULL, multiple = TRUE),
            selectInput("category", "Select Product Category:", choices = NULL, multiple = TRUE),
            selectInput("payment", "Select Payment Method:", choices = NULL, multiple = TRUE)
          ),
          box(
            title = "Analysis Options",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "analysis", 
              "Choose Analysis:",
              choices = c(
                "Sales Trends",
                "Top-Selling Products",
                "Category Breakdown",
                "Regional Analysis",
                "Payment Method Analysis",
                "Revenue vs. Units Sold",
                "Summary Statistics"
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Results",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot"),
            DTOutput("summary_table"),
            verbatimTextOutput("stats")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  dataset <- reactiveVal() # Store the uploaded dataset
  
  observeEvent(input$load, {
    # Debugging log
    print("Loading dataset...")
    
    tryCatch({
      # Load the dataset
      file_path <- "D:/Documents/DSU/SEM 5/SEC/Project/R Project/Online Sales Data.csv"
      data <- read.csv(file_path, header = TRUE, sep = ",")
      
      # Debug: Check column names
      print("Columns in dataset:")
      print(colnames(data))
      
      # Parse Date column
      if ("Date" %in% colnames(data)) {
        data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
      } else {
        stop("Date column not found in the dataset!")
      }
      
      # Store the dataset
      dataset(data)
      
      # Update filter choices
      updateSelectInput(session, "region", choices = unique(data$Region))
      updateSelectInput(session, "category", choices = unique(data$Product.Category))
      updateSelectInput(session, "payment", choices = unique(data$Payment.Method))
    }, error = function(e) {
      # Log errors
      print(paste("Error loading dataset:", e$message))
    })
  })
  
  filtered_data <- reactive({
    req(dataset()) # Ensure dataset is available
    data <- dataset()
    
    # Apply filters
    data <- data %>%
      filter(
        Date >= input$date_range[1] & Date <= input$date_range[2],
        (is.null(input$region) | Region %in% input$region),
        (is.null(input$category) | Product.Category %in% input$category),
        (is.null(input$payment) | Payment.Method %in% input$payment)
      )
    
    # Debug: Print filtered data
    print("Filtered data preview:")
    print(head(data))
    
    data
  })
  
  output$preview <- renderDT({
    req(dataset())
    datatable(dataset())
  })
  
  output$plot <- renderPlot({
    req(filtered_data())
    data <- filtered_data()
    
    # Generate plots based on selected analysis
    if (input$analysis == "Sales Trends") {
      ggplot(data, aes(x = Date, y = Total.Revenue)) +
        geom_line(color = "orange") +
        labs(title = "Sales Trends", x = "Date", y = "Total Revenue") +
        theme_minimal()
    } else if (input$analysis == "Top-Selling Products") {
      top_products <- data %>%
        group_by(Product.Name) %>%
        summarise(Revenue = sum(Total.Revenue)) %>%
        arrange(desc(Revenue)) %>%
        head(10)
      ggplot(top_products, aes(x = reorder(Product.Name, Revenue), y = Revenue)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        labs(title = "Top-Selling Products", x = "Product Name", y = "Revenue") +
        theme_minimal()
    } else if (input$analysis == "Category Breakdown") {
      category_data <- data %>%
        group_by(Product.Category) %>%
        summarise(Revenue = sum(Total.Revenue)) %>%
        mutate(Percentage = Revenue / sum(Revenue) * 100)  # Calculate percentage
      
      ggplot(category_data, aes(x = "", y = Revenue, fill = Product.Category)) +
        geom_bar(stat = "identity") +
        coord_polar("y") +
        labs(title = "Category Breakdown") +
        theme_void() +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white")
    }
    else if (input$analysis == "Regional Analysis") {
      region_data <- data %>%
        group_by(Region) %>%
        summarise(Revenue = sum(Total.Revenue))
      ggplot(region_data, aes(x = reorder(Region, Revenue), y = Revenue)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        labs(title = "Regional Analysis", x = "Region", y = "Revenue") +
        theme_minimal()
    } else if (input$analysis == "Payment Method Analysis") {
      payment_data <- data %>%
        group_by(Payment.Method) %>%
        summarise(Revenue = sum(Total.Revenue))
      ggplot(payment_data, aes(x = reorder(Payment.Method, Revenue), y = Revenue)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        labs(title = "Payment Method Analysis", x = "Payment Method", y = "Revenue") +
        theme_minimal()
    } else if (input$analysis == "Revenue vs. Units Sold") {
      ggplot(data, aes(x = Units.Sold, y = Total.Revenue)) +
        geom_point(color = "orange") +
        labs(title = "Revenue vs. Units Sold", x = "Units Sold", y = "Total Revenue") +
        theme_minimal()
    }
  })
  
  output$summary_table <- renderDT({
    req(filtered_data())
    data <- filtered_data()
    if (input$analysis == "Summary Statistics") {
      summary_data <- data %>%
        summarise(
          Total.Revenue = sum(Total.Revenue),
          Total.Units.Sold = sum(Units.Sold),
          Average.Unit.Price = mean(Unit.Price, na.rm = TRUE)
        )
      datatable(summary_data)
    }
  })
  
  output$stats <- renderPrint({
    req(filtered_data())
    if (input$analysis == "Summary Statistics") {
      summary(filtered_data())
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)