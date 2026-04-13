# =========================
# Libraries
# =========================
library(shiny)
library(shinydashboard)
library(readr)
library(plotly)

# =========================
# Data Loading
# =========================
customers_data <- read_csv("customers.csv")

customers_data$Retention_Probability <-
  customers_data$Retained_Customers / customers_data$Total_Customers

customers_data$Poisson_Probability <-
  dpois(customers_data$x, customers_data$Avg_Purchases)

# =========================
# UI
# =========================
ui <- dashboardPage(
  
  dashboardHeader(title = "Gen-Z Shopping Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Purchase Influences", tabName = "influences", icon = icon("shopping-cart")),
      menuItem("Sales Analysis", tabName = "sales", icon = icon("chart-line")),
      menuItem("Consumer Behavior", tabName = "consumer", icon = icon("users")),
      menuItem("Purchase Influence", tabName = "purchase", icon = icon("bullhorn")),
      menuItem("Market Segmentation", tabName = "segment", icon = icon("layer-group")),
      menuItem("Buying Decision", tabName = "decision", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # -------- POOJA --------
      tabItem(
        tabName = "influences",
        fluidRow(
          box(width = 6, plotlyOutput("plot_retention", height = "350px")),
          box(width = 6, plotlyOutput("plot_poisson", height = "350px"))
        )
      ),
      
      # -------- MANJIRI --------
      tabItem(
        tabName = "sales",
        fluidRow(
          box(width = 12, plotOutput("sales_plot", height = "400px"))
        )
      ),
      
      # -------- JIGISHA --------
      tabItem(
        tabName = "consumer",
        fluidRow(
          box(width = 6, verbatimTextOutput("purchase_stats")),
          box(width = 6, plotOutput("purchase_hist", height = "350px"))
        )
      ),
      
      # -------- SAEE --------
      tabItem(
        tabName = "purchase",
        fluidRow(
          box(width = 6, plotOutput("discount_sales_plot", height = "350px")),
          box(width = 6, plotOutput("anova_boxplot", height = "350px"))
        ),
        fluidRow(
          box(width = 12, verbatimTextOutput("chi_square_result"))
        )
      ),
      
      # -------- AISHWARYA --------
      tabItem(
        tabName = "segment",
        fluidRow(
          box(width = 12, tableOutput("segmentation_table"))
        ),
        fluidRow(
          box(width = 12, verbatimTextOutput("shape_metrics"))
        )
      ),
      
      # -------- SAKSHI (CLEAN & NON-OVERLAPPING) --------
      tabItem(
        tabName = "decision",
        
        fluidRow(
          box(
            width = 12,
            title = "Buying Decision Analysis",
            status = "primary",
            solidHeader = TRUE,
            HTML("<p>This section analyzes how research time influences the final purchase decision using comparison and t-test.</p>")
          )
        ),
        
        fluidRow(
          box(
            title = "Purchase Decision Distribution",
            width = 6,
            plotOutput("decision_bar", height = "350px")
          ),
          
          box(
            title = "Research Time vs Decision",
            width = 6,
            plotOutput("decision_box", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "t-test Result",
            width = 12,
            verbatimTextOutput("ttest_result")
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  # ---- POOJA ----
  output$plot_retention <- renderPlotly({
    plot_ly(customers_data, x = seq_len(nrow(customers_data)),
            y = ~Retention_Probability, type = "scatter", mode = "lines+markers")
  })
  
  output$plot_poisson <- renderPlotly({
    plot_ly(customers_data, x = ~x,
            y = ~Poisson_Probability, type = "scatter", mode = "lines+markers")
  })
  
  # ---- MANJIRI ----
  output$sales_plot <- renderPlot({
    price <- c(500,1000,1500,2000,2500)
    sales <- c(8000,7000,6000,5200,4500)
    model <- lm(sales ~ price)
    plot(price, sales, pch = 16, col = "dodgerblue")
    abline(model, col = "red", lwd = 2)
  })
  
  # ---- JIGISHA ----
  output$purchase_stats <- renderText({
    x <- customers_data$Avg_Purchases
    paste("Mean:", round(mean(x),2),
          "\nMedian:", median(x),
          "\nMode:", as.numeric(names(sort(table(x), TRUE))[1]))
  })
  
  output$purchase_hist <- renderPlot({
    hist(customers_data$Avg_Purchases, col = "lightpink", breaks = 6)
  })
  
  # ---- SAEE ----
  output$discount_sales_plot <- renderPlot({
    Discount <- c(5,10,15,20,25,30)
    SalesAmount <- c(1800,2200,2600,3100,3500,3900)
    plot(Discount, SalesAmount, pch = 19, col = "blue")
    abline(lm(SalesAmount ~ Discount), col = "red", lwd = 2)
  })
  
  output$chi_square_result <- renderText({
    sm <- factor(c("Yes","Yes","No","Yes","No","Yes"))
    pd <- factor(c("Yes","Yes","No","Yes","No","Yes"))
    test <- chisq.test(table(sm, pd))
    paste("Chi-square value:", round(test$statistic,3),
          "\nP-value:", round(test$p.value,4))
  })
  
  output$anova_boxplot <- renderPlot({
    grp <- factor(c("Low","Medium","Medium","High","High","High"))
    sales <- c(1800,2200,2600,3100,3500,3900)
    boxplot(sales ~ grp, col = c("lightblue","lightgreen","lightpink"))
  })
  
  # ---- AISHWARYA ----
  output$segmentation_table <- renderTable({
    Spending <- c(1200,1800,2500,3200,4000,5200,6100)
    df <- data.frame(Spending)
    df$Spending_Group <- cut(
      df$Spending,
      breaks = c(-Inf, quantile(df$Spending,0.25),
                 quantile(df$Spending,0.75), Inf),
      labels = c("Low Spenders","Medium Spenders","High Spenders")
    )
    Mean <- tapply(df$Spending, df$Spending_Group, mean)
    SD <- tapply(df$Spending, df$Spending_Group, sd)
    CV <- (SD / Mean) * 100
    data.frame(Spending_Group = names(Mean),
               Mean = round(Mean,2),
               SD = round(SD,2),
               CV = round(CV,2))
  })
  
  output$shape_metrics <- renderText({
    x <- c(1200,1800,2500,3200,4000,5200,6100)
    n <- length(x); m <- mean(x); s <- sd(x)
    skew <- sum((x-m)^3)/((n-1)*s^3)
    kurt <- sum((x-m)^4)/((n-1)*s^4)
    paste("Skewness:", round(skew,3),
          "\nKurtosis:", round(kurt,3))
  })
  
  # ---- SAKSHI (CLEAN) ----
  output$decision_bar <- renderPlot({
    Decision <- factor(c("Yes","Yes","No","Yes","No","Yes","Yes","No"))
    barplot(table(Decision), col = c("lightgreen","lightcoral"))
  })
  
  output$decision_box <- renderPlot({
    ResearchTime <- c(30,45,10,60,15,50,55,20)
    Decision <- factor(c("Yes","Yes","No","Yes","No","Yes","Yes","No"))
    boxplot(ResearchTime ~ Decision,
            col = c("lightblue","lightpink"))
  })
  
  output$ttest_result <- renderText({
    ResearchTime <- c(30,45,10,60,15,50,55,20)
    Decision <- factor(c("Yes","Yes","No","Yes","No","Yes","Yes","No"))
    buyers <- ResearchTime[Decision=="Yes"]
    nonbuyers <- ResearchTime[Decision=="No"]
    t <- t.test(buyers, nonbuyers)
    paste("t-value:", round(t$statistic,3),
          "\nP-value:", round(t$p.value,4),
          "\nConclusion: Buyers spend more time researching.")
  })
}

# =========================
# Run App
# =========================
shinyApp(ui = ui, server = server)
