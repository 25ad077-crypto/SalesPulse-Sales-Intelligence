
# ============================================================
# SalesPulse - Sales Intelligence and Anomaly Detection
# RStudio + Shiny
# Dataset: Illustrative synthetic sales data
# ============================================================

# 1. Load packages
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(forecast)
library(DT)

# 2. Generate sample daily sales data
set.seed(123)

dates <- seq.Date(
  as.Date("2023-01-01"),
  as.Date("2025-12-31"),
  by = "day"
)

n <- length(dates)

sales <- data.frame(
  Date = dates,
  Revenue = pmax(
    100,
    3500 +
      seq_len(n) * 2.2 +
      700 * sin(2 * pi * yday(dates) / 365.25) +
      250 * sin(2 * pi * wday(dates) / 7) +
      rnorm(n, 0, 350)
  ),
  Product = sample(
    c("Laptop", "Mobile", "Accessories", "Tablet"),
    n, replace = TRUE
  ),
  Region = sample(
    c("North", "South", "East", "West"),
    n, replace = TRUE
  )
)

# Add unusual sales days for demonstration
high_days <- c(180, 390, 650, 900, 1050)
low_days <- c(250, 570, 820)

sales$Revenue[high_days] <- sales$Revenue[high_days] * 2.5
sales$Revenue[low_days] <- sales$Revenue[low_days] * 0.25
sales$Revenue <- round(sales$Revenue, 2)

# 3. Aggregate monthly revenue
monthly <- sales |>
  mutate(Month = floor_date(Date, "month")) |>
  group_by(Month) |>
  summarise(Revenue = sum(Revenue), .groups = "drop") |>
  arrange(Month)

# 4. Anomaly detection helper
detect_anomalies <- function(data, threshold = 3.5) {
  
  data <- data |>
    arrange(Month)
  
  data$Baseline <- NA_real_
  data$RobustScore <- NA_real_
  data$Anomaly <- FALSE
  data$AnomalyType <- "Normal"
  
  if (nrow(data) < 7) {
    return(data)
  }
  
  for (i in 7:nrow(data)) {
    
    # Use only the previous six months
    previous <- data$Revenue[(i - 6):(i - 1)]
    
    baseline <- median(previous)
    spread <- mad(previous, constant = 1.4826)
    
    data$Baseline[i] <- baseline
    
    if (spread > 0) {
      score <- (data$Revenue[i] - baseline) / spread
      
      data$RobustScore[i] <- score
      data$Anomaly[i] <- abs(score) > threshold
      
      if (data$Anomaly[i]) {
        data$AnomalyType[i] <- if (
          data$Revenue[i] > baseline
        ) {
          "Unusually high"
        } else {
          "Unusually low"
        }
      }
    }
  }
  
  data
}

# 5. Prepare forecasting data
y <- ts(monthly$Revenue, frequency = 12)

# Chronological holdout: last six months
train <- head(y, length(y) - 6)
test <- tail(y, 6)

# Seasonal-naive baseline
seasonal_test <- snaive(train, h = length(test))

# Advanced model
arima_fit <- auto.arima(
  train,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = FALSE
)

arima_test <- forecast(arima_fit, h = length(test))

# Metrics
calculate_mae <- function(actual, predicted) {
  mean(abs(as.numeric(actual) - as.numeric(predicted)))
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((as.numeric(actual) - as.numeric(predicted))^2))
}

seasonal_mae <- calculate_mae(test, seasonal_test$mean)
arima_mae <- calculate_mae(test, arima_test$mean)

seasonal_rmse <- calculate_rmse(test, seasonal_test$mean)
arima_rmse <- calculate_rmse(test, arima_test$mean)

# Select the model using holdout MAE
best_model <- if (arima_mae < seasonal_mae) {
  "ARIMA"
} else {
  "Seasonal naive"
}

# 6. Dashboard interface
ui <- page_sidebar(
  title = "SalesPulse | Sales Intelligence",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1769AA"
  ),
  
  sidebar = sidebar(
    h5("Dashboard Filters"),
    
    dateRangeInput(
      "dates",
      "Date range",
      start = min(sales$Date),
      end = max(sales$Date),
      min = min(sales$Date),
      max = max(sales$Date)
    ),
    
    selectInput(
      "region",
      "Region",
      choices = c("All", sort(unique(sales$Region)))
    ),
    
    selectInput(
      "product",
      "Product",
      choices = c("All", sort(unique(sales$Product)))
    ),
    
    sliderInput(
      "threshold",
      "Anomaly threshold",
      min = 2,
      max = 5,
      value = 3.5,
      step = 0.5
    ),
    
    numericInput(
      "horizon",
      "Forecast horizon (months)",
      value = 6,
      min = 1,
      max = 12
    ),
    
    helpText(
      "Demonstration using synthetic sales data."
    )
  ),
  
  navset_tab(
    
    # OVERVIEW
    nav_panel(
      "Overview",
      
      br(),
      
      layout_column_wrap(
        width = 1/3,
        
        value_box(
          title = "Total Revenue",
          value = textOutput("total_revenue"),
          showcase = bs_icon("cash-stack")
        ),
        
        value_box(
          title = "Monthly Growth",
          value = textOutput("growth"),
          showcase = bs_icon("graph-up-arrow")
        ),
        
        value_box(
          title = "Anomalies",
          value = textOutput("anomaly_count"),
          showcase = bs_icon("exclamation-triangle")
        )
      ),
      
      h4("Daily Revenue Trend"),
      plotlyOutput("daily_plot", height = "320px"),
      
      h4("Revenue by Product"),
      plotlyOutput("product_plot", height = "300px"),
      
      h4("Revenue by Region"),
      plotlyOutput("region_plot", height = "300px")
    ),
    
    # FORECAST
    nav_panel(
      "Forecast",
      
      br(),
      
      h4("Actual Sales and Forecast"),
      plotlyOutput("forecast_plot", height = "420px"),
      
      h4("Model Performance"),
      DTOutput("model_table"),
      
      textOutput("chosen_model")
    ),
    
    # ANOMALIES
    nav_panel(
      "Anomaly Alerts",
      
      br(),
      
      p(
        "Unusual monthly revenue is detected using a rolling ",
        "median and median absolute deviation."
      ),
      
      plotlyOutput("anomaly_plot", height = "380px"),
      
      DTOutput("anomaly_table"),
      
      downloadButton(
        "download_alerts",
        "Download Anomaly CSV"
      )
    ),
    
    # DATA EXPLORER
    nav_panel(
      "Data Explorer",
      
      br(),
      
      DTOutput("sales_table"),
      
      downloadButton(
        "download_sales",
        "Download Filtered Sales CSV"
      )
    ),
    
    # REPORT
    nav_panel(
      "Business Report",
      
      br(),
      
      h4("Business Insights"),
      
      verbatimTextOutput("insights"),
      
      downloadButton(
        "download_report",
        "Download HTML Report"
      )
    )
  )
)

# 7. Server logic
server <- function(input, output, session) {
  
  # Filter sales
  filtered_sales <- reactive({
    
    req(input$dates)
    
    sales |>
      filter(
        Date >= input$dates[1],
        Date <= input$dates[2],
        input$region == "All" | Region == input$region,
        input$product == "All" | Product == input$product
      )
  })
  
  # Aggregate filtered data by month
  filtered_monthly <- reactive({
    
    filtered_sales() |>
      mutate(Month = floor_date(Date, "month")) |>
      group_by(Month) |>
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      ) |>
      arrange(Month)
  })
  
  # Detect anomalies on filtered monthly totals
  filtered_anomalies <- reactive({
    
    detect_anomalies(
      filtered_monthly(),
      threshold = input$threshold
    )
  })
  
  # KPI: Total revenue
  output$total_revenue <- renderText({
    
    amount <- sum(filtered_sales()$Revenue)
    
    paste0(
      "$",
      format(round(amount, 2),
             big.mark = ",", nsmall = 2,
             scientific = FALSE)
    )
  })
  
  # KPI: Latest month-over-month growth
  output$growth <- renderText({
    
    dat <- filtered_monthly()
    
    if (nrow(dat) < 2) {
      return("Not enough data")
    }
    
    previous <- dat$Revenue[nrow(dat) - 1]
    current <- dat$Revenue[nrow(dat)]
    
    if (previous == 0) {
      return("N/A")
    }
    
    paste0(
      round((current / previous - 1) * 100, 1),
      "%"
    )
  })
  
  # KPI: Number of detected anomalies
  output$anomaly_count <- renderText({
    
    sum(filtered_anomalies()$Anomaly)
  })
  
  # Daily sales chart
  output$daily_plot <- renderPlotly({
    
    dat <- filtered_sales() |>
      group_by(Date) |>
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      )
    
    p <- ggplot(
      dat,
      aes(Date, Revenue)
    ) +
      geom_line(
        color = "#1769AA",
        linewidth = 0.7
      ) +
      labs(
        x = NULL,
        y = "Revenue ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Product chart
  output$product_plot <- renderPlotly({
    
    dat <- filtered_sales() |>
      group_by(Product) |>
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      )
    
    p <- ggplot(
      dat,
      aes(Product, Revenue, fill = Product)
    ) +
      geom_col(show.legend = FALSE) +
      labs(
        x = NULL,
        y = "Revenue ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Regional chart
  output$region_plot <- renderPlotly({
    
    dat <- filtered_sales() |>
      group_by(Region) |>
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      )
    
    p <- ggplot(
      dat,
      aes(Region, Revenue, fill = Region)
    ) +
      geom_col(show.legend = FALSE) +
      labs(
        x = NULL,
        y = "Revenue ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Forecast chart
  output$forecast_plot <- renderPlotly({
    
    h <- input$horizon
    
    # Use the complete series for the final forecast
    full_series <- ts(
      monthly$Revenue,
      frequency = 12
    )
    
    future <- if (best_model == "ARIMA") {
      
      forecast(
        auto.arima(
          full_series,
          seasonal = TRUE,
          stepwise = TRUE
        ),
        h = h,
        level = 95
      )
      
    } else {
      
      snaive(
        full_series,
        h = h,
        level = 95
      )
    }
    
    fc <- data.frame(
      Month = seq(
        max(monthly$Month) %m+% months(1),
        by = "month",
        length.out = h
      ),
      Forecast = as.numeric(future$mean),
      Lower = as.numeric(future$lower[, 1]),
      Upper = as.numeric(future$upper[, 1])
    )
    
    p <- ggplot(
      monthly,
      aes(Month, Revenue)
    ) +
      geom_line(
        color = "#1769AA",
        linewidth = 1
      ) +
      geom_ribbon(
        data = fc,
        aes(
          x = Month,
          ymin = Lower,
          ymax = Upper
        ),
        inherit.aes = FALSE,
        fill = "#78B7E5",
        alpha = 0.3
      ) +
      geom_line(
        data = fc,
        aes(Month, Forecast),
        inherit.aes = FALSE,
        color = "#D97706",
        linewidth = 1,
        linetype = "dashed"
      ) +
      labs(
        x = NULL,
        y = "Monthly Revenue ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Forecast comparison table
  output$model_table <- renderDT({
    
    dat <- data.frame(
      Model = c("Seasonal naive", "ARIMA"),
      MAE = round(
        c(seasonal_mae, arima_mae), 2
      ),
      RMSE = round(
        c(seasonal_rmse, arima_rmse), 2
      )
    )
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 5
      )
    )
  })
  
  output$chosen_model <- renderText({
    
    paste(
      "Selected model based on holdout MAE:",
      best_model
    )
  })
  
  # Anomaly chart
  output$anomaly_plot <- renderPlotly({
    
    dat <- filtered_anomalies()
    
    p <- ggplot(
      dat,
      aes(
        x = Month,
        y = Revenue,
        text = paste0(
          "Month: ", Month,
          "<br>Revenue: $", round(Revenue, 2),
          "<br>Baseline: $", round(Baseline, 2),
          "<br>Robust score: ", round(RobustScore, 2),
          "<br>Anomaly: ", AnomalyType
        )
      )
    ) +
      geom_line(color = "#1769AA") +
      geom_point(
        aes(color = Anomaly),
        size = 3
      ) +
      scale_color_manual(
        values = c(
          "FALSE" = "#1769AA",
          "TRUE" = "#D62728"
        )
      ) +
      labs(
        x = NULL,
        y = "Monthly Revenue ($)",
        color = "Anomaly"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Anomaly table
  output$anomaly_table <- renderDT({
    
    dat <- filtered_anomalies() |>
      filter(Anomaly) |>
      transmute(
        Month,
        Revenue = round(Revenue, 2),
        Baseline = round(Baseline, 2),
        RobustScore = round(RobustScore, 2),
        AnomalyType
      )
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  # Sales data table
  output$sales_table <- renderDT({
    
    datatable(
      filtered_sales(),
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  # Download anomaly CSV
  output$download_alerts <- downloadHandler(
    
    filename = function() {
      "salespulse_anomaly_alerts.csv"
    },
    
    content = function(file) {
      
      alerts <- filtered_anomalies() |>
        filter(Anomaly)
      
      write.csv(
        alerts,
        file,
        row.names = FALSE
      )
    }
  )
  
  # Download filtered sales CSV
  output$download_sales <- downloadHandler(
    
    filename = function() {
      "salespulse_filtered_sales.csv"
    },
    
    content = function(file) {
      
      write.csv(
        filtered_sales(),
        file,
        row.names = FALSE
      )
    }
  )
  
  # Business insights text
  output$insights <- renderText({
    
    dat <- filtered_monthly()
    anomalies <- filtered_anomalies()
    
    req(nrow(dat) > 0)
    
    growth <- if (
      nrow(dat) >= 2 &&
      dat$Revenue[nrow(dat) - 1] != 0
    ) {
      round(
        100 * (
          dat$Revenue[nrow(dat)] /
            dat$Revenue[nrow(dat) - 1] - 1
        ),
        2
      )
    } else {
      NA_real_
    }
    
    paste0(
      "SALESPULSE - BUSINESS INSIGHTS\n",
      "================================\n\n",
      "Data type: Synthetic demonstration data\n",
      "Filtered revenue: $",
      format(
        round(sum(filtered_sales()$Revenue), 2),
        big.mark = ",", nsmall = 2
      ),
      "\nLatest monthly growth: ", growth, "%\n",
      "Detected anomalies: ",
      sum(anomalies$Anomaly), "\n",
      "Forecast model: ", best_model, "\n",
      "Seasonal naive MAE: ",
      round(seasonal_mae, 2), "\n",
      "ARIMA MAE: ",
      round(arima_mae, 2), "\n",
      "Seasonal naive RMSE: ",
      round(seasonal_rmse, 2), "\n",
      "ARIMA RMSE: ",
      round(arima_rmse, 2), "\n\n",
      "Note: Alerts require human review.\n",
      "Forecasts are estimates, not guarantees."
    )
  })
  
  # Download self-contained HTML report
  output$download_report <- downloadHandler(
    
    filename = function() {
      "salespulse_business_report.html"
    },
    
    content = function(file) {
      
      dat <- filtered_monthly()
      
      alerts <- filtered_anomalies() |>
        filter(Anomaly)
      
      summary_text <- paste0(
        "Total filtered revenue: $",
        format(
          round(sum(filtered_sales()$Revenue), 2),
          big.mark = ",", nsmall = 2
        ),
        "\nDetected anomalies: ",
        sum(filtered_anomalies()$Anomaly),
        "\nForecast model: ", best_model,
        "\nSeasonal naive MAE: ",
        round(seasonal_mae, 2),
        "\nARIMA MAE: ",
        round(arima_mae, 2)
      )
      
      # Escape dynamic text before adding it to HTML
      safe_summary <- htmltools::htmlEscape(
        summary_text
      )
      
      monthly_html <- as.character(
        knitr::kable(
          dat,
          format = "html",
          digits = 2
        )
      )
      
      alerts_html <- as.character(
        knitr::kable(
          alerts,
          format = "html",
          digits = 2
        )
      )
      
      html <- paste0(
        "<!DOCTYPE html>",
        "<html><head><meta charset='utf-8'>",
        "<meta name='viewport' content='width=device-width'>",
        "<title>SalesPulse Business Report</title>",
        "<style>",
        "body{font-family:Arial,sans-serif;",
        "max-width:950px;margin:35px auto;padding:20px;}",
        "h1,h2{color:#1769AA;}",
        "table{border-collapse:collapse;width:100%;margin:15px 0;}",
        "td,th{padding:9px;border:1px solid #ddd;text-align:left;}",
        "pre{white-space:pre-wrap;background:#f3f6f9;padding:15px;}",
        "</style></head><body>",
        
        "<h1>SalesPulse Business Report</h1>",
        "<p><b>Data:</b> Illustrative synthetic sales data.</p>",
        
        "<h2>Business Summary</h2>",
        "<pre>", safe_summary, "</pre>",
        
        "<h2>Monthly Sales</h2>",
        monthly_html,
        
        "<h2>Detected Anomalies</h2>",
        alerts_html,
        
        "<p>Forecasts are estimates. Anomaly alerts require ",
        "human review and do not prove a business error.</p>",
        
        "</body></html>"
      )
      
      writeLines(
        html,
        file,
        useBytes = TRUE
      )
    }
  )
}

# 8. Run application
shinyApp(ui = ui, server = server)
