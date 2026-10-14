# SalesPulse — Sales Intelligence and Anomaly Detection

SalesPulse is an interactive sales analytics application built using R and Shiny. It helps users explore sales trends, forecast future revenue, detect unusual revenue changes, and generate business insight reports.

## Features

- Interactive sales dashboard with revenue KPIs
- Daily sales trends and product/region comparisons
- Sales forecasting using Seasonal Naive and ARIMA models
- Forecast evaluation using MAE and RMSE
- Revenue anomaly detection using Median Absolute Deviation (MAD)
- Adjustable date, product, region, and anomaly filters
- Downloadable CSV files for sales and anomaly alerts
- Exportable HTML business insights report

## Technology Stack

- R
- Shiny and bslib
- dplyr and lubridate
- ggplot2 and Plotly
- forecast
- DT and knitr

## Installation

Install the required packages in R:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "dplyr",
  "lubridate", "ggplot2", "plotly",
  "forecast", "DT", "knitr"
))
```

## Run the Application

1. Open `app.R` in RStudio.
2. Install the required packages.
3. Click **Run App**.

Alternatively, run:

```r
shiny::runApp()
```

## Dataset

The current version generates synthetic sales data for demonstration. The displayed revenue figures and model metrics are illustrative and do not represent real business results.

## Forecasting and Anomaly Detection

The application compares Seasonal Naive and ARIMA forecasts using a chronological six-month holdout set. It detects unusual monthly revenue using a rolling median and Median Absolute Deviation.

## Future Improvements

- Upload real sales CSV files
- Add PDF report generation
- Improve forecasting validation
- Evaluate anomaly detection with labelled data
- Deploy the dashboard online

## Disclaimer

Forecasts are estimates, and anomaly alerts require human review. This application is intended for educational and portfolio purposes.
