# 📊 User Behavior Dashboard (R Shiny)

An interactive R Shiny dashboard for exploring and analyzing mobile user behavior, usage trends, and device performance metrics across demographics and device models.

## 📁 Dataset Overview

This dashboard uses a dataset containing **700 user records** with **11 variables**, capturing:
- User demographics (Age, Gender)
- Device specifications (Device Model, OS)
- App usage metrics (App Usage Time, Number of Apps Installed)
- Device performance (Battery Drain, Screen-On Time)

## 🎯 Objectives

- Visualize patterns in user behavior across device models, age groups, and gender.
- Identify correlations between screen time, app usage, and battery consumption.
- Provide actionable insights for optimizing device performance and user experience.

## 🧰 Tools & Technologies

- **R & R Shiny** for interactive dashboard development
- **Plotly** for dynamic visualizations
- **dplyr, tidyr** for data manipulation
- **shinydashboard** for UI structuring

## 🖼️ Dashboard Features

### 1. 📌 Overview Tab
- View raw dataset
- Explore structure of selected data

### 2. 📊 Interactive Visuals
- 15+ dynamic Plotly graphs:
  - Bar, Pie, Violin, Funnel, Heatmap, Sankey, Bubble, Contour & more
- Value boxes summarizing:
  - Average battery drain
  - Average app usage time
  - Total number of installed apps
- Checkbox filters for device models
- Inter-graph interaction (click-to-filter functionality)

### 3. 📈 Statistical Summary
- Numerical summaries of quantitative variables
- Frequency tables for categorical data

### 4. 🧠 Conclusions
- Key behavioral insights
- Recommendations for product and UX teams:
  - Optimize battery for high-usage devices
  - Develop targeted features by behavior class
  - Personalize UX by age/gender demographics

## 📌 Key Visualizations

| Chart Type | Insight |
|------------|---------|
| Sankey Diagram | Device to Age Group flow |
| Heatmap | App usage by device and age |
| Violin Plot | Battery usage by age group |
| Bubble Chart | Usage trends with app count |
| Pie Charts | Usage and battery share |
| Histogram & Contour | Usage and performance distribution |
| Funnel Chart | Avg battery drain by device |
| Scatter & Line | Screen time vs usage by gender |

> 💡 All graphs (except one pie chart) are **reactive** to filter selections, allowing for a fully interactive experience.


