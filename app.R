install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("cluster")
install.packages("factoextra")
install.packages("shinythemes")  # For better UI themes

library(shiny)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(shinythemes)  # Optional, for UI themes

customer_data <- read.csv("C:/Users/mites/Downloads/Mall_Customers.csv")

customer_data <- customer_data[, c("Annual.Income..k..", "Spending.Score..1.100.")]
customer_data <- scale(customer_data)  # Normalize data

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # UI Theme
  titlePanel("Customer Segmentation Using K-Means Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("clusters", "Select Number of Clusters (K):", min = 2, max = 10, value = 5),
      actionButton("run_kmeans", "Run K-Means")
    ),
    
    mainPanel(
      plotOutput("clusterPlot"),
      plotOutput("elbowPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive K-Means Clustering
  kmeans_result <- reactive({
    input$run_kmeans  # React to button click
    isolate({
      kmeans(customer_data, centers = input$clusters, nstart = 25)
    })
  })
  
  # Elbow Method Plot
  output$elbowPlot <- renderPlot({
    wcss <- vector()
    for (i in 1:10) {
      kmeans_model <- kmeans(customer_data, centers = i, nstart = 25)
      wcss[i] <- kmeans_model$tot.withinss
    }
    
    plot(1:10, wcss, type="b", pch=19, col="blue",
         xlab="Number of Clusters", ylab="WCSS",
         main="Elbow Method for Optimal K")
  })
  
  # Cluster Visualization
  output$clusterPlot <- renderPlot({
    fviz_cluster(kmeans_result(), data = customer_data,
                 geom = "point", ellipse.type = "convex",
                 ggtheme = theme_minimal())
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app.R")