library(shinythemes)
library(heatmaply)
library(plotly)
library(ggcorrplot)
library(rmarkdown)
library(markdown)
library(shiny)
library(tidyverse)
library(pheatmap)
library(shinyHeatmaply)
library(shinyWidgets)
library(dplyr)
library(cluster)
library(magrittr)
library(ggplot2)
library(data.table)
library(ggbiplot)
library(shinydashboard)
library(tidyr)
library(caret)
library(factoextra)
library(NbClust)
library(fpc)
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("rstudio/d3heatmap")
library(d3heatmap)
set.seed(123)
dataset <- read.csv("credit_card.csv")
data <- dataset
#Data Pre-Processing

# 1. Drop the customer ID column
data <- data[c(-1)]

# 2. Dealing with null values
sum(is.na(data)) # 314 null values
summary(data) # Credit limit = 1, Minimum Payments = 313 

data$MINIMUM_PAYMENTS[is.na(data$MINIMUM_PAYMENTS)]<- 0
data = na.omit(data)


# 3. Scaling the data
data <- scale(data)
data <- as.data.frame(data)


# 4. Dealing with dependent variables using correlation
data_cr = cor(data)
data <- data[c(-4)]


# 5. Dealing with Outliers

library(reshape)
meltData <- melt(data)
outlier_plot <- ggplot(meltData, aes(factor(variable), value, col = factor(variable))) 
outlier_plot + geom_boxplot() + facet_wrap(~variable, scale="free")

final_data <- data

capping_outliers <- function(x){
    outliers <- quantile(x, c(.10, .90))
    x[x<outliers[1] ] <- outliers[1]
    x[x>outliers[2] ] <- outliers[2]
    return(x)
}
for(i in 1:ncol(data)){
    final_data[,i] <- capping_outliers(final_data[,i])
}

melt_finData <- melt(final_data)

# 
# outlier_plot_fin <- ggplot(melt_finData, aes(factor(variable), value,col = factor(variable)))
# dev.off()
# outlier_plot_fin + geom_boxplot() + facet_wrap(~variable, scale="free")
# 
# 
# 
# boxplot(final_data)$out
# boxplot(final_data$BALANCE,
#         col="#ff0066",
#         main="Boxplot for Descriptive Analysis of Balance")


data <- final_data

# PCA transformation

pca = prcomp(data,
             center = TRUE,scale=TRUE)
# ggbiplot(pca, scale = 0, labels=rownames(pca$x), color = TRUE) + geom_point( size = 0.5) + ggtitle("PCA Plot") + theme(plot.title = element_text(hjust = 0.5))

pr.var <- pca$sdev^2
pve <- pr.var / sum(pr.var)
Cols <- function(vec) {
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}


# par(mfrow = c(1, 2))
# plot(pve, xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained", ylim = c(0, 1),
#      type = "b")
# plot(cumsum(pve), xlab = "Principal Component",
#      ylab = "Cumulative Proportion of Variance Explained",
#      ylim = c(0, 1), type = "b")


pr_out <- pca
pca_transform = as.data.frame(-pr_out$x[,1:2])
pca_transform <- sample_n(pca_transform, 800)

dist_matrix_scale <- dist(pca_transform, method = "euclidean")
hclust_df_scale <- hclust(dist_matrix_scale, method = "average")

db <- fpc::dbscan(data, eps = 0.15, MinPts = 5)

ui <- fluidPage(
    theme = shinytheme('flatly'),
    navbarPage(
        'Credit Card Customer Segmentation',
        tabPanel('Home',
                 includeMarkdown("home.rmd")
                 ),
        tabPanel('Input Dataset',
                 sidebarLayout(
                     sidebarPanel(
                         conditionalPanel(
                             'input.dataset === "Original Input Dataset Before Preprocessing"',
                             checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                                                names(dataset), selected = names(dataset)[0:5])
                         ),
                         conditionalPanel(
                             'input.dataset === "Input Dataset After Preprocessing"',
                             checkboxGroupInput("show_vars2", "Columns in diamonds to show:",
                                                names(data), selected = names(data))
                         ),
                     ),
                     mainPanel(
                         tabsetPanel(
                             id = 'dataset',
                             tabPanel("Original Input Dataset Before Preprocessing", DT::dataTableOutput("output_dataset_1"), width = 12),
                             tabPanel("Input Dataset After Preprocessing", DT::dataTableOutput("output_dataset_2"), width = 12)
                         )
                     )
                 )
        ),
        tabPanel('Summary of Input Dataset',
                 tabsetPanel(
                     tabPanel(
                         "Features in Dataset", h3("Column names of before and after processing dataset"),
                         mainPanel(
                             box(DT::dataTableOutput('summary1')),
                             box(DT::dataTableOutput('col1'))
                         )
                     ),
                     tabPanel(
                         "Summary", h3("Summary of Features in dataset"),
                         mainPanel(
                             h4("Summary of Original dataset"), DT::dataTableOutput('summary2'),
                             h4("Summary of Processed dataset"), DT::dataTableOutput('summary2_1')
                         )
                     )
                 )
        ),
        tabPanel('Exploratory Data Analysis',
                 tabsetPanel(
                     tabPanel(
                         "Correlation Plot", h3("Correlation between different features in order to eliminate correlated features"),
                         mainPanel(
                             d3heatmapOutput("heatmap", width = "100%", height= 600)
                         )
                     ),
                     tabPanel(
                         "Histogram Plot", h3("Histogram plot to visualize the distribution of values in every dataset"),
                         mainPanel(
                             selectizeInput(inputId = "barmode",
                                            label = "Features",
                                            choices = names(dataset[2:18]),
                                            selected = "PURCHASES"),
                             plotlyOutput("hist_plot")
                         )
                     ),
                     tabPanel(
                         "Outliers Plot", h3("Box plot of the data before and after preprocessing of the selected feature "),
                         mainPanel(
                             selectizeInput(inputId = "out_input",
                                            label = "Features",
                                            choices = names(data),
                                            selected = "PURCHASES"),
                             plotOutput("out_plot_1" ),
                             plotOutput("out_plot_2")
                         )
                     ),
                     tabPanel(
                         "Clusters", h3("Correlation between different features in order to eliminate correlated features"),
                         mainPanel(
                             fluidRow(
                                 box(h2("Elbow Method"),plotOutput("method1")),
                                 box(h2("Average silhouette method"),plotOutput("method2"))
                                 ),
                             fluidRow(  
                                 box(h2("Gap Statistic method"),plotOutput("method3")),
                                 box(h2("NbClust method"),plotOutput("method4"))
                             )
                         )
                     )
                 )
        ),
        tabPanel('Clustering',
                 tabsetPanel(
                     tabPanel(
                         "K-Means Clustering", 
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("clustnum","Number of clusters",2,10,5)
                             ),
                             
                             mainPanel(h3("Clustering PCA transformed dataset using K-means clustering"),
                                       fluidRow(
                                           plotOutput("clusterchart", width = "100%", height = 600))
                             )
                             #   fluidRow(
                             #     box(h3("Cluster Statistics"),htmlOutput("stat1")),
                             #     box(h3("Dunn Index"),htmlOutput("stat2")))
                         )
                     ),
                     tabPanel(
                         "Hierarchial Clustering",
                         sidebarLayout(
                             sidebarPanel(
                                 sliderInput("clustnum2","Number of clusters",2,10,5)
                             ),
                             mainPanel(h3("Clustering PCA transformed dataset using Hierarchial clustering"),
                                       fluidRow(
                                           plotOutput("clusterchart_2", width = "100%", height = 600),
                                           plotOutput("dendogram_1", width = "100%", height = 600)
                                       )
                             )
                             # fluidRow(
                             #   box(h3("Cluster Statistics"),htmlOutput("stat3")),
                             #   box(h3("Dunn Index"),htmlOutput("stat4")))
                         )
                     ),
                     tabPanel(
                         "DB Scan", 
                         mainPanel(h3("Clustering PCA transformed dataset using DB Scan clustering"),
                                   fluidRow(
                                       plotOutput("clusterchart_3", width = "100%", height = 800)
                                   )
                         )
                     )
                 )
        )
    )
)


server <- function(input, output, session) ({
    output$output_dataset_1 <- DT::renderDataTable({
        DT::datatable(
            dataset[, input$show_vars, drop = FALSE], options = list(scrollX = TRUE,
                                                                     lengthMenu = list(c(5, 15, 50, 100, 200, -1), c('5', '15', '50', '100', '200', 'All')),
                                                                     pageLength = 10
            )
        )
    })
    output$output_dataset_2 <- DT::renderDataTable({
        DT::datatable(
            data[, input$show_vars2, drop = FALSE], options = list(scrollX = TRUE,
                                                                   lengthMenu = list(c(5, 15, 50, 100, 200, -1), c('5', '15', '50', '100', '200', 'All')),
                                                                   pageLength = 10
            )
        )
    })
    output$heatmap <- renderD3heatmap({d3heatmap(cor(data), main = "Correlation Heat Map")})
    
    output$summary1 <- DT::renderDataTable(DT::datatable(as.data.frame(names(dataset)),
                                                         options = list(scrollX = TRUE, paging = FALSE, searching = FALSE)))
    
    output$col1 <- DT::renderDataTable(DT::datatable(as.data.frame(names(data)),
                                                     options = list(scrollX = TRUE, paging = FALSE, searching = FALSE)))
    output$summary2 <- DT::renderDataTable(DT::datatable(
        as.data.frame(do.call(cbind, lapply(dataset, summary))),options = list(scrollX = TRUE, paging = FALSE, searching = FALSE)))
    output$summary2_1 <- DT::renderDataTable(DT::datatable(
        as.data.frame(do.call(cbind, lapply(data, summary))),options = list(scrollX = TRUE, paging = FALSE, searching = FALSE)))
    
    output$summary3 <- DT::renderDataTable(DT::datatable(as.data.frame(str(dataset))))
    
    output$clusterchart <- renderPlot({
        fviz_cluster((eclust(pca_transform, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE)), geom = "point", ellipse.type = "norm",
                     palette = "jco", ggtheme = theme_minimal())
    })
    # output$stat1 <- renderPrint({
    #   cluster.stats(dist(pca_transform),(eclust(pca_transform, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster)
    # })
    # output$stat2 <- renderPrint({
    #   (cluster.stats(dist(pca_transform),(eclust(pca_transform, "kmeans", k = input$clustnum, nstart = 25, graph = FALSE))$cluster))$dunn
    # })
    output$dendogram_1 <- renderPlot({
        plot(hclust_df_scale,
             main = "Dendrogram",
             xlab = "States",
             ylab = "Euclidean Distances")
    })
    
    output$clusterchart_2 <- renderPlot({
        fviz_cluster(list(data = pca_transform, cluster = cutree(hclust_df_scale, k= input$clustnum2)), geom = "point", ellipse.type = "norm",
                     palette = "jco", ggtheme = theme_minimal())
    })
    
    output$clusterchart_3 <- renderPlot({
        plot(pca_transform,col = db$cluster, main = "DBScan")
    })
    
    output$hist_plot <- renderPlotly({
        fig <- plot_ly(dataset, x= ~get(input$barmode), type = "histogram") %>%
            layout(title = 'Histogram', plot_bgcolor = "#e5ecf6", xaxis = list(title = input$barmode), 
                   yaxis = list(title = 'Values')) 
    })
    output$out_plot_1 <- renderPlot({
        outlier_plot <- ggplot(melt(dataset[,input$out_input, drop = FALSE]), aes(factor(variable), value, col = factor(variable))) 
        outlier_plot + geom_boxplot() + xlab(input$out_input) + ylab("Values") + ggtitle("Outliers box plot before data preprocessing") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    output$out_plot_2 <- renderPlot({
        outlier_plot <- ggplot(melt(data[,input$out_input, drop = FALSE]), aes(factor(variable), value, col = factor(variable))) 
        outlier_plot + geom_boxplot()  + xlab(input$out_input) + ylab("Values") + ggtitle("Outliers box plot after data preprocessing") +
            theme(plot.title = element_text(hjust = 0.5))
    })
    output$method1 <- renderPlot({
        fviz_nbclust(pca_transform, kmeans, method = "wss") +
            geom_vline(xintercept = 4, linetype = 2)+
            labs(subtitle = "Elbow method")
    })
    output$method2 <- renderPlot({
        fviz_nbclust(pca_transform, kmeans, method = "silhouette") +
            labs(subtitle = "Silhouette Method")
    })
    output$method3 <- renderPlot({
        set.seed(123)
        fviz_nbclust(pca_transform, kmeans, nstart = 25,  method = "gap_stat", nboot = 10, verbose = FALSE)+
            labs(subtitle = "Gap statistic method")
    })
    output$method4 <- renderPlot({
        NbClust(pca_transform,distance = "euclidean",
                min.nc = 4, max.nc = 10, method = "kmeans", index = "all")
    })
})

# running the application
shinyApp(ui, server)