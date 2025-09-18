# Load required libraries
library(dplyr)
library(cluster)
library(factoextra)
library(MASS)
library(ggplot2)
library(GGally)
library(pheatmap)
library(psych)
library(robustbase)
library(caret)
library(Hotelling)             
library(car)
library(MVN)
library(biotools)
library(reshape2)
library(ggcorrplot)
library(tidyr)
library(psych)
library(wbacon)
library(corrplot)
library(pheatmap)

dff <- read.csv("space_missions_dataset.csv")
dftrain_space<- read.csv("df_train_space.csv")


# Inspect the data
str(dff)
summary(dff)

# Check for missing values
col_na_count <- colSums(is.na(dff))
na_count <- sum(is.na(dff))


# Define quantitative columns
quantitative_cols <- c(
  "Distance.from.Earth..light.years.",
  "Mission.Duration..years.",
  "Mission.Cost..billion.USD.",
  "Scientific.Yield..points.",
  "Crew.Size",
  "Mission.Success....",
  "Fuel.Consumption..tons.",
  "Payload.Weight..tons."
)

# Extract quantitative data
quantitative_data <- dff[, quantitative_cols]

# Define qualitative variables
qualitative_vars <- c("Target.Type", "Mission.Type","Launch.Vehicle")
str(quantitative_data)
dff[qualitative_vars] <- lapply(dff[qualitative_vars], as.factor)
qualitative_vars <- dff[, qualitative_vars]
##check the outliers using box plot_uni
data_long <- quantitative_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots for All Variables", x = "Variable", y = "Value")+theme_minimal()

outliers <- wBACON(dff[, quantitative_cols]) #multi


normality<-lapply(quantitative_data,shapiro.test) 
result <- mvn(quantitative_data, 
              subset = NULL, 
              mvnTest = "royston", 
              univariatePlot = "qq", 
              multivariatePlot = "qq", 
              multivariateOutlierMethod = "quan", 
              showOutliers = TRUE, 
              showNewData = TRUE)

melted_dataa<- melt(quantitative_data) # it convert the columns into one column and one another column for their values
custom_colors <- c("#001A6E", "#074799", "#009990", "#FFE31A","#C62300","#131010","#2A004E","#C6E7FF")
names(custom_colors) <- unique(melted_dataa$L1)
ggplot(melted_dataa, aes(x = value, color = variable , fill = variable )) +
  geom_density(alpha = 0.3) + # Adjust transparency
  labs(title = "Distributions of Numerical Variables",
       x = "Value",
       y = "Density") +scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()



ggplot(melted_dataa, aes(x = value, color = variable, fill = variable)) +
  geom_density(alpha = 0.5, size = 1) + # Enhance transparency and line thickness
  labs(
    title = "Distributions of Numerical Variables",
    subtitle = "Log-Scaled Comparison of Density",
    x = "Log(Value)",
    y = "Density"
  ) +
  scale_x_log10(labels = scales::comma) + # Apply log transformation
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

correlation_matrix<-cor(quantitative_data, method = "spearman")# Spearman correlation as the data is not normally distributed
ggcorrplot(correlation_matrix, hc.order = TRUE, 
           type = "lower", lab = TRUE, lab_size = 3, colors = c("#B03052", "white", "#16423C")) 

cor_matrix <- cor(quantitative_data, use = "complete.obs")

ggpairs(quantitative_data)
    #visualization of categorical var
# List of categorical columns
categorical_columns <- c("Target.Type", "Target.Name", "Mission.Type", "Launch.Vehicle")

# Function to create bar plots for each categorical variable
plot_categorical <- function(data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_bar(fill = "#C8A8E9", color = "black") +
    theme_minimal() +
    labs(
      title = paste("Distribution of", column),
      x = column,
      y = "Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
# Generate and save plots for all categorical variables
for (col in categorical_columns) {
  plot <- plot_categorical(dff, col)
  print(plot) # Display the plot in RStudio or R console
}

# pie chart
#mission type 
mis.type<- table(dff$Mission.Type)

pct <- round(mis.type/sum(mis.type)*100)
lbls <- paste0(levels(dff$Mission.Type), "",pct , "%")
pie(mis.type, main = "Pie chart of mission type"
    , col = c("#FFD167","#06d7a0", "#108AB1", "#073A4B"), border = "white", labels = lbls )  
#launch 
launch<- table(dff$Launch.Vehicle)

pct <- round(launch/sum(launch)*100)
lbls <- paste0(levels(dff$Launch.Vehicle), "",pct , "%")
pie(launch, main = "Pie chart of launch vehicle"
    , col = c("#F6BCBA","#108AB1", "#C8A8E9", "#213A58"), border = "white", labels = lbls )  

ggplot(dff, aes(x =dff$Mission.Type , fill = dff$Launch.Vehicle)) +
  geom_bar() +
  scale_fill_brewer(palette = "BuPu") + labs(
    title = ("Bar plot for Mission Type VS launch vehicl"),
    x = "Mission Type",
    y = "launch vehicle Count")

ggplot(dff, aes(x = dff$Launch.Vehicle, fill = dff$Target.Type)) +
  geom_bar() +
  scale_fill_brewer(palette = "Purples") + labs(
    title = ("Bar plot for Target Type VS launch vehicl"),
    x ="launch vehicle Count" ,
    y = "target Type")

ggplot(dff, aes(x = Launch.Vehicle, fill = Target.Type)) +
  geom_bar() +
  facet_wrap(~ Mission.Type) +
  theme_minimal() +
  labs(title = "Launch.Vehicle by Target.Type including Mission.Type", x = "Launch.Vehicle", y = "Target.Type")

          #numeric vs cate
# Run Kruskal-Wallis tests and summarize results
for (numeric in names(quantitative_data)) {
  for (cate in names(qualitative_vars)) {
    if (numeric %in% names(dff) && cate %in% names(dff)) {
      kruskal_result <- kruskal.test(dff[[numeric]] ~ dff[[cate]])
      
      # Highlight significant results (p-value < 0.05)
      if (kruskal_result$p.value < 0.05) {
        cat("\n*** Significant Result ***\n")
        cat("Kruskal-Wallis Test for", numeric, "and", cate, "\n")
        print(kruskal_result)
      } else {
        cat("\nNo significant difference for", numeric, "and", cate, " (p =", kruskal_result$p.value, ")\n")
      }
    }
  }
}

ggplot(dff, aes(x = Mission.Type, y = Mission.Cost..billion.USD.)) +
  geom_boxplot(color = "#E5E3D4", fill = "#001A6E") +
  theme_minimal() +
  labs(title = "Mission Cost by Mission Type", 
       x = "Mission Type", 
       y = "Mission Cost (Billion USD)") +
  theme(legend.position = "none") 


ggplot(dff, aes(x = Target.Name, y =Crew.Size)) +
  geom_boxplot(color = "#E5E3D4", fill = "#001A6E") +
  theme_minimal() +
  labs(title = "Crew.Size by Target.Name", 
       x = "Target.Name", 
       y = "Crew.Size") +
  theme(legend.position = "none")

ggplot(dff, aes(x = Mission.Success...., 
                y = Mission.Cost..billion.USD., 
                color=Mission.Type)) +
  geom_point() +  scale_color_manual(values = c("#074799", "#009990", "#88C273", "#AE445A")) +  # Manually set the colors for each type
  
  
  labs(title = "Mission.Success  and  Mission.Cost by Mission.Type")

ggplot(dff, aes(x = Mission.Duration..years., 
                y = Crew.Size,
                color = Target.Type)) +
  geom_point() +
  scale_color_manual(
    values = c("#074799", "#009990", "#FFE31A", "#000B58","#6A669D")) +
  labs(
    title = "Mission.Duration..years and Crew.Size  by Target.Type",
    x = "Mission.Duration..years.",
    y = "Crew.Size",
    color = "Target.Type"
  ) +
  theme_minimal()

ggplot(dff, aes(x = Distance.from.Earth..light.years., 
                y = Mission.Duration..years.,
                color = Target.Type)) +
  geom_point() +
  scale_color_manual(
    values = c("#074799", "#009990", "#FFE31A", "#000B58","#6A669D")) +
  labs(
    title = "Distance.from.Earth..light.years. and Distance.from.Earth..light.years.  by Target.Type",
    x = "Mission.Duration..years.",
    y = "Distance.from.Earth..light.years.",
    color = "Target.Type"
  ) +
  theme_minimal()


ggplot(dff, aes(x = Fuel.Consumption..tons., y = Mission.Type, fill = Mission.Type)) +
  geom_violin() +  # Create the violin plot
  scale_fill_manual(values = c("#074799", "#009990", "#FFE31A", "#D4EBF8")) +  # Set the custom colors
  theme_minimal() +
  labs(title = "Mission Type by Fuel Consumption (Tons)", 
       x = "Fuel Consumption (Tons)", 
       y = "Mission Type")


ggplot(dff, aes(x = Scientific.Yield..points., y = Mission.Type, fill = Mission.Type)) +
  geom_violin() +  # Create the violin plot
  scale_fill_manual(values = c("#074799", "#009990", "#FFE31A", "#D4EBF8")) +  # Set the custom colors
  theme_minimal() +
  labs(title = "Mission Type by Scientific.Yield..points.", 
       x = "Scientific.Yield..points.", 
       y = "Mission Type")




####################     Research questions    ###########################
#################################################################
                           #1
# Scale the data to ensure equal weighting of variables
scaled_features <- scale(dff[,c("Crew.Size","Fuel.Consumption..tons.","Distance.from.Earth..light.years.","Mission.Duration..years.")])
fviz_nbclust(scaled_features, kmeans, method = "wss")
fviz_nbclust(scaled_features, kmeans, method = "wss") + 
  theme_minimal() +
  ggtitle("Elbow Method for Optimal Number of Clusters")
dist_matrix <- dist(scaled_features)  # Calculate the distance matrix
hc <- hclust(dist_matrix, method = "ward.D")  # Hierarchical clustering using complete linkage
plot(hc, main = "Dendrogram for Hierarchical Clustering", xlab = "", sub = "", cex = 0.9)

kmeans_result <- kmeans(scaled_features, centers = 2, nstart = 1000)
kmeans_result2 <- kmeans(scaled_features, centers = 3, nstart = 1000)
silhouette_2 <- silhouette(kmeans(scaled_features, centers = 2, nstart = 1000)$cluster, dist(scaled_features))
silhouette_3 <- silhouette(kmeans(scaled_features, centers = 3, nstart = 1000)$cluster, dist(scaled_features))

mean(silhouette_2[,3])  # Average silhouette score for 2 clusters
mean(silhouette_3[,3])  # Average silhouette score for 3 clusters

# Explained Variance
kmeans_2 <- kmeans(scaled_features, centers = 2, nstart = 1000)
kmeans_3 <- kmeans(scaled_features, centers = 4, nstart = 1000)

explained_2 <- kmeans_2$betweenss / kmeans_2$totss
explained_3 <- kmeans_3$betweenss / kmeans_3$totss
explained_2  # Variance explained by 2 clusters
explained_3  # Variance explained by 3 clusters


# Compare cluster means
kmeans_2$centers
kmeans_3$centers
# PCA for visualization
pca_result <- prcomp(scaled_features, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_result$x[,1:3], Cluster = as.factor(kmeans_result$cluster))
ggplot(pca_data, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2) +
  ggtitle("Clusters Visualization (2D PCA)") +
  theme_minimal()
############################################
                            #2
# Define quantitative columns
my_cols <- c(
  "Distance.from.Earth..light.years.",
  "Mission.Duration..years.",
  "Mission.Cost..billion.USD.",
  "Crew.Size",
  "Fuel.Consumption..tons.",
  "Payload.Weight..tons."
)
categorical_columns<-c ( "Target.Type","Target.Name","Mission.Type","Launch.Vehicle")
# Extract quantitative data
my_data <- dff[, my_cols]
cat_data <- dff[,categorical_columns]



#---------------------------------------------------------------
data<- scale(my_data)
d <-dist(data,method = "euclidean")
fit <- hclust(d,method = "ward.D")

# dendrogram
plot(fit)
rect.hclust(fit, k=2 ,border = "red")
plot(fit)
rect.hclust(fit, k=3 ,border = "red")
plot(fit)
rect.hclust(fit, k=4 ,border = "red")
plot(fit)
rect.hclust(fit, k=5 ,border = "red")
plot(fit)
rect.hclust(fit, k=6 ,border = "red")


# elbow for k-means
fviz_nbclust(data, kmeans, method = "wss")

#means 
result.kmeans<-kmeans(data,centers = 3 ,nstart = 25)
result.kmeans
result.kmeans1<-kmeans(data,centers = 2 ,nstart = 25)
result.kmeans1

#plot
data_with_clusters <- data.frame(my_data, Cluster = as.factor(result.kmeans$cluster))
ggplot(data_with_clusters, aes(x = my_data$Fuel.Consumption..tons., y = my_data$Mission.Cost..billion.USD., color = Cluster)) +
  geom_point(size = 2) + 
  theme_minimal()+
  labs(title = "Cluster Visualization",
       x = "cost", 
       y = "fuel consumption ") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
#------------------------------------------------------------------------------------------------------------------------------------
                                #3
data_numeric<- dftrain_space[,-c(1,2,3,4,5,6,15)]
#-----
#check if the data can apply the factor or not 
KMO(data_numeric)

#step 1 ,determine m factor 
scree(data_numeric)

#step 2 , extract the factors 
fa<-fa(data_numeric,nfactors = 2,fm="pa",rotate = "none")
#eigenvalues 
fa$values
#factor loading
fa$loadings
#factor loading plot
corrplot(fa$loadings)



