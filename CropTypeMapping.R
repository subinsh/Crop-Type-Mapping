
#installing required packages
install.packages("sf")      
install.packages("leaflet") 
install.packages("caret")
install.packages("randomForest")

#loading libraries
library(sf)      
library(leaflet) 
library(rgdal)
library(raster)
library(terra)
library(dplyr)
library(ggplot2)
library(rgeos)
library(viridis)
library(rasterVis)
library(randomForest)
library(knitr)
library(tidyr)

#setting working directory

setwd("D:\\Geoinformatics\\Scientific Geo-Computing\\ProjectWork")

#loading in-situ data
in_situ_data <- st_read("LULC_Crop-Types_In-SituData2021_USP/LULC_Crop-Types_In-SituData2021_USP.shp")

#removing unwanted columns from in-situ data
in_situ_data <- in_situ_data %>%
  select(-data_Other, -data_Photo, -data_UserN, -data_Phone, -data_Subsc, -data_Email, -data_EndTi, -data_meta_, -F17, -F18)

#filtering data_LULC 'agriculture' row
in_situ_filtered <- in_situ_data %>% filter(data_LULC == 'Agriculture')

head(in_situ_filtered)

# Load the district shapefile
district_shp <- st_read("districts/districts.shp")

# Define the target districts
target_districts <- c("SAPTARI", "SIRAHA", "DHANUSHA","MAHOTTARI", "SARLAHI", "RAUTAHAT","BARA", "PARSA")

# Filter the shapefile to include only the target districts
madhesh_districts <- district_shp[district_shp$DISTRICT %in% target_districts, ]


# Defining a custom color palette for the specific class types
color_palette <- colorFactor(
  palette = c("green", "yellow", "purple", "gold4", "blue"),  # Define colors for each class type
  domain = c("Sugarcane", "PaddyRice", "Orchid","Bamboo", "OtherCrop")  # Define class types
)

# Creating a leaflet map
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(data = madhesh_districts, color = "red", weight = 1, opacity = 1, fillOpacity = 0.2) %>%
  addCircleMarkers(data = in_situ_filtered, 
                   lng = ~st_coordinates(geometry)[, 1], 
                   lat = ~st_coordinates(geometry)[, 2], 
                   popup = ~data_CropT,
                   color = ~color_palette(data_CropT), 
                   radius = 1)%>%
  setView(lng = 85.793, lat = 26.953, zoom = 8.0)

#importing landsat 8 image data
red_band <- raster("LC08_L2SP_140041_20211219_20211223_02_T1_SR_B4.tif")
nir_band <- raster("LC08_L2SP_140041_20211219_20211223_02_T1_SR_B5.tif")

# Calculation NDVI
ndvi_funtion <- function(red_band, nir_band) {
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  return(ndvi)
}

ndvi <- ndvi_funtion(red_band , nir_band)

#plotting NDVI
gplot(ndvi) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  coord_quickmap() +
  ggtitle("NDVI") +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +   					   
  theme(plot.title = element_text(hjust = 0.5),             
        text = element_text(size=15),		       	    
        axis.text.x = element_text(angle = 90, hjust = 1)) 

#creating target coordinate reference system
crs_target <- st_crs("+init=EPSG:32645")

#projecting coordinate systerm
in_situ_filtered_projected <- st_transform(in_situ_filtered, crs_target)

# Extracting NDVI values for in-situ data locations
ndvi_values <- raster::extract(ndvi, in_situ_filtered_projected)

# Adding the extracted NDVI values to the in_situ_filtered_projected data
in_situ_merged <- cbind(in_situ_filtered_projected, NDVI = ndvi_values)

# Remove rows with NA values in the NDVI column
in_situ_merged <- in_situ_merged[!is.na(in_situ_merged$NDVI), ]

# Defining the proportion for the training data (e.g., 70% for training, 30% for testing)
train_prop <- 0.7

# Setting a seed for reproducibility
set.seed(123)

# Creating an index for splitting the data
train_index <- sample(1:nrow(in_situ_merged), size = round(train_prop * nrow(in_situ_merged)))

# Splitting the data into training and testing sets
train_data <- in_situ_merged[train_index, ]
test_data <- in_situ_merged[-train_index, ]

#converting the data_CropT column in train_data dataset into a factor
train_data$data_CropT <- as.factor(train_data$data_CropT)

#removeing rows with missing values (NA) from train_data dataset
train_data <- na.omit(train_data)

#training Random Forest model using the randomForest function
rf_model <- randomForest(data_CropT ~ NDVI, data = train_data, ntree = 500)

# Making predictions on the test data
rf_predictions <- predict(rf_model, test_data)

confusion_matrix <- table(Actual = test_data$data_CropT, Predicted = rf_predictions)
confusion_matrix

# Convert the confusion matrix to a data frame
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))

# Rename the columns for better labels
colnames(confusion_matrix_df) <- c("Predicted", "Actual", "Frequency")

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(confusion_matrix_df, aes(Actual, Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1, size = 4) +
  scale_fill_gradient(low = "pink", high = "red") +  # Define color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted")

# Display the heatmap
print(heatmap_plot)

# Initialize vectors to store precision, recall, and F1 values
precision_values <- numeric(length(class_names))
recall_values <- numeric(length(class_names))
f1_values <- numeric(length(class_names))

# Define class names
class_names <- c("Bamboo", "Orchid", "PaddyRice", "Sugarcane", "OtherCrop")

for (i in 1:length(class_names)) {
  class_name <- class_names[i]
  TP <- confusion_matrix[class_name, class_name]
  FP <- sum(confusion_matrix[, class_name]) - TP
  FN <- sum(confusion_matrix[class_name, ]) - TP
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_value <- 2 * (precision * recall) / (precision + recall)
  
  
  
  # Store values in respective vectors
  precision_values[i] <- precision
  recall_values[i] <- recall
  f1_values[i] <- f1_value
}

# Create a data frame with  results
results_df <- data.frame(
  Class = class_names,
  Precision = round(precision_values, 2),
  Recall = round(recall_values, 2),
  F1_Score = round(f1_values, 2)
)

# Use kable to format the results as a table
kable(results_df, caption = "Precision, Recall, and F1-Score for Each Class")

# Transform the data for a grouped bar chart
results_long <- results_df %>%
  pivot_longer(cols = c(Precision, Recall, F1_Score), names_to = "Metric", values_to = "Value")

# Create a grouped bar chart for Precision, Recall, and F1-Score
grouped_bar <- ggplot(results_long, aes(Class, Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Metrics for Each Class", x = "Class", y = "Value") +
  scale_fill_manual(values = c("Precision" = "blue", "Recall" = "green", "F1_Score" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Display the grouped bar chart
print(grouped_bar)




#initializing matrices to store producer accuracy (PA) and user accuracy (UA)
PA_scores <- matrix(0, nrow = length(class_names), ncol = 1)
UA_scores <- matrix(0, nrow = length(class_names), ncol = 1) 

#calculating producer accuracy and user accuracy for each class
for (i in 1:length(class_names)){
  class_name <- class_names[i]
  PA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[class_name, ])   
  PA_scores[i] <- PA
  UA <- confusion_matrix[class_name, class_name] / sum(confusion_matrix[, class_name])   
  UA_scores[i] <- UA
} 

# Create a data frame for  Producer Accuracy and User Accuracy results
PA_UA_df <- data.frame(
  Class = class_names,
  Producer_Accuracy = round(PA_scores, 2),
  User_Accuracy = round(UA_scores, 2))

# Use kable to format the results as a table
kable(PA_UA_df, caption = "Producer and User Accuracy for Each Class")

# Transform the data for a grouped bar chart for PA and UA
PA_UA_long <- PA_UA_df %>%
  pivot_longer(cols = c(Producer_Accuracy, User_Accuracy), names_to = "Metric", values_to = "Value")

# Create a grouped bar chart for PA and UA
grouped_bar_PA_UA <- ggplot(PA_UA_long, aes(Class, Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Producer and User Accuracy for Each Class", x = "Class", y = "Value") +
  scale_fill_manual(values = c("Producer_Accuracy" = "blue", "User_Accuracy" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Display the grouped bar chart for PA and UA
print(grouped_bar_PA_UA)


