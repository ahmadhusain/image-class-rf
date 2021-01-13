library(EBImage)
library(tidyverse)
library(randomForest)
library(caret)
library(imager)


# create custom function for convert image to pixel dataframe -------------


convert <- function(x){

  df <- data.frame()

  for (i in 1:nrow(x)) {

    resize_img <- imager::load.image(x[i,]$path) %>%
      EBImage::resize(w = 28, h = 28) %>%
      grayscale()

    temp <- resize_img %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(pixel = paste0("pix",  rownames(.)),
             image = paste0("image", i)) %>%
      select(image, pixel, value)

    df <- bind_rows(df, temp)
  }

  df <- as_tibble(df)



  return(df)

}


# data wrangling ------------------------------------------------------



data_train <- read_csv(file = "data/data-train.csv")

data_test <- read_csv(file = "data/data-test.csv")

train <- convert(data_train) %>%
  pivot_wider(values_from = value, names_from = pixel) %>%
  mutate(label = data_train %>%  pull(class)) %>%
  select(label, everything(.)) %>%
  select(-image)

test <- convert(data_test) %>%
  pivot_wider(values_from = value, names_from = pixel) %>%
  mutate(label = data_test %>%  pull(class)) %>%
  select(label, everything(.)) %>%
  select(-image)

dim(train)


data_train <- train %>%
  mutate(label = as.factor(label))


# k fold cross validation -------------------------------------------------


ctrl <- trainControl(
  ## 2-fold CV
  method = "repeatedcv",
  number = 2
)

# modeling random forest --------------------------------------------------

model_rf <- train(
  label ~ .,
  data = data_train,
  method = 'rf',
  trControl = ctrl
)

saveRDS(model_rf, file = "models/model-rf.RDS")

model_rf <- readRDS("models/model-rf.RDS")

# predict test dataset ----------------------------------------------------


prediction_rf <- predict(model_rf, newdata = test)

table(prediction = prediction_rf, actual = test$label)

confusionMatrix(prediction_rf, as.factor(test$label))

# Acuracy = 77.58%
# Recall = 73.29%
# Specificity = 73.29%
# Precision = 75.49%



# save prediction result into csv -----------------------------------------

data_test %>%
  mutate(`XGBoost Model` = prediction_xgb,
         `Random Forest Model` = prediction_rf) %>%
  write_csv("data/prediction-result.csv")

# visual prediction

plotResults <- function(data, images, preds, actual){

  set.seed(100)

  idx <- sample(x = nrow(data), size = nrow(data))

  temp_preds <- preds[idx]
  temp_data <- data[idx,]
  temp_actual <- actual[idx]

  x <- ceiling(sqrt(length(images)))
  par(mfrow=c(x,x), mar=c(0, 0, 3.5, 0))

  for (i in images){
    temp <- load.image(temp_data[i,]$path) %>%
      EBImage::resize(w = 42, h = 42) %>%
      grayscale()

    predicted_label <- temp_preds[i]
    true_label <- temp_actual[i]
    if (predicted_label == true_label) {
      color <- 'darkgreen'
    } else {
      color <- 'red'
    }

    plot(temp,
         axes = FALSE,
         main = paste0("predicted: ", predicted_label, "\n","actual: ",
                       true_label),
         col.main = color)
  }

}

plotResults(data = data_test,
            images = 1:16,
            preds = prediction_rf,
            actual = data_test$class)


