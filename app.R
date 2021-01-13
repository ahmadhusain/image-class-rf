# prepare environment -----------------------------------------------------

options(repos = BiocManager::repositories())

# clear-up environment
rm(list = ls())

# import libs
library(imager)
library(EBImage)
library(glue)
library(magick)
library(scales)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(caret)
library(randomForest)

# import model
model <- readRDS("models/model-rf.RDS")



convert <- function(x){


    resize_img <- imager::load.image(x) %>%
      EBImage::resize(w = 28, h = 28) %>%
      grayscale()

    temp <- resize_img %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(pixel = paste0("pix",  rownames(.)),
             image = paste0("image", 1)) %>%
      select(image, pixel, value)



  return(temp)

}


ui <- shinyUI(
  # full ui -----------------------------------------------------------------

  # page
  fluidPage(

    # settings
    theme = shinytheme("cerulean"),

    # spacing
    br(),
    br(),

    # row
    fluidRow(

      # column
      column(width = 4, offset = 4, align = "center",



             # file input
             fileInput(inputId = "img",
                       label = "Please upload an image",
                       buttonLabel = "Browse",
                       placeholder = "No image selected",
                       accept = c('image/png', 'image/jpeg'),
                       multiple = FALSE
             ),

             p("Note:
model were not trained to differentiate images other than malaria cells. so, please upload the cell image only."),



             # spacing
             br(),
             br(),

             # image plot
             plotOutput("img_output"),

             # spacing
             br(),

             # prediction
             uiOutput("img_pred_title")




      )

    )

  )
)

server <- # server start ------------------------------------------------------------

function(input, output, session) {

  # global reactive ---------------------------------------------------------

  # image input reactive
  img_input <- reactive({

    # resolve input
    if (is.null(input$img)) {

      results <- "assets/default-img.jpg"

    } else {

      results <- input$img$datapath

    }

    # return the results
    results

  })

  # image prediction
  img_pred <- reactive({

    # prepare image input
    x <- convert(x = img_input())


    data_unseen <- x %>%
      pivot_wider(values_from = value,
                  names_from = pixel) %>%
      select(everything(.)) %>%
      select(-image)

    # make prediction
    results <- predict(model, data_unseen)


    # return the result
    results

  })

  # outputs -----------------------------------------------------------------

  # image output
  output$img_output <- renderPlot({

    image_read(img_input()) %>%
      image_ggplot()

  })

  # prediction output
  output$img_pred_title <- renderUI({

    # get prediction
    img_pred <- img_pred()

    # convert prediction to text
    results <- glue("<h1>Predicted: {img_pred}</h2>")

    # return the results
    HTML(results)

  })

  # server end --------------------------------------------------------------

}

shinyApp(ui, server)
