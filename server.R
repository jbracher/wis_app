library(shiny)

invisible(Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8"))

path_hub <- "../covid19-forecast-hub"

truth <- read.csv(paste0(path_hub, "/data-truth/truth-Cumulative Deaths.csv"),
                  colClasses = c(location = "character", date = "Date"))

source("functions.R")


models <- list.dirs(paste0(path_hub, "/data-processed"), recursive = FALSE, full.names = FALSE)
choices_location0 <- unique(sort(truth$location))
choices_location <- choices_location0[nchar(choices_location0) == 2]
names(choices_location) <- unique(truth$location_name[order(truth$location)])

# Define server logic
shinyServer(function(input, output) {

  output$inp_select_model <- renderUI(
    selectInput("select_model", "Select model:", choices = models, selected = "COVIDhub-ensemble")
  )

  output$inp_select_location <- renderUI(
    selectInput("select_location", "Select location:", choices = choices_location, selected = "US")
  )

  lims <- reactiveValues()

  # limits of two top plots:
  # initializazion:
  observe({
    lims$xlim <- c(as.Date("2020-04-01"), Sys.Date() + 28)
    lims$ylim_forecast <- c(0, 1.1*max(to_plot$forecasts$value))
  })
  # reactive:
  observe({

    if(!is.null(input$dblclick_forecast)){
      lims$xlim <- c(as.Date("2020-04-01"), Sys.Date() + 28)
      lims$ylim_forecast <- c(0, 1.1*max(to_plot$forecasts$value))
    }

    if(!is.null(input$brush_forecast)){
      lims$xlim <- as.Date(c(input$brush_forecast$xmin, input$brush_forecast$xmax), origin = "1970-01-01")
      lims$ylim_forecast <- c(input$brush_forecast$ymin, input$brush_forecast$ymax)
    }
  })

  # initialization:
  observe({
    lims$xlim <- c(as.Date("2020-04-01"), Sys.Date() + 28)
    lims$ylim_scores <- c(0, 1.1*max(c(to_plot$wis$wis, to_plot$ae$ae)))
  })
  # reactive:
  observe({
    if(!is.null(input$dblclick_scores)){
      lims$xlim <- c(as.Date("2020-04-01"), Sys.Date() + 28)
      lims$ylim_scores <- c(0, 1.1*max(c(to_plot$wis$wis, to_plot$ae$ae)))
    }
    if(!is.null(input$brush_scores)){
      lims$xlim <- as.Date(c(input$brush_scores$xmin, input$brush_scores$xmax), origin = "1970-01-01")
      lims$ylim_scores <- c(input$brush_scores$ymin, input$brush_scores$ymax)
    }
  })

  forecasts <- reactiveValues()

  update_forecasts <- observe({
    if(!is.null(input$select_model)){
      forecasts$dir <- paste0(path_hub, "/data-processed/", input$select_model)
      files0 <- list.files(forecasts$dir)
      files0 <- files0[grepl("2020-", files0)]
      dates <- get_date_from_filename(files0)
      relevant_dates <- choose_relevant_dates(dates)
      forecasts$files <- files0[dates %in% relevant_dates]
      print(forecasts$files)

      withProgress(message = 'Loading data...', value = 0, {

        dat <- read_week_ahead(paste0(forecasts$dir, "/", forecasts$files[1]))
        incProgress(1/length(forecasts$files), detail = paste("Loading file", 1))

        for (i in 2:length(forecasts$files)) { #
          dat <- rbind(dat,
                       read_week_ahead(paste0(forecasts$dir, "/", forecasts$files[i])))

          # Increment the progress bar, and update the detail text.
          incProgress(1/length(forecasts$files), detail = paste("Loading file", i))
        }
        forecasts$forecasts <- dat
      })
    }
  })

  update_scores <- observe({
    if(!is.null(forecasts$forecasts)){
      wis <- wis_table(forecasts = forecasts$forecasts, truth = truth)
      forecasts$wis <- wis[!is.na(wis$wis), ]
      ae <- ae_table(forecasts = forecasts$forecasts, truth = truth)
      forecasts$ae <- ae[!is.na(ae$ae), ]
    }
  })

  to_plot <- reactiveValues()

  update_data_to_plot <- observe({
    if(!is.null(forecasts$wis)){
      to_plot$forecasts <- subset(forecasts$forecasts, location == input$select_location & target == input$select_target)
      to_plot$wis <- subset(forecasts$wis, location == input$select_location & target == input$select_target)
      to_plot$ae <- subset(forecasts$ae, location == input$select_location & target == input$select_target)
      to_plot$truth <- subset(truth, location == input$select_location)
    }
  })

  output$plot_forecast <- renderPlot({

    if(!is.null(forecasts$wis)){
      par(mar = c(4, 6, 3, 1), las = 1, cex = 1.2)
      plot_forecast(forecasts = to_plot$forecasts,
                    target = input$select_target,
                    truth = to_plot$truth,
                    xlim = lims$xlim,
                    ylim = lims$ylim_forecast)
      title(main = paste0(input$select_model, ":", input$select_target, ", ", input$select_location))
    }

  })

  output$plot_scores <- renderPlot({
    if(!is.null(forecasts$wis)){
      par(mar = c(4, 6, 3, 1), las = 1, cex = 1.2)

      plot_scores(wis_tab = to_plot$wis,
                  ae_tab = to_plot$ae,
                  target = input$select_target,
                  xlim = lims$xlim,
                  ylim = lims$ylim_scores)
    }
  })

  output$plot_scores_summary <- renderPlot({
    if(!is.null(forecasts$wis)){
      # par(las = 1, cex = 1.5)
      layout(matrix(c(1, 2, 3), ncol = 3))

      pit_hist(wis_tab = to_plot$wis, target = input$select_target)
      coverage_plot(wis_tab = to_plot$wis, target = input$select_target)
      plot_average_scores(wis_tab = to_plot$wis, ae_tab = to_plot$ae, target = input$select_target,
                          ylim = c(0, 1.1*max(c(to_plot$wis$wis, to_plot$ae$ae))))
    }
  })


})
