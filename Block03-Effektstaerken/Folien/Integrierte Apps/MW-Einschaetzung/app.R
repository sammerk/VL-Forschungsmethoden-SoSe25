########## First App for the estimation of effect magnitudes ##########

###### LOAD PACKAGES ######
library(dplyr)
library(shiny)
library(shinycssloaders)
library(bslib)
library(ggdist) # for the dot plots
#library(shinyWidgets) # for the action buttons
library(bayestestR) # for the distribution_normal function
library(effectsize) # for Cohens d calculation
library(ggtext) 


# Define UI for the application 
ui <- page_fillable(
  theme = bs_theme(
    base_font = "Open Sans",
    heading_font = "Open Sans",
    fg = "#267326",
    bg = "#F2F2F2",
    bgdark = "#F2F2F2",
    primary = "#267326"),
  
  # Layout columns for the two plots
  layout_columns(
    card(
      card_header("OECD Durchschnitt vs. Deutschland"),
      card_body(
        card(
          # Center the first plot
          div(class = "d-flex justify-content-center",
              shinycssloaders::withSpinner(
                plotOutput("plot1", 
                           width = "500px",
                           height = "500px"),
                color = "#267326"
              )
          )
          ),
        layout_columns(
          actionButton("smaller_plot1", icon("minus"), label = "Unterschied verkleinern"),
          actionButton("larger_plot1", icon("plus"), label = "Unterschied vergrößern")
        ),
        verbatimTextOutput("cohend_renderedtext"),
        actionButton("send_results", 
                     icon("paper-plane", lib = "font-awesome"), 
                     label = "Einschätzung Abschicken")
      )
    ),
  ))

server <- function(input, output, session) {
  
  shift_per_click <- .2 # for randomization of the shift + cohens d increase
  
  net_shift_clicks <- reactiveVal(0)  # set shift = 0
  
  observeEvent(input$larger_plot1, {
    net_shift_clicks(net_shift_clicks() + 1) # each clicks adds 1 to the shift
  })
  
  observeEvent(input$smaller_plot1, {
    new_val <- net_shift_clicks() - 1 # each click decreases the shift by 1
    if (new_val >= 0) {               # if the shift is 0 or negative, set it to 0
      net_shift_clicks(new_val)
    }
  })
  
  dist_data <- reactive({
    n_group <- 500
    Regular_group <- distribution_normal(n_group, 50/7, 2.5)
    effective_shift <- shift_per_click * net_shift_clicks()
    Advance_organizers_group <- distribution_normal(n_group, 50/7, 2.5) + effective_shift 
    
    # Build one data frame for plotting
    df <- data.frame(
      Words = c(Regular_group, Advance_organizers_group),
      Group = factor(
        rep(c
            ( 
              "<span style='color:#236327;'>OECD Durchschnitt</span>",
              "<span style='color:#d77d00;'>Deutschland</span>"), 
            each = n_group
        ),
        levels = c(
          "<span style='color:#236327;'>OECD Durchschnitt</span>", 
          "<span style='color:#d77d00;'>Deutschland</span>"
        )
      ),
      GroupFill = factor(rep(c("OECD Durchschnitt", "Deutschland"), each = n_group))
    )
    
    # Return a *list* so we can easily access separate vectors if needed
    list(
      df = df,
      Regular = Regular_group,
      Intervention = Advance_organizers_group
    )
  })
  
  # 2) Plot from that reactive data
  output$plot1 <- renderPlot({
    # Grab the data frame
    data <- dist_data()$df
    
    ggplot(data, aes(x = Words, fill = GroupFill, color = GroupFill)) +
      stat_dots( 
        geom = "dots",
        binwidth = 1/4,
        dotsize = .8,
        stackratio = 1,
        overflow = "keep", 
        subguide = subguide_count(label_side = "left",
                                  breaks = scales::breaks_width(4)),
        position = "identity",
        alpha = 0.6
      ) +
      facet_wrap(~ Group, ncol = 1, axes ="all", axis.labels = "all_x") +
      scale_fill_manual(
        values = c("OECD Durchschnitt" = "#267326",
                   "Deutschland" = "#d77d00")
      ) +
      scale_color_manual(
        values = c("OECD Durchschnitt" = "#267326",
                   "Deutschland" = "#d77d00")
      ) +
      labs(x = "Bildschirmzeit in Stunden pro Tag", y = "Anzahl Kinder",
           caption = "Jeder Punkt stellt eine:n 15-Jährige:n dar") +
      theme_minimal() +
      theme(strip.text.x = element_markdown(size = 16)) +
      scale_x_continuous(
        limits = c(0, max(data$Words)),
        breaks = seq(0, max(data$Words), by = 2)
      ) +
      theme(
        strip.text = element_text(size = 16, margin = margin(t = 30)), # for the panel title font size 
        panel.spacing = unit(1.2, "lines"), # adds spacing between the x-axis and the title of the lower plot
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(color = "#11111100"),
        axis.title.x = element_text(margin = margin(t = 20)), # adds margin between the plot and the y-axis title
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.ticks.y = element_blank(), # removes the "initial" y-axis ticks
        panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#F2F2F2",
                                       color = "#f2f2f2"),
        panel.background = element_rect(fill = "#F2F2F2",
                                        color = "#f2f2f2"))
  })
  
  
  # 3) Compute Cohen's d from the same data
  cohend <- reactive({
    # Extract the same exact values used by the plot
    regular <- dist_data()$Regular
    intervention <- dist_data()$Intervention
    
    # Now you get the same random draws
    result <- cohens_d(regular, intervention, paired = FALSE)
    result$Cohens_d
  })
  
  output$cohend_renderedtext <- renderPrint({cohend()})
  
  ## URL Variable fetching #####################################################
  url_vars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  observeEvent(input$send_results, {
    showModal(modalDialog(
      title = "Vielen Dank!",
      "Ganz herzlichen Dank für das Absenden Ihrer Einschätzung",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ## Usage Logging #############################################################
 # observeEvent(cohend(), {
 #   sheet_append("1j-Dh0VrNSKBVenbMllVr6EASX3O9_DX_op0s95VXFpw",
 #                tibble(PROLIFIC_PID = ifelse(is.null(url_vars()$PROLIFIC_PID), 
 #                                             "code is missing", #to keep ncol constant
 #                                             url_vars()$PROLIFIC_PID), # Person identifier from URL
 #                       task_name = "ES_estimation",
 #                       task_version = "main",
 #                       cohend = cohend(),
 #                       time = Sys.time(),
 #                       timezone = Sys.timezone(),
 #                       shift_per_click = shift_per_click,
 #                ),
 #                sheet = 2)
 #   
 #   
 # })
}


# Run the application 
shinyApp(ui = ui, server = server)
