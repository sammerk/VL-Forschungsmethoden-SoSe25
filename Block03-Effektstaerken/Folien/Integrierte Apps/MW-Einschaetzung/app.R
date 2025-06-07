########## First App for the estimation of effect magnitudes ##########

###### LOAD PACKAGES ######
library(dplyr)
library(ggplot2)
library(shiny)
library(shinycssloaders)
library(munsell)
library(bslib)
library(ggdist) # for the dot plots
#library(shinyWidgets) # for the action buttons
library(bayestestR) # for the distribution_normal function
library(effectsize) # for Cohens d calculation
library(ggtext) 


# Define UI for the application 
ui <- page(
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
      card_header("OECD vs. Deutschland"),
      card_body(
        card(markdown("**Aufgabe**: Stellen Sie mit den ±-Buttons die Grafik so ein, dass Sie Ihrem Eindruck nach die Situation der folgenden Schlagzeile bestmöglich wiederspiegelt")),
        uiOutput("headline"),
        card(
          # Center the first plot
          div(class = "d-flex justify-content-center",
              shinycssloaders::withSpinner(
                plotOutput("plot1", height = "400px", 
                           width = "300px"),
                color = "#267326"
              )
          )
          ),
        layout_columns(
          actionButton("smaller_plot1", icon("minus"), label = "Unterschied verkleinern"),
          actionButton("larger_plot1", icon("plus"), label = "Unterschied vergrößern"),
          actionButton("send_results", 
                       icon("paper-plane", lib = "font-awesome"), 
                       label = "Einschätzung Abschicken")
        ),
       # verbatimTextOutput("cohend_renderedtext"),
        
      )
    ),
  ))

server <- function(input, output, session) {
  
  condition <- sample(c("headline_original",
                        "headline_u3",
                        "headline_overlap"), 
                      1)
  
  output$headline <- renderUI({
    if (condition == "headline_original") {
      card(
        tags$figure(
          class = "centerFigure",
          tags$img(
            src = "https://raw.githubusercontent.com/sammerk/VL-Forschungsmethoden-SoSe25/refs/heads/master/Block03-Effektstaerken/Folien/Integrierte%20Apps/MW-Einschaetzung/www/headline_original.jpg",
            style = "max-width: 500px; height: auto;",
            alt = "ARD Headline zur Bildschirmzeit"
          )
        )
      )
    } else {
      if (condition == "headline_u3") {
        card(
          tags$figure(
            class = "centerFigure",
            tags$img(
              src = "https://raw.githubusercontent.com/sammerk/VL-Forschungsmethoden-SoSe25/refs/heads/master/Block03-Effektstaerken/Folien/Integrierte%20Apps/MW-Einschaetzung/www/headline_original_u3.jpg",
              style = "max-width: 500px; height: auto;",
              alt = "ARD Headline zur Bildschirmzeit"
            )
          )
        )
      } else {
        card(
          tags$figure(
            class = "centerFigure",
            tags$img(
              src = "https://raw.githubusercontent.com/sammerk/VL-Forschungsmethoden-SoSe25/refs/heads/master/Block03-Effektstaerken/Folien/Integrierte%20Apps/MW-Einschaetzung/www/headline_original_overlap.jpg",
              style = "max-width: 500px; height: auto;",
              alt = "ARD Headline zur Bildschirmzeit"
            )
          )
        )
      }
      
    }
  })
  
  
  
  
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
              "<span style='color:#236327;'>OECD</span>",
              "<span style='color:#d77d00;'>Deutschland</span>"), 
            each = n_group
        ),
        levels = c(
          "<span style='color:#236327;'>OECD</span>", 
          "<span style='color:#d77d00;'>Deutschland</span>"
        )
      ),
      GroupFill = factor(rep(c("OECD", "Deutschland"), each = n_group))
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
    
    ggplot(data, aes(x = Words,
                     fill = GroupFill, color = GroupFill)) +
      stat_dots( 
        geom = "dots",
        #binwidth = 1/4,
        #dotsize = .8,
        stackratio = 1,
        overflow = "keep", 
        subguide = subguide_integer(position = "left"),
        position = "identity",
        alpha = 0.6
      ) +
     facet_wrap(~ Group, ncol = 1, axes ="all", 
                axis.labels = "all") +
      scale_fill_manual(
        values = c("OECD" = "#267326",
                   "Deutschland" = "#d77d00")
      ) +
      scale_color_manual(
        values = c("OECD" = "#267326",
                   "Deutschland" = "#d77d00")
      ) +
      labs(x = "Bildschirmzeit in Stunden pro Tag",
           caption = "Jeder Punkt stellt eine:n 15-Jährige:n dar",
           y = "Anzahl der Befragten") +
      theme_minimal() +
      
      scale_x_continuous(
        limits = c(0, max(data$Words)),
        breaks = seq(0, max(data$Words), by = 2)
      ) +
      scale_y_continuous(
        breaks   = NULL                  # no left‐axis ticks or labels
        ) +
        theme(
        strip.text = element_markdown(size = 13), # for the panel title font size 
        axis.line.y.right  = element_line(),
        axis.ticks.y.right = element_line(),
        panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#F2F2F2",
                                       color = "#f2f2f2"),
        panel.background = element_rect(fill = "#F2F2F2",
                                        color = "#f2f2f2"),
        plot.margin = margin(0, 0, 0, 0))
    
  }, height = 400 
  )
  
  
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
  
  observeEvent(input$send_results, {
    con <- 
      url(
        paste0("https://survey.ph-karlsruhe.de/test23/?condition=",
               condition,
               "&cohend=",
               cohend(),
               "&PID=",
               url_vars()$PID),
           open = "r")
    readLines(con, warn = FALSE)
     close(con)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
