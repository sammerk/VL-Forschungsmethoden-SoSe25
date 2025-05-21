library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(munsell)
library(thematic)
library(bslib)
library(shinyWidgets)
library(bayestestR)
library(viridis)


ui <-  page_sidebar(
  theme = bs_theme(
    base_font = "Open Sans",
    heading_font = "Open Sans",
    fg = "#267326",
    bg = "#F2F2F2",
    primary = "#174717"),
  title = "App: Überlappung vs. Mittelwertsdifferenz",
  sidebar = sidebar(
    sliderInput("sample_n", "Größe der Stichprobe", 1, 800, 164),
      sliderTextInput(
        inputId = "form",
        label = "Verteilungsform", 
        choices = c("1 = umgekehrt u-förmig", 
                    2:9, 
                    "10 = u-förmig"),
        selected = "4"
      ),
      sliderTextInput(
        inputId = "overlap",
        label = "Überlappung", 
        choices = c("1 = (fast) keine", 
                    2:9, 
                    "10 = (fast) völlständige"),
        selected = "6"
        
      ),
      numericInput("mean1", "Mittelwert Gruppe 1", 508),
      numericInput("mean2", "Mittelwert Gruppe 2", 512), 
      selectInput(
        "plottype",
        "Art der Visualisierung",
        c("Histogramm" = "Histogramm",
          "Dotplot" = "Dotplot",
          "Densityplot" = "Densityplot",
          "Violinplot" = "Violinplot",
          "Sinaplot" = "Sinaplot",
          "Jitterplot" = "Jitterplot",
          "Boxplot" = "Boxplot",
          "Errorbarplot" = "Errorbarplot"))
      ),
  
 
  card(
      uiOutput("plotUI")
  )
)


server <- function(input, output, session) {
  
  shape1_shape2 <- reactive({
    case_when(input$form == "1 = umgekehrt u-förmig" ~ 20,
              input$form == "2" ~ 16,
              input$form == "3" ~ 12,
              input$form == "4" ~ 8,
              input$form == "5" ~ 4,
              input$form == "6" ~ 2,
              input$form == "7" ~ 1,
              input$form == "8" ~ .7,
              input$form == "9" ~ .4,
              input$form == "10 = u-förmig" ~ .2)
  })
  
  range_diff <- reactive({
    case_when(input$overlap == "1 = (fast) keine" ~ 2,
              input$overlap == "2" ~ 1.5,
              input$overlap == "3" ~ 1,
              input$overlap == "4" ~ .8,
              input$overlap == "5" ~ .6,
              input$overlap == "6" ~ .5,
              input$overlap == "7" ~ .4,
              input$overlap == "8" ~ .3,
              input$overlap == "9" ~ .2,
              input$overlap == "10 = (fast) völlständige" ~ .1)
  })

  mean_diff <- reactive({
    abs(input$mean1 - input$mean2)
  })
  
  # adapt data to input
  data1 <- reactive({
    tibble(G1 = distribution_beta(input$sample_n,
                                   shape1_shape2(), 
                                   shape1_shape2()),
           G2 = distribution_beta(input$sample_n,
                                   shape1_shape2(), 
                                   shape1_shape2()))
  })
  
  range_emp <- reactive({max(data1()$G1) - min(data1()$G1)})
  
  data <- reactive({
    tibble(
      G1 = (data1()$G1 - .5) / (max(data1()$G1) - .5) *  # centering and range =1
            mean_diff() * 1/range_diff() + 0.5 + input$mean1,
      G2 = (data1()$G2 - .5) / (max(data1()$G1) - .5) *  # centering and range =1
            mean_diff() * 1/range_diff() + 0.5 + input$mean2
    ) %>% 
      gather(Gruppe, Ausprägung)
  })
  
  
  output$plot <- renderPlot({
    
    
    if (input$plottype == "Dotplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           fill = Gruppe)) +
        labs(title = "Dotplot") +
        geom_dotplot(
          method = "histodot",
          color = "#ffffff00") +
        theme_minimal(base_size = 18) +
        scale_fill_manual(values = c("#26732690", "#d77d0090")) +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    if (input$plottype == "Histogramm")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           color = Gruppe, 
                           fill = Gruppe)) + 
        geom_histogram(color = "#26732600",
                       position="identity") +
        theme_minimal(base_size = 18) +
        labs(title = "Histogramm") +
        theme_minimal(base_size = 18) +
        scale_fill_manual(values = c("#26732690", "#d77d0090")) +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    if (input$plottype == "Boxplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_boxplot(color = "#267326",
                     fill = "#26732620") + 
        theme_minimal(base_size = 18) +
        labs(title = "Boxplot") +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
  
    if (input$plottype == "Jitterplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_jitter(color = "#267326") + 
        theme_minimal(base_size = 18) +
        labs(title = "Jitterplot")  +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    if (input$plottype == "Densityplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, 
                           color = Gruppe, 
                           fill = Gruppe)) + 
        labs(title = "Densityplot") +
        theme_minimal(base_size = 18) +
        scale_fill_manual(values = c("#26732650", "#d77d0050")) +
        scale_color_manual(values = c("#267326", "#d77d00")) +
        geom_density()  +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
      
    
    if (input$plottype == "Violinplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        geom_violin(color = "#267326",
                     fill = "#26732620") + 
        theme_minimal(base_size = 18) +
        labs(title = "Violinplot") +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    if (input$plottype == "Sinaplot")
      plot <-
        ggplot(data(), 
               aes(x = Ausprägung, y = Gruppe)) + 
        ggforce::geom_sina(color = "#267326",
                    fill = "#26732620") + 
        theme_minimal(base_size = 18) +
        labs(title = "Violinplot") +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    if (input$plottype == "Errorbarplot")
      plot <-
        ggplot(data(), aes(x = Ausprägung, y = Gruppe)) + 
        stat_summary(fun.data = mean_sdl, 
                     geom = "linerange", 
                     fun.args = list(mult = 1), 
                     width = 1.3,
                     color = "#267326") + 
        stat_summary(fun.y = mean, 
                     geom = "point",
                     color = "#267326",
                     size = 3) + 
        theme_minimal(base_size = 18) +
        labs(title = "Errorbarplot") +
        theme(plot.background = element_rect(fill = "#F2F2F2",
                                             color = "#f2f2f2"),
              panel.background = element_rect(fill = "#F2F2F2",
                                              color = "#f2f2f2"))
    
    return(plot)
  })
  
  output$plotUI <- renderUI({
    plotOutput("plot", 
               height = case_when(input$plottype == "Dotplot" ~ "400px",
                                  input$plottype == "Histogramm" ~ "400px",
                                  input$plottype == "Boxplot" ~ "200px",
                                  input$plottype == "Jitterplot" ~ "300px",
                                  input$plottype == "Densityplot" ~ "400px",
                                  input$plottype == "Violinplot" ~ "330px",
                                  input$plottype == "Sinaplot" ~ "330px",
                                  input$plottype == "Errorbarplot" ~ "200px")
    )
  })
}

shinyApp(ui = ui, server = server)    