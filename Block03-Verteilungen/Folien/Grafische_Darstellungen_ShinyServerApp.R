library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(bslib)



ui <- fluidPage(
  theme = bs_theme(
    #    heading_font = font_google("Source Sans Pro"),
    #    base_font = font_google("Source Sans Pro"),
    preset = "shiny",
    #   bg = "#ffffff",
    #   fg = "#000",
    primary = "#8cd000"
  ),
  
  titlePanel("Verteilungen visualisieren"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("sample_n", "Größe der Stichprobe", 1, 800, 164),
      selectInput("Eigenschaft", "Streuung, Schiefe & Modalität:",
                  choices = c("große Streuung" = "große Streuung",
                              "kleine Streuung" = "kleine Streuung",
                              "Bimodalität" = "Bimodalität",
                              "Rechtsschiefe" = "Rechtsschiefe")),
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
    
    
    mainPanel(
      uiOutput("plotUI")
    )
  )
)


server <- function(input, output, session) {
  # bs_themer()
  # adapt data to input
  data_subsetted <- reactive({
    tibble(`kleine Streuung` = rbeta(input$sample_n, 30, 10)*15,
           `große Streuung` = rbeta(input$sample_n, 9, 3)*15,
           `Bimodalität` = c(rbeta(floor(input$sample_n/2), 38, 15)*15,
                             rbeta(ceil(input$sample_n/2), 28, 35)*15),
           `Rechtsschiefe` = rbeta(input$sample_n, 2, .5)*15) %>%
      gather(Variable, Notenpunkte)%>%
      mutate(dummy = 1) %>%
      filter(Variable == input$Eigenschaft) %>%
      sample_n(input$sample_n) %>%
      mutate(Note = (Notenpunkte - 7.5) * (-6 / 15) + 4)
  })
  
  # adapt plot title to input
  subtitle_reactive <- reactive({
    if(input$Eigenschaft == "große Streuung")
      sr <- "Verteilung mit vglw. großer Streuung"
    if(input$Eigenschaft == "kleine Streuung")
      sr <- "Verteilung mit vglw. kleiner Streuung"
    if(input$Eigenschaft == "Bimodalität")
      sr <- "Bimodale Verteilung"
    if(input$Eigenschaft == "Rechtsschiefe")
      sr <- "Rechtsschiefe Verteilung"
    return(sr)
  })
  
  output$plot <- renderPlot({
    
    
    if (input$plottype == "Dotplot")
      plot <-
        ggplot(data_subsetted(), aes(x = Note)) +
        geom_dotplot(
          method = "histodot",
          binwidth = .1,
          right = F, # intervalls cloed to the left
          #dotsize = .5,
          fill = "#8cd000",
          color = "#8cd000"
        ) +
        theme_minimal(base_size = 18) +
        labs(
          title = "Dotplot",
          subtitle = paste(subtitle_reactive()),
          y = "Anzahl Schüler*innen"
        ) +
        xlim(.5, 6.5)
    
    if (input$plottype == "Histogramm")
      plot <-
        ggplot(data_subsetted(), aes(x = Note)) +
        geom_histogram(
          binwidth = .5,
          fill = "#8cd000",
          color = "#8cd000"
        ) +
        theme_minimal(base_size = 18) +
        labs(
          title = "Histogramm",
          subtitle = paste(subtitle_reactive()),
          y = "Anzahl Schüler*innen"
        ) +
        xlim(.5, 6.5)
    
    if (input$plottype == "Boxplot")
      plot <-
        ggplot(data_subsetted(), 
               aes(y = Note, x = dummy)) + 
        geom_boxplot(color = "#8cd000",
                     fill = "#8cd00020") + 
        coord_flip() +
        labs(
          title = "Boxplot",
          subtitle = paste(subtitle_reactive()),
          x = ""
        ) +
        ylim(1, 6) +
        xlim(0, 2)+
        theme_minimal(base_size = 18) +
        theme(axis.text.y = element_blank())
    
    if (input$plottype == "Jitterplot")
      plot <-
        ggplot(data_subsetted(), aes(x = Note, y = dummy)) + 
        geom_jitter(color = "#8cd000") + 
        labs(title = "Jitterplot",
             subtitle = paste(subtitle_reactive()),
             y = "") + 
        ylim(0,2) +
        xlim(1,6)+
        theme_minimal(base_size = 18) + 
        theme(axis.text.y = element_blank())
    
    if (input$plottype == "Densityplot")
      plot <-
        ggplot(data_subsetted(), aes(x = Note)) + 
        geom_density(color = "#8cd000",
                     fill = "#8cd00020") + 
        labs(title = "Densityplot",
             subtitle = paste(subtitle_reactive()),
             y = "") + 
        #ylim(0,2) +
        xlim(1,6)+
        theme_minimal(base_size = 18) + 
        theme(axis.text.y = element_blank())
    
    if (input$plottype == "Violinplot")
      plot <-
        ggplot(data_subsetted(), aes(x = Note, y = dummy)) + 
        geom_violin(color = "#8cd000",
                    fill = "#8cd00020") + 
        labs(title = "Violinplot",
             subtitle = paste(subtitle_reactive()),
             y = "") + 
        ylim(0,2) +
        xlim(1,6)+
        theme_minimal(base_size = 18) + 
        theme(axis.text.y = element_blank())
    
    if (input$plottype == "Sinaplot")
      plot <-
        ggplot(data_subsetted(), aes(x = Note, y = dummy)) + 
        ggforce::geom_sina(color = "#8cd000") + 
        labs(title = "Sinaplot",
             subtitle = paste(subtitle_reactive()),
             y = "") + 
        ylim(0,2) +
        xlim(1,6)+
        theme_minimal(base_size = 18) + 
        theme(axis.text.y = element_blank())
    
    if (input$plottype == "Errorbarplot")
      plot <-
        ggplot(data_subsetted(), aes(y=Note, x = dummy)) + 
        stat_summary(fun.data = mean_sdl, 
                     geom = "linerange", 
                     fun.args = list(mult = 1), 
                     color = "#8cd000") + 
        stat_summary(fun.y = mean, 
                     geom = "point",
                     color = "#8cd000",
                     size = 3) + 
        coord_flip() +
        labs(title = "Errorbarplot",
             subtitle = paste(subtitle_reactive()),
             x = "") + 
        ylim(1,6)+
        theme_minimal(base_size = 18) +
        theme(axis.text.y = element_blank())
    
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