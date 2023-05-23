#' @title shiny_dur
#'
#' @description A basic shiny app that shows the duration distribution for all columns by cluster
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table
#' @import shiny
#' @import ggplot2


shiny_dur <- function(fit, facet = TRUE){

  ui <- shiny::fluidPage(

    shiny::titlePanel("Duration distributions for items"),

    shiny::sidebarLayout(

      shiny::sidebarPanel(

        shiny::selectInput(
          inputId = "select_var",
          label = "Select variable",
          choices = unique(fit$long_data$item)
        ),
        shiny::numericInput(
          inputId = "max_x",
          label = "Maximum of x axis",
          value = 100
        ),
        if(!facet){

          shiny::selectInput(
            inputId = "select_cluster",
            label = "Select cluster",
            choices = unique(fit$cluster_id$cluster)
          )
        }
      ),

      shiny::mainPanel(

        shiny::plotOutput(outputId = "dur_dist")
      )
    )
  )

  server <- function(input, output){

    output$dur_dist <- shiny::renderPlot({
      plot_data <- fit$long_data
      plot_data <- fit$cluster_id[plot_data, on = colnames(plot_data)[1]]
      plot_data_item <- plot_data[item == input$select_var,]


      if(facet){
        ggplot(data = plot_data_item,
               aes(x = duration)) +
          geom_histogram(binwidth = 1) +
          xlim(c(0, input$max_x)) +
          theme_light()+
          facet_wrap(~cluster, scales = "free_y") +
          theme(strip.background =element_rect(fill="white", color = "darkgray"),
                strip.text = element_text(colour = 'black'))
      } else{
        plot_data_cluster <- plot_data_item[cluster == input$select_cluster,]
        ggplot(data = plot_data_cluster,
               aes(x = duration)) +
          geom_histogram(binwidth = 1) +
          xlim(c(0, input$max_x)) +
          theme_light()
      }

    })
  }

  shiny::shinyApp(ui, server)
}
