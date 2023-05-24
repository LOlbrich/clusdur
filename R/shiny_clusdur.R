#' @title shiny_clusdur
#'
#' @description A basic shiny app that shows the duration distribution for all columns by cluster.
#'
#' @param fit A "clusdur" object
#'
#' @return A shiny app showing the distribution of durations faceted by cluster membership.
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import data.table
#' @import shiny
#' @import ggplot2


shiny_clusdur <- function(fit, facet = TRUE){

  # check if fit is a clusdur object
  if(!is(fit, "clusdur")){
    stop("fit is not a clusdur object")
  }


  ui <- shiny::fluidPage(

    shiny::titlePanel("Duration distributions for screens"),

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
