#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

varnames <- names(iris)
#source("R_CROSSTAB_V2.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PE Exit Survey"),
   
   selectInput("variable", "Variable:",
               # c("Cylinders" = "cyl",
               #   "Transmission" = "am",
               #   "Gears" = "gear")),
               varnames),

   tableOutput("data")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$data <- renderTable({
  #   mtcars[, c("mpg", input$variable), drop = FALSE]
  # }, rownames = TRUE)

  
 output$data <- renderTable({
   x <- crosstab(iris, input$variable, "Species", 3, 3, TRUE)
   x[[1]]
   #crosstab(mtcars$cyl, mtcars$gear)
#   x <- crosstab(mtcars, "cyl", "disp", 6, 2, TRUE)
 })

}

# Run the application 
shinyApp(ui = ui, server = server)

