#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmltools)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

library(gtools)
library(rjson)
library(plyr)
library(magrittr)
library(tidyverse)

list.dirs(path = '..',
          full.names = TRUE,
          recursive = FALSE) %>%
  str_subset('r-.*') %>%
  file.path('renv') %>%
  list.files(pattern = '.*.lock', full.names = TRUE) %>%
  enframe(name=NULL, value='lockfile_id') -> lockfiles

lockfiles %>%
  deframe() %>%
  set_names() %>%
  llply(\(x) fromJSON(file = x)) -> lockfiles_content

lockfiles_content %>%
  llply(pluck, 'Packages') %>%
  ldply(.id = 'lockfile_id', \(x) ldply(x, pluck, 'Version', .id = 'package')) %>%
  rename(version = 'V1') -> packages

c('Azimuth',
  'Seurat',
  'DESeq2',
  'tidyverse',
  'sceasy',
  'clustree',
  'classifyr') -> poi

packages |>
  select(package) |>
  unique() |>
  mutate(accordion = if_else(package %in% poi, 'curated', 'misc')) %>%
  dlply(~ accordion, pluck, 'package') %>%
  llply(mixedsort) -> classified_packages


# Define UI for application that draws a histogram
ui <- shinydashboardPlus::dashboardPage(
  header = dashboardHeader(title = 'christainers'),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "example.css")
    ),
    accordion(
      id = "packages_accordion",
      accordionItem(
        title = 'Recommended lockfiles',
        collapsed = FALSE,
        tableOutput(outputId = 'recommended_lockfiles_table')
      ),
      accordionItem(
        title = "Curated packages",
        collapsed = FALSE,
        awesomeCheckboxGroup(
          inputId = "curated-packages",
          label = NULL,
          choices = classified_packages$curated,
          inline = TRUE
        )
      ),
      accordionItem(
        title = "Miscellaneous packages",
        collapsed = TRUE,
        map(
          letters,
          \(x) awesomeCheckboxGroup(
            inputId = str_glue('misc-packages-{x}'),
            label = x,
            choices = str_subset(
              string = classified_packages$misc,
              pattern = str_glue('^({x}|{str_to_upper({x})})')
            ),
            inline = TRUE
          )
        )
      )
    )
  ),
  controlbar = NULL,
  footer = NULL,
  skin = 'green-light',
  preloader = NULL,
  scrollToTop = TRUE,
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  get_selected_packages <- reactive({
    x <- c()
    for(i in c('curated-packages', str_c('misc-packages', letters, sep='-')))
      x %<>% c(input[[i]])
    req(x)
  })

  get_recommended_lockfiles <- reactive({
    selected_packages <- get_selected_packages()
    packages %>%
      filter(package %in% selected_packages) %>%
      spread(key=package, value=version) %>%
      drop_na()
  })
  
  format_recommended_lockfiles <- reactive({
    get_recommended_lockfiles() %>%
      separate(col='lockfile_id', sep='/', into=c(NA, 'R version', NA, 'Renv lockfile name'))
  })
  
  observe({
    format_recommended_lockfiles() %>%
      print()
  })
  
  output$recommended_lockfiles_table <- renderTable({format_recommended_lockfiles()})
}

# Run the application
shinyApp(ui = ui, server = server)
