#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#.libPaths(c('/nemo/stp/babs/working/barrinc/projects/babs/christopher.barrington/christainers/app/renv/library/R-4.1/x86_64-conda-linux-gnu' '/nemo/stp/babs/working/barrinc/cache/R/renv/sandbox/R-4.1/x86_64-conda-linux-gnu/55439b24'))

library(shiny)
library(htmltools)
library(shinyWidgets)
library(shinydashboard)
library(DT)
# library(shinydashboardPlus)

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
	rename(version = 'V1') %>%
	mutate_at('package', \(x) factor(x) |> fct_relevel(mixedsort)) -> packages

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
	mutate(curated = package %in% poi,
	       all=TRUE) %>%
	select(package, curated:last_col()) %>%
	gather(key=group, value=included, -package) %>%
	filter(included==TRUE) %>%
	dlply(~group, pluck, 'package') -> grouped_packages

# Define UI elements for application

ui_elements <- list()

dashboardHeader(title = 'christainers') -> ui_elements$header

dashboardSidebar(
      sidebarMenu(menuItem('Find R environments', tabName='find-r-env', icon=icon('dashboard'))),
      disable=FALSE,
      collapsed=TRUE) -> ui_elements$sidebar

list(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'example.css')),
     tabItems(tabItem(tabName='find-r-env',
     	              valueBox(value = length(lockfiles_content),
     	                       subtitle = 'Renvironments are available to use',
                               icon = icon('arrow-right-to-bracket'),
                               color='green', width=3),
     	              valueBox(value = length(grouped_packages$all),
     	                       subtitle = 'Packages are included in at least one renvironment',
                               icon = icon('cubes-stacked'),
                               color='green', width=3),
     	              valueBoxOutput(outputId='n_selected_packages_valuebox', width=3),
     	              valueBoxOutput(outputId='n_suggested_lockfiles_valuebox', width=3),
                      # first
                      box(title = 'Available renvironments',
                          footer = 'These renvironments contain the selected combination of packages. The package version in the renvironment is indicated.',
                          status='primary',
                          solidHeader=TRUE,
                          background=NULL,
                          width = 12,
                          collapsible = FALSE,
                          collapsed = FALSE,
                          materialSwitch(inputId='show-full-table',
                                         label='Include all available renvironments, irrespective of package availability', 
                                         value=FALSE,
                                         status='success',
                                         right=TRUE),
                          tableOutput(outputId = 'suggested_lockfiles_table'),
                          dataTableOutput(outputId='suggested_lockfiles_dtable'),
                          uiOutput('suggested_lockfiles_ui')),
                      
                      # second
                      box(title = 'Favourite packages',
                          footer = 'This is a manually curated set of (major) packages that may be more frequently used.',
                          status='primary',
                          solidHeader=TRUE,
                          background=NULL,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          awesomeCheckboxGroup(inputId = 'curated-packages',
                                               label = NULL,
                                               choices = grouped_packages$curated,
                                               inline = TRUE)),
                      
                      # third
                      box(title = 'All packages',
                          footer = 'These are all of the packages provided by one or more renvironments.',
                          status='danger',
                          solidHeader=TRUE,
                          background=NULL,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          map(letters,
                              \(x) awesomeCheckboxGroup(inputId=str_glue('all-packages-{x}'),
                                                        label = x,
                                                        choices = str_subset(string=grouped_packages$all, pattern = str_glue('^({x}|{str_to_upper({x})})')), inline = TRUE)))))) %>%
	do.call(what=dashboardBody) -> ui_elements$body

list(controlbar = NULL,
     footer = NULL,
     skin = 'green-light',
     preloader = NULL,
     scrollToTop = TRUE) -> ui_args

append(ui_elements, ui_args) %>%
	do.call(what=shinydashboardPlus::dashboardPage) -> ui

# Define server logic required to draw a histogram
server <- function(input, output, session) {

	# collect the user's selected packages
	get_selected_packages <- reactive({
		x <- c()
		for(i in c('curated-packages', str_c('all-packages', letters, sep='-')))
			x %<>% c(input[[i]])
		x})

	# filter the available packages by the user's selection
	get_filtered_lockfiles <- reactive({
		selected_packages <- get_selected_packages()
		show_full_table <- input[['show-full-table']]

		packages %>%
			filter(package %in% selected_packages) %>%
			spread(key=package, value=version) %>%
			when(length(selected_packages)==0 ~ lockfiles,
			     show_full_table==TRUE ~ left_join(x=lockfiles, y=., by='lockfile_id'),
			     drop_na(.)) %>%
			mutate(`All packages`=!if_any(-lockfile_id, is.na), .after=lockfile_id) %>%
			separate(col='lockfile_id', sep='/', into=c(NA, 'R version', NA, 'Renv lockfile name')) %>%
			arrange(desc(`All packages`), mixedsort(`R version`), `Renv lockfile name`)})
	
	# render the output table of environments
	# renderDataTable({
	# 	pos_glyph <- as.character(icon('check'))
	# 	neg_glyph <- as.character(icon('xmark'))
	#
	# 	get_filtered_lockfiles() |>
	# 		mutate_at('All packages', \(x) if_else(x, true=pos_glyph, false=neg_glyph)) |>
	# 		mutate_at(vars(-'Renv lockfile name'), as.factor)},
	# 	options=list(autoWidth=FALSE,
	# 	             columnDefs=list(list(targets=c('Renv lockfile name', 'All packages'), searchable=FALSE),
	# 	                             list(targets=c('All packages', get_selected_packages()), className='dt-center')),
	# 	             dom='t',
	# 	             paging=FALSE,
	# 	             # preDrawCallback=\() {clear()},
	# 	             searching=TRUE,
	# 	             stateSave=TRUE),
	# 	escape=FALSE,
	# 	rownames=FALSE,
	# 	filter='top',
	# 	style='auto') -> output$suggested_lockfiles_dtable

	# output$suggested_lockfiles_table <- renderTable({
	# 	pos_glyph <- icon('check') |> as.character()
	# 	neg_glyph <- icon('xmark') |> as.character()
	#
	# 	get_filtered_lockfiles() |>
	# 		mutate_at('All packages', \(x) if_else(x, true=pos_glyph, false=neg_glyph)) %>% print()},
	# 	striped=TRUE,
	# 	hover=TRUE,
	# 	spacing='m',
	# 	na='-',
	# 	sanitize.text.function=function(x) x)

	output$suggested_lockfiles_ui <- renderUI({
		pos_glyph <- icon('check') |> as.character()
		neg_glyph <- icon('xmark') |> as.character()

		get_filtered_lockfiles() |>
			mutate_at('All packages', \(x) if_else(x, true=pos_glyph, false=neg_glyph)) -> data

		renderTable({data},
		            striped=TRUE,
		            hover=TRUE,
		            spacing='m',
		            na='-',
		            sanitize.text.function=function(x) x,
		            align=c('l', 'l', rep(x='c', times={ncol(data) |> subtract(2)})) |> str_c(collapse=''),
		            rownames=FALSE) -> output$table

		tableOutput('table')})

	# render the number of packages a user has selected
	output$n_selected_packages_valuebox <- renderValueBox({
		n_selected_packages <- get_selected_packages() %>% length() %>% as.character()

		switch(n_selected_packages,
		       `1`='Package has been selected',
		       'Packages have been selected') -> subtitle

		valueBox(value = n_selected_packages,
		         subtitle = subtitle,
		         icon = icon('magnifying-glass'),
		         color='green', width=3)})

	# render the number of renvironments that satisfy the user's selected packages
	output$n_suggested_lockfiles_valuebox <- renderValueBox({
		filtered_lockfiles <- get_filtered_lockfiles()
		n_filtered_lockfiles <- filtered_lockfiles %>% pluck(3) %>% sum() |> as.character()
		no_packages_selected <- filtered_lockfiles %>% ncol() %>% equals(3)

		if(no_packages_selected | n_filtered_lockfiles=='0') {
			value <- 0
			subtitle <- 'Renvironments contain all selected packages'
		} else {
			value <- n_filtered_lockfiles
			switch(n_filtered_lockfiles,
			       `1`='Renvironment contains all selected packages',
			       'Renvironments contain all selected packages') -> subtitle
		}

		valueBox(value = value,
		         subtitle = subtitle,
		         icon = icon('arrow-right-from-bracket'),
		         color='green', width=3)})
}

# Run the application
shinyApp(ui = ui, server = server)
