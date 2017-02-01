#####################################
# dashboard header
#####################################


header = dashboardHeader(
  title = uiOutput("shinyTitle"), titleWidth = 350, disable = FALSE 
)

# add id to sidebar toggle link so that we can refresh when clicked
tmp = header$children[[3]]$children[[2]]
tmp[[2]]$id = "sidebarToggle"
header$children[[3]]$children[[2]] = tmp

