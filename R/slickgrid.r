#' Table output, rendered with an interactive slickgrid
#'
#' @param expr An expression that returns a data frame or matrix.
#' @param env The environment in which to evaluate \code{expr}.
#' @param ... Other properties or elements to include.
#'
#' @seealso \code{\link{slickgridOutput}}
#'
#' @examples
#' \dontrun{
#' shinyUI(bootstrapPage(
#'
#' ))
#' }
#' @export
renderSlickgrid <- function(expr, ..., env=parent.frame(), quoted=FALSE) {
  func <- exprToFunction(expr, env, quoted)

  function() {
    data <- func()
    if (is.null(data))
      data <- data.frame()

    return(list(
      colnames = names(data),
      rownames = row.names(data),
      values = unname(data)
    ))
  }
}


#' Create a slickgrid output element
#'
#' @param outputId output variable to read the table from
#' @export
slickgridOutput <- function(outputId, width = 600, height = 600) {
  addResourcePath(
    prefix = "slickgrid",
    directoryPath = system.file("slickgrid", package="shinySlickgrid"))

  tagList(
    singleton(tags$head(
      tags$script(src = "slickgrid/lib/jquery.event.drag-2.2.js"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "slickgrid/slick.grid.css"),
      tags$script(src = "slickgrid/slick.core.js"),
      tags$script(src = "slickgrid/slick.grid.js"),
      tags$script(src = "slickgrid/shiny-slickgrid.js")
    )),
    div(id = outputId, class = "slickgrid shiny-slickgrid-output",
      style = paste0("width:", width, "px; height:", height, "px;"))
  )
}
