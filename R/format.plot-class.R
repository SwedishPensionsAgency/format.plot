#' Saving plots and output (La)TeX, html code to embed the plot in your document
#' 
#' @field plot a plot object
#' @field name character, name of the plot, used to save the plot in \code{path}
#' @field path character, path to save the plot
#' @field data named list, the elements are used to replace the whisker tags in your template \code{\link{whisker.render}}
#' @field device function name of the device to save the plot. Saves the plot with all devices that are provided, \code{\link{Devices}}
#' @field plot.options parameters passed to the graphical device, \code{\link{Devices}}
#' @field pdf2png should Ghostscript create the png from the pdf? 
#' @field pdf2png.options options passed to \code{\link{pdf_to_png}}
#' @field embed.fonts logical, should the fonts be embedded in the plot, recommended for (La)TeX 
#' @field gs path to your Ghostscript executable
#' @field base54.images logical, embed images base64 encoded in output, works only in non (La)TeX output
#' @field overwrite logical, should existing destination files be overwritten?
#' @field fontpaths a character vector giving directories that Ghostscript will search for fonts
#' 
#' @export format.plot
#' @exportClass format.plot
format.plot <- setRefClass(
  "format.plot",
  fields = list(
    plot = "ANY",
    name = "character",
    path = "character",
    data = "ANY",
    devices = "character", 
    plot.options = "ANY", 
    pdf2png = "logical",
    pdf2png.options = "ANY", 
    embed.fonts = "logical", 
    gs = "character", 
    base64.images = "logical", 
    overwrite = "logical", 
    fontpaths = "ANY"
  ),
  methods = list(
    initialize = function (
      plot, 
      name, 
      path = "figure", 
      data = NULL,
      devices = "pdf", 
      plot.options = NULL,
      pdf2png = TRUE, 
      pdf2png.options = NULL, 
      embed.fonts = TRUE, 
      gs = Sys.getenv("R_GSCMD"), 
      base64.images = FALSE, 
      overwrite = TRUE, 
      fontpaths = NULL) {
      "Create a new format.plot object"
      .self$plot = plot
      .self$name = name
      .self$path = path
      
      .self$data = data
      
      .self$devices = devices
      .self$plot.options = plot.options
      .self$pdf2png = pdf2png
      .self$pdf2png.options = pdf2png.options
      .self$embed.fonts = embed.fonts
      .self$gs = gs
      .self$base64.images = base64.images
      .self$overwrite = overwrite
      .self$fontpaths = fontpaths
      
    }, 
    save = function () {
      "Save the plot with the device in .self$device"
      saved.plot.paths <- list()
      for (device in .self$devices) {
        
        outfile <- normalizePath(file.path(.self$path, paste(.self$name, device, sep = ".")), 
                                 mustWork = FALSE)
        if (!file_test("-d", dirname(outfile))) {
          dir.create(dirname(outfile), recursive = TRUE)
        }
        
        
        tmpfile <- tempfile(fileext = paste0(".", device))
        device.call <- ifelse(device == "png" & .self$pdf2png, "pdf", device)
        
        do.call(device.call, c(list(tmpfile), .self$plot.options))
        if ("gtable" %in% class(.self$plot) & "grob" %in% class(.self$plot)) {
          grid.draw(.self$plot)
        } else {
          print(.self$plot)
        }
        
        dev.off()
        
        if (device == "png" & .self$pdf2png) {
          do.call.merge.args("pdf_to_png", 
                             .self$pdf2png.options, 
                             list(file = tmpfile, 
                                  outfile = tmpfile, 
                                  fontpaths = .self$fontpaths, 
                                  gs = .self$gs)
          )
        }
        
        if (.self$embed.fonts && device == "pdf") {
          embedFonts(file = tmpfile, 
                     format = "pdfwrite", 
                     outfile = tmpfile, 
                     fontpaths = .self$fontpaths)
        }
        
        if (device != "pdf" & .self$base64.images) {
          saved.plot.paths[[device]] <- markdown:::.b64EncodeFile(tmpfile)
        } else {
          if (file.copy(tmpfile, outfile, overwrite = .self$overwrite)) {
            saved.plot.paths[[device]] <- paste(.self$path, paste(.self$name, device, sep = "."), sep = .Platform$file.sep)
          } else {
            warning("Failed to copy save plot to destination directory. Target file was: '", outfile, "'.")
          }
        }
        
        
      }
      
      return(saved.plot.paths)
    }, 
    render = function (type = c("tex", "html"), 
                       template = NULL) {
      "Render the template with the saved plot and the object fields"
      #browser()
      type = match.arg(type)
      
      if (is.null(template)) {
        if (type == "tex") {
          template <- system.file("template", "template.tex", package=getPackageName())
        } else if (type == "html"){
          template <- system.file("template", "template.html", package=getPackageName())
        }
      }
      
      plot.path <- .self$save()
      
      template.whisker <- paste(readLines(template, warn = FALSE), collapse = "\n")
      
      
      if (type == "tex") {
        plot.path <- plot.path[["pdf"]]
        template.whisker <- gsub("<%&plotPath%>", plot.path, template.whisker)
      } else {
        plot.path <- plot.path[[1]]
      }
      
      whisker.data <- c(list(plotPath = plot.path, 
                             name = .self$name), .self$data)
      return(whisker.render(template.whisker, data = whisker.data))
      
      
      
    }
  )
  
)