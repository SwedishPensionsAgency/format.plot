#' Convert PDF to PNG
#' 
#' Converts a PDf file to PNG with Ghostscript
#' 
#' @param file a character string giving the name of the original pdf file
#' @param outfile the name of the new png file including ".png" extension
#' @param fontpaths a character vector giving directories that Ghostscript will search for fonts
#' @param resolution target resolution of the png file, default is 600
#' @param options a character string containing further options to Ghostscript
#' 
#' @export
pdf_to_png <- function(
  file, 
  outfile, 
  fontpaths = "", 
  resolution = 600, 
  options = "",
  gs = Sys.getenv("R_GSCMD"), 
  overwrite = TRUE
  ){
  tmpfile <- tempfile("Rpng")
  cmd <- paste0(gs, 
                " ", "-dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -r", resolution, " -dTextAlphaBits=4 -dGraphicsAlphaBits=4",
                " ", shQuote(paste0("-sFONTPATH=", paste(fontpaths, collapse = .Platform$path.sep))), 
                " ", options, 
                " ", "-sOutputFile=", tmpfile, 
                " ", shQuote(file))
  ret <- system(cmd, intern = TRUE)
  status = attr(ret, "status")
  if (!is.null(status) && status != 0) {
    stop(gettextf("status %d in running command '%s'", ret, 
                  cmd), domain = NA)
  }
    
  return(file.copy(tmpfile, outfile, overwrite = overwrite))
}
