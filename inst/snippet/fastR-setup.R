includeChapter <- 1:7 %in% (1:7) # [-6]
includeApp <- 1:4 %in% 1:3

require(MASS)   # make sure this comes before dplyr loads
require(Matrix) # make sure this comes before mosaic loads
require(fastR2)
require(mosaic)
theme_set(theme_bw())
require(knitr)
require(xtable)
options(xtable.floating = FALSE)
opts_knit$set(width=74)
opts_knit$set(self.contained=FALSE)
opts_chunk$set(
  digits = 3,
  dev="pdf",  # don't need EPS files anymore
  dev.args=list(colormodel="cmyk"),
  comment="##",
  prompt=FALSE,
  size="small",
  cache=TRUE,
  cache.path='cache/c-',
  cache.lazy=FALSE,
  tidy=FALSE,
  fig.width=8*.45,
  fig.height=6*.45,
  fig.show="hold",
  fig.align="center",
  out.width=".47\\textwidth",
#  background="gray88",
#  background="white",
#  background="transparent",
  boxedLabel=TRUE
  )

opts_template$set(fig3 = list(fig.height = 7*.40, fig.width = 8*.40, out.width=".31\\textwidth"))
opts_template$set(figtall = list(fig.height = 8*.45, fig.width = 8*.45, out.width=".47\\textwidth"))
opts_template$set(fig1 = list(fig.height = 3*0.9, fig.width = 8 * 0.9, out.width=".95\\textwidth"))
opts_template$set(figbig = list(fig.height = 9*0.9, fig.width = 12*0.9, out.width=".95\\textwidth"))

knit_hooks$set(seed = function(before, options, envir) {
    if (before) set.seed(options$seed) 
})

knit_hooks$set(digits = function(before, options, envir) {
    if (before) {
      options(digits = options$digits)
    } else {
      options(digits = 3)
    }
})


knit_hooks$set(
  document = function(x) {
    sub('\\usepackage[]{color}', '\\usepackage{xcolor}', x, fixed = TRUE)
    gsub(
      "\\definecolor{shadecolor}{rgb}{0.969, 0.969, 0.969}",
      "\\definecolor{shadecolor}{gray}{0.8}", x, fixed = TRUE)
  }
)

knit_hooks$set(chunk = function (x, options) {
	if ( !is.null(options$boxedLabel) && options$boxedLabel && 
         ! grepl("unnamed-chunk", options$label) &&
		(is.null(options$echo) || options$echo) ) {
		labeling <- paste0( 
			"\\endgraf\\nobreak\\null\\endgraf\\penalty-2\\kern-.5\\baselineskip",
			"\n\n",
			"\\hfill \\makebox[0pt][r]{\\fbox{\\tiny ",
			options$label,
			"}}", 
			"\\endgraf",
			"\\kern-4.5ex\n\n")
	}  else {
		labeling <- ""
	}
    ai = knitr:::output_asis(x, options)
    col = if (!ai)
        paste(knitr:::color_def(options$background), 
              if (!knitr:::is_tikz_dev(options)) "\\color{fgcolor}", 
              sep = "")
    k1 = paste(col, "\\begin{kframe}\n", sep = "")
    k2 = "\\end{kframe}"
    x = knitr:::.rm.empty.envir(paste(k1, labeling, x, k2, sep = ""))
    size = if (options$size == "normalsize")
        ""
    else sprintf("\\%s", options$size)
    if (!ai)
        x = sprintf("\\begin{knitrout}%s\n%s\n\\end{knitrout}",
            size, x)
    if (options$split) {
        name = knitr:::fig_path(".tex", options)
        if (!file.exists(dirname(name)))
            dir.create(dirname(name))
        cat(x, file = name)
        sprintf("\\input{%s}", name)
}
else x 
}
)

blackAndWhite = TRUE
fastRlty = rep(1,20)
fastRlty = c(1,2,5,6,1,2,5,6,1,2,5,6)
trellis.par.set(theme=col.whitebg())
# trellis.par.set(theme=col.fastR(bw=blackAndWhite),lty=fastRlty)
trellis.par.set(theme=col.fastR())
# options(width=70)
options(continue="  ")
options(str = strOptions(strict.width = "wrap"))
options(show.signif.stars=FALSE)
options(digits=3)


# # omit some of the output from summary( lm( ) )
# print.summary.lm <- 
#   function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
#             signif.stars = getOption("show.signif.stars"), ...) 
#   {
#     output <- capture.output( stats:::print.summary.lm(x, digits=digits, symbolic.cor = symbolic.cor,
#                               signif.stars=signif.stars, ...) )
#     l <- sapply( output, nchar )
#     w1 <- min( grep("Call", output) ) 
#     w2 <- min( grep("Resid", output) ) 
#     w3 <- min( grep("Coef", output) ) 
# 	rows <- 1:length(output)
# 	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
#     cat( paste(output[keep], collapse="\n") )
#     return(invisible(x))
#   }
# 
# print.summary.glm <- 
#   function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, 
#             signif.stars = getOption("show.signif.stars"), ...) 
#   {
#     output <- capture.output( stats:::print.summary.glm(x, digits=digits, symbolic.cor = symbolic.cor,
#                               signif.stars=signif.stars, ...) )
#     l <- sapply( output, nchar )
#     w1 <- min( grep("Call", output) ) 
#     w2 <- min( grep("Resid", output) ) 
#     w3 <- min( grep("Coef", output) ) 
# 	rows <- 1:length(output)
# 	keep <- (rows >= w1 & rows < w2) | (rows >=w3)
#     cat( paste(output[keep], collapse="\n") )
#     return(invisible(x))
#   }

