#' Convert to a reveal.js presentation
#'
#' Format for converting from R Markdown to a reveal.js presentation.
#'
#' @inheritParams beamer_presentation
#' @inheritParams pdf_document
#' @inheritParams html_document
#'
#' @param center \code{TRUE} to vertically center content on slides
#' @param height the default height (in pixels) of the presentation slides. 
#'   "default" uses the reveal.js default of 700.
#' @param width the default width (in pixels) of the presentation slides. 
#'   "default" uses the reveal.js default of 960.
#' @param theme Visual theme ("default", "simple", sky", "beige", "serif", 
#'   "solarized", or "local"). If choosing "local", you must supply a
#'   values for \code{local_theme}.
#' @param transition Slide transition ("default", "cube", "page", "concave",
#'   "zoom", "linear", "fade", or "none")
#' @param template Pandoc template to use for rendering. Pass "default"
#'   to use the rmarkdown package default template; pass \code{NULL}
#'   to use pandoc's built-in template; pass a path to use a custom template
#'   that you've created. Note that if you don't use the "default" template
#'   then some features of \code{revealjs_presentation} won't be available
#'   (see the Templates section below for more details).
#' @param reveal_version The version of reveal.js to use. Defaults to 2.6.1
#' @param reveal_source A path to the source for reveal.js. Defaults to the 
#'   source supplied with rmarkdown.
#' @param plugin_dir The directory for reveal.js to look for plugin javascript.
#'   Defaults to \code{reveal_source}/plugin or \code{lib_dir}/plugin if
#'   \code{lib_dir} is specified.
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @details
#'
#' In reveal.js presentations you can use level 1 or level 2 headers for
#' slides. If you use a mix of level 1 and level 2 headers then a
#' two-dimensional layout will be produced, with level 1 headers building
#' horizontally and level 2 headers building vertically.
#'
#' For more information on markdown syntax for presentations see
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/producing-slide-shows-with-pandoc.html}{producing
#' slide shows with pandoc}.
#'
#' @section Templates:
#'
#' You can provide a custom HTML template to be used for rendering. The syntax
#' for templates is described in the documentation on
#' \href{http://johnmacfarlane.net/pandoc/demo/example9/templates.html}{pandoc
#' templates}. You can also use the basic pandoc template by passing
#' \code{template = NULL}.
#'
#' Note however that if you choose not to use the "default" reveal.js template
#' then several aspects of reveal.js presentation rendering will behave
#' differently:
#'
#' \itemize{
#'   \item{The \code{center} parameter does not work (you'd need to
#'      set this directly in the template).
#'   }
#'   \item{The built-in template includes some additional tweaks to styles
#'      to optimize for output from R, these won't be present.
#'   }
#'   \item{MathJax will not work if \code{self_contained} is \code{TRUE}
#'      (these two options can't be used together in normal pandoc templates).
#'   }
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#'
#' # simple invocation
#' render("pres.Rmd", revealjs_presentation())
#'
#' # specify an option for incremental rendering
#' render("pres.Rmd", revealjs_presentation(incremental = TRUE))
#' }
#'
#' @export
revealjs_presentation <- function(incremental = FALSE,
                                  center = FALSE,
                                  fig_width = 8,
                                  fig_height = 6,
                                  fig_retina = if (!fig_caption) 2,
                                  fig_caption = FALSE,
                                  smart = TRUE,
                                  self_contained = TRUE,
                                  theme = "default",
                                  transition = "default",
                                  highlight = "default",
                                  mathjax = "default",
                                  template = "default",
                                  vertical = FALSE,
                                  height = NULL,
                                  width = NULL,
                                  local_theme = NULL,
                                  local_highlight = NULL,
                                  reveal_version = "default",
                                  reveal_source = "default",
                                  includes = NULL,
                                  keep_md = FALSE,
                                  lib_dir = NULL,
                                  plugin_dir = NULL,
                                  pandoc_args = NULL,
                                  ...) {

  # base pandoc options for all reveal.js output
  args <- c()
  
  # reveal.js version
  if (identical(reveal_version,"default"))
    reveal_version <- "2.6.1"
  
  if (identical(reveal_source,"default"))
    reveal_source <- rmarkdown_system_file("rmd/revealjs/")

  message(paste("reveal_source =", reveal_source))
  test_path <- file.path(reveal_source, paste("reveal.js-",reveal_version,sep=''))
  if (file.exists(test_path)) {
    reveal_source <- test_path
    message(paste("Updating reveal_source <-", reveal_source))
  } else {
    message(paste("Rejecting test path", test_path))
    reveal_json_package <- file.path(reveal_source, "package.json")
    if (file.exists(reveal_json_package)) {
      test_reveal_version <- get_reveal_version(reveal_json_package)
      if (reveal_versions_match(reveal_version, test_reveal_version)) {
        reveal_version <- test_reveal_version
        message(paste("Updating reveal_version <-", reveal_version))
      } else {
        stop(paste("Could not find reveal.js version ", reveal_version, 
                    " at ", reveal_source, ".", sep=''))
      }
    } else {
      stop(paste("Could not find reveal.js directory at ", reveal_source))
    }
  }

  # template path and assets
  if (identical(template, "default"))
    args <- c(args, "--template",
              pandoc_path_arg(rmarkdown_system_file(
                "rmd/revealjs/default.html")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))

  # incremental
  if (incremental)
    args <- c(args, "--incremental")

  # centering
  if (center)
    args <- c(args, "--variable", "center")
  
  # height
  if (! is.null(height) && ! identical(height, "default"))
    args <- c(args, "--variable", paste("height=",height,sep=''))
  
  # width
  if (! is.null(width) && ! identical(width, "default"))
    args <- c(args, "--variable", paste("width=",width,sep=''))
  
  # vertical
  if (vertical) {
    args <- c(args,"--slide-level=2")
  }
  
  # theme
  theme <- match.arg(theme, revealjs_themes())
  if (identical(theme, "default"))
    theme <- "simple"
  else if (identical(theme, "dark"))
    theme <- "default"
  else if (identical(theme, "local"))
    args <- c(args, "--variable", paste("local-theme=", local_theme, sep=""))
  if (theme %in% c("default", "blood", "moon", "night"))
    args <- c(args, "--variable", "theme-dark")
    
  args <- c(args, "--variable", paste("theme=", theme, sep=""))

  # transition
  transition <- match.arg(transition, revealjs_transitions())
  args <- c(args, "--variable", paste("transition=", transition, sep=""))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # reveal.js
    revealjs_path <- reveal_source
    if (!self_contained)
      revealjs_path <- relative_to(
        output_dir, render_supporting_files(revealjs_path, lib_dir))
    args <- c(args, "--variable", 
              paste("revealjs-url=",gsub('/','\\\\',
                                         pandoc_path_arg(revealjs_path)),sep=""))
    if (is.null(plugin_dir))
      plugin_dir <- file.path(pandoc_path_arg(revealjs_path), "plugin")
    message(paste('plugin_dir = "', plugin_dir, '"', sep=""))
    args <- c(args, "--variable", 
              paste("plugin-url=", URLencode(plugin_dir), sep=""))


    # highlight
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))
    if (identical(highlight,"local"))
      args <- c(args, "--variable", paste("local-highlight=", local_highlight, 
                sep=""))

    # return additional args
    args
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md),
    pandoc = pandoc_options(to = "revealjs",
                            from = from_rmarkdown(fig_caption),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = html_document_base(smart = smart, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args, ...))
}



get_reveal_version <- function(package.json) {
  package <- RJSONIO::fromJSON(package.json)
  if (identical(package$name, "reveal.js"))
    return(package$version)
  else
    return(NULL)
}

reveal_versions_match <- function(target, ondisk){
  if (is.null(ondisk))
    return(FALSE)
  if (identical(target,"any"))
    return(TRUE)
  return(identical(target,ondisk))
}

revealjs_themes <- function() {
  c("default",
    "simple",
    "sky",
    "beige",
    "serif",
    "solarized",
    "dark",
    "blood",
    "moon",
    "night",
    "local")
}


revealjs_transitions <- function() {
  c("default",
    "cube",
    "page",
    "concave",
    "zoom",
    "linear",
    "fade",
    "local",
    "none")
}

