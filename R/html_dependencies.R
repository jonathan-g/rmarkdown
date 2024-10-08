#' @importFrom htmltools htmlDependency copyDependencyToDir
#'   makeDependencyRelative renderDependencies tagList tags
#'   restorePreserveChunks extractPreserveChunks HTML
NULL

#' Provide common HTML dependencies for R Markdown formats
#'
#' These functions provide common HTML dependencies (e.g. jquery, bootstrap)
#' for re-use by other R Markdown formats.
#'
#' @inheritParams html_document
#' @name html-dependencies
NULL

# Create an HTML dependency for jQuery
#' @rdname html-dependencies
#' @export
html_dependency_jquery <- function()  {
  jquerylib::jquery_core(major_version = getOption('rmarkdown.jquery.version', 3))
}

# Create an HTML dependency for jQuery UI
#' @rdname html-dependencies
#' @export
html_dependency_jqueryui <- function() {

  htmlDependency(
    name = "jqueryui",
    version = version_jqueryui,
    src = pkg_file("rmd/h/jqueryui"),
    script = "jquery-ui.min.js")
}

# Create an HTML dependency for Bootstrap
#' @rdname html-dependencies
#' @export
html_dependency_bootstrap <- function(theme) {
  theme <- resolve_theme(theme)

  # Bootstrap with bslib package
  if (is_bs_theme(theme)) {
    # TODO: would it make sense for these additional rules to come as a part of
    # bslib::bs_theme_dependencies() (for consistency sake)?
    h1_size <- if ("3" %in% theme_version(theme)) "font-size-h1" else "h1-font-size"
    theme <- bslib::bs_add_rules(
      theme, c(
        paste0("h1.title {margin-top: 1.25rem; font-size: 1.15 * $", h1_size, "}"),
        "pre:not([class]) { background-color: $body-bg }"
      )
    )
    return(bslib::bs_theme_dependencies(theme))
  }

  # Rmarkdown own BS3 dependency
  htmlDependency(
    name = "bootstrap",
    version = "3.3.5",
    src = pkg_file("rmd/h/bootstrap"),
    meta = list(viewport = "width=device-width, initial-scale=1"),
    script = c(
      "js/bootstrap.min.js",
      # These shims are necessary for IE 8 compatibility
      "shim/html5shiv.min.js",
      "shim/respond.min.js"),
    stylesheet = paste0("css/", theme, ".min.css"),
    # CSS rules yanked from inst/rmd/h/default.html that should remain for historical
    # reasons, but shouldn't be included if bslib is relevant
    head = format(tags$style(HTML(
      "h1 {font-size: 34px;}
       h1.title {font-size: 38px;}
       h2 {font-size: 30px;}
       h3 {font-size: 24px;}
       h4 {font-size: 18px;}
       h5 {font-size: 16px;}
       h6 {font-size: 12px;}
       code {color: inherit; background-color: rgba(0, 0, 0, 0.04);}
       pre:not([class]) { background-color: white }"
    )))
  )
}

bootstrap_dependencies <- function(theme) {
  deps <- html_dependency_bootstrap(theme)
  if (inherits(deps, "html_dependency")) list(deps) else deps
}

# resolves boostrap theme for bslib compatibility
resolve_theme <- function(theme) {
  # theme = NULL means no Bootstrap
  if (is.null(theme)) return(theme)

  # Bootstrap/Bootswatch 3 names (backwards-compatibility)
  if (is.character(theme)) {
    if (length(theme) != 1) {
      stop2("`theme` must be character vector of length 1.")
    }
    if (identical(theme, "default")) {
      theme <- "bootstrap"
    }

    # special handling triggered when bootstrap folder removed from installation folder
    # In this case, bslib is used as default.
    bslib_forced_mode <- getOption("rmarkdown.bslib_forced_mode", FALSE)
    if (bslib_forced_mode || !dir.exists(pkg_file("rmd/h/bootstrap"))) {
      # we are in special bslib default mode, theme is tweaked to be a list
      if (!is_available("bslib")) {
        stop2(
          "bslib package is required and must be installed",
          if (bslib_forced_mode) " when in special bslib forced mode",
          "."
        )
      }
      return(bslib::bs_theme(version = 3, bootswatch = theme))
    }

    return(match.arg(theme, themes()))
  }

  # Bootstrap theme handled by bslib package
  if (is.list(theme)) {
    if (!is_available("bslib")) {
      stop2("Providing a list to `theme` requires the bslib package.")
    }
    return(as_bs_theme(theme))
  }
  stop2(
    "`theme` expects any one of the following values: \n",
    "    (1) NULL (no Bootstrap), \n",
    "    (2) a character string referencing a Bootswatch 3 theme name, \n",
    "    (3) a list of arguments to bslib::bs_theme(), \n",
    "    (4) a bslib::bs_theme() object."
  )
}

# At the moment, theme may be either NULL (no Bootstrap), a string (Bootswatch
# 3 name), a bslib::bs_theme(), or a list of arguments to bs_theme().
as_bs_theme <- function(theme) {
  if (is_bs_theme(theme)) {
    return(theme)
  }
  if (is.list(theme)) {
    return(do.call(bslib::bs_theme, theme))
  }
  NULL
}

is_bs_theme <- function(theme) {
  is_available("bslib") &&
    bslib::is_bs_theme(theme)
}

theme_version <- function(theme) {
  if (is_bs_theme(theme)) return(bslib::theme_version(theme))
  substr(html_dependency_bootstrap("default")$version, 1, 1)
}

needs_sass <- function(css) {
  grepl("\\.s[ac]ss$", css)
}


# Create an HTML dependency for tocify
#' @rdname html-dependencies
#' @export
html_dependency_tocify <- function() {

  htmlDependency(
    name = "tocify",
    version = "1.9.1",
    src = pkg_file("rmd/h/tocify"),
    script = "jquery.tocify.js",
    stylesheet = "jquery.tocify.css")
}

# Create an HTML dependency for Pandoc code block accessibility
html_dependency_accessible_code_block <- function() {
  ver <- pandoc_version()
  if (ver < '2.7.3' || ver > '2.9.2.1') return()
  htmlDependency(
    name = "accessible-code-block",
    version = "0.0.1",
    src = pkg_file("rmd/h/accessibility"),
    script = "empty-anchor.js"
  )
}

# Create an HTML dependency for FontAwesome
#' @rdname html-dependencies
#' @export
html_dependency_font_awesome <- function() {
  fontawesome::fa_html_dependency()
}

# Create an HTML dependency for Ionicons
#' @rdname html-dependencies
#' @export
html_dependency_ionicons <- function() {

  htmlDependency(
    name = "ionicons",
    version = "2.0.1",
    src = pkg_file("rmd/h/ionicons"),
    stylesheet = "css/ionicons.min.css")
}

html_dependency_navigation <- function(code_menu, source_embed) {

  # dynamically build script list
  script <- c("tabsets.js")
  if (code_menu)
    script <- c(script, "codefolding.js")
  if (source_embed)
    script <- c(script, "sourceembed.js")

  htmlDependency(name = "navigation",
                 version = "1.1",
                 src = pkg_file("rmd/h/navigation-1.1"),
                 script = script)
}

html_dependency_anchor_sections <- function(style = c("hash", "symbol", "icon"),
                                            section_divs = FALSE) {
  style <- match.arg(style)
  # dynamically build css list
  content_style <- function(style) sprintf("anchor-sections-%s.css", style)
  htmlDependency(name = "anchor-sections",
                 version = "1.1.0",
                 src = pkg_file("rmd/h/anchor-sections"),
                 script = if (section_divs) "anchor-sections.js",
                 stylesheet = c("anchor-sections.css", content_style(style))
  )
}

# analyze navbar html source for icon dependencies
navbar_icon_dependencies <- function(navbar) {

  # read the navbar source
  source <- read_utf8(navbar)

  # find icon references
  res <- regexec('<(span|i) +class *= *("|\') *(fa\\w? fa|ion ion)-', source)
  matches <- regmatches(source, res)
  libs <- c()
  for (match in matches) {
    if (length(match) > 0)
      libs <- c(libs, match[[4]])
  }
  libs <- unique(libs)

  # return their dependencies
  any_fa <- any(grepl("fa\\w? fa", libs))
  any_ion <- any(grepl("ion ion", libs))
  html_dependencies_fonts(any_fa, any_ion)
}

# utility function to return a list of font dependencies based
# whether we are including font_awesome and/or ionicons
html_dependencies_fonts <- function(font_awesome, ionicons) {
  deps <- list()
  if (font_awesome)
    deps <- append(deps, list(html_dependency_font_awesome()))
  if (ionicons)
    deps <- append(deps, list(html_dependency_ionicons()))
  deps
}

# flattens an arbitrarily nested list and returns all of the dependency
# objects it contains
flatten_dependencies <- function(knit_meta, test) {

  all_dependencies <- list()

  # knit_meta is a list of 'meta' attributes returned from custom knit_print
  # functions. since the 'meta' attribute could either be an html dependency or
  # a list of dependencies we recurse on lists that aren't named
  for (dep in knit_meta) {
    if (is.null(names(dep)) && is.list(dep)) {
      inner_dependencies <- flatten_dependencies(dep, test)
      all_dependencies <- append(all_dependencies, inner_dependencies)
    } else if (test(dep)) {
      all_dependencies[[length(all_dependencies) + 1]] <- dep
    }
  }

  all_dependencies
}

flatten_html_dependencies <- function(knit_meta) {
  flatten_dependencies(knit_meta, is_html_dependency)
}

# consolidate dependencies (use latest versions and remove duplicates). this
# routine is the default implementation for version dependency resolution;
# formats may specify their own.
html_dependency_resolver <- function(all_dependencies) {

  dependencies <- htmltools::resolveDependencies(all_dependencies)

  # validate each surviving dependency
  dependencies <- lapply(dependencies, validate_html_dependency)

  # return the consolidated dependencies
  dependencies
}

html_reference_path <- function(path, lib_dir, output_dir) {
  # write the full OS-specific path if no library
  if (is.null(lib_dir))
    pandoc_path_arg(path)
  else
    relative_to(output_dir, path)
}

copy_if_changed <- function(from, to, recursive = FALSE,
                            overwrite = FALSE, copy.mode = FALSE) {
  isdir = dir.exists(from)
  if (isdir) {
    if (! dir.exists(to)) {
      dir.create(to, recursive = TRUE)
    }
    if (recursive) {
      from2 = list.files(from, recursive = FALSE, include.dirs = TRUE,
                        all.files = TRUE, no.. = TRUE)
      mapply(copy_if_changed, from = file.path(from, from2),
             to = file.path(to, from2),
             MoreArgs = list(recursive = recursive, overwrite = overwrite,
                             copy.mode = copy.mode))
    }
  } else {
    digests <- tools::md5sum(c(from, to))
    if (!isTRUE(digests[1] == digests[2])) {
      file.copy(from, to, overwrite = TRUE, copy.mode = FALSE)
    }
  }
}

# This is an almost exact copy of htmltools::copyDependencyToDir, except that
# it only copies files if necessary.
#
# Sometimes, a process (e.g., web server) will be accessing an HTML dependency
# file when RMarkdown tries to overwrite it, and R throws an error reporting
# insufficient privilege to delete or overwrite the file or directory.
#
# This function reduces that by only copying if the file has changed.
#
copy_html_dependency <- function(dependency, outputDir, mustWork = TRUE) {
  dir <- dependency$src$file
  if (is.null(dir)) {
    if (mustWork) {
      stop("Dependency ", dependency$name, " ",
           dependency$version, " is not disk-based")
    }
    else {
      return(dependency)
    }
  }
  if (!is.null(dependency$package))
    dir <- system.file(dir, package = dependency$package)
  if (length(outputDir) != 1 || outputDir %in% c("",
                                                 "/"))
    stop("outputDir must be of length 1 and cannot be \"\" or \"/\"")
  target_dir <- if (getOption("htmltools.dir.version",
                              TRUE)) {
    paste(dependency$name, dependency$version, sep = "-")
  }
  else dependency$name
  target_dir <- file.path(outputDir, target_dir)
  if (same_path(dir, target_dir))
    return(dependency)
  if (!dir_exists(outputDir))
    dir.create(outputDir)

  # Unlike htmltools::copyDependencyToDir(),
  # we do not delete the target directory,  but we
  # do create it if necessary.
  if (! dir_exists(target_dir)) dir.create(target_dir)
  files <- if (dependency$all_files)
    list.files(dir)
  else {
    unlist(dependency[c("script", "stylesheet",
                        "attachment")])
  }
  srcfiles <- file.path(dir, files)
  if (any(!file.exists(srcfiles))) {
    stop(sprintf("Can't copy dependency files that don't exist: '%s'",
                 paste(srcfiles, collapse = "', '")))
  }
  destfiles <- file.path(target_dir, files)
  isdir <- file.info(srcfiles)$isdir
  destfiles <- ifelse(isdir, dirname(destfiles), destfiles)
  mapply(copy_if_changed, from = srcfiles, to = destfiles,
         recursive = isdir,
         MoreArgs = list(overwrite = TRUE, copy.mode = FALSE))
  dependency$src$file <- normalizePath(target_dir, "/", TRUE)
  dependency
}

# return the html dependencies as an HTML string suitable for inclusion
# in the head of a document
html_dependencies_as_string <- function(dependencies, lib_dir, output_dir,
                                        allow_uptree_lib_dir = FALSE) {
  if (!is.null(lib_dir)) {
    if (allow_uptree_lib_dir && grepl("^\\.\\.", lib_dir)) {
      abs_lib_dir <- normalizePath(lib_dir, winslash = "/")
      dependencies <- lapply(dependencies, copy_html_dependency, abs_lib_dir,
                             mustWork = FALSE)
      dependencies <- lapply(dependencies, makeDependencyRelative, abs_lib_dir,
                             mustWork = FALSE)
      return(renderDependencies(dependencies, "file", encodeFunc = identity,
                                hrefFilter = function(path) {
                                  file.path(lib_dir, path)
                                }))
    } else {
      # using mustWork=FALSE insures non-disk based dependencies are
      # return untouched, keeping the order of all deps.
      dependencies <- lapply(dependencies, copyDependencyToDir,
                             outputDir = lib_dir, mustWork = FALSE)
      dependencies <- lapply(dependencies, makeDependencyRelative,
                             basepath = output_dir, mustWork = FALSE)
    }
  }

  # Dependencies are iterated on as file based dependencies needs to be
  # processed a specific way for Pandoc compatibility.
  html <- c()
  for (dep in dependencies) {
    tags <- if (is.null(dep$src[["file"]])) {
      renderDependencies(list(dep), "href")
    } else {
      renderDependencies(list(dep), "file",
                         encodeFunc = identity,
                         hrefFilter = function(path) {
                           html_reference_path(path, lib_dir, output_dir)
                         })
    }
    html <- c(html, tags)
  }
  HTML(paste(html, collapse = "\n"))
}

# check class of passed list for 'html_dependency'
is_html_dependency <- function(list) {
  inherits(list, "html_dependency")
}

# validate that the passed list is a correctly formed html_dependency
validate_html_dependency <- function(list) {

  # ensure it's the right class
  if (!is_html_dependency(list))
    stop2("passed object is not of class html_dependency")

  # validate required fields
  if (is.null(list$name))
    stop2("name for html_dependency not provided")
  if (is.null(list$version))
    stop2("version for html_dependency not provided")
  list <- fix_html_dependency(list)

  # check src path or href are given
  if (is.null(list$src)) {
    stop2("src for html_dependency not provided")
  }
  if (!is.null(list$src$file)) {
    file <- list$src$file
    if (!is.null(list$package))
      file <- system.file(file, package = list$package)
    if (!file.exists(file)) {
      stop2("path for html_dependency not found: ", file)
    }
  }

  list
}

# monkey patch HTML dependencies; currently only supports highlight.js
fix_html_dependency <- function(list) {
  if (!identical(list$name, 'highlightjs') || !identical(list$version, '1.1'))
    return(list)
  if (!identical(list$src$file, '')) return(list)
  html_dependency_highlightjs(gsub('[.]css$', '', list$stylesheet))
}

# check if the passed knit_meta has any (e.g. html/latex) dependencies
has_dependencies <- function(knit_meta, class) {

  if (inherits(knit_meta, class))
    return(TRUE)

  if (is.list(knit_meta)) {
    for (dep in knit_meta) {
      if (is.null(names(dep))) {
        if (has_dependencies(dep, class))
          return(TRUE)
      } else {
        if (inherits(dep, class))
          return(TRUE)
      }
    }
  }
  FALSE
}

has_html_dependencies <- function(knit_meta) {
  has_dependencies(knit_meta, "html_dependency")
}

# create an html_dependency for pagedtable
#' @rdname html-dependencies
#' @export
html_dependency_pagedtable <- function() {
  htmlDependency(
    "pagedtable",
    version = "1.1",
    src = pkg_file("rmd/h/pagedtable-1.1"),
    script = "js/pagedtable.js",
    stylesheet = "css/pagedtable.css"
  )
}

#' @param highlight Highlighter to use
#' @rdname html-dependencies
#' @export
html_dependency_highlightjs <- function(highlight) {
  htmlDependency(
    "highlightjs",
    version = "9.12.0",
    src = pkg_file("rmd/h/highlightjs"),
    script = "highlight.js",
    stylesheet = paste0(highlight, ".css")
  )
}

# create an html_dependency for rsiframe
html_dependency_rsiframe <- function() {

  # add session port to meta if we have it (note that RStudio will
  # only provide this in desktop mode)
  meta <- NULL
  session_port <- Sys.getenv("RSTUDIO_SESSION_PORT")
  if (nzchar(session_port))
    meta <- list(rstudio_origin = paste0("127.0.0.1:", session_port))

  htmlDependency(
    "rstudio-iframe",
    version = "1.1",
    src = pkg_file("rmd/h/rsiframe-1.1"),
    script = "rsiframe.js",
    meta = meta
  )
}

# Pandoc 2.9 adds attributes on both headers and their parent divs. We remove
# the ones on headers since they are unnecessary (#1723).
html_dependency_header_attrs <- function() {
  if (getOption('rmarkdown.html_dependency.header_attr', TRUE) && pandoc_available('2.9')) {
    list(
      htmlDependency(
        "header-attrs",
        version = packageVersion("rmarkdown"),
        src = pkg_file("rmd/h/pandoc"),
        script = "header-attrs.js"
      )
    )
  }
}

# Store KaTeX as a html dependency to include in our template
html_dependency_katex <- function(href = NULL) {
  # supporting custom url
  if (is.null(href)) {
    href <- "https://cdn.jsdelivr.net/npm/katex@0.15.2/dist/"
    version <- "0.15.2"
    integrity <- "sha256-bgI9WhLOPSUlPrdgHUg3rPIB2vq+D+mYkUTM3q0U8fw="
  } else {
    # if a custom url is passed we can't set value we don't know
    integrity <- NULL
    # guessing version from a CDN
    r <- "^.*katex@([0-9.]+).*"
    version <- if (grepl(r, href)) {
      gsub(r, "\\1", href)
    } else  {
      "99.99.99"
    }
  }

  htmlDependency(
    name = "katex",
    version = version,
    src = c(href = href),
    script = c(
      list(src = "katex.min.js",
           'data-external' = "1",
           defer = NA),
      if (!is.null(integrity)) {
        list(integrity = integrity, crossorigin = 'anonymous')
      }
    ),
    # TODO: reactivate when we can set data-external = 1 in htmltools
    #
    # stylesheet = c("katex.min.css"),
    # from Pandoc
    # https://github.com/jgm/pandoc/blob/7ddb609aae48ec10ef9899b25c52a4abecfbc176/src/Text/Pandoc/Writers/HTML.hs#L313
    head = c(
      '<script>document.addEventListener("DOMContentLoaded", function () {',
      '  var mathElements = document.getElementsByClassName("math");',
      '  var macros = [];',
      '  for (var i = 0; i < mathElements.length; i++) {',
      '    var texText = mathElements[i].firstChild;',
      '    if (mathElements[i].tagName == "SPAN") {',
      '      katex.render(texText.data, mathElements[i], {',
      '        displayMode: mathElements[i].classList.contains("display"),',
      '        throwOnError: false,',
      '        macros: macros,',
      '        fleqn: false',
      '      });',
      '    }}});',
      '</script>',
      sprintf('<link rel="stylesheet" href="%skatex.min.css" data-external="1">', href)
    )
  )
}

#' @rdname html-dependencies
#' @export
html_dependency_tabset <- function() {
  htmlDependency(
    "tabset",
    version = "1.0",
    src = pkg_file("rmd/h/tabset"),
    script = "tabset.js",
    stylesheet = "tabset.css",
    all_files = FALSE
  )
}

#' @rdname html-dependencies
#' @export
html_dependency_codefolding_lua <- function() {
  htmlDependency(
    "codefolding-lua",
    version = "1.1",
    src = pkg_file("rmd/h/navigation-1.1"),
    stylesheet = "codefolding-lua.css",
    all_files = FALSE
  )
}
