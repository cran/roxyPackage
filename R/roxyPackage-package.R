#' Utilities to automate package builds.
#'
#' \tabular{ll}{
#' Package: \tab roxyPackage\cr
#' Type: \tab Package\cr
#' Version: \tab 0.03-6\cr
#' Date: \tab 2013-04-07\cr
#' Depends: \tab R (>= 2.9.0),methods,roxygen2,tools,XiMpLe (>= 0.03-12)\cr
#' Encoding: \tab UTF-8\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' URL: \tab http://reaktanz.de/?c=hacking&s=roxyPackage\cr
#' }
#'
#' The intention of this package is to make packaging R code as
#' easy as possible. roxyPackage uses tools from the roxygen2 package to generate documentation. It also automatically
#' generates and updates files like *-package.R, DESCRIPTION, CITATION, ChangeLog and NEWS.Rd. Building packages
#' supports source format, as well as several binary formats (MS Windows, Mac OS X, Debian GNU/Linux) if the
#' package contains pure R code only. The packages built are stored in a fully functional local R package repository
#' which can be synced to a web server to share them with others. This includes the generation of browsable HTML
#' pages similar to CRAN, with support for RSS feeds from the ChangeLog. Please read the vignette for a more detailed
#' explanation by example.
#'
#' @aliases roxyPackage-package roxyPackage
#' @name roxyPackage-package
#' @docType package
#' @title The roxyPackage Package
#' @author Meik Michalke \email{meik.michalke@@hhu.de}
#' @keywords package
NULL
