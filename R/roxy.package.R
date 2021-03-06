#' Automatic doc creation, package building and repository update
#'
#' This function should help to create R packages with full documentation and updates to a local repository.
#' It supports source and binary packaging (Windows and Mac OS X; see Note section on the limitations).
#'
#' For the documentation \code{roxygen2}[1] is used. Next to the actual in-line documentation of the package's contents, you only need to
#' prepare a data.frame to be used to write a package \code{DESCRIPTION} file. See the example section for details on that. This means
#' that you \emph{neither} edit the \code{DESCRIPTION} \emph{nor} the \code{*-package.R} file manually, they will both be created \emph{automatically}
#' by this function with contents according to these settings!
#' 
#' @section Sandboxing:
#' If you want to check out the effects of roxy.package() without touching you actual package sources, try \code{\link[roxyPackage:sandbox]{sandbox}}
#' to set up a safe testing environment.
#'
#' @section Repository layout:
#' The repository will have this directory structure, that is, below the defined \code{repo.root}:
#'
#' \describe{
#'		\item{\code{./src/contrib}}{Here go the source packages}
#'		\item{\code{./bin/windows/contrib/$RVERSION}}{Here go the Windows binaries}
#'		\item{\code{./bin/macosx/leopard/contrib/$RVERSION}}{Here go the Mac OS X binaries}
#'		\item{\code{./pckg/index.html}}{A global package index with links to packages' index files, if actions included \code{"html"}}
#'		\item{\code{./pckg/web.css}}{A CRAN-style CSS file, if actions included \code{"html"}}
#'		\item{\code{./pckg/$PACKAGENAME}}{Here go documentation PDF and vignette, as well as a \code{ChangeLog} file, if found.
#'			and an \code{index.html} with package information, if actions included \code{"html"}.
#'			This is probably a bit off-standard, but practical if you several packages.}
#' }
#'
#' @section Converting ChangeLogs into NEWS:
#' See \code{\link[roxyPackage:cl2news]{cl2news}} for details.
#' 
#' @section Build for several R versions:
#' The options \code{R.libs} and \code{R.homes} can actually take more than one string, but a vector of strings. This can be used
#' to build packages for different R versions, provided you installed them on your system. If you're running GNU/Linux, an easy way
#' of doing so is to fetch the R sources from CRAN, calling \code{"./configure"} with something like \code{"--prefix=$HOME/R/<R version>"},
#' so that \code{"make install"} installs to that path. Let's assume you did that with R 2.12.2 and 2.11.1, you could then call \code{roxy.package}
#' with options like \code{R.homes=c("home/user/R/R-2.11.1", "home/user/R/R-2.12.2")} and \code{R.libs=c("home/user/R/R-2.11.1/lib64/R/library",}
#' \code{"home/user/R/R-2.12.2/lib64/R/library")}. \code{roxy.package} will then call itself recursively for each given R installation.
#' 
#' One thing you should be aware of is that \code{roxy.package} will not perform all actions each time. That is because some of them, namely
#' \code{"deb"}, \code{"roxy"}, \code{"cite"}, \code{"license"}, \code{"doc"}, \code{"cl2news"} and \code{"news2rss"}, should produce identical
#' results anyway, so they are only considered during the first run. You should always place the R version which should be linked to from the
#' HTML index last in line, because \code{"html"} will overwrite previous results.
#'
#' @section Windows: On Windows, the actions \code{"doc"} and \code{"check"} will only work correctly if you have installed and configured LaTeX
#' accordingly, and you will also need Rtools set up for packaging.
#'
#' @note The binary packaging is done simply by zipping (Windows) or targzipping (Mac OS X) the built and installed package. This should
#' do the trick as long as your package is written in pure R code. It will most likely not produce usable packages if it contains
#' code in other languages like C++.
#'
#' @param pck.source.dir Character string, path pointing to the root directory of your package sources.
#' @param pck.version Character string, defining the designated version number. Can be omitted if actions don't
#'		include \code{"roxy"}, then this information is read from the present DESCRIPTION file.
#' @param pck.description Data frame holding the package description (see Examples section).
#' @param R.libs Character string, valid path to the R library where the package should be installed to.
#' @param repo.root Character string, valid path to a directory where to build/update a local package repository.
#' @param pck.date Character string of the release date in YYYY-MM-DD format. Defaults to \code{Sys.Date()}. If actions don't
#'		include \code{"roxy"}, then this information is read from the present DESCRIPTION file.
#' @param actions Character vector, must contain at least one of the following values:
#'		\describe{
#'			\item{"roxy"}{Roxygenize the docs}
#'			\item{"cite"}{Update CITATION file}
#'			\item{"license"}{Update LICENSE.txt file; it's not called LICENSE to prevent an automatic installation}
#'			\item{"check"}{Do a full package check, calling \code{R CMD check}}
#'			\item{"package"}{Build & install the package, update source repository, calling \code{R CMD build} and \code{R CMD INSTALL}}
#'			\item{"cl2news"}{Try to convert a ChangeLog file into an NEWS.Rd file}
#'			\item{"news2rss"}{Try to convert \code{inst/NEWS.Rd} into an RSS feed. You must also set
#'				\code{URL} accordingly.}
#'			\item{"doc"}{Update PDF documentation and vignette (if present), \code{R CMD Rd2pdf} (or \code{R CMD Rd2dvi} for R < 2.15)}
#'			\item{"html"}{Update HTML index files}
#'			\item{"win"}{Update the Windows binary package}
#'			\item{"macosx"}{Update the Mac OS X binary package}
#'			\item{"log"}{Generate initial ChangeLog or update a present ChangeLog file}
#'			\item{"deb"}{Update the Debian binary package with \code{\link[roxyPackage:debianize]{debianize}} (works only on Debian systems;
#'				see \code{deb.options}, too)}
#'		}
#'		Note that \code{"cl2news"} will write the \code{NEWS.Rd} file to the \code{inst} directory of your sources, which will overwrite
#'		an existing file with the same name! Also note that if both a \code{NEWS/NEWS.Rd} and \code{ChangeLog} file are found, only
#'		news files will be linked by the \code{"html"} action.
#' @param local.roxy.dir Character string, path to a directory to roxygenize docs in, if you don't want to do it in place.
#'		If \code{NULL} (the default) or identical to \code{pck.source.dir}, docs will be created in place.
#' @param cleanup Logical, if \code{TRUE} will remove backup files (matching \code{.*~$} or \code{.*backup$}) from the source directory.
#' @param roxy.unlink.target Logical, setting the \code{unlink.target} option of \code{\link[roxygen2:roxygenize]{roxygenize}}
#' @param rm.vignette Logical, if \code{TRUE} and a vignette PDF was build during the \code{"doc"} action, it will not be kept
#'		in the source package but just be moved to the \code{./pckg/$PACKAGENAME} directory of the repository.
#' @param R.homes Path to the R installation to use. Can be set manually to build packages for other R versions than the default one,
#'		if you have installed them in parallel. Should probably be used together with \code{R.libs}.
#' @param html.index A character string for the headline of the global index HTML file.
#' @param html.title A character string for the title tag prefix of the package index HTML file.
#' @param Rcmd.options A named character vector with options to be passed on to the internal calls of \code{R CMD build},
#'		\code{R CMD INSTALL}, \code{R CMD check} and \code{R CMD Rd2pdf} (or \code{R CMD Rd2dvi} for R < 2.15). Change these only if you know what you're doing!
#'		Will be passed on as given here. To deactivate, options must explicitly be se to \code{""}, missing options will be used with the default values.
#' @param URL A character string defining the URL to the root of the repository (i.e., which holds the directories \code{src}
#'		etc.). This is not the path to the local file system, but should be the URL to the repository as it is available
#'		via internet. This option is neccessary for (and only interpreted by) the action \code{"news2rss"}.
#' @param deb.options A named list with parameters to pass through to \code{\link[roxyPackage:debianize]{debianize}}. By default, \code{pck.source.dir}
#'		and \code{repo.root} are set to the values given to the parameters above. As for the other options, if not set, the defaults of \code{debianize}
#'		will be used.
#' @param ChangeLog A named list of character vectors with log entry items. The element names will be used as section names in the ChangeLog entry,
#'		and each character string in a vector will be pasted as a log item. The news you provide here will be appended to probably present news, while
#'		trying to prevent duplicate entries to appear. If you need more control, don't use the \code{"log"} action, but have a look at
#'		\code{\link[roxyPackage:updateChangeLog]{updateChangeLog}}. Also note that the date of altered entries will be updated automatically, unless
#'		you don't call the \code{"roxy"} action, too.
#' @param ... Additional options passed through to \code{roxygenize}.
#' @references
#' [1] \url{http://cran.r-project.org/web/packages/roxygen2/}
#' @seealso \code{\link[roxyPackage:sandbox]{sandbox}} to run roxy.package() in a sandbox.
#' @export
#' @examples
#' \dontrun{
#' ## package description as data.frame:
#' pckg.dscrptn <- data.frame(
#'   Package="SquareTheCircle",
#'   Type="Package",
#'   Title="Squaring the circle using Heisenberg compensation",
#'   Author="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   AuthorR="c(person(given=\"Ernst\", family=\"Dölle\",
#'   email=\"doelle@@eternalwondermaths.example.org\", role=c(\"aut\", \"cre\")))",
#'   Maintainer="E.A. Dölle <doelle@@eternalwondermaths.example.org>",
#'   Depends="R (>= 2.10.0),heisenberg (>= 0.23),tools",
#'   Enhances="rkward",
#'   Description="This package squares the circle using Heisenberg compensation.
#'       The code came from a meeting with Yrla Nor that i had in a dream. Please
#'       don't forget to chain your computer to the ground, because these
#'       algorithms might make it fly.",
#'   License="GPL (>= 3)",
#'   Encoding="UTF-8",
#'   LazyLoad="yes",
#'   URL="http://eternalwondermaths.example.org",
#'   stringsAsFactors=FALSE)
#' # hint no. 1: you *don't* specify version number and release date here,
#' #   but all other valid fields for DESCRIPTION files must/can be defined
#' # hint no. 2: most of this rarely changes, so you can add this to the
#' #   internals of your package and refer to it as
#' #   roxy.package(pck.description=SquareTheCircle:::pckg.dscrptn, ...)
#' # hint no. 3: use "AuthorR" for the "Author@@R" field, or "AuthorsR" for R >= 2.14, to work around naming
#' # problems
#'
#' roxy.package(pck.source.dir="~/my_R_stuff/SquareTheCircle",
#'   pck.version="0.01-2",
#'   pck.description=pckg.dscrptn,
#'   R.libs="~/R",
#'   repo.root="/var/www/repo",
#'   actions=c("roxy", "package", "doc"))
#' }

roxy.package <- function(
	pck.source.dir,
	pck.version,
	pck.description,
	R.libs,
	repo.root,
	pck.date=Sys.Date(),
	actions=c("roxy", "package"),
	local.roxy.dir=NULL,
	cleanup=FALSE,
	roxy.unlink.target=TRUE,
	rm.vignette=FALSE,
	R.homes=R.home(),
	html.index="Available R Packages",
	html.title="R package",
	Rcmd.options=c(
		install="",
		build="--no-manual --no-vignettes",
		check="--as-cran",
		Rd2pdf="--pdf --no-preview"),
	URL=NULL,
	deb.options=NULL,
	ChangeLog=list(changed=c("initial release"), fixed=c("missing ChangeLog")),
	...){

	# avoid some NOTEs from R CMD check
	AuthorR <- AuthorsR <- Author.R <- Authors.R <- NULL

	# check the OS first
	unix.OS <- isUNIX()

	# let's check if packages are to be build for several R versions
	R.versions <- length(R.homes)
	R.libraries <- length(R.libs)
	if(R.versions > 1){
		if(R.libraries != R.versions){
			stop(simpleError("If you specify more than one R.home, you must also define as many R.libs!"))
		} else {}
		# if so, iterate recursively through it and then end
		for (this.R in 1:R.versions){
			this.home <- R.homes[this.R]
			this.libs <- R.libs[this.R]
			if(this.R > 1){
				# for the time being, debianizing the package once is enough
				actions <- actions[!actions %in% "deb"]
				# well, the same is true for some other actions
				actions <- actions[!actions %in% c("roxy", "cite", "license", "doc", "cl2news", "news2rss")]
			} else {}
			roxy.package(
				pck.source.dir=pck.source.dir,
				pck.version=pck.version,
				pck.description=pck.description,
				R.libs=this.libs,
				repo.root=repo.root,
				pck.date=pck.date,
				actions=actions,
				local.roxy.dir=local.roxy.dir,
				cleanup=cleanup,
				roxy.unlink.target=roxy.unlink.target,
				rm.vignette=rm.vignette,
				R.homes=this.home,
				html.index=html.index,
				html.title=html.title,
				Rcmd.options=Rcmd.options,
				URL=URL,
				deb.options=deb.options,
				ChangeLog=ChangeLog,
				...)
		}
		return(invisible(NULL))
	} else {}

	old.dir <- getwd()
	on.exit(setwd(old.dir))

	# fist see if we need to rename an "AuthorR" field. data.frame definitions tend to
	# replace the "@" with a dot
	if("AuthorR" %in% names(pck.description)){
		pck.description$`Author@R` <- pck.description[["AuthorR"]]
		pck.description <- subset(pck.description, select=-AuthorR)
	} else if("AuthorsR" %in% names(pck.description)){
		pck.description$`Authors@R` <- pck.description[["AuthorsR"]]
		pck.description <- subset(pck.description, select=-AuthorsR)
	} else if("Author.R" %in% names(pck.description)){
		pck.description$`Author@R` <- pck.description[["Author.R"]]
		pck.description <- subset(pck.description, select=-Author.R)
	} else if("Authors.R" %in% names(pck.description)){
		pck.description$`Authors@R` <- pck.description[["Authors.R"]]
		pck.description <- subset(pck.description, select=-Authors.R)
	} else {}

	# normalize all root paths
	R.homes <- normalizePathByOS(path=R.homes, is.unix=unix.OS, mustWork=TRUE)
	repo.root <- normalizePathByOS(path=repo.root, is.unix=unix.OS, mustWork=FALSE)
	pck.source.dir <- normalizePathByOS(path=pck.source.dir, is.unix=unix.OS, mustWork=TRUE)

	# get info on the R version used
	R.Version.full <- getRvers(R.homes=R.homes)
	R.Version.win <- getRvers(R.homes=R.homes, win=TRUE)
	if(nchar(R.Version.full) < 3 | nchar(R.Version.win) < 3){
		stop(simpleError("R version number cannot be detected, it seems."))
	} else {}

	# special cases: if actions do not include "roxy",
	# take infos from the DESCRIPTION file
	if(!"roxy" %in% actions | is.null(pck.description)){
		pck.dscrptn <- as.data.frame(read.dcf(file=file.path(pck.source.dir, "DESCRIPTION")), stringsAsFactors=FALSE)
		# clean from newlines
		pck.dscrptn <- as.data.frame(t(sapply(pck.dscrptn, function(x) gsub("\n", " ", x))), stringsAsFactors=FALSE)
		pck.version <- getDescField(pck.dscrptn, field="Version")
		# if "Date" is missing, try some fallbacks
		pck.date <- as.character(as.Date(getDescField(pck.dscrptn, field=c("Date","Packaged","Date/Publication"))))
		pck.package <- getDescField(pck.dscrptn, field="Package")
		pck.title <- getDescField(pck.dscrptn, field="Title")
		rss.description <- getDescField(pck.dscrptn, field="Description")
		pckg.license <- getDescField(pck.dscrptn, field="License")
		pckg.dscrptn <- pck.description <- pck.dscrptn
	} else {
		rss.description <- pck.description[["Description"]]
		pck.package <- roxy.description("package", description=pck.description)
		pck.title <- roxy.description("title", description=pck.description)
		pckg.dscrptn <- roxy.description("description", description=pck.description, version=pck.version, date=as.character(pck.date), R.vers=R.Version.full)
		pckg.license <- pck.description[["License"]]
	}
	pckg.package <- roxy.description("pckg.description", description=pck.description, version=pck.version, date=as.character(pck.date))

	## check for sandboxing
	if(isTRUE(check.sandbox())){
		message("preparing sandbox...")
		# prepare folder structure; this will also insure sane values and abort
		# if locations are invalid. the function returns a list of paths to use
		adjust.paths <- prepare.sandbox(
			package=pck.package,
			description=pckg.dscrptn,
			pck.source.dir=pck.source.dir,
			R.libs=R.libs,
			R.version=R.Version.full,
			repo.root=repo.root)
		# replace paths with sandbox
		pck.source.dir <- adjust.paths[["pck.source.dir"]]
		R.libs <- adjust.paths[["R.libs"]]
		repo.root <- adjust.paths[["repo.root"]]
		sandbox.status()
	} else {}

	# check environment
	R.bin <- file.path(R.homes, "bin", "R")
	message(paste("R environment\n  R.home:", R.homes, "\n  R.libs:", R.libs))

	repo.src.contrib <- file.path(repo.root, "src", "contrib")
	repo.win <- file.path(repo.root, "bin", "windows", "contrib", R.Version.win)
	repo.macosx <- file.path(repo.root, "bin", "macosx","leopard", "contrib", R.Version.win)
	repo.pckg.info.main <- file.path(repo.root, "pckg")
	repo.pckg.info <- file.path(repo.pckg.info.main, pck.package)
	pckg.basename <- paste0(pck.package, "_", pck.version)
	pckg.name.src <- paste0(pckg.basename, ".tar.gz")
	pckg.name.win <- paste0(pckg.basename, ".zip")
	pckg.name.mac <- paste0(pckg.basename, ".tgz")
	pckg.inst.dir <- file.path(pck.source.dir, "inst")
	pckg.cite.file <- file.path(pckg.inst.dir, "CITATION")
	pckg.cite.file.html <- file.path(repo.pckg.info, "citation.html")
	src.changelog <- file.path(pck.source.dir, "ChangeLog")
	pckg.changelog <- file.path(repo.pckg.info, "ChangeLog")
	repo.src.gz <- file.path(repo.src.contrib, pckg.name.src)
	win.package <- file.path(repo.win, pckg.name.win)
	macosx.package <- file.path(repo.macosx, pckg.name.mac)
	pckg.NEWS.Rd <- file.path(pckg.inst.dir, "NEWS.Rd")
	pckg.NEWS.inst <- file.path(pckg.inst.dir, "NEWS")
	pckg.NEWS <- file.path(pck.source.dir, "NEWS")
	pckg.NEWS.html <- file.path(repo.pckg.info, "NEWS.html")
	RSS.file.name <- "RSS.xml"
	pckg.NEWS.rss <- file.path(repo.pckg.info, RSS.file.name)
	pckg.license.file <- file.path(pck.source.dir, "LICENSE.txt")
	pckg.license.file.old <- file.path(pck.source.dir, "LICENSE")
	pckg.pdf.doc <- paste0(pck.package, ".pdf")
	pckg.vignette.dir <- tools:::pkgVignettes(dir=pck.source.dir)$dir

	# debian package specific stuff, probably needed for "html" action
	deb.defaults <- formals(debianize)
	deb.defaults[["pck.source.dir"]] <- pck.source.dir
	deb.defaults[["repo.root"]] <- repo.root
	deb.args.def <- names(deb.defaults)
	deb.args.set <- names(deb.options)
	deb.defaults[deb.args.set[deb.args.set %in% deb.args.def]] <- deb.options[deb.args.set[deb.args.set %in% deb.args.def]]
	# try to set pckg.name.deb and deb.repo.path
	# this will only work if repo.root is unchanged, the rest is too messy now...
	deb.repo.path.part <- paste0("deb/dists/", deb.defaults[["distribution"]], "/", deb.defaults[["component"]], "/", deb.defaults[["arch"]])
	deb.repo.path <- paste0("../../", deb.repo.path.part)
	pckg.name.deb.part <- gsub("\\.", "-", tolower(paste("r", deb.defaults[["origin"]], pck.package, sep="-")))
	pckg.name.deb <-  paste0(pckg.name.deb.part, "_", pck.version, "-", deb.defaults[["revision"]], "_", deb.defaults[["arch"]], ".deb")
	deb.package <- file.path(repo.root, deb.repo.path.part, pckg.name.deb)

	# check for additional CMD options
	if("build" %in% names(Rcmd.options)){
		Rcmd.opt.build <- paste0(Rcmd.options[["build"]], " ")
	} else {
		# --no-manual was introduced with R 2.12, need version check here
		Rcmd.opt.build <- ifelse(isTRUE(R_system_version(R.Version.full) < "2.12"), "--no-vignettes ", "--no-manual --no-vignettes ")
	}
	Rcmd.opt.install <- ifelse("install" %in% names(Rcmd.options), paste0(Rcmd.options[["install"]], " "), "")
	# --as-cran was introduced with R 2.15, strip if this is an older version
	if(isTRUE(R_system_version(R.Version.full) < "2.15")){
		Rcmd.options[["check"]] <- gsub("--as-cran", "", Rcmd.options[["check"]])
	} else {}
	Rcmd.opt.check <- ifelse("check" %in% names(Rcmd.options), paste0(Rcmd.options[["check"]], " "), "")
	# R 2.15 switched from Rd2dvi to Rd2pdf
	Rcmd.cmd.Rd2pdf <- ifelse(isTRUE(R_system_version(R.Version.full) < "2.15"), "Rd2dvi", "Rd2pdf")
	if("Rd2pdf" %in% names(Rcmd.options)){
		Rcmd.opt.Rd2pdf <- paste0(Rcmd.options[["Rd2pdf"]], " ")
	} else if("Rd2dvi" %in% names(Rcmd.options)){
		warning("Rcmd.options: Rd2dvi is now called Rd2pdf, please update your scripts! used the settings anyway, though.", call.=FALSE)
		Rcmd.opt.Rd2pdf <- paste0(Rcmd.options[["Rd2dvi"]], " ")
	} else {
		Rcmd.opt.Rd2pdf <- "--pdf --no-preview "
	}

	# check for/create info directory
	createMissingDir(dirPath=repo.pckg.info, action="repo")
	
	# clean up
	if(isTRUE(cleanup)){
		unlink(list.files(pck.source.dir, pattern=".*~$", full.names=TRUE, recursive=TRUE))
		unlink(list.files(pck.source.dir, pattern=".*backup$", full.names=TRUE, recursive=TRUE))
	} else {}

	if("license" %in% actions){
		if(checkLicence(pckg.license)){
			copyLicence(pckg.license, pckg.license.file, overwrite=TRUE)
			if(file.exists(pckg.license.file.old)){
				warning("license: you have both LICENSE and LICENSE.txt in your project! if LICENSE is one of the standard licenses, please rename it to prevent its intallation.", call.=FALSE)
			} else {}
		} else {
			stop(simpleError(paste0("license: unrecognized license (", pckg.license, "), please provide your own LICENSE file! ")))
		}
	} else {}

	if("roxy" %in% actions){
		if(is.null(local.roxy.dir)){
			local.roxy.dir <- pck.source.dir
		} else {}
		# re-write DESCRIPTION files
		write.dcf(pckg.dscrptn, file=file.path(pck.source.dir, "DESCRIPTION"))
		# copy DESCRIPTION to pckg directory for easy HTML indexing
		stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
		pckg.package.file.R <- file.path(pck.source.dir, "R", paste0(pck.package, "-package.R"))
		cat(paste(pckg.package), file=pckg.package.file.R)
		message(paste0("roxy: updated ", pckg.package.file.R, "."))
# #		if(isTRUE(roxy2)){
# 			roxygenize(pck.source.dir, roxygen.dir=local.roxy.dir, unlink.target=roxy.unlink.target, ...)
# #		} else {
# #			require(roxygen)
# #			roxygen:::roxygenize(pck.source.dir, roxygen.dir=local.roxy.dir, use.Rd2=TRUE, unlink.target=TRUE, ...)
# #		}
		roxygenVersion <- get.roxyEnv("roxygenVersion")
		if(roxygenVersion == 3){
			# unforunately, there is no roxygen3 package, and there probably never will be. so, in case
			# you want to test it, you have to build, install and load it yourself, since we cannot
			# officially import it's functions. that would invalidate a package check for roxyPackage
			roxygen3IsLoaded <- any(grepl("package:roxygen3", search()))
			if(roxygen3IsLoaded){
				roxygenise(pck.source.dir, ...)
			} else {
				stop(simpleError("roxy: you want to use roxygen3, but the package isn't loaded. please call:\n  require(roxygen3)"))
			}
		} else {
			roxygen2::roxygenize(pck.source.dir, roxygen.dir=local.roxy.dir, unlink.target=roxy.unlink.target, ...)
		}
	} else {}

	if("cite" %in% actions){
		createMissingDir(dirPath=pckg.inst.dir, action="cite")
		# calling internal function to generate citation object
		cite.obj <- citationText(pck.dscr=pck.description, pck.version=pck.version, pck.date=pck.date)
		cat(cite.obj, file=pckg.cite.file)
		message(paste0("cite: updated ", pckg.cite.file, "."))
		if("html" %in% actions){
			cite.obj.html <- roxy.html.cite(cite.obj=eval(parse(text=cite.obj)), page.css="../web.css", package=pck.package)
			cat(cite.obj.html, file=pckg.cite.file.html)
			message(paste0("cite: updated ", pckg.cite.file.html, "."))
		} else {}
	} else {}

	if("check" %in% actions){
		# check for examples check file before
		chk.ex.file <- file.path(pck.source.dir, paste0(pck.package, "-Ex.R"))
		chk.ex.file.present <- ifelse(file_test("-f", chk.ex.file), TRUE, FALSE)
		jmp.back <- getwd()
		tryCatch(chk.out.dir <- tempdir(), error=function(e) stop(e))
		setwd(chk.out.dir)
		message(paste0("check: calling R CMD check, this might take a while..."))
		if(isTRUE(unix.OS)){
			r.cmd.check.call <- paste0("R_LIBS_USER=", R.libs, " ; ",
				R.bin, " CMD check ", Rcmd.opt.check, pck.source.dir)
			print(system(r.cmd.check.call, intern=TRUE))
		} else {
			r.cmd.check.call <- paste0("set R_LIBS_USER=", shQuote(R.libs, type="cmd"), " && ",
				R.bin, " CMD check ", Rcmd.opt.check, shQuote(pck.source.dir, type="cmd"))
			print(shell(r.cmd.check.call, translate=TRUE, intern=TRUE))
		}
		on.exit(message(paste0("check: saved results to ", chk.out.dir, "/", pck.package, ".Rcheck")), add=TRUE)
		setwd(jmp.back)
		# need to clean up?
		if(!isTRUE(chk.ex.file.present) & file_test("-f", chk.ex.file)){
			# there's an example file which wasn't here before
			unlink(chk.ex.file)
		} else {}
	} else {}

	if("log" %in% actions){
		if(!file.exists(src.changelog)){
			newLog <- initChangeLog(entry=ChangeLog, package=pck.package, version=pck.version, date=pck.date)
			writeChangeLog(log=newLog, file=src.changelog)
			message("log: generated initial ChangeLog")
		} else {
			oldLog <- readChangeLog(src.changelog)
			newLog <- updateChangeLog(oldLog, entry=ChangeLog, version=pck.version, date=pck.date, append=TRUE)
			newLogEntry <- getChangeLogEntry(newLog, pck.version)
			if(!identical(oldLog, newLog)){
				writeChangeLog(log=newLog, file=src.changelog)
				message("log: updated ChangeLog")
				print(newLogEntry)
			} else {
				message("log: ChangeLog is up-to-date, skipped")
			}
		}
	} else {}

	if("package" %in% actions){
		if("roxy" %in% actions){
			if(!identical(local.roxy.dir, pck.source.dir)){
				stopifnot(file.copy(Sys.glob(file.path(local.roxy.dir, "man", "*")), file.path(pck.source.dir, "man"), overwrite=TRUE))
				message("build: copied Rd files from roxygen to man.")
				stopifnot(file.copy(file.path(local.roxy.dir, "NAMESPACE"), file.path(pck.source.dir, "NAMESPACE"), overwrite=TRUE))
				message("build: copied NAMESPACE from roxygen to build dir.")
			} else {}
		} else {}

		## fill source repo
		createMissingDir(dirPath=repo.src.contrib, action="repo")
		jmp.back <- getwd()
		setwd(file.path(pck.source.dir, ".."))
		if(isTRUE(unix.OS)){
			r.cmd.build.call <- paste0(R.bin, " CMD build ", Rcmd.opt.build, pck.source.dir)
			system(r.cmd.build.call, intern=TRUE)
		} else {
			r.cmd.build.call <- paste0(R.bin, " CMD build ", Rcmd.opt.build, shQuote(pck.source.dir, type="cmd"))
			shell(r.cmd.build.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
		}
		file.mv(from=file.path(pck.source.dir,"..",pckg.name.src), to=repo.src.gz, overwrite=TRUE)
		setwd(jmp.back)
		message(paste0("repo: copied ", pckg.name.src, " to src/contrib."))
		# install.packages() doesn't work if we want to build for/with other R installations than
		# the actual running one, so we'll use  R CMD INSTALL instead
		if(isTRUE(unix.OS)){
			r.cmd.install.call <- paste0(R.bin, " CMD INSTALL -l ", R.libs, " ",
				Rcmd.opt.install, pck.source.dir)
			system(r.cmd.install.call, intern=TRUE)
		} else {
			r.cmd.install.call <- paste0(R.bin, " CMD INSTALL -l ", shQuote(R.libs, type="cmd"), " ",
				Rcmd.opt.install, shQuote(pck.source.dir, type="cmd"))
			shell(r.cmd.install.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
		}
		message("build: built and installed package")
		tools:::write_PACKAGES(dir=repo.src.contrib, type="source", verbose=TRUE, latestOnly=FALSE)
		message("repo: updated src/contrib/PACKAGES (source)")

		## update ChangeLog
		if(file.exists(src.changelog)){
			stopifnot(file.copy(src.changelog, pckg.changelog, overwrite=TRUE))
			message("pckg: updated ChangeLog")
		} else {}
	} else {}

	## create NEWS.Rd from ChangeLog
	if("cl2news" %in% actions){
		cl2news(log=src.changelog, news=pckg.NEWS.Rd, codify=TRUE, overwrite=TRUE)
	} else {}

	## create RSS feed from NEWS.Rd
	add.RSS <- FALSE
	if("news2rss" %in% actions){
		if(is.null(URL)){
			warning("news: no URL specified, RSS feed creation was skipped!", call.=FALSE)
		} else {
			package.URL <- paste(gsub("/*$", "", URL), "pckg", pck.package, sep="/")
			RSS.atom.URL <- paste(package.URL, RSS.file.name, sep="/")
			news2rss(
				news=pckg.NEWS.Rd, rss=pckg.NEWS.rss, html=FALSE, encoding="UTF-8",
				channel=c(
					title=pck.package,
					link=package.URL,
					description=rss.description,
					atom=RSS.atom.URL))
			add.RSS <- TRUE
		}
	} else {}

	## update PDF docs
	if("doc" %in% actions){
		if(!is.null(pckg.vignette.dir)){
			# create and move vignette
			tools:::buildVignettes(dir=pck.source.dir)
			# check for possible vignette documents
			# becomes character(0) if none found
			pdf.vignette.files <- list.files(pckg.vignette.dir, pattern="*.pdf", ignore.case=TRUE)
			createMissingDir(file.path(R.libs, pck.package, "doc"), action="doc")
			for(thisVignette in pdf.vignette.files){
				pdf.vignette.src <- file.path(pckg.vignette.dir, thisVignette)
				pdf.vignette.dst <- file.path(R.libs, pck.package, "doc", thisVignette)
				stopifnot(file.copy(pdf.vignette.src, pdf.vignette.dst, overwrite=TRUE))
				if(isTRUE(rm.vignette)){
					stopifnot(file.remove(pdf.vignette.src))
				} else {}
				message(paste0("build: created PDF vignette (", thisVignette, ")"))
				## copy vignettes
				pdf.vignette.repo <- file.path(repo.pckg.info, thisVignette)
				if(file.exists(pdf.vignette.dst)){
					stopifnot(file.copy(pdf.vignette.dst, pdf.vignette.repo, overwrite=TRUE))
					message(paste0("repo: updated vignette (", thisVignette, ")"))
				} else {}
			}
		} else {}

		pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
		removeIfExists(filePath=pdf.docs)
		if(isTRUE(unix.OS)){
			r.cmd.doc.call <- paste0(R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", pdf.docs, " ", pck.source.dir)
			system(r.cmd.doc.call, intern=TRUE)
		} else {
			r.cmd.doc.call <- paste0(R.bin, " CMD ", Rcmd.cmd.Rd2pdf, " ", Rcmd.opt.Rd2pdf, "--output=", shQuote(pdf.docs, type="cmd"), " ", shQuote(pck.source.dir, type="cmd"))
			shell(r.cmd.doc.call, translate=TRUE, ignore.stderr=TRUE, intern=TRUE)
		}
		message("build: created PDF docs")
	} else {}

	## fill windows repo
	if("win" %in% actions){
		createMissingDir(dirPath=repo.win, action="repo")
		removeIfExists(filePath=win.package)
		jmp.back <- getwd()
		setwd(R.libs)
		# make a list of backup files to exclude
		win.exclude.files <- unique(list.files(pck.package, pattern=".*~$", recursive=TRUE))
		if(length(win.exclude.files) > 0){
			win.exclude.files <- paste0("-x \"", paste(file.path(pck.package, win.exclude.files), collapse="\" \""), "\"")
		} else {}
		suppressWarnings(zip(win.package, pck.package, extras=win.exclude.files))
		message(paste0("repo: created ", pckg.name.win, " (windows)"))
		setwd(jmp.back)
		tools:::write_PACKAGES(dir=repo.win, type="win.binary", verbose=TRUE, latestOnly=FALSE)
		message("repo: updated bin/PACKAGES (windows)")
	} else {}

	## fill macosx repo
	if("macosx" %in% actions){
		createMissingDir(dirPath=repo.macosx, action="repo")
		removeIfExists(filePath=macosx.package)
		# since not all tar implementations (especially the BSD default on Mac OS X) support --exclude-vcs,
		# we'll exclude these manually
		VCS.directories <- c(".svn", "CVS", ".git", "_darcs", ".hg")
		VCS.allDirs <- list.dirs(file.path(R.libs,pck.package))
		VCS.excludeDirs <- VCS.allDirs[grepl(paste(paste0(".*", VCS.directories, "$"), collapse="|"), VCS.allDirs)]
		if(length(VCS.excludeDirs) > 0){
			tar.extraFlags <- paste(paste0(" --exclude='", VCS.excludeDirs, "'"), collapse="")
			message(paste("repo: excluded these directories from the mac binary:\n  ", VCS.excludeDirs, collapse="\n  "))
		} else {
			tar.extraFlags <- ""
		}
		# failsafe exclusion of backup files
		# --exclude=*\\~ caused trouble for path names with tilde
		tilde.allFiles <- list.files(file.path(R.libs,pck.package))
		tilde.excludeFiles <- tilde.allFiles[grepl(paste(paste0(".*~$"), collapse="|"), tilde.allFiles)]
		if(length(tilde.excludeFiles) > 0){
			tar.extraFlags <- paste(paste0(" --exclude='", tilde.excludeFiles, "'"), tar.extraFlags, collapse="")
			message(paste("repo: excluded these files from the mac binary:\n  ", tilde.excludeFiles, collapse="\n  "))
		} else {}

		jmp.back <- getwd()
		setwd(R.libs)
		tar(macosx.package, files=pck.package,
			tar=Sys.which("tar"),
			compression="gzip", extra_flags=paste("-h ", tar.extraFlags))
		message(paste0("repo: created ", pckg.name.mac, " (mac OS X)"))
		setwd(jmp.back)
		tools:::write_PACKAGES(dir=repo.macosx, type="mac.binary", verbose=TRUE, latestOnly=FALSE)
		message("repo: updated bin/PACKAGES (mac OS X)")
	} else {}

	## fill debian repo
	if("deb" %in% actions){
		formals(debianize) <- deb.defaults
		debianize()
	} else {}

	## update HTML index
	if("html" %in% actions){
		if(!"roxy" %in% actions){
			# copy DESCRIPTION to pckg directory
			stopifnot(file.copy(file.path(pck.source.dir, "DESCRIPTION"), file.path(repo.pckg.info, "DESCRIPTION"), overwrite=TRUE))
		} else {}
		# write CSS file, if none is present
		css.file <- file.path(repo.pckg.info.main, "web.css")
		if(!file_test("-f", css.file)){
			cat(rx.css(), file=css.file)
			message(paste0("html: created CSS file ", css.file))
		} else {}
		# copy RSS image, if not present
		RSS.image <- file.path(repo.pckg.info.main, "feed-icon-14x14.png")
		if(!file_test("-f", RSS.image)){
			RSS.local.image <- file.path(roxyPackage.lib.dir(), "images", "feed-icon-14x14.png")
			stopifnot(file.copy(RSS.local.image, RSS.image))
			message(paste0("html: copied RSS image to ", RSS.image))
		} else {}
		# check for binaries to link
		url.src <- url.win <- url.mac <- url.deb <- url.doc <- url.vgn <- deb.repo <- NULL
		if(file_test("-f", repo.src.gz)){
			url.src <- pckg.name.src
		} else {}
		if(file_test("-f", win.package)){
			url.win <- pckg.name.win
		} else {}
		if(file_test("-f", macosx.package)){
			url.mac <- pckg.name.mac
		} else {}
		url.debRepo.info <- NULL
		if(file_test("-f", deb.package)){
			if(!is.null(URL)){
				# generate repository info
				url.debRepo.info <- file.path(repo.pckg.info, "deb_repo.html")
				cat(debRepoInfo(
					URL=URL,
					dist=deb.defaults[["distribution"]],
					comp=deb.defaults[["component"]],
					package=pckg.name.deb.part,
					repo=deb.defaults[["origin"]],
					gpg.key=deb.defaults[["gpg.key"]],
					page.css="../web.css",
					package.full=pckg.name.deb,
					repo.path=deb.repo.path),
				file=url.debRepo.info)
				message(paste0("html: updated ", url.debRepo.info))
			} else {
				message("html: you need to specify 'URL' to generate debian repository information!")
			}
		} else {}
		# check if there is actually any built debian package in the repo
		# if not, the link to the debian installation notes will be omitted
		deb.built.packages <- dir(file.path(repo.root, deb.repo.path.part), pattern=paste0(pckg.name.deb.part, ".*", ".deb"))
		url.deb.repo <- NULL
		if(length(deb.built.packages) > 0){
			url.deb.repo <- "deb_repo.html"
		} else {}
		# check for docs to link
		pdf.docs.repo.files <- list.files(repo.pckg.info, pattern="*.pdf", ignore.case=TRUE)
		pdf.docs <- file.path(repo.pckg.info, pckg.pdf.doc)
		pdf.vignette.repo <- pdf.docs.repo.files[!pdf.docs.repo.files %in% pckg.pdf.doc]
		if(file_test("-f", pdf.docs)){
			url.doc <- pckg.pdf.doc
		} else {}
		if(length(pdf.vignette.repo) > 0){
			url.vgn <- pdf.vignette.repo
		} else {}
		# check for NEWS.Rd or NEWS file
		if(file_test("-f", pckg.NEWS.Rd)){
			roxy.NEWS2HTML(newsRd=pckg.NEWS.Rd, newsHTML=file.path(repo.pckg.info, "NEWS.html"), pckg=pck.package, css="../web.css", R.version=R.Version.full)
			url.NEWS <- pckg.NEWS.html
		} else if(file_test("-f", pckg.NEWS.inst)){
			stopifnot(file.copy(pckg.NEWS.inst, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
			url.NEWS <- file.path(repo.pckg.info, "NEWS")
		} else if(file_test("-f", pckg.NEWS)){
			stopifnot(file.copy(pckg.NEWS, file.path(repo.pckg.info, "NEWS"), overwrite=TRUE))
			url.NEWS <- file.path(repo.pckg.info, "NEWS")
		} else {
			url.NEWS <- ""
		}
		# generate package index file
		if(!file_test("-f", pckg.NEWS.rss)){
			RSS.file.name <- NULL
		} else {}
		package.html <- roxy.html(pckg.dscrptn, index=FALSE, css="web.css", R.version=R.Version.win,
			url.src=url.src, url.win=url.win, url.mac=url.mac, url.doc=url.doc, url.vgn=url.vgn,
			url.deb.repo=url.deb.repo,
			title=html.title, cite=pckg.cite.file.html, news=url.NEWS,
			changelog=pckg.changelog, rss.file=RSS.file.name)
		target.file.pckg <- file.path(repo.pckg.info, "index.html")
		cat(package.html, file=target.file.pckg)
		message(paste0("html: updated ", target.file.pckg))
		# now generate the global index file by scanning for DESCRIPTIONs in all pckg folders
		all.descs <- list()
		for (this.elmt in file.path(repo.pckg.info.main, dir(repo.pckg.info.main))){
				desc.path <- file.path(this.elmt, "DESCRIPTION")
				if(file_test("-f", desc.path)){
					# ok, there a DESCRIPTION file
					all.descs[[length(all.descs) + 1]] <- read.dcf(desc.path)
				} else {}
			}
		target.file.pckg <- file.path(repo.pckg.info.main, "index.html")
		pckg.index.html <- roxy.html(all.descs, index=TRUE, css="web.css", title=html.index)
		cat(pckg.index.html, file=target.file.pckg)
		message(paste0("html: updated pckg index ", target.file.pckg))
		target.file.glob <- file.path(repo.root, "index.html")
		global.html <- roxy.html(all.descs, index=TRUE, css="web.css", title=html.index, redirect="pckg/")
		cat(global.html, file=target.file.glob)
		message(paste0("html: updated global index ", target.file.glob))
	} else {}

	return(invisible(NULL))
} ## end function roxy.package()
