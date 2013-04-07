# internal package description
pckg.dscrptn <- data.frame(
		Package="roxyPackage",
		Type="Package",
		Title="Utilities to automate package builds",
		Author="m.eik michalke <meik.michalke@hhu.de>",
		AuthorsR="c(person(given=\"Meik\", family=\"Michalke\", email=\"meik.michalke@hhu.de\",
			role=c(\"aut\", \"cre\")))",
		Maintainer="m.eik michalke <meik.michalke@hhu.de>",
		Depends="R (>= 2.9.0),methods,roxygen2,tools,XiMpLe (>= 0.03-12)",
		Description="The intention of this package is to make packaging R code as
			easy as possible. roxyPackage uses tools from the roxygen2 package to generate documentation. It also automatically
			generates and updates files like *-package.R, DESCRIPTION, CITATION, ChangeLog and NEWS.Rd. Building packages
			supports source format, as well as several binary formats (MS Windows, Mac OS X, Debian GNU/Linux) if the
			package contains pure R code only. The packages built are stored in a fully functional local R package repository
			which can be synced to a web server to share them with others. This includes the generation of browsable HTML
			pages similar to CRAN, with support for RSS feeds from the ChangeLog. Please read the vignette for a more detailed
			explanation by example.",
		License="GPL (>= 3)",
		Encoding="UTF-8",
		LazyLoad="yes",
		URL="http://reaktanz.de/?c=hacking&s=roxyPackage",
		stringsAsFactors=FALSE)

# empty environment for experimental tweaks
.roxyPackage.env <- new.env()

# internal helper functions to get/set values in that environment
get.roxyEnv <- function(name=NULL){
	if(is.null(name)){
		# if no name was specified, return the whole environment as a list
		result <- as.list(.roxyPackage.env)
	} else {
		result <- get(name, envir=.roxyPackage.env)
	}
	return(result)
}
set.roxyEnv <- function(name="roxygenVersion", value=2){
	assign(name, value, envir=.roxyPackage.env)
}

# set default version of roxygen to use
set.roxyEnv(name="roxygenVersion", value=2)


## wrapper for paste0() needed?
if(isTRUE(R_system_version(getRversion()) < 2.15)){
	# if this is an older R version, we need a wrapper function for paste0()
	# which was introduced with R 2.15 as a more efficient shortcut to paste(..., sep="")
	paste0 <- function(..., collapse=NULL){
		return(paste(..., sep="", collapse=collapse))
	}
} else {}


## function getRvers()
# this looks like too complicated, why not simply read R.version$major/minor?
# well, it's because we need to support R versions which are not currently running!
getRvers <- function(R.homes=R.home(), win=FALSE){
	R.bin <- file.path(R.homes, "bin", "R")
	if(isUNIX()){
		R.Version.full <- system(paste0(R.bin, " --version"), intern=TRUE)
	} else {
		R.Version.full <- shell(paste0(R.bin, " --version"), translate=TRUE, intern=TRUE)
	}
	R.Version.full <- R.Version.full[grep("R version ([[:digit:]]).([[:digit:]])", R.Version.full)]
	R.Version.full <- gsub("R version ([[:digit:]]+).([[:digit:]]+)([.]?)([[:digit:]]+)(.*)", "\\1.\\2\\3\\4", R.Version.full, perl=TRUE)
	if(isTRUE(win)){
		R.Version.full <- gsub("([[:digit:]]+).([[:digit:]]+)(.*)", "\\1.\\2", R.Version.full, perl=TRUE)
	}
	return(R.Version.full)
} ## end function getRvers()


## function listRDirs()
# tries to find directories named after R versions, which is used by archive.packages()
listRDirs <- function(path, full.path=TRUE){
	if(grepl("^file:(/)+", path)){
		path <- gsub("^file:(/)+", "/", path)
		add.file.prefix <- "file:/"
	} else {
		add.file.prefix <- ""
	}
	path <- gsub("/$", "", path)
	all.dirs <- dir(path, full.names=FALSE, recursive=FALSE)
	R.dirs <- all.dirs[grepl("^([[:digit:]]+).([[:digit:]]+)$", all.dirs)]
	if(isTRUE(full.path)){
		return(file.path(paste0(add.file.prefix, path, collapse=""), R.dirs))
	} else {
		return(R.dirs)
	}
} ## end function listRDirs()


## function roxyPackage.lib.dir()
roxyPackage.lib.dir <- function(){
	# find.package() was introduced with R 2.13, need version check here
	if(isTRUE(R_system_version(getRvers()) < "2.13")){
		return(.find.package("roxyPackage"))
	} else {
		return(find.package("roxyPackage"))
	}
} ## end function roxyPackage.lib.dir()


## function filter.repo.packages()
# simple function to strip irrelevant packages
filter.repo.packages <- function(pkg.list, packages){
	# inpuit needs to be a matrix
	if(!is.matrix(pkg.list)){
		pkg.list <- t(as.matrix(pkg.list))
	}
	if(!is.null(packages)){
		relevant.pkgs <- pkg.list[,"Package"] %in% packages
		pkg.list <- pkg.list[relevant.pkgs,]
	} else {}
	# even if there's only one package, ensure output is still a matrix
	if(!is.matrix(pkg.list)){
		pkg.list <- t(as.matrix(pkg.list))
	}
	return(pkg.list)
}
## end function filter.repo.packages()

## function get.by.role()
# filters a vector with person objects by roles
get.by.role <- function(persons, role="aut"){
	role.filter <- function(x){is.null(r <- x$role) | role %in% r}
	filtered.persons <- Filter(role.filter, persons)
	return(filtered.persons)
} ## end function get.by.role()

## function get.authors()
get.authors <- function(description, maintainer=TRUE, contributor=FALSE){
	if("Authors@R" %in% names(description)){
		got.aut <- paste(format(get.by.role(eval(parse(text=description[["Authors@R"]]))), include=c("given", "family", "email")), collapse=", ")
		got.cre <- ifelse(isTRUE(maintainer),
			paste(format(get.by.role(eval(parse(text=description[["Authors@R"]])), role="cre"), include=c("given", "family", "email")), collapse=", "),
			"")
		got.ctb <- ifelse(isTRUE(contributor),
			paste(format(get.by.role(eval(parse(text=description[["Authors@R"]])), role="ctb"), include=c("given", "family", "email")), collapse=", "),
			"")
	} else if("Author@R" %in% names(description)){
		got.aut <- paste(format(get.by.role(eval(parse(text=description[["Author@R"]]))), include=c("given", "family", "email")), collapse=", ")
		got.cre <- ifelse(isTRUE(maintainer),
			paste(format(get.by.role(eval(parse(text=description[["Author@R"]])), role="cre"), include=c("given", "family", "email")), collapse=", "),
			"")
		got.ctb <- ifelse(isTRUE(contributor),
			paste(format(get.by.role(eval(parse(text=description[["Author@R"]])), role="ctb"), include=c("given", "family", "email")), collapse=", "),
			"")
	} else {
		got.aut <- description[["Author"]]
		got.cre <- ifelse(isTRUE(maintainer),
			description[["Maintainer"]],
			"")
		# contributors should already be named in got.aut
		got.ctb <- ""
	}
	got.aut.clean <- gsub("<([^@]*)@([^>]*)>", "\\\\email{\\1@@\\2}", gsub("\n[[:space:]]*", "\n#' ", got.aut))
	got.cre.clean <- gsub("<([^@]*)@([^>]*)>", "\\\\email{\\1@@\\2}", gsub("\n[[:space:]]*", "\n#' ", got.cre))
	got.ctb.clean <- gsub("<([^@]*)@([^>]*)>", "\\\\email{\\1@@\\2}", gsub("\n[[:space:]]*", "\n#' ", got.ctb))
	# append contributors
	if(isTRUE(contributor) && got.ctb != ""){
		got.aut.clean <- paste0(got.aut.clean, ", with contributions from ",
			gsub("<([^@]*)@([^>]*)>", "\\\\email{\\1@@\\2}", gsub("\n[[:space:]]*", "\n#'\t\t", got.ctb)))
	} else {}
	gotAuthors <- list(aut=got.aut, aut.clean=got.aut.clean, cre=got.cre, cre.clean=got.cre.clean, ctb=got.ctb, ctb.clean=got.ctb.clean)
	return(gotAuthors)
} ## end function get.authors()

## function getDescField()
# extracts fields from description, including checks
# if "field" is a vector, they are tested in order, befor an error is returned
getDescField <- function(desc, field, stopOnErr=TRUE){
	valid.fields <- field %in% colnames(desc)
	if(any(valid.fields)){
		# give a warning if the first alternative didn't check out
		if(!isTRUE(valid.fields[1])){
			warning(paste0("field \"", field[1],"\" missing in DESCRIPTION file, used \"", field[valid.fields][1], "\" as fallback!"), call.=FALSE)
		} else {}
		this.field <- desc[,field[valid.fields]][1]
	} else {
		this.field <- NULL
	}
	if(is.null(this.field) & isTRUE(stopOnErr)){
		stop(simpleError(paste0("fields missing in DESCRIPTION file: \"", paste(field, collapse="\", \""), "\"")))
	} else {}
	return(as.character(this.field))
} ## end function getDescField()


## function isUNIX()
# if isUNIX=TRUE only checks if this is a unix OS
isUNIX <- function(){
	thisOS <- base::.Platform[["OS.type"]]
	return(
		switch(thisOS,
			windows=FALSE,
			unix=TRUE)
	)
} ## end function isUNIX()


## function dateRFC2822()
dateRFC2822 <- function(date=Sys.time()){
	if(!inherits(date, c("POSIXct", "POSIXt"))){
		date <- as.Date(date)
	} else {}
	# for valid RFC2822 date conversions, temporarily set english locale
	orig.locale <- Sys.getlocale(category = "LC_TIME")
	if(!isTRUE(grepl("^en", orig.locale))){
		Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")
		on.exit(Sys.setlocale(category = "LC_TIME", locale=orig.locale))
	}
	return(format(date, "%a, %d %b %Y %H:%M:%S %z"))
} ## end function dateRFC2822()


## function checkLicence()
checkLicence <- function(license, logical=TRUE, deb=TRUE){
	# for easier matching, remove spaces and turn into lowercase
	normaLice <- gsub("[[:space:]>=()]", "", tolower(license))

	knownLicenses <- list(
		agpl="3",
		apache="2",
		artistic=c("1","2"),
		bsd="",
		gfdl=c("1.2","1.3"),
		lgpl=c("2.1","2","3"),
		gpl=c("1","2","3")
	)

	# these licenses are also part of roxyPackage, in the file common-licenses.zip
	debianLicenses <- list(
		agpl=c(
			file="AGPL-3",
			name="GNU Affero General Public License",
			version="3"),
		agpl3=c(
			file="AGPL-3",
			name="GNU Affero General Public License",
			version="3"),
		apache=c(
			file="Apache-2.0",
			name="Apache License",
			version="2.0"),
		apache2=c(
			file="Apache-2.0",
			name="Apache License",
			version="2.0"),
		artistic=c(
			file="Artistic-1.0",
			name="Artistic License",
			version="1.0"),
		artistic1=c(
			file="Artistic-1.0",
			name="Artistic License",
			version="1.0"),
		artistic2=c(
			file="Artistic-2.0",
			name="Artistic License",
			version="2.0"),
		bsd=c(
			file="BSD",
			name="BSD License",
			version=NA),
		gfdl=c(
			file="GFDL-1.3",
			name="GNU Free Documentation License (GFDL)",
			version="1.3"),
		gfdl1.2=c(
			file="GFDL-1.2",
			name="GNU Free Documentation License (GFDL)",
			version="1.2"),
		gfdl1.3=c(
			file="GFDL-1.3",
			name="GNU Free Documentation License (GFDL)",
			version="1.3"),
		lgpl=c(
			file="LGPL-3",
			name="GNU Lesser General Public License (LGPL)",
			version="3"),
		lgpl2.1=c(
			file="LGPL-2.1",
			name="GNU Lesser General Public License (LGPL)",
			version="2.1"),
		lgpl2=c(
			file="LGPL-2",
			name="GNU Lesser General Public License (LGPL)",
			version="2"),
		lgpl3=c(
			file="LGPL-3",
			name="GNU Lesser General Public License (LGPL)",
			version="3"),
		gpl=c(
			file="GPL-3",
			name="GNU General Public License (GPL)",
			version="3"),
		gpl1=c(
			file="GPL-1",
			name="GNU General Public License (GPL)",
			version="1"),
		gpl2=c(
			file="GPL-2",
			name="GNU General Public License (GPL)",
			version="2"),
		gpl3=c(
			file="GPL-3",
			name="GNU General Public License (GPL)",
			version="3")
	)

	knownLicNames <- names(knownLicenses)
	licFound <- FALSE
	licName <- ""
	licVers <- ""
	licIter <- 1
	while(!isTRUE(licFound) & licIter <= length(knownLicNames)){
		thisLic <- knownLicNames[licIter]
		if(grepl(thisLic, normaLice)){
			licFound <- TRUE
			licName <- thisLic
			versFound <- FALSE
			versIter <- 1
			licVersions <- knownLicenses[[thisLic]]
			if(length(licVersions) > 0){
				while(!isTRUE(versFound) & versIter <= length(licVersions)){
					thisVers <- licVersions[versIter]
					if(grepl(thisVers, normaLice)){
						versFound <- TRUE
						licVers <- thisVers
					} else {
						versIter <- versIter + 1
					}
				}
			} else {}
		} else {
			licIter <- licIter + 1
		}
	}
	licenseString <- paste(licName, licVers, sep="")

	if(!identical(licenseString, "")){
		if(isTRUE(logical)){
			return(TRUE)
		} else if(isTRUE(deb)){
			return(debianLicenses[[licenseString]])
		} else {
			return(licenseString)
		}
	} else {
		if(isTRUE(logical)){
			return(FALSE)
		} else {
			warning("license: sorry, didn't recognize license string!", call.=FALSE)
			return(licenseString)
		}
	}
} ## end function checkLicence()


## function copyLicence()
copyLicence <- function(license, destFile, overwrite=FALSE){
	if(!file_test("-f", destFile) | isTRUE(overwrite)){
		stopifnot(checkLicence(license))
		licenseInfo <- checkLicence(license, deb=TRUE, logical=FALSE)
		# get location of roxyPackage installation
		stopifnot("roxyPackage" %in% rownames(installed.packages()))
		rPHome <- installed.packages()["roxyPackage","LibPath"]
		cLPath <- file.path(rPHome, "roxyPackage","common-licenses.zip")
		if(!file.exists(cLPath)){
			stop(simpleError(paste("file not found:", cLPath)))
		} else {}
#		unzip(LCC.path, files=LCC.zip.wanted, junkpaths=TRUE, exdir=tmp.path)
		licenseText <- readLines(licenseCon <- unz(cLPath, filename=file.path("common-licenses", licenseInfo[["file"]])))
		close(licenseCon)
		writeLines(licenseText, con=destFile)
		message(paste0("license: saved a copy of the ", licenseInfo[["name"]], " as LICENSE"))
	} else {
		message(paste("skipping, license file exists:", destFile))
	}
	return(invisible(NULL))
} ## end function copyLicence()


## function file.mv()
file.mv <- function(from, to, overwrite=recursive, recursive=FALSE){
	stopifnot(file.copy(from=from, to=to, overwrite=overwrite, recursive=recursive))
	stopifnot(file.remove(from))
	return(invisible(NULL))
} ## end function file.mv()


## function createMissingDir()
createMissingDir <- function(dirPath, action="files"){
	if(!file_test("-d", dirPath)){
		stopifnot(dir.create(dirPath, recursive=TRUE))
		message(paste0(action, ": created ", dirPath, "."))
	} else {}
	return(invisible(NULL))
} ## end function createMissingDir()


## function removeIfExists()
removeIfExists <- function(filePath){
	if(file.exists(filePath)){
		stopifnot(file.remove(filePath))
	} else {}
	return(invisible(NULL))
} ## end function removeIfExists()


## function mvToArchive()
mvToArchive <- function(package, repo, archive, versions, type=NA, file=NA, overwrite=FALSE,
	reallyDoIt=FALSE, justDelete=FALSE){

	repo <- gsub("^file:(/)+", "/", repo)
	archive <- gsub("^file:(/)+", "/", archive)

	if(isTRUE(reallyDoIt) && !isTRUE(justDelete)){
		createMissingDir(dirPath=repo, action="archive")
		createMissingDir(dirPath=archive, action="archive")
	} else {}

	file.ending <- switch(type,
			source=".tar.gz",
			win.binary=".zip",
			mac.binary.leopard=".tgz"
		)
	pkg.names <- paste0(package, "_", versions, file.ending)
	sapply(pkg.names, function(this.package){
		pkg.from <- file.path(repo, this.package)
		pkg.to <- file.path(archive, this.package)
		if(!file.exists(pkg.from)){
			stop(simpleError(paste0("file doesn't exist:\n  ", pkg.from)))
		} else {}
		# don't archive, just remove files
		if(isTRUE(justDelete)){
			if(isTRUE(reallyDoIt)){
				message(paste0("archive: deleting file ", pkg.from))
				removeIfExists(pkg.from)
			} else {
				message(paste0("archive: deleting file ", pkg.from, " (NOT RUN!)"))
			}
		} else {
			if(isTRUE(reallyDoIt)){
				message(paste0("archive: moving ", pkg.from, " to ", pkg.to))
				file.mv(from=pkg.from, to=pkg.to, overwrite=overwrite)
			} else {
				message(paste0("archive: moving ", pkg.from, " to ", pkg.to, " (NOT RUN!)"))
			}
		}
	})
	return(invisible(NULL))
} ## end function mvToArchive()


## function trim()
# cuts off space at start and end of a character string
trim <- function(char){
	char <- gsub("^[[:space:]]*", "", char)
	char <- gsub("[[:space:]]*$", "", char)
	return(char)
} ## end function trim()


## function normalizePathByOS()
# path: the root path to be normalized
# unix.OS: logical value to set the OS
# filePrefix: if TRUE, file:/// is prefixed
normalizePathByOS <- function(path, is.unix=isUNIX(), mustWork=FALSE, filePrefix=FALSE){
	# normalize a given root path
	# this may give bogus results on windows, so we'll encapsulate
	# it in shortPathName(), which is not available on all OSs
	if(isTRUE(is.unix)){
		result <- normalizePath(path, mustWork=mustWork)
		slashes <- "//"
	} else {
		result <- shortPathName(normalizePath(path, mustWork=mustWork))
		slashes <- "///"
	}
	if(isTRUE(filePrefix) && !grepl("^file://", result)){
		result <- paste0("file:", slashes, result)
	} else {}
	return(result)
} ## end function normalizePathByOS()
