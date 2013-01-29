## function roxy.description()
# create package description
roxy.description <- function(val, description, version=NULL, date=NULL, R.vers=NULL){

	if(!is.null(version)){
		description[["Version"]] <- version
	} else {}
	if(!is.null(date)){
		description[["Date"]] <- date
	} else {}

	# prefer Author@R/Authors@R over Author/Maintainer
	pck.authors <- get.authors(description, maintainer=TRUE, contributor=TRUE)
	pck.author <- pck.authors[["aut"]]
	pck.maintainer <- pck.authors[["cre"]]
	pck.contributor <- pck.authors[["ctb"]]
	pck.author.clean <- pck.authors[["aut.clean"]]
	pck.maintainer.clean <- pck.authors[["cre.clean"]]
	pck.contributor.clean <- pck.authors[["ctb.clean"]]

	## basic package info
	if(identical(val, "package")){
		return(description[["Package"]])
	} else {}
	if(identical(val, "title")){
		return(description[["Title"]])
	} else {}
	if(identical(val, "depds")){
		return(description[["Depends"]])
	} else {}
	if(identical(val, "description")){
		# check if we need to add old Author/Maintainer fields
		if(is.null(R.vers) || isTRUE(R_system_version(R.vers) < "2.14")){
			if(!"Author" %in% names(description)){
				if(pck.contributor != "") {
					description[["Author"]] <- paste(pck.author, ", with contributions from ", pck.contributor, sep="")
				} else {
					description[["Author"]] <- pck.author
				}
			} else {}
			if(!"Maintainer" %in% names(description)){
				description[["Maintainer"]] <- pck.maintainer
			} else {}
		} else {}
		return(description)
	} else {}

	checkFor <- c("Version","Date","Depends","Enhances","Encoding","License","LazyLoad","URL")
	desc.parts <- sapply(checkFor, function(this.entry){
			found <- ifelse(this.entry %in% names(description), paste("\n#' ",this.entry,": \\tab ",description[[this.entry]],"\\cr", sep=""),"")
			found <- gsub("^[[:space:]]*#'[[:space:]]*$", ".\n", found)
			return(found)
		})
	# extra check for package type
	if("Type" %in% names(description)){
		if(description[["Type"]] %in% c("Package", "Frontend", "Translation")){
			pck.type <- description[["Type"]]
		} else {
			pck.type <- "Package"
		}
	} else {
		pck.type <- "Package"
	}

	pckg.package.v <- paste(
			"#' ",description[["Title"]],".\n#'\n#' \\tabular{ll}{",
			"\n#' Package: \\tab ",description[["Package"]],"\\cr",
			"\n#' Type: \\tab ",pck.type,"\\cr",
			paste(desc.parts, collapse=""),
			"\n#' }\n#'",
			"\n#' ",gsub("\n#' #'\n#'","\n#'\n#'",gsub("\n[[:space:]]*", "\n#' ", description[["Description"]])),"\n#'",
			"\n#' @aliases ",description[["Package"]],"-package ",description[["Package"]],
			"\n#' @name ",description[["Package"]],"-package",
			"\n#' @docType package",
			"\n#' @title The ",description[["Package"]]," Package",
			"\n#' @author ",pck.author.clean,
			"\n#' @keywords package",
			"\nNULL\n",
			sep="")
	if(identical(val, "pckg.description")){
		return(pckg.package.v)
	} else {}
} ## end function roxy.description()
