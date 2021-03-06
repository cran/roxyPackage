## sandbox specific classes and methods
# this class will only be used internally, in one single instance.
# it holds all information needed for sandboxing and will be stored
# in the package internal environment .roxyPackage.env

# classes for ChangeLog objects
setClass("roxySandbox",
	representation=representation(
		active="logical",
		sandbox.dir="character",
		pck.source.dir="character",
		R.libs="character",
		repo.root="character",
		archive.root="character",
		to.dir="character"),
	prototype=prototype(
		active=FALSE,
		sandbox.dir="",
		pck.source.dir="",
		R.libs="",
		repo.root="",
		archive.root="",
		to.dir="")
)

# sandboxing defaults to false
set.roxyEnv(name="sandbox", value=new("roxySandbox", active=FALSE))

# show method, used internally to print dstatus reports
setMethod("show", signature(object="roxySandbox"), function(object){
	missionMessage <- paste("sandbox settings\n")

	missionSettings <- c(
		sandbox.dir=object@sandbox.dir,
		pck.source.dir=object@pck.source.dir,
		R.libs=object@R.libs,
		repo.root=object@repo.root,
		archive.root=object@archive.root)
	# for prettier message printout, longest option + 1
	alignment.max <- 15

	for (thisSetting in names(missionSettings)){
		if(identical(thisSetting, "archive.root")){
			# we need to combine this with to.dir first,
			# but only if there's actual values set
			archive.root <- missionSettings[["archive.root"]]
			to.dir <- slot(object, "to.dir")
			if(!identical(archive.root, "") && !identical(to.dir, "")){
				new.setting <- file.path(archive.root, to.dir)
			} else {
				new.setting <- ""
			}
			# for the printout, overwrite thisSetting with "archive"
			thisSetting <- "archive"
		} else {
			new.setting <- missionSettings[[thisSetting]]
		}
		alignment <- paste(rep(" ", alignment.max - nchar(thisSetting)), collapse="")
		if(identical(new.setting, "")){
			create.status <- "(not sandboxed)\n"
		} else {
			if(!file_test("-d", new.setting)){
				create.status <- " (created on demand)\n"
			} else {
				create.status <- " (exists)\n"
			}
		}
		missionMessage <- paste0(missionMessage, "  ", thisSetting, ":", alignment, new.setting, create.status)
		rm("new.setting")
	}
	missionMessage <- paste0(missionMessage, "\n  sandboxing active: ", object@active)
	message(missionMessage)
})

# unction to check if sandboxing is activated
check.sandbox <- function(){
	snd.config <- get.roxyEnv(name="sandbox")
	return(slot(snd.config, "active"))
}

## function pck.deps()
# this function returns the *names* of packages the given package depends on.
# if recursive=TRUE, it will also check those packages' dependencies
# recursively and return all names it finds down the way.
pck.deps <- function(package, R.libs, description=NULL, recursive=TRUE,
	depLevel=c("Depends", "Imports"), known.deps=c()){
	# try to get a matrix with dependencies. first column should be the package names
	# if we hav a description, use that, otherwise try to read DESCRIPTION
	if(is.null(description)){
		description <- read.dcf(file.path(R.libs, package, "DESCRIPTION"))
	} else {}
	# package.dependencies() seems to be broken, since it only checks for "Depends"
	# in the default setting. but only try depLevel if it appears in DESCRIPTION,
	# because package.dependencies() will stop with an error otherwise...
	depLevel.tmp <- depLevel[depLevel %in% colnames(description)]
	pck.deps.packages <- unique(unlist(sapply(depLevel.tmp, function(thisLevel){
			pck.dep.tmp <- as.data.frame(
				package.dependencies(as.matrix(description), depLevel=thisLevel),
				stringsAsFactors=FALSE)
			return(pck.dep.tmp[,1])
		})))
	# clean packages from those who are not available in R.libs
	pck.deps.packages <- pck.deps.packages[file_test("-d", file.path(R.libs, pck.deps.packages))]
	if(isTRUE(recursive)){
		pck.deps.recursive <- unlist(sapply(pck.deps.packages, function(thisPck){
			pck.deps(
				package=thisPck, R.libs=R.libs, description=NULL, recursive=TRUE,
				depLevel=depLevel, known.deps=pck.deps.packages
			)
		}))
		pck.deps.packages <- unique(c(known.deps, pck.deps.recursive))
	} else {}
	return(pck.deps.packages)
} ## end function pck.deps()


## function prep.sndbx.source.dir()
# a helper function for the following prepare.sandbox() functions
prep.sndbx.source.dir <- function(snd.pck.source.dir, pck.source.dir, package){
	if(!identical(snd.pck.source.dir, pck.source.dir) && !identical(snd.pck.source.dir, "")){
		createMissingDir(dirPath=snd.pck.source.dir, action="sandbox")
		# copy sources to sandbox, but only if not already present
		if(!file_test("-d", file.path(snd.pck.source.dir, package))){
			file.copy(
				from=pck.source.dir,
				to=snd.pck.source.dir,
				overwrite=TRUE,
				recursive=TRUE)
			message(paste0("sandbox: copied '", package, "' sources to ", snd.pck.source.dir))
		} else {}
		return(file.path(snd.pck.source.dir, package))
	} else {
		return(pck.source.dir)
	}
} ## end function prep.sndbx.source.dir()

## function prep.sndbx.repo.root()
# a helper function for the following prepare.sandbox() functions
prep.sndbx.repo.root <- function(snd.repo.root, repo.root){
	if(!identical(snd.repo.root, repo.root) && !identical(snd.repo.root, "")){
		createMissingDir(dirPath=snd.repo.root, action="sandbox")
		return(snd.repo.root)
	} else {
		return(repo.root)
	}
} ## end function prep.sndbx.repo.root()


## function prepare.sandbox()
prepare.sandbox <- function(package, description, pck.source.dir, R.libs, R.version, repo.root, depLevel=c("Depends", "Imports")){
	snd.config <- get.roxyEnv("sandbox")
	if(!inherits(snd.config, "roxySandbox")){
		stop(simpleError("got strange readings for sandbox settings, please check!"))
	} else {}
	snd.pck.source.dir <- slot(snd.config, "pck.source.dir")
	snd.R.libs <- slot(snd.config, "R.libs")
	snd.repo.root <- slot(snd.config, "repo.root")
	result <- list()
	# now check if the given files differ from sandbox definitions. if so
	#  - try to create the neccessary directories
	#  - return the new directories as the ones to use

	result[["pck.source.dir"]] <- prep.sndbx.source.dir(
		snd.pck.source.dir=snd.pck.source.dir,
		pck.source.dir=pck.source.dir,
		package=package)

	if(!identical(snd.R.libs, R.libs) && !identical(snd.R.libs, "")){
		# the new sandbox R root will get different folders for different R versions
		snd.R.libs.Rver <- file.path(snd.R.libs, R.version)
		createMissingDir(dirPath=snd.R.libs.Rver, action="sandbox")
		# calculate package dependecies
		pck.dep.packages <- pck.deps(package=package, R.libs=R.libs, description=description,
			recursive=TRUE, depLevel=depLevel, known.deps=c())
		# we'll assume that the developer installed all needed dependencies here,
		# but we will not abort if packages are not found -- worst case is that
		# packaging will not work
		for (thisDep in pck.dep.packages){
			pck.dep.packages.path <- file.path(R.libs, thisDep)
			# check if package is in R.libs and not already in snd.R.libs
			if(file_test("-d", pck.dep.packages.path) && !file_test("-d", file.path(snd.R.libs.Rver, thisDep))){
				file.copy(
					from=pck.dep.packages.path,
					to=snd.R.libs.Rver,
					overwrite=TRUE,
					recursive=TRUE)
				message(paste0("sandbox: copied dependency '", thisDep, "' to ", snd.R.libs.Rver))
			} else {}
			rm(pck.dep.packages.path)
		}
		result[["R.libs"]] <- snd.R.libs.Rver
	} else {
		result[["R.libs"]] <- R.libs
	}

	result[["repo.root"]] <- prep.sndbx.repo.root(
		snd.repo.root=snd.repo.root,
		repo.root=repo.root)

	return(result)
} ## end function prepare.sandbox()


## function prepare.sandbox.archive()
# this is almost the same function as above, but only used in archiving contexts
prepare.sandbox.archive <- function(repo.root, archive.root, to.dir){
	snd.config <- get.roxyEnv("sandbox")
	if(!inherits(snd.config, "roxySandbox")){
		stop(simpleError("got strange readings for sandbox settings, please check!"))
	} else {}
	snd.repo.root <- slot(snd.config, "repo.root")
	snd.archive.root <- slot(snd.config, "archive.root")
	snd.to.dir <- slot(snd.config, "to.dir")
	result <- list()
	# now check if the given files differ from sandbox definitions. if so
	#  - try to create the neccessary directories
	#  - return the new directories as the ones to use
	result[["repo.root"]] <- prep.sndbx.repo.root(
		snd.repo.root=snd.repo.root,
		repo.root=repo.root)
	if(!identical(snd.archive.root, archive.root) && !identical(snd.archive.root, "")){
		snd.archive <- file.path(snd.archive.root, snd.to.dir)
		createMissingDir(dirPath=snd.archive, action="sandbox")
		result[["archive.root"]] <- snd.archive.root
		result[["to.dir"]] <- snd.to.dir
	} else {
		result[["archive.root"]] <- archive.root
		result[["to.dir"]] <- to.dir
	}
	return(result)
} ## end function prepare.sandbox.archive()

## function prepare.sandbox.deb()
# this is almost the same function as above, but only used in debianzing contexts
# it can work without "description" and "package", but must be called after a DESCRIPTION was generated
prepare.sandbox.deb <- function(pck.source.dir, repo.root){
	snd.config <- get.roxyEnv("sandbox")
	if(!inherits(snd.config, "roxySandbox")){
		stop(simpleError("got strange readings for sandbox settings, please check!"))
	} else {}
	snd.pck.source.dir <- slot(snd.config, "pck.source.dir")
	snd.repo.root <- slot(snd.config, "repo.root")
	result <- list()

	# get the package description, mainly to read the package name from it
	description <- read.dcf(file.path(pck.source.dir, "DESCRIPTION"))
	package <- getDescField(desc=description, field="Package", stopOnErr=TRUE)

	result[["pck.source.dir"]] <- prep.sndbx.source.dir(
		snd.pck.source.dir=snd.pck.source.dir,
		pck.source.dir=pck.source.dir,
		package=package)

	result[["repo.root"]] <- prep.sndbx.repo.root(
		snd.repo.root=snd.repo.root,
		repo.root=repo.root)

	return(result)
} ## end function prepare.sandbox.deb()
