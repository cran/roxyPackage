\docType{methods}
\name{getChangeLogEntry}
\alias{getChangeLogEntry}
\alias{getChangeLogEntry,ChangeLog,ANY,ANY,ANY,ANY-method}
\alias{getChangeLogEntry,ChangeLog-method}
\alias{getChangeLogEntry,-methods}
\title{Read/write ChangeLog objects}
\arguments{
  \item{log}{An object of class \code{ChangeLog}.}

  \item{version}{Character string, version number to look
  up.}
}
\value{
  An object of class \code{ChangeLog}.
}
\description{
  These methods can be used to manage ChangeLog objects.
}
\details{
  \code{getChangeLogEntry} takes a ChangeLog object and a
  version number string and returns the according entry.

  \code{updateChangeLog} takes a ChangeLog object and a
  version number string, replaces the complete entry with
  the contents of \code{entry} and updates the time stamp
  to \code{date}.
}
\examples{
\dontrun{
changelog <- readChangeLog("/home/user/myRsources/myRpackage/ChangeLog")
CL.entry <- getChangeLogEntry(changelog, version="0.02-22")
}
}
\seealso{
  \code{\link[roxyPackage:readChangeLog]{readChangeLog}}
}

