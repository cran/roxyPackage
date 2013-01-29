\docType{methods}
\name{updateChangeLog}
\alias{updateChangeLog}
\alias{updateChangeLog,ChangeLog,ANY,ANY,ANY,ANY-method}
\alias{updateChangeLog,ChangeLog-method}
\alias{updateChangeLog,-methods}
\title{Update ChangeLog objects}
\arguments{
  \item{log}{An object of class \code{ChangeLog}.}

  \item{entry}{A (named) list of character vectors. The
  element names will become the ChangeLog sections, each
  vector element an item.}

  \item{version}{Character string, version number to look
  up.}

  \item{date}{The date of the ChangeLog entry in
  \code{YYYY-MM-DD} format. will be coerced into character.
  To keep the date stamp of a present entry, set
  \code{date=NULL}.}

  \item{append}{Logical, whether a present entry should be
  replaced or added to.}
}
\value{
  An object of class \code{ChangeLog}.
}
\description{
  This method can be used to update ChangeLog objects.
}
\details{
  \code{updateChangeLog} takes a ChangeLog object and a
  version number string, replaces the complete entry with
  the contents of \code{entry} and updates the time stamp
  to \code{date}.
}
\seealso{
  \code{\link[roxyPackage:readChangeLog]{readChangeLog}}
}

