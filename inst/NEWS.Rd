\name{NEWS}
\title{News for Package 'roxyPackage'}
\encoding{UTF-8}
\section{Changes in roxyPackage version 0.03-6 (2013-04-07)}{
  \subsection{added}{
    \itemize{
      \item \code{debianize()} now supports both strict package names according to the
        Debain R Policy as well as package names with a dot, whichever is expected by
        your r-cran.mk file. Ignoring the policy is the default, as that is the
        de-facto standard.
    }
  }
}
\section{Changes in roxyPackage version 0.03-5 (2013-03-30)}{
  \subsection{fixed}{
    \itemize{
      \item .\code{find.package()} will soon be removed from R, replaced it with
        \code{find.package()} for R >= 2.13
    }
  }
}
\section{Changes in roxyPackage version 0.03-4 (2013-03-12)}{
  \subsection{changed}{
    \itemize{
      \item internally replaced paste(..., sep=\code{""}) with paste0(...), and added a
        version dependent wrapper for compatibility with R < 2.15
    }
  }
  \subsection{added}{
    \itemize{
      \item AGPL and Artistic License 2.0 have been added to the common licenses
    }
  }
  \subsection{fixed}{
    \itemize{
      \item license files are now called LICENSE.txt; this is to prevent their
        installation, as suggested by 'Writing R Extensions'
    }
  }
}
\section{Changes in roxyPackage version 0.03-3 (2013-02-13)}{
  \subsection{added}{
    \itemize{
      \item extended sandboxing possibilities to \code{archive.packages()} and \code{debianize()}
    }
  }
  \subsection{changed}{
    \itemize{
      \item \code{sandbox()} will now remove all paths set if \code{"active"} is FALSE
    }
  }
}
\section{Changes in roxyPackage version 0.03-2 (2013-02-11)}{
  \subsection{added}{
    \itemize{
      \item new sandbox feature, see ?\code{sandbox()} and ?\code{sandbox.status()}. this needs
        testing, and future releases will get support for this in all relevant
        functions, not just \code{roxy.package()}, as it is now.
    }
  }
}
\section{Changes in roxyPackage version 0.03-1 (2013-01-29)}{
  \subsection{changed}{
    \itemize{
      \item split documentation for ChangeLog functions and methods
    }
  }
  \subsection{added}{
    \itemize{
      \item there is now an EXPERIMENTAL option to be able to toy around with
        roxygen3: call 'roxyPackage:::set.roxyEnv(name=\code{"roxygenVersion"}, value=3)' to
        enable some hidden features, i.e. use \code{roxygen3::roxygenise()} instead of
        \code{roxygen2::roxygenize()}. you need to install and load the roxygen3 package first
        -- temper with this at your own risk. please note: there will probably
        never be an official roxygen3 package on CRAN, and once its features were
        merged into roxygen2, this option might be patched out of roxyPackage again.
      \item this is the first version of roxyPackage supposed to be released on CRAN
    }
  }
}
\section{Changes in roxyPackage version 0.02-23 (2013-01-25)}{
  \subsection{changed}{
    \itemize{
      \item updated vignette
    }
  }
  \subsection{fixed}{
    \itemize{
      \item debian installation notes will not be linked from a package's HTML page
        if there is no built debian package in the repository
    }
  }
}
\section{Changes in roxyPackage version 0.02-22 (2012-10-30)}{
  \subsection{changed}{
    \itemize{
      \item the title for *-package.R files now states "The <package name> Package"
        instead of the Title: entry from the description
    }
  }
  \subsection{fixed}{
    \itemize{
      \item order of ChangeLog entries is now checked, so that the newest entries get
        on top
    }
  }
}
\section{Changes in roxyPackage version 0.02-21 (2012-10-14)}{
  \subsection{added}{
    \itemize{
      \item new action \code{"log"} generates an initial ChangeLog, if none is found
      \item introducing new functions \code{initChangeLog()}, \code{readChangeLog()},
        \code{writeChangeLog()}, as well as the methods \code{getChangeLogEntry()} and \code{updateChangeLog()}.
        they are probably not bug-free, but should ease the management of proper R
        ChangeLogs considerably.
    }
  }
  \subsection{changed}{
    \itemize{
      \item this ChangeLog was read-in by \code{readChangeLog()}, updated with the log
        entries for this release by \code{updateChangeLog()}, and written back with
        \code{writeChangeLog()}.
    }
  }
}
\section{Changes in roxyPackage version 0.02-20 (2012-09-29)}{
  \subsection{fixed}{
    \itemize{
      \item contributors in Author@R/Authors@R fields are now also added to
        *-package.R and DESCRIPTION
      \item the \code{"check"} option "--as-cran" is automatically stripped for R < 2.15
    }
  }
}
\section{Changes in roxyPackage version 0.02-19 (2012-09-09)}{
  \subsection{changed}{
    \itemize{
      \item the \code{"check"} action now has "--as-cran" set as the default option
    }
  }
  \subsection{added}{
    \itemize{
      \item the \code{"doc"} action now also supports \code{"vignette"} subdirectories in addition
        to "inst/doc" for vignette files
      \item added new function \code{archive.packages()} to move old versions of packages to
        an archive directory, or to remove them for good. since this is
        potentially dangerous and needs some rounds of testing, its \code{"reallyDoIt"}-option
        needs to be set to TRUE manually, otherwise a dry run is done, i.e., all
        actions are only printed and not done, so you can check the outcome first.
    }
  }
}
\section{Changes in roxyPackage version 0.02-18 (2012-07-30)}{
  \subsection{fixed}{
    \itemize{
      \item using \code{shortPathName()} to fix problems on windows caused by the path
        normalization introduced with 0.02-17 (thanks to henrik singmann for tracking
        this one down)
    }
  }
  \subsection{changed}{
    \itemize{
      \item wrote some new internal helper functions, namely \code{createMissingDir()} and
        \code{removeIfExists()}, and replaced their manual pendants in the \code{roxy.package()}
        code, which is now easier to read
    }
  }
}
\section{Changes in roxyPackage version 0.02-17 (2012-07-20)}{
  \subsection{fixed}{
    \itemize{
      \item the tar option "--exclude-vcs" is not always implemented, especially not
        in the BSD tar of Mac OS X; so excluding version control directories is
        now done in a more manual manner in roxyPackage
      \item also, on Mac OS X the tar binary is now identified using Sys.which(\code{"tar"})
      \item finally, "--exclude=*~" caused trouble if path names contained a tilde
        for $HOME, so this was replaced with a more reliable way to exclude backup~
        files as well
      \item all root paths are now normalized before first use, especially to perform
        tilde expansion, because this lead to errors with \code{tar()}
    }
  }
}
\section{Changes in roxyPackage version 0.02-16 (2012-07-12)}{
  \subsection{fixed}{
    \itemize{
      \item HTML generation now also works if there's no "Author:" field in the
        DESCRIPTION
    }
  }
}
\section{Changes in roxyPackage version 0.02-15 (2012-06-10)}{
  \subsection{changed}{
    \itemize{
      \item the data.frame with the package description doesn't need to have entries
        for \code{"Author"} or \code{"Maintainer"} any more, as long as "Authors@R" is present
    }
  }
}
\section{Changes in roxyPackage version 0.02-14 (2012-06-05)}{
  \subsection{fixed}{
    \itemize{
      \item roxyPackage now supports multiple vignette files in one package
      \item fixed problems if the repository isn't on the same file system as the
        sources
    }
  }
}
\section{Changes in roxyPackage version 0.02-13 (2012-05-12)}{
  \subsection{fixed}{
    \itemize{
      \item fixed malformed BibTeX entry in CITATION if more than one author given
    }
  }
}
\section{Changes in roxyPackage version 0.02-12 (2012-05-11)}{
  \subsection{fixed}{
    \itemize{
      \item line breaks were missing in repo HTML, which lead to malformed <pre>
        sections
      \item debian repo information will now generally be prepared for binary and
        source packages
    }
  }
}
\section{Changes in roxyPackage version 0.02-11 (2012-04-29)}{
  \subsection{added}{
    \itemize{
      \item added a vignette
      \item \code{debianize()} now creates debian/compat, by default set to level 7
    }
  }
  \subsection{changed}{
    \itemize{
      \item status messages are more consistent now
      \item the link to a debian binary package moved from the general package HTML
        page to the debian repository info page
    }
  }
}
\section{Changes in roxyPackage version 0.02-10 (2012-04-19)}{
  \subsection{added}{
    \itemize{
      \item \code{debianize()} now checks if a given GnuPG key is already available as a
        file in the repository, and if not, saves is as <key ID>.asc
      \item the \code{"html"} action now also adds auto-generated information on how to
        install debian packages from the maintained repository
    }
  }
}
\section{Changes in roxyPackage version 0.02-9 (2012-04-15)}{
  \subsection{added}{
    \itemize{
      \item this is the first package, along with its dependecies, which is also
        available as a debian package build with roxyPackage
    }
  }
  \subsection{changed}{
    \itemize{
      \item if information is to be fetched from a DESCRIPTION file and there is no
        \code{"Date"} field, \code{roxy.package()} considers \code{"Packaged"} and then
        "Date/Publication" as fallback alternatives (and throws a warning)
      \item the actions \code{"deb"}, \code{"roxy"}, \code{"cite"}, \code{"license"}, \code{"doc"}, \code{"cl2news"} and
        \code{"news2rss"} are now only run once for builds against multiple R installations,
        because their outcome should be the same anyway
    }
  }
  \subsection{fixed}{
    \itemize{
      \item 'deb.options' are now also passed to recursive calls of \code{roxy.package()},
        although there's still no solution for actual builds against multiple R
        installations
    }
  }
}
\section{Changes in roxyPackage version 0.02-8 (2012-04-14)}{
  \subsection{added}{
    \itemize{
      \item \code{roxy.package()} now checks for needed fields in DESCRIPTION, if that file
        is used to get info on the package, and throws an error if something is
        missing
      \item installation now includes common-licenses.zip
      \item \code{roxy.package()} has a new action \code{"license"} to detect the package license
        and save a copy of common free software licenses as the package's LICENSE
        file
    }
  }
  \subsection{changed}{
    \itemize{
      \item added hints on dependencies to the \code{debianize()} docs
    }
  }
  \subsection{fixed}{
    \itemize{
      \item regex to get author info without e-mail address for debian/copyright now
        works with multiple authors
      \item fixed some formatting issues with debian/copyright
    }
  }
}
\section{Changes in roxyPackage version 0.02-7 (2012-04-11)}{
  \subsection{added}{
    \itemize{
      \item \code{debianize()} can now sign Release files with GnuPG
      \item \code{roxy.package()} has a new action \code{"deb"} and a new parameter 'deb.options',
        to invoke \code{debianize()} directly
      \item debian packages are now added to the html index files
    }
  }
  \subsection{changed}{
    \itemize{
      \item Release files in \code{debianize()} are now generated with apt-ftparchive
      \item \code{debianize()} talks a little more about what it's busy with
      \item debian package dependencies now detect R version for "r-base" and
        "r-base-dev"
    }
  }
  \subsection{fixed}{
    \itemize{
      \item debian package dependencies now default to "r-base" instead of
        "r-base-dev"
    }
  }
}
\section{Changes in roxyPackage version 0.02-6 (2012-04-10)}{
  \subsection{added}{
    \itemize{
      \item initial (and rather experimental...) support for debianizing packages
        with the \code{debianize()} function
    }
  }
  \subsection{changed}{
    \itemize{
      \item internal function \code{dateRFC2822()} now does the date conversion for
        \code{news2rss()}
    }
  }
}
\section{Changes in roxyPackage version 0.02-5 (2012-04-07)}{
  \subsection{added}{
    \itemize{
      \item generated index.html pages now have a generator meta tag, like the RSS
        feeds
    }
  }
  \subsection{fixed}{
    \itemize{
      \item removed initial tabulator in BibTeX entries of generated citation.html
    }
  }
}
\section{Changes in roxyPackage version 0.02-4 (2012-04-06)}{
  \subsection{fixed}{
    \itemize{
      \item \code{zip()} should now *really* ignore backup files (ending with a tilde)
    }
  }
  \subsection{changed}{
    \itemize{
      \item 'cleanup=TRUE' now recursively removes backup files in the source
        directory
    }
  }
}
\section{Changes in roxyPackage version 0.02-3 (2012-04-05)}{
  \itemize{
    \item using Rd2pdf instead of Rd2dvi for R >= 2.15 now; in 'Rcmd.options',
      Rd2dvi will still be recognized, though
    \item \code{zip()} should now ignore backup files (ending with a tilde)
  }
}
\section{Changes in roxyPackage version 0.02-2 (2012-03-18)}{
  \itemize{
    \item RSS feeds in HTML header now also state the package name (action \code{"html"})
    \item re-wrote HTML genrating functions, now all is done use XiMpLe methods
  }
}
\section{Changes in roxyPackage version 0.02-1 (2012-03-14)}{
  \itemize{
    \item added new function \code{news2rss()} to write RSS feeds (XML) from
      NEWS.Rd/NEWS.html files
    \item action \code{"html"} now adds the RSS feed for a package to the HTML header and
      news link
    \item exported a new function, \code{cl2news()}, which now carries out the action by
      the same name, but can also used stand-alone
    \item added dependency to the XiMpLe package to make the handling of all
      HTML/XML stuff easier
    \item removed \code{"target"} attributes from HTML anchors to comply with XHTML 1.0
      strict
  }
}
\section{Changes in roxyPackage version 0.01-20 (2012-03-11)}{
  \itemize{
    \item action \code{"html"} now also writes an index.html file to the repository root
      directory
  }
}
\section{Changes in roxyPackage version 0.01-19 (2012-03-10)}{
  \itemize{
    \item added option to manually specify options to the internal R CMD calls to
      \code{roxy.package()}
    \item fixed build for multiple R homes, now using R CMD INSTALL instead of
      \code{install.packages()}
    \item OS is now checked and on Windows \code{shell()} is used instead of \code{system()}
    \item fixed broken \code{"check"} action on Windows
    \item added action \code{"cl2news"} to transform ChangeLogs into NEWS.Rd
    \item action \code{"html"} now transforms inst/NEWS.Rd into NEWS.html and links this
      in index.html, if found
    \item replaced unicode symbols in \code{entities()} with \\uxxxx ASCII escapes
  }
}
\section{Changes in roxyPackage version 0.01-18 (2012-02-09)}{
  \itemize{
    \item removed null function \code{roxygen()} from package description files, NULL
      alone is sufficient in roxygen2
  }
}
\section{Changes in roxyPackage version 0.01-17 (2012-01-04)}{
  \itemize{
    \item fixed a problem with line breaks in author information (e.g., for
      multiple contributors)
  }
}
\section{Changes in roxyPackage version 0.01-16 (2011-11-21)}{
  \itemize{
    \item repo directory is now checked and created automatically from the start
      (thanks to natalia belgorodski for reporting the issue)
  }
}
\section{Changes in roxyPackage version 0.01-15 (2011-11-10)}{
  \itemize{
    \item added support for HTML citation info
    \item added function \code{entities()}, mainly to scramble mail adresses
    \item html package info now also links to ChangeLog, if found
    \item fixed typo in html descriptions (some missing colons)
  }
}
\section{Changes in roxyPackage version 0.01-14 (2011-11-09)}{
  \itemize{
    \item \code{roxy.package()} now tries to read DESCRIPTION if pck.description=NULL
  }
}
\section{Changes in roxyPackage version 0.01-13 (2011-11-08)}{
  \itemize{
    \item added support to create HTML index pages to list available packages
  }
}
\section{Changes in roxyPackage version 0.01-12 (2011-10-20)}{
  \itemize{
    \item \code{roxy.package()} now tries to get every descriptive information from
      DESCRIPTION if actions don't include \code{"roxy"}
  }
}
\section{Changes in roxyPackage version 0.01-11 (2011-10-04)}{
  \itemize{
    \item \code{roxy.package()} now uses version number and date from DESCRIPTION if
      actions don't include \code{"roxy"}
  }
}
\section{Changes in roxyPackage version 0.01-10 (2011-09-23)}{
  \itemize{
    \item further tweaked handling of Authors@R for CITATION files
  }
}
\section{Changes in roxyPackage version 0.01-9 (2011-09-17)}{
  \itemize{
    \item added support for manual setting of R.home
  }
}
\section{Changes in roxyPackage version 0.01-8 (2011-09-16)}{
  \itemize{
    \item enhanced handling of Authors@R for CITATION files
  }
}
\section{Changes in roxyPackage version 0.01-7 (2011-09-11)}{
  \itemize{
    \item added support for "Authors@R", as needed by R >= 2.14
    \item added support for different \code{"Type"} values in DESCRIPTION
  }
}
\section{Changes in roxyPackage version 0.01-6 (2011-09-01)}{
  \itemize{
    \item added package check action
    \item added option to keep/remove vignette PDF in source package
  }
}
\section{Changes in roxyPackage version 0.01-5 (2011-08-22)}{
  \itemize{
    \item updated docs
  }
}
\section{Changes in roxyPackage version 0.01-4 (2011-08-21)}{
  \itemize{
    \item fixed a path bug in \code{roxy.package()}
  }
}
\section{Changes in roxyPackage version 0.01-3 (2011-08-18)}{
  \itemize{
    \item added support for "Author@R" and CITATION files
  }
}
\section{Changes in roxyPackage version 0.01-2 (2011-08-17)}{
  \itemize{
    \item added tools dependency to use \code{write_PACKAGES()}
    \item added mac OS X support (pure R only)
  }
}
\section{Changes in roxyPackage version 0.01-1 (2011-08-14)}{
  \itemize{
    \item initial release via reaktanz.de
  }
}
