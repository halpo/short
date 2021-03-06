% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag_release.R
\name{tag_release}
\alias{tag_release}
\title{Tag a package release}
\usage{
tag_release(pkg = ".", commit = info$commit, ..., dev.version = TRUE,
  push = TRUE)
}
\arguments{
\item{pkg}{the base directory of the package}

\item{commit}{the string identifying the git commit to tag with the
release.  By default this is read from the
"CRAN-RELEASE" file.}

\item{...}{passed to the \code{\link[git2r:tag]{git2r::tag()}} function}

\item{dev.version}{Should the version number be incremented
after committing the tag?}

\item{push}{Should the changes be pushed to the default git repository?}
}
\description{
Use the CRAN-RELEASE file to automate the tagging of the
commit with the version.
}
\seealso{
\code{\link[git2r:tags]{git2r::tags()}}, \code{\link[usethis:use_dev_version]{usethis::use_dev_version()}}, \code{\link[git2r:push]{git2r::push()}}
}
