% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git.R
\name{git_tag_info}
\alias{git_tag_info}
\title{git_tag_info}
\usage{
git_tag_info(repo)
}
\arguments{
\item{repo}{(Local directory) path to a git repository.}
}
\value{
list of tag information. Keys are tag names, values are lists with
keys:
\itemize{
\item name: tag name.
\item commit: commit hash.
\item points_to: commit hash that the tag points to.
}
}
\description{
Get tag information for a repository.
}
