% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_manifest.R
\name{export_manifest}
\alias{export_manifest}
\title{Export manifest file with metadata}
\usage{
export_manifest(
  manifest_path,
  input_files,
  output_files,
  params,
  ...,
  file_summary_info = FALSE
)
}
\arguments{
\item{manifest_path}{Path to the manifest file.}

\item{input_files}{List or vector (named or unnamed) of files that are
inputs to the workflow. Passed to \verb{[get_file_metadata()]}.}

\item{output_files}{List or vector (named or unnamed) of files that are
outputs from the workflow. Passed to \verb{[get_file_metadata()]}.}

\item{params}{List parameters used to define the workflow.}

\item{...}{Nested (named) lists to be included in manifest. Passed on to}

\item{file_summary_info}{Logical. If \code{TRUE}, include file summary
information.
\code{create_manifest}.}
}
\value{
(invisible) JSON string with metadata manifest.
}
\description{
Export manifest file with metadata
}
