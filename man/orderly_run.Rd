% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orderly.R
\name{orderly_run}
\alias{orderly_run}
\title{Run a report}
\usage{
orderly_run(name, parameters = NULL, envir = NULL, root = NULL, locate = TRUE)
}
\arguments{
\item{name}{Name of the report to run}

\item{parameters}{Parameters passed to the report. A named list of
parameters declared in the \code{orderly.yml}.  Each parameter
must be a scalar character, numeric, integer or logical.}

\item{envir}{The environment that will be used to evaluate the
report script; by default we use the global environment, which
may not always be what is wanted.}

\item{root}{The path to an orderly root directory, or \code{NULL}
(the default) to search for one from the current working
directory if \code{locate} is \code{TRUE}.}

\item{locate}{Logical, indicating if the configuration should be
searched for.  If \code{TRUE} and \code{config} is not given,
then orderly looks in the working directory and up through its
parents until it finds an \code{.outpack} directory}
}
\value{
The id of the newly created report
}
\description{
Run a report.  This will create a new directory in
\verb{drafts/<reportname>}, copy your declared resources there, run
your script and check that all expected artefacts were created.
}
\section{Differences with orderly}{


There is no longer a "commit" phase, we will write a vignette
explaining this change more fully.

Not supported yet:
\itemize{
\item automatic handling of README files
\item dependencies (at least mrc-3113)
\item global resources (requires more orderly2 configuration)
\item fields (requires more orderly2 configuration)
\item tags (we will probably remove these)
\item environment
\item secrets
\item all database support (connection, data and views, plus interpolation
of parameters into queries)
\item automatic installation of missing packages (VIMC-6626)
\item check balance of the device and conneciton stack (VIMC-6628)
\item store random number state in metadata (VIMC-6627)
\item store git status in metadata (mrc-3380)
\item save metadata on failure (mrc-3379)
\item loading environment variables from orderly_envir.yml
\item echoing run output to console not supported (mrc-3381)
}

Smaller behavioural changes that might be up for changing later
\itemize{
\item Strip leading \verb{src/} from \code{name}
\item Allow \code{name = NULL} and use current working directory
\item The \verb{.outpack/} directory now marks the root, not the
\code{orderly_config.yml}  file, though this must also exist.
}
}

