% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plugin.R
\name{orderly_plugin}
\alias{orderly_plugin}
\title{Create an orderly plugin}
\usage{
orderly_plugin(config, read, run, schema = NULL)
}
\arguments{
\item{config}{A function to read, check and process the
configuration section in \code{orderly_config.yml}. This function
will be passed the deserialised data from the plugin's section
of \code{orderly_config.yml}, and the full path to that file.  As the
order of loading of plugins is not defined, each plugin must
standalone and should not try and interact with other plugins at
load. It should return a processed copy of the configuration
data, to be passed in as the second argument to \code{read}.}

\item{read}{A function to read, check and process the
configuration section in \code{orderly.yml}, i.e., for a single
report. It will be passed the data for the plugin section of
\code{orderly.yml}, the full path to \code{orderly.yml} and also the
orderly root object.  This last object is not yet exported
normally and so is subject to change! It should return the
processed per-report configuration data, which can then be used
by the \code{run} function.}

\item{run}{A function to mutate the report state at runtime. This
is evaluated at a specific point in the process that needs
describing. It can have an effect by mutating the environment
and by creating files in the working directory. It will be
passed five arguments; (1) the data returned from the \code{read}
function, (2) the root object (as passed to \code{read}), (3) the
report environment, which your function can read and write, (4)
the parameters as passed to \link{orderly_run}, and (5) the
path to the working directory for the report, which your
function can use to write new files into.}

\item{schema}{Optionally a path to a schema for the metadata
created by this plugin. See \code{vignette("plugins")} for details.}
}
\value{
An \code{orderly_plugin} object, though normally this would not
be interacted with by users. Typically, this will be passed
directly to \link{orderly_plugin_register}
}
\description{
Create an orderly plugin. A plugin is typically defined by a
package and is used to extend orderly by enabling new
functionality, declared in \code{orderly_config.yml} and \code{orderly.yml}
and affecting the running of reports primarily by creating new
objects in the report environment.  This system is discussed in
more detail in \code{vignette("plugins")}, but will be expanded (likely
in breaking ways) soon.
}
