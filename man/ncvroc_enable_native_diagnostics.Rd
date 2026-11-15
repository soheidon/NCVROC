% Generated manually from the roxygen documentation in R/native_diagnostics.R.
\name{ncvroc_enable_native_diagnostics}
\alias{ncvroc_enable_native_diagnostics}
\alias{ncvroc_disable_native_diagnostics}
\alias{ncvroc_disable_windows_crash_dumps}
\alias{ncvroc_native_diagnostics_report}
\alias{print.ncvroc_native_diagnostics_report}
\title{Enable or Disable Native Execution Diagnostics}
\usage{
ncvroc_enable_native_diagnostics(
  log_dir,
  windows_crash_dumps = FALSE,
  dump_type = c("mini", "full"),
  dump_count = 5L
)

ncvroc_disable_native_diagnostics()

ncvroc_disable_windows_crash_dumps()

ncvroc_native_diagnostics_report(log_file)
}
\arguments{
\item{log_dir}{Directory in which per-run diagnostic logs and optional dumps
will be written.}

\item{windows_crash_dumps}{Configure Windows Error Reporting LocalDumps for
Rscript, Rterm, Rgui, and Positron/RStudio \code{rsession.exe} processes.
Existing registry keys are never overwritten.}

\item{dump_type}{\code{"mini"} (default) or \code{"full"}. Full dumps can contain
sensitive data held in process memory.}

\item{dump_count}{Maximum number of dumps Windows retains per executable.}

\item{log_file}{Path to a TSV file produced by
\code{ncvroc_enable_native_diagnostics()}, or a directory containing such
files. When a directory is supplied, the most recently modified log is used.}
}
\value{
The enable function invisibly returns the configured directory. Disable
functions invisibly return \code{NULL} or the names of removed process keys.
The report function returns a \code{ncvroc_native_diagnostics_report} object.
}
\description{
Enable append-only, data-free event logs around Native Streaming candidate
batches. On Windows, LocalDumps can optionally be configured for common R
process executables. Minidumps are the default; full dumps may contain analysis
data in process memory.
}
\details{
Diagnostic logs record batch start and successful return boundaries, allowing
the last active fold and candidate-rank interval to be identified after a
native process crash. They do not contain candidate feature names, outcomes,
or row-level data. Diagnostics are disabled by default.

Windows dump settings are written only when explicitly requested. Existing
LocalDumps registry keys are not overwritten. The disable function removes
only keys carrying the NCVROC ownership marker. Full dumps can contain study
data and other sensitive memory; use them only in a protected directory.
\code{ncvroc_native_diagnostics_report()} identifies candidate batches that
have a start record but no corresponding terminal record, and batches with
ordinary R or native status errors. An unfinished range points to the last
in-flight native batch; it does not by itself
identify the failing C++ source line.

For a symbolized Windows crash stack, install a diagnostic build from source
with the environment variable \code{NCVROC_NATIVE_DEBUG=1}, which enables
\code{-g -O0} and disables DLL stripping for that build. Do not pass
\code{--strip}. Normal package builds remain optimized and stripped. The
generated WER dump and the matching NCVROC DLL from that same build are both
needed for address-to-source-line analysis.
}
\seealso{\code{nested_sum_roc}, \code{ncvroc}}
