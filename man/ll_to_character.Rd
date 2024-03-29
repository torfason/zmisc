% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelled_light.R
\name{ll_to_character}
\alias{ll_to_character}
\title{Get the character representation of a labelled variable}
\usage{
ll_to_character(x, default = x, preserve_var_label = FALSE)
}
\arguments{
\item{x}{A labelled variable}

\item{default}{Vector providing a default label for any values not found in
the \code{val_labels} (unlabelled values). Must be of length 1 or of the same
length as x. Useful possibilities are \code{x} (use values where labels are not
found), \code{NA} (return NA for such values), and \code{""} (an empty string).
Missing (\code{NA}) values in \code{x}, however, are never replaced with the default,
they remain \code{NA}.}

\item{preserve_var_label}{Should any \code{var_label} in x be preserved, or
should they be dropped from the result (ensuring that the result is bare
and without any attributes).}
}
\description{
Returns a character representation of a labelled variable, using the value
labels to look up the label for a given value.

The default behavior of this function is similar to
\code{\link[labelled:to_character]{labelled::to_character()}}. The options, however, are slightly different.
Most importantly, instead of specifying \code{NA} handling using parameters, the
function relies on the \code{default} parameter to determine what happens for
unlabelled variables, allowing users to specify including the original values
of \code{x} instead of the labels, returning \code{NA}, or returning a specific string
value. Also, the default behavior is to drop any variable label attribute, in
line with the default \code{\link[=as.character]{as.character()}} method.
}
\seealso{
Other labelled light: 
\code{\link{ll_assert_labelled}()},
\code{\link{ll_labelled}()},
\code{\link{ll_val_labels}()},
\code{\link{ll_var_label}()},
\code{\link{threadbare}()}
}
\concept{labelled light}
\keyword{internal}
