\name{CallMatlab}
\alias{CallMatlab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function that executes Matlab code}
\description{Given a string of Matlab code, a list of inputs, and a
  vector of names of output variables, this
  function opens Matlab, runs the Matlab code, closes Matlab, and then returns
  all the output variables.  This function assumes that Matlab can be
  called from the command line using "matlab" (or more generally the string given in the argument matlab.call)
}
\usage{
CallMatlab(matlab.code, inputs = list(), output.names = NULL, delete.temp = TRUE, norun = FALSE, matlab.call = "matlab")
}
\arguments{
  \item{matlab.code}{
    A string containing Matlab code.  Use semicolons to separate lines.
    This function assumes that the Matlab code is valid.  The names of
    variables used in this string should match those in the inputs argument.
}
  \item{inputs}{
    List containing all variables that need to be passed to
    Matlab. The names of the elements of the list should match the
    names used in matlab.code.  All variables must be numeric
    (e.g. matrices, vectors, scalars)
}
  \item{output.names}{
    Array of names of all variables that should be returned from
    Matlab.  The names should match those used in matlab.code.  All
    variables must be numeric (no strings).
}
  \item{delete.temp}{
    Default TRUE.  Indicates whether the temporary files that are created should be deleted.
}
  \item{norun}{
Default FALSE.  Mostly for debugging purposes.  Returns the command that
would be run in Matlab without it actually opening Matlab.
}
  \item{matlab.call}{How Matlab can be invoked through the "system" command.  Default: "matlab" but even if this is the alias in your default shell, "system" might use a different shell in which "matlab" is not recognized.
}
}
\details{
This function is pretty low-tech but works well.  It begins by writing every variable in inputs to a separate text file
named temp_in_<variablename>.txt.  A Matlab expression is then formed
that does the following:

1) reads in each of the input files;
2) executes matlab.code;
3) writes each output variable to a file named
temp_out_<variablename>.txt.

CallMatlab uses the R function "system" to call Matlab from the command
line.  Finally, R reads in each output file and deletes all the files
that were created (if delete.temp==TRUE).
}
\value{
  A list containing all the outputs that were listed in output.names
  with the values they have after matlab.code was executed in Matlab.
\item{command}{The string that is run on the command line.}
}
\author{
Jacob Bien
}
\examples{
# 1) example of basic usage
x <- rnorm(100)
y <- 1:100
inputs <- list(x=x, y=y)
\dontrun{
out <- CallMatlab("z=x+y", inputs, "z")
out2 <- CallMatlab("z=x+y;x=x+1", inputs, c("z","x"))
}

# 2) example where there are no input variables
\dontrun{
CallMatlab("I = eye(100)", output.names="I")
}
}
