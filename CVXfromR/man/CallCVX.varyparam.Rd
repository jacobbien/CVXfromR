\name{CallCVX.varyparam}
\alias{CallCVX.varyparam}
\title{Simple R interface to CVX to solve sequence of problems.}
\description{
This function takes a string containing what you'd normally put between
cvx_begin and cvx_end.  Returns CVX output.  Allows a sequence of problems 
to be performed that are identical except that a single scalar parameter is varied.
}
\usage{
CallCVX.varyparam(cvx.code, const.vars, tuning.param, opt.var.names, 
setup.dir = NULL, norun=FALSE, matlab.call = "matlab", cvx.modifiers = NULL)
}
\arguments{
  \item{cvx.code}{
String containing call to CVX, i.e. what's inside cvx_begin and cvx_end
}
  \item{const.vars}{
List of non-optimization variables used in CVX expression.  Labels of
list elements should match what the corresponding variable name in
cvx.code.  E.g. for the Lasso, this would be list(y=y, x=x, lam=lam, p=p)
}
  \item{tuning.param}{A list with a single vector (with a name).
 E.g. list(lam=c(0.1, 1, 2)).  This is a non-optimization variable used in 
 CVX expression that you want varied.  Each element of this vector is a level
 of the parameter. The problem will be solved at each such level.}
  \item{opt.var.names}{Array of names of optimization
    variables. E.g. for the Lasso, this would be "beta"
}
  \item{setup.dir}{
Directory containing the file cvx_setup.  If not needed, leave NULL.
}
  \item{norun}{
Default FALSE.  Mostly for debugging purposes.  Doesn't call Matlab.  Returns the command that would be run in Matlab.
}
  \item{matlab.call}{How Matlab can be invoked through the "system" command.  Default: "matlab" but even if this is the alias in your default shell, "system" might use a different shell in which "matlab" is not recognized.
}
  \item{cvx.modifiers}{Optional string of modifiers passed to CVX on same line as cvx_begin. E.g. "quiet" or "sdp".}
}
\details{
This function is very similar to \code{CallCVX}, but allows one to solve a sequence of
problems in which only one (scalar) non-optimization variable is varied.  The intended use is
for when a problem has a tuning parameter, and you want to solve the problem at various values
of this parameter.  Using this function is much more efficient than wrapping \code{CallCVX} in
a loop.
}
\value{
  A list of the optimization variables specified in opt.var.names.  Each variable is a list of length
 equal to the number of problems solved.  Also,
  \item{cvx_optval}{The optimal values as returned by CVX in each of the problems.}
  \item{time}{Elapsed total time specifically for all the CVX calls (i.e. excludes
    opening Matlab, loading data, etc.)}
}
\references{
M. Grant and S. Boyd. CVX: Matlab software for disciplined convex programming, version 1.21. http://cvxr.com/cvx, April 2011.

M. Grant and S. Boyd. Graph implementations for nonsmooth convex
programs, Recent Advances in Learning and Control (a tribute to
M. Vidyasagar), V. Blondel, S. Boyd, and H. Kimura, editors, pages
95-110, Lecture Notes in Control and Information Sciences, Springer,
2008. http://stanford.edu/~boyd/graph_dcp.html.
}
\author{
Jacob Bien
}

\seealso{
\code{\link{CallCVX}}
}
\examples{
# Solve the lasso at multiple lambda values using CVX
n <- 50
p <- 10
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
y <- x \%*\% beta + 0.1 * rnorm(n)
lam <- seq(2, 0.1, length=10)
# to call CVX, set setup.dir to be the directory containing "cvx_setup.m"
\dontrun{
setup.dir <- "change/this/to/your/cvx/directory"
lasso <- CallCVX.varyparam("variables beta(p);minimize(square_pos(norm(y-x*beta,2))/2+lam*norm(beta,1))",
                 const.vars=list(p=p, y=y, x=x),
		 tuning.param=list(lam=lam),
                 opt.var.names="beta", setup.dir=setup.dir)
}
}
