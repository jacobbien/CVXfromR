# This file contains three functions:
#   - CallMatlab allows you to execute Matlab code from R.
#   - CallCVX allows you to use CVX from R.
#   - CallCVX.varyparam allows you to use CVX for a sequence of problems from R.
# 
# See examples.R for usage of these functions.

CallMatlab <- function(matlab.code, inputs=list(), output.names=NULL, delete.temp=TRUE, norun=FALSE, matlab.call="matlab", unique.string="") {
  # function that executes Matlab code
  #
  # Args:
  #  matlab.code: string containing matlab code
  #  inputs: list containing all variables referred to in matlab.code.
  #          The names of the elements of the list should match the name
  #          used in matlab.code
  #  ouput.names: array of names of any variables that should be outputted.
  #               The names should match those used in matlab.code.
  #  delete.temp: indicates whether the temp files that are created should be
  #               deleted.
  #  norun: doesn't call Matlab.  Returns the command that would be run in Matlab.
  #  matlab.call: how Matlab can be invoked through "system" command.  Default: "matlab" but
  #               even if this is the alias in your default shell, "system" might use a different shell
  #               in which "matlab" is not recognized.
  #  unique.string: a character string added to temporary files. This is necessary if running
  #                 in parallel (for example with parallel::mclapply) since the
  #                 base::tempfile function may not produce unique file names otherwise.
  #                 see help(tempfile)
  #
  #
  # Returns:
  #   outputs: list containing all variables that were listed in output.names
  #            with the values they have after matlab.code was executed in Matlab
  #   command: the string that is run on the command line.
  infiles <- NULL
  outfiles <- NULL
  before <- ""
  if (length(inputs) > 0) {
    for (i in seq(length(inputs))) {
      # write input variable to text file:
      input.name <- names(inputs)[i]
      if (!is.numeric(inputs[[i]]))
        stop(sprintf("All inputs must be numeric! (check %s)", input.name))
      ## file <- sprintf("temp_in_%s.txt", input.name)
      file <- tempfile(pattern=sprintf("temp_in_%s_%s.txt", input.name, unique.string),
                       fileext='.txt')      
      write.table(inputs[[i]],
                  file=file,
                  row.names=FALSE,
                  col.names=FALSE)
      # form matlab expression to read in this file:
      before <- sprintf("%s disp('Reading %s into Matlab...'); %s = dlmread('%s');",
                        before, input.name, input.name, file)
      infiles <- c(infiles, file)
    }
  }
  after <- ";"
  if (!is.null(output.names)) {
    for (out in output.names) {
      # form matlab expression to write output variables to file:
      ## file <- sprintf("temp_out_%s.txt", out)
      file <- tempfile(pattern=sprintf("temp_out_%s_%s.txt", out, unique.string),
                       fileext='.txt')
      after <- sprintf("%s dlmwrite('%s', full(%s), 'precision', '%s10.10f');",
                       after, file, out, "%")
      outfiles <- c(outfiles, file)
    }
  }
  # put together full matlab command:
  if (.Platform$OS.type == "unix")
    command <- sprintf("%s -nodisplay -r \"%s%s%s%s\"",
                       matlab.call, before, matlab.code, after, "exit;")
  else if (.Platform$OS.type == "windows")
    command <- sprintf("%s -wait -nosplash -nodesktop -r \"%s%s%s%s\"",
                       matlab.call, before, matlab.code, after, "exit;")
  else stop("Not a recognized operating system.")
  if (norun) return(command)
  # execute command to open matlab, run code, and exit:
  system(command)
  
  # read matlab output files into R:
  outputs <- list()
  if (!is.null(output.names)) {
    for (i in seq(length(output.names))) {
      outputs[[output.names[[i]]]] <- drop(as.matrix(read.csv(outfiles[i], header=FALSE)))
      colnames(outputs[[output.names[[i]]]]) <- NULL
    }
  }
  if (delete.temp) {
    # delete text files that were created:
    files <- paste(c(infiles, outfiles), collapse=" ")
    system(sprintf("rm %s", files))
  }
  outputs[["command"]] <- command
  
  outputs
}

CallCVX <- function(cvx.code, const.vars, opt.var.names, setup.dir=NULL, norun=FALSE,
                    matlab.call="matlab", cvx.modifiers=NULL, unique.string="") {
  # Simple R interface to CVX.
  #
  # Args:
  #  cvx.code: string containing call to CVX, i.e. what's inside cvx_begin and cvx_end
  #  const.vars: list of non-optimization variables used in CVX expression.
  #              labels of list elements should be the name of the variable.
  #  opt.var.names: array of names of optimization variables.
  #  setup.dir: directory containing the file cvx_setup.  If not needed, leave null
  #  norun: doesn't call Matlab.  Returns the command that would be run in Matlab.
  #  matlab.call: how Matlab can be invoked through "system" command.  Default: "matlab" but
  #               even if this is the alias in your default shell, "system" might use a different shell
  #               in which "matlab" is not recognized.
  #  cvx.modifiers: optional string of modifiers passed to CVX on same line as cvx_begin. E.g. "quiet" or "sdp".
  #  unique.string: a character string added to temporary files. This is necessary if running
  #                 in parallel (for example with parallel::mclapply) since the
  #                 base::tempfile function may not produce unique file names otherwise.
  #                 see help(tempfile)

  #
  # Returns:
  #   cvx_optval (as returned by CVX) and an optimal point found by CVX
  setup <- ""
  if (!is.null(setup.dir)) {
    nc <- nchar(setup.dir)
    if (substr(setup.dir, nc, nc) == "/") setup.dir <- substr(setup.dir, 1, nc - 1)
    if (!("cvx_setup.m" %in% list.files(setup.dir)))
      stop("Could not find cvx_setup.m in user-provided \'setup.dir\'.")
    setup <- sprintf("run %s/cvx_setup;", setup.dir)
  }
  if (is.null(cvx.modifiers)) cvx.modifiers <- ""
  # seems to get caught when " * " is used.  So remove spaces in this case:
  cvx.code <- gsub(" +[*] +", "*", cvx.code)
  if ("time" %in% opt.var.names)
    stop("'time' is an invalid name for an optimization variable.")
  matlab.code <- sprintf("%s tStart=tic; cvx_begin %s; %s; cvx_end;time=toc(tStart);disp(['time elapsed: ',num2str(time)])",
                         setup, cvx.modifiers, cvx.code)
  CallMatlab(matlab.code,
             inputs=const.vars,
             output.names=c(opt.var.names, "cvx_optval", "time"), norun=norun,
             matlab.call=matlab.call, unique.string=unique.string)
}

CallCVX.varyparam <- function(cvx.code, const.vars, tuning.param, opt.var.names, setup.dir=NULL, norun=FALSE, matlab.call="matlab", cvx.modifiers=NULL, unique.string="") {
  # Simple R interface to CVX.  Allows a sequence of problems to be solved where a single scalar parameter
  #   is varied, but otherwise the problems are identical.
  #
  # Example: For lasso, tuning.param would be list(lam=c(0.1, 1, 2)).  const.vars would contain x and y as usual.  This would solve
  #          the lasso at the provided values of the tuning parameter lam.
  #
  # Args:
  #  cvx.code: string containing call to CVX, i.e. what's inside cvx_begin and cvx_end.  Use the special word "tuningparam"
  #            for the variable that will be iterated.
  #  const.vars: list of non-optimization variables used in CVX expression.
  #              labels of list elements should be the name of the variable.
  #  tuning.param: a list with a single vector (with a name).  E.g. list(lam=c(0.1, 1, 2)).  Each element of this vector is a
  #               level of the parameter.  The problem will be solved at each such level.
  #  opt.var.names: array of names of optimization variables.
  #  setup.dir: directory containing the file cvx_setup.  If not needed, leave null
  #  norun: doesn't call Matlab.  Returns the command that would be run in Matlab.
  #  matlab.call: how Matlab can be invoked through "system" command.  Default: "matlab" but
  #               even if this is the alias in your default shell, "system" might use a different shell
  #               in which "matlab" is not recognized.
  #  cvx.modifiers: optional string of modifiers passed to CVX on same line as cvx_begin. E.g. "quiet" or "sdp".
  #  unique.string: a character string added to temporary files. This is necessary if running
  #                 in parallel (for example with parallel::mclapply) since the
  #                 base::tempfile function may not produce unique file names otherwise.
  #                 see help(tempfile)
  #
  #  # Returns:
  #   cvx_optval (as returned by CVX), a sequence of optimal points found by CVX, and the total time to solve all problems.
  setup <- ""
  if (!is.null(setup.dir)) {
    if (!("cvx_setup.m" %in% list.files(setup.dir)))
      stop("Could not find cvx_setup.m in user-provided \'setup.dir\'.")
    setup <- sprintf("dir = pwd; cd %s; cvx_setup; cd(dir);", setup.dir)
  }
  if (is.null(cvx.modifiers)) cvx.modifiers <- ""
  # seems to get caught when " * " is used.  So remove spaces in this case:
  cvx.code <- gsub(" +[*] +", "*", cvx.code)
  special <- c("time", "iter", "out")
  if (any(special %in% opt.var.names))
    stop("'time' and 'iter' are invalid names.")
  if (class(tuning.param) != "list" || length(tuning.param) != 1)
    stop("tuning.param must be a list with a single element.")
  if (class(tuning.param[[1]]) != "numeric") stop("tuning.param[[1]] must be a numeric vector.")
  if (is.null(names(tuning.param))) stop("names(tuning.param) must be nonnull.")
  param.name <- names(tuning.param)
  const.vars[[sprintf("%s_all", param.name)]] <- tuning.param[[1]]
  nprob <- length(tuning.param[[1]])
  after <- ""
  opt.var.names <- c(opt.var.names, "cvx_optval")
  for (out in opt.var.names) {
    # rename each out variable: myvar_iter = myvar (where eval in matlab will replace iter by its value)
    after <- sprintf("%s eval(sprintf('%s_%s=%s;', iter));", after, out, "%d", out)
  }
  code <- sprintf("for iter=1:%s; %s=%s_all(iter); cvx_begin %s; %s; cvx_end; %s end",
                  nprob, param.name, param.name, cvx.modifiers, cvx.code, after)
  matlab.code <- sprintf("%s tStart=tic; %s; time=toc(tStart);disp(['time elapsed: ',num2str(time)])",
                         setup, code)
  output.names <- NULL
  for (out in opt.var.names) {
    output.names <- c(output.names, sprintf("%s_%s", out, seq(nprob)))
  }
  cvx <- CallMatlab(matlab.code,
             inputs=const.vars,
                    output.names=c(output.names, "time"), norun=norun,
                    matlab.call=matlab.call, unique.string=unique.string)
  if (norun) return(cvx)
  cvx2 <- list()
  for (out in opt.var.names) {
    cvx2[[out]] <- list()
    for (i in seq(nprob)) {
      cvx2[[out]][[i]] <- cvx[[sprintf("%s_%s", out, i)]]
    }
  }
  cvx2[[param.name]] <- tuning.param[[1]]
  cvx2$cvx_optval <- as.numeric(cvx2$cvx_optval)
  cvx2$time <- cvx$time
  cvx2$command <- cvx$command
  cvx2
}
