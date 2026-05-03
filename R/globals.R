# Suppress R CMD check notes about "no visible binding for global variable"
# These are used in parallel processing or for gganimate transitions
utils::globalVariables(c(
  ".L_DATA",
  ".L_PARAMS",
  ".L_ANCHOR",
  ".L_NFRAMES",
  ".L_FPL",
  ".d",
  ".p",
  ".pa",
  ".nf",
  ".fpl",
  ".frame"
))
