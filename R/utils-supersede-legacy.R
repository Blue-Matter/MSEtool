# Marks a legacy function as superseded; no runtime warning
.SupersedeLegacy <- function(what, with = NULL) {
  lifecycle::signal_stage("superseded", what, with)
}
