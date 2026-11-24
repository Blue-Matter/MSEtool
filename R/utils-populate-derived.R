# Track the slots that a Populate*() function derives from other inputs, so
# that re-populating an object after its inputs change gives the same result
# as populating it from scratch.
#
# Without this, derived slots that are only filled when empty (e.g. `MeanAtAge`
# converted from `MeanAtLength`, `MeanAtLength` from `CalcAtLength`,
# `Fecundity` from weight x maturity, relative selectivity scaled by maturity
# L50) keep the values from the first call when the inputs change.
#
# Precedence rule applied on every (non-cached) call:
# 1. A slot derived by a previous call, and not edited since, is reset (usually
#    to `NULL`), and so is derived again from the current inputs.
# 2. If `Pars` holds a resolvable model, `MeanAtAge`, `MeanAtLength`, and
#    `MeanAtWeight` are regenerated from it (any existing values are
#    overwritten).
# 3. Otherwise a supplied `MeanAtAge` is used as is; if it is empty, it is
#    derived from `MeanAtLength`, then `MeanAtWeight`.
#
# A derived slot that is edited after population (its values no longer match
# the recorded hash) is treated as supplied by the user.
#
# Inputs sampled per simulation by Populate (`Pars` and `CVatAge` bounds,
# numeric spatial inputs) are never reset: the sampled values are kept, so
# re-population (including internal calls without a seed) does not re-sample
# them. They are recorded only to detect edits.

# slots that a Populate*() call can fill from other inputs
.PopulateDerivableSlots <- c(
  'MeanAtAge', 'MeanAtLength', 'MeanAtWeight', 'Classes', 'ALK', 'AWK',
  'Semelparous', 'isRel',
  'Movement', 'UnfishedDist', 'ProbStaying', 'RelativeSize', 'FracOther',
  # stock-recruitment: default SD/AC and generated recruitment deviations
  'SD', 'AC', 'RecDevInit', 'RecDevHist', 'RecDevProj'
)

# inputs recorded to detect edits; reset only where noted in .RecordDerived()
.PopulateInputSlots <- c('Pars', 'Model', 'CVatAge')

.PopulateAtSlots <- c('MeanAtAge', 'MeanAtLength', 'MeanAtWeight')

# size classes and keys, recalculated only when the object's own inputs change
.PopulateSizeSlots <- c('Classes', 'ALK', 'AWK')

# slots that must be reset together: if any member was edited since
# population, all members are kept (e.g. `Pars` scaled by maturity L50 with
# `isRel` set to FALSE; size `Classes` with the `ALK` and at-size values)
.PopulateLinkedSlots <- list(
  c('Pars', 'Model', 'isRel'),
  c('Classes', 'ALK', 'AWK', 'MeanAtLength', 'MeanAtWeight')
)

# hash of the values only: renaming dimensions (e.g. .RepairSimDimnames()) is
# not an edit
.StripNames <- function(x) {
  if (is.list(x) && !is.data.frame(x)) return(lapply(unname(x), .StripNames))
  if (is.atomic(x)) { dimnames(x) <- NULL; names(x) <- NULL }
  x
}
.SlotHash <- function(x) digest::digest(.StripNames(x), algo = 'xxhash64')

.IsEmptySlot <- function(x) is.null(x) || !length(x)

# `Pars` with a model that Populate will use to generate the at-age/at-size arrays
.HasParsModel <- function(object) {
  if (!'Pars' %in% methods::slotNames(object) || .ParsEmpty(object@Pars))
    return(FALSE)
  tryCatch({
    object@Pars <- .AddDefaultPars(object)
    !is.null(.FindModel(object, doCheck = FALSE))
  }, error = function(e) FALSE)
}

#' Reset Derived Slots Before Populating
#'
#' Resets the slots derived by a previous `Populate*()` call (and not edited
#' since), and clears the at-age and at-size arrays when `Pars` holds a
#' resolvable model. See `R/utils-populate-derived.R`.
#'
#' @param object A life-history, fleet schedule, or spatial object.
#' @return `object`, ready to be populated.
#' @keywords internal
#' @noRd
.ResetDerived <- function(object) {
  Derived <- object@Misc$Derived
  Edited  <- \(s) s %in% names(Derived) &&
    !identical(.SlotHash(methods::slot(object, s)), Derived[[s]]$Hash)
  AnyEdited <- \(slots) any(purrr::map_lgl(slots, Edited))

  # `Pars` or `Model` edited since population (e.g. `Pars` removed to use the
  # arrays directly): the at-age/at-size arrays are kept as supplied (and are
  # regenerated below if the new `Pars` hold a resolvable model)
  ModelEdited <- AnyEdited(c('Pars', 'Model'))
  # members of a linked group with an edited member are kept
  Keep <- unlist(purrr::keep(.PopulateLinkedSlots, AnyEdited))
  # an object's own size classes and key (length ALK, weight AWK) are kept
  # unless its own inputs changed; other objects copy the classes of the
  # length or weight object, so they are reset
  OwnsKey <- any(c('ALK', 'AWK') %in% methods::slotNames(object))
  if (OwnsKey && !AnyEdited(c('Pars', 'CVatAge', 'MeanAtAge')))
    Keep <- c(Keep, .PopulateSizeSlots)

  for (s in intersect(names(Derived), methods::slotNames(object))) {
    if (!isTRUE(Derived[[s]]$Reset)) next
    if (ModelEdited && s %in% .PopulateAtSlots) next
    if (s %in% Keep || Edited(s)) next
    methods::slot(object, s) <- Derived[[s]]$Input
  }
  object@Misc$Derived <- NULL
  # restore the unpopulated form, so that e.g. an object that was empty before
  # population is recognised as empty (EmptyObject()) again
  if (!length(object@Misc)) object@Misc <- list()
  attributes(object)$digest <- NULL

  if (.HasParsModel(object)) {
    for (s in intersect(.PopulateAtSlots, methods::slotNames(object)))
      methods::slot(object, s) <- NULL
  }
  object
}

#' Record Derived Slots After Populating
#'
#' Records, with a hash of the populated value, the slots that `Populate*()`
#' filled from other inputs (empty in `Input`, populated in `object`) and the
#' inputs it transformed, so that `.ResetDerived()` can reset them on the next
#' call. Reset to the input: derived slots (to empty), a logical `Semelparous`
#' expanded to an array, and relative `Pars` scaled by maturity L50 (to the
#' sampled relative values, with `isRel = TRUE`). Other inputs are recorded
#' only to detect edits.
#'
#' @param object The populated object.
#' @param Input The object as returned by `.ResetDerived()` at the start of
#'   the call.
#' @return `object` with `Misc$Derived` set.
#' @keywords internal
#' @noRd
.RecordDerived <- function(object, Input) {
  Derived <- list()
  Record <- \(s, Reset, In = NULL) list(Hash = .SlotHash(methods::slot(object, s)), Reset = Reset, Input = In)
  Slots <- methods::slotNames(object)

  # values saved by Populate before overwriting an input in place, restored on
  # the next call: relative `Pars` (sampled) before scaling by maturity L50,
  # the sampled `UnfishedDist` target before the equilibrium of the fitted
  # movement replaces it
  Restore <- object@Misc$Restore

  for (s in intersect(.PopulateDerivableSlots, Slots)) {
    In  <- methods::slot(Input, s)
    Out <- methods::slot(object, s)
    if (identical(In, Out) || .IsEmptySlot(Out)) next
    Expanded <- s == 'Semelparous' && is.logical(In) && !is.array(In)
    Scaled   <- s == 'isRel' && isTRUE(as.logical(In)) && !is.null(Restore$Pars)
    if (!is.null(Restore[[s]]) && s != 'isRel') {
      Derived[[s]] <- Record(s, Reset = TRUE, In = Restore[[s]])
    } else if (.IsEmptySlot(In) || Expanded || Scaled) {
      Derived[[s]] <- Record(s, Reset = TRUE, In = In)
    }
  }

  for (s in intersect(.PopulateInputSlots, Slots)) {
    Derived[[s]] <- if (!is.null(Restore[[s]])) Record(s, Reset = TRUE, In = Restore[[s]]) else
      Record(s, Reset = FALSE)
  }
  object@Misc$Restore <- NULL
  object@Misc$Derived <- if (length(Derived)) Derived else NULL
  object
}

# refresh the recorded hash of derived `slots` modified after population by a
# step that is re-applied on every population (e.g. AdjustSeasonalFecundity()),
# so they remain derived rather than treated as edited by the user
.RefreshDerived <- function(object, slots) {
  Derived <- object@Misc$Derived
  for (s in intersect(slots, names(Derived)))
    Derived[[s]]$Hash <- .SlotHash(methods::slot(object, s))
  object@Misc$Derived <- Derived
  object
}

# mark `slots` of `object` copied from another object after population (e.g.
# SRR values shared from the SPFrom source stock) as derived, so they are reset
# to the values the object was populated from (its own input) on the next
# population and copied again, rather than treated as supplied by the user.
# `Before` is the object before the copy.
.MarkCopied <- function(object, Before, slots) {
  Derived <- object@Misc$Derived
  for (s in slots) {
    if (identical(methods::slot(object, s), methods::slot(Before, s))) next
    Prev  <- Derived[[s]]
    Input <- if (!is.null(Prev) && isTRUE(Prev$Reset)) Prev$Input else methods::slot(Before, s)
    Derived[[s]] <- list(Hash = .SlotHash(methods::slot(object, s)), Reset = TRUE, Input = Input)
  }
  object@Misc$Derived <- Derived
  object
}

# record derived slots and set the digest at the end of a Populate*() call
.FinishPopulate <- function(object, Input, argList) {
  .SetDigest(.RecordDerived(object, Input), argList)
}

# relative (`isRel`) selectivity or retention Pars, including Pars already
# scaled by maturity L50 in a previous call (`isRel` then FALSE)
.IsRelativePars <- function(object) {
  isTRUE(as.logical(object@isRel)) || isTRUE(as.logical(object@Misc$Derived$isRel$Input))
}
