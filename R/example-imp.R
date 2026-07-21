#' Full-Compliance Implementation Error
#'
#' An [imp-class] object representing perfect implementation of management
#' advice: TAC and effort recommendations are realised exactly, and any
#' size-based regulation (a change to `Advice@Retention` or
#' `Advice@Selectivity`) is adopted immediately by the entire fleet. This is
#' the [imp-class] object assigned to every built-in example [om-class]
#' object (e.g. [SingleStockOM], [TwoFleetOM]).
#'
#' @format An [imp-class] object. Populated slots:
#'
#' - **`TAC`** ([impslot-class]): `Mean = 1`, `SD = 0` -- the realised TAC
#'   always equals the advised TAC exactly, no bias or stochastic error.
#' - **`Effort`** ([impslot-class]): `Mean = 1`, `SD = 0` -- same, for
#'   effort-based advice.
#' - **`Size`** ([impslot-class]): `Compliance = 1` -- 100% of the fleet
#'   adopts a newly-advised size regulation in the year it changes; there is
#'   no partial/lagged uptake.
#'
#' @details
#' Because this is behaviourally identical to leaving `Imp` unspecified
#' (`NULL`) on an [om-class] object -- a missing `Imp` entry is already
#' treated as full compliance throughout the simulation -- attaching
#' `FullComplianceImp` makes that default explicit rather than implicit. See
#' [OverageImp] and [UnderageImp] for TAC/effort implementation error
#' examples, and [PartialSizeComplianceImp] for a partial size-regulation
#' adoption example.
#'
#' @seealso
#' [imp-class], [Imp()], [ImpSlot()], [OverageImp], [UnderageImp],
#' [PartialSizeComplianceImp]
#'
#' @family imp
#'
#' @examples
#' FullComplianceImp
#' slot(FullComplianceImp, "TAC")
#' slot(FullComplianceImp, "Size")
#'
"FullComplianceImp"


#' Overage Implementation Error
#'
#' An [imp-class] object representing a fleet that consistently realises
#' more catch and effort than advised -- e.g. under-reporting, unrecorded
#' discards landed elsewhere, or effort creep beyond the recommended level.
#'
#' @format An [imp-class] object. Populated slots:
#'
#' - **`TAC`** ([impslot-class]): `Mean` in `[1.05, 1.25]`, `SD` in
#'   `[0.05, 0.15]` -- realised catch is on average 5-25% above the advised
#'   TAC, with additional simulation-to-simulation and year-to-year
#'   stochastic variation.
#' - **`Effort`** ([impslot-class]): same `Mean`/`SD` ranges, applied to
#'   effort-based advice.
#'
#' Empty slots: `Size` (this example isolates TAC/effort overage; see
#' [PartialSizeComplianceImp] for a size-regulation-focused example).
#'
#' @seealso
#' [imp-class], [Imp()], [ImpSlot()], [FullComplianceImp], [UnderageImp],
#' [PartialSizeComplianceImp]
#'
#' @family imp
#'
#' @examples
#' OverageImp
#' slot(OverageImp, "TAC")
#'
"OverageImp"


#' Partial Size-Regulation Compliance Implementation Error
#'
#' An [imp-class] object isolating partial fleet adoption of a size-based
#' regulation (a change to `Advice@Retention` and/or `Advice@Selectivity`),
#' while TAC and effort advice are still implemented perfectly.
#'
#' @format An [imp-class] object. Populated slots:
#'
#' - **`TAC`** ([impslot-class]): `Mean = 1`, `SD = 0` -- perfect TAC
#'   implementation, as in [FullComplianceImp].
#' - **`Effort`** ([impslot-class]): `Mean = 1`, `SD = 0` -- perfect effort
#'   implementation, as in [FullComplianceImp].
#' - **`Size`** ([impslot-class]): `Compliance = 0.6` -- only 60% of the
#'   fleet adopts a newly-advised size regulation in the year it changes; the
#'   remaining 40% continues fishing under the previous Retention/Selectivity
#'   curve. See `.UpdateSelectivitySim()` for how this blend is applied.
#'
#' @seealso
#' [imp-class], [Imp()], [ImpSlot()], [FullComplianceImp], [OverageImp],
#' [UnderageImp]
#'
#' @family imp
#'
#' @examples
#' PartialSizeComplianceImp
#' slot(PartialSizeComplianceImp, "Size")
#'
"PartialSizeComplianceImp"


#' Underage Implementation Error
#'
#' An [imp-class] object representing a fleet that consistently realises
#' less catch and effort than advised -- e.g. quota left unfilled because the
#' fishery isn't profitable enough to fully prosecute, or an effort target
#' that isn't reached due to weather, capacity, or market constraints.
#'
#' @format An [imp-class] object. Populated slots:
#'
#' - **`TAC`** ([impslot-class]): `Mean` in `[0.7, 0.9]`, `SD` in
#'   `[0.05, 0.15]` -- realised catch is on average 10-30% below the advised
#'   TAC, with additional simulation-to-simulation and year-to-year
#'   stochastic variation.
#' - **`Effort`** ([impslot-class]): same `Mean`/`SD` ranges, applied to
#'   effort-based advice.
#'
#' Empty slots: `Size` (this example isolates TAC/effort underage; see
#' [PartialSizeComplianceImp] for a size-regulation-focused example).
#'
#' @seealso
#' [imp-class], [Imp()], [ImpSlot()], [FullComplianceImp], [OverageImp],
#' [PartialSizeComplianceImp]
#'
#' @family imp
#'
#' @examples
#' UnderageImp
#' slot(UnderageImp, "TAC")
#'
"UnderageImp"
