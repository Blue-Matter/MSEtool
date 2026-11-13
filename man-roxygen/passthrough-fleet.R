#' @section Pass-Through Access:
#' When `<%= arg %>` is an object that contains `<%= slot %>` objects,
#' `<%= slot %>()` returns them instead of constructing a new object:
#'
#' - a [fleet-class] object: its `<%= slot %>` slot.
#' - an [om-class], [hist-class], or [mse-class] object: a list by stock of
#'   lists by fleet, with the `<%= slot %>` slot of each fleet. A list of
#'   [fleet-class] objects, or a list of them by stock, returns a list with
#'   the same structure.
<% if (exists("containers") && containers == "yes") { -%>
#' - a [data-class] or [obs-class] object, or an [exploitationdata-class] or
#'   [exploitationobs-class] object or a list of them: the `<%= slot %>` slot
#'   of the exploitation information.
<% } -%>
<% if (exists("extra")) { -%>
#' - <%= extra %>
<% } -%>
#'
#' `<%= slot %>(x) <- value` replaces the slot of a [fleet-class] or
#' [om-class] object<% if (exists("containers") && containers == "yes") { %>, a [data-class] or [obs-class] object, an
#' [exploitationdata-class] or [exploitationobs-class] object,<% } %> or of
#' each object in a list of them. For an [om-class] object, `value` is a
#' single object when the OM has one stock and one fleet, a list named by
#' fleet (see [FleetNames()]) when it has one stock and several fleets, and a
#' list named by stock (see [StockNames()]) of lists named by fleet when it
#' has several stocks. The slot cannot be replaced in a [hist-class] or
#' [mse-class] object, because it was used to simulate the historical
#' dynamics.
