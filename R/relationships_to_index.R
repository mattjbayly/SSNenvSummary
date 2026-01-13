#' Relationships table to upstream reach index
#'
#' Computes the upstream (ancestor) reach IDs for each downstream reach in a directed
#' stream network defined by edge relationships (`fromedge` \eqn{\rightarrow} `toedge`).
#' The result is the transitive closure of the graph: for each reach (`rid`), all reaches
#' that can flow into it (`usrid`) are returned, including the reach itself.
#'
#' @param rel A `data.frame` or `data.table` with two integer-like columns:
#'   \describe{
#'     \item{fromedge}{Upstream reach ID (source node).}
#'     \item{toedge}{Downstream reach ID (target node).}
#'   }
#' @param materialize Logical. If `TRUE` (default), returns a flat two-column table
#'   with one row per `(rid, usrid)` pair. If `FALSE`, returns a compact table with one
#'   row per `rid` and a list-column of upstream IDs.
#'
#' @return A `data.table`.
#' \describe{
#'   \item{If `materialize = TRUE`:}{A two-column table with integer columns `rid` and `usrid`.}
#'   \item{If `materialize = FALSE`:}{A two-column table with integer column `rid` and list-column
#'     `usrid`, where each element is an integer vector of upstream reach IDs (including `rid`).}
#' }
#'
#' @details
#' This function is designed for large stream networks where a full upstream index is useful
#' for fast joins and upstream aggregation (e.g., summing habitat metrics upstream of each reach).
#'
#' Internally, reach IDs are first re-indexed to a compact `1..n` range for speed. Upstream sets
#' are computed efficiently using a dynamic-programming pass over a topological ordering of the
#' graph (i.e., assuming the directed network is acyclic as is typical for dendritic stream
#' networks). Upstream membership is represented as a bitset, enabling fast set unions.
#'
#' If the directed network contains cycles, results may be slower and the interpretation of
#' "upstream" may be ambiguous; consider checking for cycles prior to use.
#'
#' @examples
#' library(data.table)
#' library(SSNenvSummary)
#' rel <- data.table(fromedge = c(2, 3, 4, 9, 14, 15),
#'                   toedge   = c(1, 2, 2, 10, 10, 14))
#'
#' # Flat (materialized) index
#' idx <- relationships_to_index(rel, materialize = TRUE)
#' idx[rid == 1]
#'
#' @seealso
#' \code{\link[igraph]{topo_sort}}, \code{\link[igraph]{graph_from_data_frame}}
#'
#' @export
#' @importFrom data.table as.data.table data.table
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom bit bit as.which
relationships_to_index <- function(rel, materialize = TRUE) {

    rel <- data.table::as.data.table(rel)[, .(fromedge = as.integer(fromedge),
                                  toedge   = as.integer(toedge))]
    rel <- unique(rel)

    # Compact IDs to 1..n (major speedup for indexing/bitsets)
    nodes <- sort(unique(c(rel$fromedge, rel$toedge)))
    n <- length(nodes)
    idx <- stats::setNames(seq_len(n), nodes)

    rel_i <- rel[, .(from = idx[as.character(fromedge)],
                     to   = idx[as.character(toedge)])]

    # Parents list: for each 'to' node, which upstream 'from' nodes flow into it
    parents <- split(rel_i$from, rel_i$to)

    # Topological order (upstream -> downstream) since edges are from->to
    g <- igraph::graph_from_data_frame(rel_i, directed = TRUE, vertices = data.frame(name = seq_len(n)))
    topo <- as.integer(topo_sort(g, mode = "out"))

    # Bitset ancestor sets
    anc <- vector("list", n)
    for (i in seq_len(n)) {
      b <- bit(n)
      b[i] <- TRUE
      anc[[i]] <- b
    }

    # DP propagate ancestors downstream
    for (v in topo) {
      ps <- parents[[as.character(v)]]
      if (length(ps)) {
        # union ancestors of parents into v
        for (p in ps) anc[[v]] <- anc[[v]] | anc[[p]]
        # ensure parents themselves are included (usually already via anc[[p]] but harmless)
        anc[[v]][ps] <- TRUE
      }
    }

    if (!materialize) {
      # Return a compact index: one row per rid with an integer vector of upstream ids
      out <- data.table::data.table(
        rid = nodes,
        usrid = lapply(seq_len(n), function(i) nodes[which(anc[[i]])])
      )
      return(out)
    }

    # Materialize full (rid, usrid) pairs — can be huge.
    # Do it efficiently and optionally write to disk in chunks if needed.
    out_list <- vector("list", n)
    for (i in seq_len(n)) {
      us <- as.which(anc[[i]])
      out_list[[i]] <- data.table(
        rid   = nodes[i],
        usrid = nodes[us]
      )
    }
    out <- data.table::rbindlist(out_list, use.names = TRUE)
    data.table::setkey(out, rid, usrid)
    out[]
  }

