#' Relationships table to Upstream reach ID Idex Table
#'
#' Given a table of directed stream‐network edges (`fromedge` → `toedge`), this function
#' summarizes each downstream reach (`rid`) every upstream reach ID (`usrid`), including itself.
#'
#' @param rel A `data.table` or `data.frame` with two integer columns:
#'   \describe{
#'     \item{fromedge}{The upstream reach ID.}
#'     \item{toedge}{The downstream reach ID.}
#'   }
#' @return A `data.table` with integer columns:
#'   \describe{
#'     \item{rid}{Downstream reach ID.}
#'     \item{usrid}{Upstream reach ID (includes the reach itself).}
#'   }
#' @details
#' This function iteratively expands the set of upstream relationships via fast keyed joins
#' in **data.table**.  It first seeds each node as upstream of itself, adds all direct
#' parents, then “peels back” generation by generation until no new pairs appear.
#'
#' If starting from the `SSN2` or `SSNBler` R packages use the `relationships.csv` table from
#' the LSN (land scape network) folder.
#'
#' @examples
#' library(data.table)
#' rel <- data.table(fromedge = c(2, 3, 4, 9, 14, 15), toedge = c(1, 2, 2, 10, 10, 14))
#' # For rid = 1, usrid will be 1,2,3,4; for rid = 2, usrid = 2,3,4; etc.
#' relationships_to_index(rel)
#'
#' @importFrom data.table setDT setkey rbindlist
#' @importFrom data.table data.table
#' @export
relationships_to_index <- function(rel) {

    ## pacify “no visible binding” notes
    rid <- usrid <- fromedge <- toedge <- i.rid <- . <- NULL

    setDT(rel)
    setkey(rel, toedge)

    # seed each node as upstream of itself
    nodes   <- unique(c(rel$fromedge, rel$toedge))
    DT_self <- data.table(rid = nodes, usrid = nodes)

    # direct parent→child
    DT_dir  <- rel[, .(rid = toedge, usrid = fromedge)]

    # initialize
    res      <- unique(rbindlist(list(DT_self, DT_dir)))
    frontier <- DT_dir

    # peel back one generation at a time
    while (nrow(frontier)) {
      # nomatch=0L drops any frontier rows whose usrid isn't a toedge
      nxt <- rel[frontier, on = .(toedge = usrid), nomatch = 0L,
                 .(rid = i.rid, usrid = fromedge)]

      # remove already‐seen pairs
      nxt <- unique(nxt)[!res, on = c("rid","usrid")]
      if (nrow(nxt) == 0L) break

      res      <- rbindlist(list(res, nxt))
      frontier <- nxt
    }

    setkey(res, rid, usrid)
    res[]

}
