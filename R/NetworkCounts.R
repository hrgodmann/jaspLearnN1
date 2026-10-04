# Counts describe the breadth of the entered links, independently of their size.

.ln1NetConnectionCounts <- function(edgelist, nodeAttributes) {
  nodeNames <- as.character(nodeAttributes[["name"]])
  result <- data.frame(name = nodeNames, countIn = integer(length(nodeNames)),
                       countOut = integer(length(nodeNames)), stringsAsFactors = FALSE)
  if (is.null(edgelist) || nrow(edgelist) == 0L || length(nodeNames) == 0L)
    return(result)

  weights <- edgelist[["weight"]]
  if (!is.numeric(weights) || is.complex(weights))
    return(result)
  from <- as.character(edgelist[["from"]])
  to <- as.character(edgelist[["to"]])
  valid <- is.finite(weights) & weights >= -1 & weights <= 1 & weights != 0 &
    !is.na(from) & !is.na(to) & from != to & from %in% nodeNames & to %in% nodeNames
  pairs <- unique(data.frame(from = from[valid], to = to[valid], stringsAsFactors = FALSE))
  result[["countIn"]] <- tabulate(match(pairs[["to"]], nodeNames), nbins = length(nodeNames))
  result[["countOut"]] <- tabulate(match(pairs[["from"]], nodeNames), nbins = length(nodeNames))
  return(result)
}
