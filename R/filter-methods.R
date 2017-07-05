## Methdos for AnnotationFilter classes

setGeneric("filter_subset", function(filter, fields_subset) standardGeneric("AnnotationFilterList"))

#' @description Subset \code{AnnotationFilterList} objects while retaining
#'		\code{conditions} slot and \code{AnnotationFilterList} object.
#'
#' @param filter An \code{AnnotationFilterList} object.
#' @param fields_subset A vector of type numeric, character, or logical
#'		which will subset the \code{AnnotationFilterList} object.
#'
#' @noRd

setMethod("filter_subset", "AnnotationFilterList", 
	    function(filter, fields_subset) {
    if (is.null(filter))
        return(NULL)
	if (!is.character(fields_subset) & !is.numeric(fields_subset) &
			!is.logical(fields_susbet))
		stop("fields_subset must be of type character, numeric, or logical.")
    if (is.character(fields_subset))
        fields_subset <- .fields(filter) %in% fields_subset
    if (is.numeric(fields_subset))
        fields_subset <- seq_len(length(filter)) %in% fields_subset
    res <- value(filter)
    names(res) <- .fields(filter)
    ops <- .logicOp_subset(logicOp(filter), fields_subset)
    do.call(AnnotationFilterList, c(res[fields_subset], list(logicOp=ops)))
})

.logicOp_subset <- function(op, fields_subset) {
    keepOp <- rep(TRUE, length(op))
    for (i in seq_len(length(fields_subset))) {
        if (!fields_subset[i]) {
            first = i-1
            second = i
            if (first == 0 | second == length(fields_subset)) {
                if (first == 0)
                    keepOp[second] <- FALSE
                else
					keepOp[first] <- FALSE
			} else {
                if (((op[first] == '&') & (op[second] == '|')) |
                        ((op[first] == '|') & (op[second] == '&'))) {
                    if (op[first] == '&')
                        keepOp[second] <- FALSE
                    else
                        keepOp[first] <- FALSE
	                } else {
                    keepOp[second] <- FALSE
                }
            }
        }
    }
    if (!any(fields_subset))
        return(character())
    if (table(fields_subset)["TRUE"] <= 1)
        character()
    else
        op[keepOp]
}
