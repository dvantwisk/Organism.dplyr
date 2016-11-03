.tbl_filter <- function(keep1, table, filter) {
    values <- paste0("'", filter[[keep1]], "'", collapse=", ")
    op <- if (length(filter[[keep1]]) == 1) "==" else "%in%"
    sprintf("%s %s c(%s)", keep1, op, values)
}

.tbl_join <- function(x, table, tbls, filter) {
    if (is.null(filter))
        return(table)
    
    fields <- names(filter)
    
    fields1 <- fields[fields %in% colnames(table)]
    if (length(fields1) != 0) {
        filters <- sapply(fields1, .tbl_filter, table, filter)
        filters <- paste0(filters, collapse=" & ")
        table <- table %>% filter_(filters)
        filter <- filter[setdiff(fields, fields1)]
    }
    
    fields <- names(filter)
    for (i in tbls) {
        keep <- fields[fields %in% colnames(tbl(x, i))]
        if (is.null(keep) || length(keep) == 0)
            next
        filters <- sapply(keep, .tbl_filter, table, filter)
        filters <- paste0(filters, collapse=" & ")
        table <- inner_join(table, tbl(x, i)) %>% filter_(filters)
        fields <- setdiff(fields, keep)
    }
    table
}

#' @rdname src_organism
#' @export
transcripts_tbl <- function(x, filter = NULL) {
    table <- tbl(x, "ranges_tx")
    tbls <- setdiff(src_tbls(x), "ranges_tx")
    schema <- x$schema
    table <- .tbl_join(x, table, tbls, filter)
    fields <- unique(
        c("tx_chrom", "tx_start", "tx_end", "tx_strand", 
          schema, "tx_id", "tx_name", names(filter)))
    do.call(select_, c(list(table), as.list(fields))) %>% arrange(tx_id)
}

#' Generic functions to extract genomic features from an object.
#'
#' @param x A src_organism object
#'
#' @param filter Either NULL or a named list of vectors to be used to
#'     restrict the output.
#'
#' @examples
#' organism <- src_ucsc("human")
#' filters <- list(symbol=c("PTEN", "BRCA1"),
#'                entrez="5728",
#'                go=c("GO:0000079", "GO:0001933"))
#' transcripts(organism, filters)
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures transcripts
#' @export

setMethod("transcripts", "src_organism", function(x, filter = NULL) {
    transcripts_tbl(x, filter) %>% collect(n=Inf) %>% as("GRanges")
})

.exons <- function(x, filter = NULL) {
    table <- tbl(x, "ranges_exon")
    tbls <- setdiff(src_tbls(x), "ranges_exon")
    schema <- x$schema
    table <- .tbl_join(x, table, tbls, filter)
    fields <- unique(
        c("exon_chrom", "exon_start", "exon_end", "exon_strand",
          schema, "tx_id", "exon_id", "exon_name", "exon_rank",
          names(filter)))
    do.call(select_, c(list(table), as.list(fields)))
}

#' @rdname src_organism
#' @export
exons_tbl <- function(x, filter = NULL) {
    .exons(x, filter) %>% collect(n = Inf) %>% 
        dplyr::select(-c(entrez, tx_id, exon_rank)) %>% distinct() %>% 
        arrange(exon_id)
}

#' @examples 
#' exons(organism, filter=list(symbol="PTEN"))
#' 
#' @rdname src_organism
#' @importFrom GenomicFeatures exons
#' @export

setMethod("exons", "src_organism", function(x, filter = NULL) {
    exons_tbl(x, filter) %>% collect(n=Inf) %>% as("GRanges")
})


.cds <- function(x, filter = NULL) {
    table <- tbl(x, "ranges_cds")
    tbls <- setdiff(src_tbls(x), "ranges_cds")
    schema <- x$schema
    table <- .tbl_join(x, table, tbls, filter)
    fields <- unique(
        c("cds_chrom", "cds_start", "cds_end", "cds_strand", 
          schema, "tx_id", "cds_id", "cds_name", "exon_rank", names(filter)))
    do.call(select_, c(list(table), as.list(fields))) %>% arrange(cds_id)
}

#' @rdname src_organism
#' @export
cds_tbl <- function(x, filter = NULL) {
    .cds(x, filter) %>% collect(n = Inf) %>% 
        dplyr::select(-c(entrez, tx_id, exon_rank)) %>% distinct() %>% 
        arrange(cds_id)
}

#' @rdname src_organism
#' @importFrom GenomicFeatures cds
#' @export

setMethod("cds", "src_organism", function(x, filter = NULL) {
    cds_tbl(x, filter) %>% collect(n=Inf) %>% as("GRanges")
})


#' @rdname src_organism
#' @importFrom RSQLite dbGetQuery
#' @export

genes_tbl <- function(x, filter = NULL) {
    table <- dbGetQuery(x$con, 
            "SELECT * FROM ranges_gene 
            WHERE entrez NOT IN 
                (SELECT entrez FROM ranges_gene 
                GROUP BY entrez 
                having count(*) > 1)")
    tbls <- setdiff(src_tbls(x), "ranges_gene")
    schema <- x$schema
    table <- .tbl_join(x, table, tbls, filter)
    fields <- unique(
        c("gene_chrom", "gene_start", "gene_end", "gene_strand", 
          schema, names(filter)))
    do.call(select_, c(list(table), as.list(fields))) %>% arrange_(schema)
}

#' @rdname src_organism
#' @importFrom GenomicFeatures genes
#' @export

setMethod("genes", "src_organism", function(x, filter = NULL) {
    genes_tbl(x, filter) %>% collect(n=Inf) %>% as("GRanges")
})


#' @param upstream For \code{promoters()}: An integer(1) value indicating 
#' the number of bases upstream from the transcription start site.
#'
#' @param downstream For \code{promoters()}: An integer(1) value indicating
#' the number of bases downstream from the transcription start site.
#' 
#' @rdname src_organism
#' @export

promoters_tbl <- function(x, upstream, downstream, filter = NULL) {
    if (!isSingleNumber(upstream)) 
        stop("'upstream' must be a single integer")
    if (!is.integer(upstream)) 
        upstream <- as.numeric(upstream)
    if (!isSingleNumber(downstream)) 
        stop("'downstream' must be a single integer")
    if (!is.integer(downstream)) 
        downstream <- as.numeric(downstream)
    if (upstream < 0 | downstream < 0) 
        stop("'upstream' and 'downstream' must be integers >= 0")
    if(missing(upstream))
        upstream = 0
    if(missing(downstream))
        downstream = 2200
    
    table <- transcripts_tbl(organism, filter = filter) %>%
        mutate(start = ifelse(tx_strand == "-", 
                              tx_end - downstream + 1, 
                              tx_start - upstream),
               end = ifelse(tx_strand == "-", 
                            tx_end + upstream, 
                            tx_start + downstream - 1)) %>% collect(n=Inf)
    
    table <- dplyr::rename(table, chrom = tx_chrom)
    table <- dplyr::rename(table, strand = tx_strand)
    
    fields <- unique(
        c("chrom", "start", "end", "strand", "tx_id", "tx_name", 
          names(filter)))
    do.call(select_, c(list(table), as.list(fields))) %>% arrange(tx_id)
}

#' @examples
#' promoters(organism, upstream=100, downstream=50,
#'           filter = list(symbol="BRCA1"))
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures promoters
#' @importFrom dplyr mutate
#' @importFrom S4Vectors isSingleNumber
#' @export

setMethod("promoters", "src_organism",
function(x, upstream, downstream, filter = NULL) {
    promoters_tbl(x, upstream, downstream, filter) %>% collect(n=Inf) %>% 
        as("GRanges")
})

# .splitby <- function(by, schema) {
#     # if (by == "gene") {
#     #     ifelse(schema == "entrez", split(.$entrez), split(.$ensembl))
#     # }
#     switch (by,
#             "gene" = ifelse(schema == "entrez", 
#                             split(.$entrez), split(.$ensembl)),
#             "tx" = split(.$tx_id),
#             "exon" = split(.$exon_id),
#             "cds" = split(.$cds_id)
#     )
# }


#' @rdname src_organism
#' @export

transcriptsBy_tbl <- 
function(x, by = c("gene", "exon", "cds"), filter = NULL, schema) {
    by <- match.arg(by)
    tx <- transcripts_tbl(x, filter = filter)
    
    if (by == "gene") {
        table <- inner_join(tx, tbl(x, "ranges_gene")) 
        fields <- unique(
            c("tx_chrom", "tx_start", "tx_end", "tx_strand",  
              schema, "tx_id", "tx_name", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% 
            arrange_(schema)
    }
    else if (by == "exon") {
        table <- inner_join(tx, tbl(x, "ranges_exon"), by = "tx_id") 
        fields <- unique(
            c("tx_chrom", "tx_start", "tx_end", "tx_strand",  
              "exon_id", "tx_id", "tx_name", "exon_rank", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% 
            arrange(exon_id)
    }
    else if (by == "cds") {
        table <- inner_join(tx, tbl(x, "ranges_cds"), by = "tx_id") 
        fields <- unique(
            c("tx_chrom", "tx_start", "tx_end", "tx_strand",  
              "cds_id", "tx_id", "tx_name", "exon_rank", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% 
            arrange(cds_id)
    }
}

#' @param by One of "gene", "exon", "cds" or "tx". Determines the grouping.
#'
#' @examples
#' transcriptsBy(organism, by = "gene", filter = list(symbol="PTEN"))
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures transcriptsBy
#' @importFrom dplyr inner_join
#' @export

setMethod("transcriptsBy", "src_organism",
function(x, by = c("gene", "exon", "cds"), filter = NULL) {
    schema <- x$schema
    transcriptsBy_tbl(x, by, filter, schema) %>% collect(n=Inf) %>% 
        as("GRanges")
})


#' @examples
#' exonsBy(organism, by = "gene", filter = list(symbol="PTEN"))
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures exonsBy
#' @export

setMethod("exonsBy", "src_organism",
function(x, by = c("tx", "gene"), filter = NULL) {
    by <- match.arg(by)
    exons <- .exons(x, filter = filter)
    schema <- x$schema

    if (by == "tx") {
        table <- inner_join(exons, tbl(x, "ranges_tx"), by = "tx_id") 
        fields <- unique(
            c("exon_chrom", "exon_start", "exon_end", "exon_strand",  
              "tx_id", "exon_id", "exon_name", "exon_rank", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% arrange(tx_id)
    }
    else if (by == "gene") {
        table <- inner_join(exons, tbl(x, "ranges_gene")) 
        fields <- unique(
            c("exon_chrom", "exon_start", "exon_end", "exon_strand",  
              schema, "exon_id", "exon_name", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% 
            arrange_(schema)
    }
})


#' @examples
#' cdsBy(organism, by = "gene", filter = list(symbol="PTEN"))
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures cdsBy
#' @export

setMethod("cdsBy", "src_organism",
function(x, by = c("tx", "gene"), filter = NULL) {
    by <- match.arg(by)
    cds <- .cds(x, filter = filter)
    schema <- x$schema

    if (by == "tx") {
        table <- inner_join(cds, tbl(x, "ranges_tx"), by = "tx_id") 
        fields <- unique(
            c("cds_chrom", "cds_start", "cds_end", "cds_strand",  
              "tx_id", "cds_id", "cds_name", "exon_rank", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% arrange(tx_id) 
    }
    else if (by == "gene") {
        table <- inner_join(cds, tbl(x, "ranges_gene")) 
        fields <- unique(
            c("cds_chrom", "cds_start", "cds_end", "cds_strand", 
              schema, "cds_id", "cds_name", names(filter)))
        do.call(select_, c(list(table), as.list(fields))) %>% 
            arrange_(schema)
    }
})


#' @examples
#' intronsByTranscript(organism, filter = list(symbol="PTEN"))
#'
#' @rdname src_organism
#' @importFrom GenomicFeatures intronsByTranscript
#' @importFrom GenomicRanges split mcols mcols<-
#' @importFrom IRanges psetdiff
#' @export
setMethod("intronsByTranscript", "src_organism",
function(x, filter=NULL) {
    tx <- transcripts(x, filter=filter)
    exn <- exonsBy(x, filter=filter)

    tx_gr <- tx %>% 
        dplyr::select(tx_id, tx_chrom, tx_start, tx_end, tx_strand) %>%
        collect(n=Inf) %>% as("GRanges")
    exn_grl <- exn %>% collect(n=Inf) %>% 
        dplyr::select(tx_id, exon_id, exon_chrom, exon_start, exon_end,
               exon_strand) %>%
        as("GRanges") %>% split(.$tx_id)
    
    tx_gr<- tx_gr[match(names(exn_grl), mcols(tx_gr)[, "tx_id"])]
    ans <- unlist(psetdiff(tx_gr, exn_grl))

    mcols(ans)[, "tx_id"] <- names(ans)
    unname(ans) %>% as.data.frame %>% tbl_df %>%
        dplyr::select(tx_id, intron_chrom=seqnames, intron_start=start,
               intron_end=end, intron_strand=strand)
})


.getSplicings <- function(x, filter=NULL) {
    exon <- .exons(x, filter=filter)
    cds <- .cds(x, filter=filter)
    
    exon_txid <- exon %>% dplyr::select(tx_id) %>% 
        collect(n = Inf) %>% .[["tx_id"]]
    cds_txid <- cds %>% dplyr::select(tx_id) %>% collect(n = Inf) %>% 
        .[["tx_id"]]
    exclude <- setdiff(exon_txid, cds_txid)
    
    splicings <- 
    left_join(exon, cds, by = c("tx_id", "exon_rank")) %>% 
        dplyr::select(tx_id, exon_rank, exon_id, exon_name, exon_chrom, 
                      exon_strand, exon_start, exon_end, cds_id, cds_start,  
                      cds_end) %>% 
        collect(n=Inf) %>%
        arrange(tx_id, exon_rank)
    if(length(exclude) != 0) 
        splicings <- splicings %>% filter(!tx_id %in% exclude)
    splicings
}

.UTRsByTranscript <- function(x, filter, strand1, strand2) {
    splicings <- .getSplicings(x, filter)
    
    exons_with_cds <- which(!is.na(splicings$cds_id))
    ifelse (strand1 == "-", 
            idx <- GenomicFeatures:::.exons_with_5utr
                    (splicings$tx_id, exons_with_cds),
            idx <- GenomicFeatures:::.exons_with_3utr
                    (splicings$tx_id, exons_with_cds))
    
    splicings <- S4Vectors:::extract_data_frame_rows(splicings, idx)
    
    splicings %>% 
        mutate(start = 
                   ifelse(!is.na(cds_id) & exon_strand == strand1,
                          cds_end + 1L, 
                          exon_start), 
               end = 
                   ifelse(!is.na(cds_id) & exon_strand == strand2, 
                          cds_start - 1L, 
                          exon_end)) %>% 
        filter(start <= end) %>% collect(n=Inf) %>% tbl_df %>% 
        dplyr::select(chrom=exon_chrom, start, end, strand=exon_strand, 
                      tx_id, exon_id, exon_name, exon_rank)
}

#' @examples
#' fiveUTRsByTranscript(organism, filter = list(symbol="PTEN"))
#' 
#' @rdname src_organism
#' @importFrom GenomicFeatures fiveUTRsByTranscript
#' @export
setMethod("fiveUTRsByTranscript", "src_organism",function(x, filter=NULL) {
    .UTRsByTranscript(x, filter, "-", "+")
})


#' @examples
#' threeUTRsByTranscript(organism, filter = list(symbol="PTEN"))
#' 
#' @rdname src_organism
#' @importFrom GenomicFeatures threeUTRsByTranscript
#' @export
setMethod("threeUTRsByTranscript", "src_organism",function(x, filter=NULL) {
    .UTRsByTranscript(x, filter, "+", "-")
})