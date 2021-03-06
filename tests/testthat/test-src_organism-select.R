context("src_organism-select")

suppressPackageStartupMessages({
    library(TxDb.Hsapiens.UCSC.hg38.knownGene)
})
txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene

hg38light <- system.file(
    package="Organism.dplyr", "extdata", "light.hg38.knownGene.sqlite"
)
src <- src_organism(dbpath=hg38light)

test_that("keytypes", {
    expect_equal(class(keytypes(src)), "character")
    expect_true(length(keytypes(src)) > 0)
})

test_that("columns", {
    expect_equal(class(columns(src)), "character")
    expect_true(length(columns(src)) > 0)
})

test_that("keys", {
    expect_error(keys(src, "foo"))
    expect_equal(class(keys(src)), "character")
    expect_true(length(keys(src)) > 0)
    expect_true(all(keys(src, "tx_id") %in% keys(txdb, "TXID")))
})

test_that("select", {
    keys <- c(
        "uc001hzz.2", "uc001iab.3", "uc001pde.4", "uc001pdf.5", "uc001xmf.5", 
        "uc001yxg.4"
    )
    columns_src <- c("entrez", "tx_id", "tx_name","exon_id")
    keytype_src <- "tx_name"
    columns_txdb <- c("GENEID", "TXID", "TXNAME","EXONID")
    keytype_txdb <- "TXNAME"
    
    rs_src <- select(src, keys, columns_src, keytype_src) %>% collect()
    rs_txdb <- select(txdb, keys, columns_txdb, keytype_txdb)
    
    expect_equal(dim(rs_src), dim(rs_txdb))
    expect_equal(rs_src$tx_id, rs_txdb$TXID)
})

test_that("mapIds", {
    keys <- c(
        "uc001hzz.2", "uc001iab.3", "uc001pde.4", "uc001pdf.5", "uc001xmf.5", 
        "uc001yxg.4"
    )
    
    rs_src <- mapIds(src, keys, "exon_id", "tx_name")
    rs_txdb <- mapIds(txdb, keys, "EXONID", "TXNAME")
    
    expect_equal(rs_src, rs_txdb)
})
