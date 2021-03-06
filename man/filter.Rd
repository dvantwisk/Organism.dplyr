% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\docType{class}
\name{BasicFilter-class}
\alias{BasicFilter-class}
\alias{AccnumFilter}
\alias{AliasFilter}
\alias{CdsChromFilter}
\alias{CdsIdFilter}
\alias{CdsNameFilter}
\alias{CdsStrandFilter}
\alias{EnsemblFilter}
\alias{EnsemblprotFilter}
\alias{EnsembltransFilter}
\alias{EntrezFilter}
\alias{EnzymeFilter}
\alias{EvidenceFilter}
\alias{EvidenceallFilter}
\alias{ExonChromFilter}
\alias{ExonIdFilter}
\alias{ExonNameFilter}
\alias{ExonRankFilter}
\alias{ExonStrandFilter}
\alias{FlybaseFilter}
\alias{FlybaseCgFilter}
\alias{FlybaseProtFilter}
\alias{GeneChromFilter}
\alias{GeneStrandFilter}
\alias{GenenameFilter}
\alias{GoFilter}
\alias{GoallFilter}
\alias{IpiFilter}
\alias{MapFilter}
\alias{MgiFilter}
\alias{OmimFilter}
\alias{OntologyFilter}
\alias{OntologyallFilter}
\alias{PfamFilter}
\alias{PmidFilter}
\alias{PrositeFilter}
\alias{RefseqFilter}
\alias{SymbolFilter}
\alias{TxChromFilter}
\alias{TxIdFilter}
\alias{TxNameFilter}
\alias{TxStrandFilter}
\alias{TxTypeFilter}
\alias{UnigeneFilter}
\alias{UniprotFilter}
\alias{WormbaseFilter}
\alias{ZfinFilter}
\alias{CdsStartFilter}
\alias{CdsEndFilter}
\alias{ExonStartFilter}
\alias{ExonEndFilter}
\alias{GeneStartFilter}
\alias{GeneEndFilter}
\alias{TxStartFilter}
\alias{TxEndFilter}
\alias{GRangesFilter-class}
\alias{GRangesFilter}
\alias{show,GRangesFilter-method}
\alias{show,BasicFilter-method}
\alias{supportedFilters}
\title{Filtering src_organism objects}
\usage{
AccnumFilter(value, condition = "==")
AliasFilter(value, condition = "==")
CdsChromFilter(value, condition = "==")
CdsIdFilter(value, condition = "==")
CdsNameFilter(value, condition = "==")
CdsStrandFilter(value, condition = "==")
EnsemblFilter(value, condition = "==")
EnsemblprotFilter(value, condition = "==")
EnsembltransFilter(value, condition = "==")
EntrezFilter(value, condition = "==")
EnzymeFilter(value, condition = "==")
EvidenceFilter(value, condition = "==")
EvidenceallFilter(value, condition = "==")
ExonChromFilter(value, condition = "==")
ExonIdFilter(value, condition = "==")
ExonNameFilter(value, condition = "==")
ExonRankFilter(value, condition = "==")
ExonStrandFilter(value, condition = "==")
FlybaseFilter(value, condition = "==")
FlybaseCgFilter(value, condition = "==")
FlybaseProtFilter(value, condition = "==")
GeneChromFilter(value, condition = "==")
GeneStrandFilter(value, condition = "==")
GenenameFilter(value, condition = "==")
GoFilter(value, condition = "==")
GoallFilter(value, condition = "==")
IpiFilter(value, condition = "==")
MapFilter(value, condition = "==")
MgiFilter(value, condition = "==")
OmimFilter(value, condition = "==")
OntologyFilter(value, condition = "==")
OntologyallFilter(value, condition = "==")
PfamFilter(value, condition = "==")
PmidFilter(value, condition = "==")
PrositeFilter(value, condition = "==")
RefseqFilter(value, condition = "==")
SymbolFilter(value, condition = "==")
TxChromFilter(value, condition = "==")
TxIdFilter(value, condition = "==")
TxNameFilter(value, condition = "==")
TxStrandFilter(value, condition = "==")
TxTypeFilter(value, condition = "==")
UnigeneFilter(value, condition = "==")
UniprotFilter(value, condition = "==")
WormbaseFilter(value, condition = "==")
ZfinFilter(value, condition = "==")
CdsStartFilter(value, condition = "==")
CdsEndFilter(value, condition = "==")
ExonStartFilter(value, condition = "==")
ExonEndFilter(value, condition = "==")
GeneStartFilter(value, condition = "==")
GeneEndFilter(value, condition = "==")
TxStartFilter(value, condition = "==")
TxEndFilter(value, condition = "==")

GRangesFilter(value)

\S4method{show}{GRangesFilter}(object)

\S4method{show}{BasicFilter}(object)

supportedFilters()
}
\arguments{
\item{value}{Value of the filter. For \code{GRangesFilter} value should be a
\code{GRanges} object.}

\item{object}{A \code{BasicFilter} or \code{GRangesFilter} object}

\item{condition}{The condition to be used in filter for genomic
extractors, one of "==", "!=", "startsWith", "endsWith", ">",
"<", ">=", "<=".  For character values "==", "!=", "startsWith"
and "endsWith" are allowed, for numeric values
(\code{CdsStartFilter}, \code{CdsEndFilter},
\code{ExonStartFilter}, \code{ExonEndFilter},
\code{GeneStartFilter}, \code{GeneEndFilter},
\code{TxStartFilter} and \code{TxEndFilter}), "==", "!=", ">",
">=", "<" and "<=". Default condition is "==".}
}
\value{
A Filter object showing class, value and condition of the filter
}
\description{
These functions create filters to be used by the "select"
interface to \code{src_organism} objects.
}
\details{
All filters except \code{GRangesFilter()} takes value(s) from
corresponding fields in the data base. For example,
\code{AccnumFilter()} takes values of accession number(s), which
come from field \code{accnum}. See \code{keytypes()} and
\code{keys()} for possible values.

\code{GRangesFilter()} takes a \code{GRanges} object as filter, and returns
genomic extractors (\code{genes}, \code{transcripts}, etc.) that are
partially overlapping with the region.

\code{supportedFilters()} lists all available filters for
\code{src_organism} object.
}
\examples{
src <- src_organism(dbpath=hg38light())
keytypes(src)
head(keys(src, "ensembl"))

## filter by ensembl
EnsemblFilter("ENSG00000171862")

## filter by gene symbol start with "BRAC"
SymbolFilter("BRCA", "startsWith")

## filter by GRanges
GRangesFilter(GenomicRanges::GRanges("chr10:87869000-87876000"))

## filter by transcript start position
TxStartFilter(87863438, ">")

}
\seealso{
\code{\link{src_organism}} for creating a \code{src_organism}
    object.

    \code{\link[Organism.dplyr]{transcripts_tbl}} for generic functions
     to extract genomic features from a \code{src_organism} object.

     \code{\link[Organism.dplyr]{select,src_organism-method}} for "select"
    interface on \code{src_organism} objects.
}
\author{
Yubo Cheng.
}
