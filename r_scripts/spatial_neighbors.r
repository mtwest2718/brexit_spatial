## Defining neighbors based on commuter flow
library(readr)
library(stringr)
library(dplyr)
library(spdep)

# set working path for this script by its current location
rel_path <- getSrcDirectory(function(x) {x})
path <- ifelse(
    rel_path != '',
    paste(normalizePath(rel_path), 'r_scripts', sep='/'),
    dirname(rstudioapi::getActiveDocumentContext()$path)
)
setwd(path)

#----------------------------------------------------------------------------#
## Get best fit matching ONS codes linking MSOA11CD to LAD17CD
msoa_to_lad <- function(data_loc) {
    fn.ons <- paste(
        data_loc, 'ons_codes',
        'Middle_Layer_Super_Output_Area_2011_to_Ward_2017_Lookup_in_England_and_Wales.csv',
        sep='/'
    )
    # Filter out unwanted Ward information and keep only codes
    msoa_codes <- read_csv(fn.ons) %>%
        dplyr::select(matches('^[^WF]+CD$'))

    return(msoa_codes)
}

#----------------------------------------------------------------------------#
## Get counts for commuters from 2011 census data at LAD level
lad_commuters_2011 <- function(data_loc, msoa_codes) {
    fn.commute <- paste(
        data_loc, 'boundaries', 'wf01ew_msoa_v1.csv.gz', sep='/'
    )

    dir.temp <- paste(data_loc, 'temp', sep='/')
    unzip(fn.commute, exdir=dir.temp, overwrite=TRUE)
    ## Load 51Mb MSOA-pair commuting counts file and keep only those where
    #   both residence and workplace are in England
    fn.csv <- normalizePath(paste(dir.temp, 'wf01ew_msoa_v1.csv', sep='/'))
    commute <- read_csv(
        fn.csv,
        col_names=FALSE, col_types='cci', n_max=2.5e6
    ) %>%
        filter_at(vars(c(X1, X2)), all_vars(str_detect(., '^E02'))) %>%
        rename(msoa_count = X3)

    ## Aggregate counts to local authority level (LAD)
    lad.agg <- commute %>%
        inner_join(msoa_codes, by=c('X1'='MSOA11CD')) %>%
        inner_join(msoa_codes, by=c('X2'='MSOA11CD')) %>%
        group_by(LAD17CD.x, LAD17CD.y) %>%
        rename(cd.reside = LAD17CD.x, cd.work = LAD17CD.y) %>%
        summarize(lad_count = sum(msoa_count))

    # remove commute data.frame to free up memory
    rm(commute)
    # remove unpacked files
    file.remove(
        list.files(dir.temp, full.names=TRUE, pattern='^wf01ew_msoa_v1')
    )

    return(lad.agg)
}

#----------------------------------------------------------------------------#
reduce_neighbors <- function(lad.counts) {
    # the  number of people who work in the same district they live
    home <- lad.counts %>%
        ungroup() %>%
        filter(cd.reside == cd.work) %>%
        transmute(ons_code = cd.reside, h_count = lad_count)

    # The number of people who reside & work in England
    residents <- lad.counts %>%
        group_by(cd.reside) %>%
        summarize(r_count = sum(lad_count)) %>%
        inner_join(home, by=c('cd.reside'='ons_code')) %>%
        rename(hr_count = h_count)
    workers <- lad.counts %>%
        group_by(cd.work) %>%
        summarize(w_count = sum(lad_count))  %>%
        inner_join(home, by=c('cd.work'='ons_code')) %>%
        rename(hw_count = h_count)

    thresh <- 0.001
    # NOTE: Dropping 'Isle of Scilly' and 'City of London' at the end due to
    #   lack of demographic data in other categories
    lad.pct <- lad.counts %>%
        inner_join(residents, by='cd.reside') %>%
        inner_join(workers, by='cd.work') %>%
        mutate(
            r_pct = lad_count / r_count,
            w_pct = lad_count / w_count,
            r_ratio = lad_count / hr_count,
            w_ratio = lad_count / hw_count
        ) %>%
        mutate_at(vars(c('r_ratio', 'w_ratio')), ~ifelse(. > 1,1, .)) %>%
        ungroup() %>%
        filter(r_ratio > thresh | w_ratio > thresh) %>%
        filter_at(
            vars(c('cd.reside', 'cd.work')),
            all_vars( !str_detect(., 'E06000053|E09000001') )
        ) %>%
        mutate_if(is.numeric, round, digits=5)
    # NOTE: r_pct is the percentage of residents of 1 that work in 2
    # w_pct is the percentage of workers in 2 that live in 1
    # ratios are the number of people who work in 1 relative to the number who
    #   work in the same district they live i.e. 1(r) or 2(w)

    return(lad.pct)
}

#----------------------------------------------------------------------------#
define_neighbors <- function(sn.ratios, symm=TRUE, w_type='pct', thresh=0.015) {
    # file needs to be read in as a data.frame
    commute.sn <- sn.ratios %>%
        group_by(cd.reside, cd.work) %>%
        summarize(
            pct = max(r_pct, w_pct),
            ratio = max(r_ratio, w_ratio)
        ) %>%
        filter(cd.reside != cd.work) %>%
        filter(pct >= thresh) %>%
        as.data.frame()

    # which weight is desired
    if (w_type=='pct') {
        commute.sn <- commute.sn %>% mutate(weights = pct)
    } else if (w_type=='ratio') {
        commute.sn <- commute.sn %>% mutate(weights = ratio)
    } else if (w_type=='binary') {
        commute.sn <- commute.sn %>% mutate(weights = 1)
    }
    commute.sn <- commute.sn %>%
        dplyr::select(cd.reside, cd.work, weights)

    # Restructure table so it works with spdep methods
    class(commute.sn) <- c("spatial.neighbour", class(commute.sn))
    reside <- ordered(commute.sn$cd.reside)
    attr(commute.sn, "region.id") <- levels(reside)
    # replace ONS-code characters with integers
    commute.sn$cd.reside <- as.integer(reside)
    attr(commute.sn, "n") <- length(unique(commute.sn$cd.reside))

    # identify work locations with residential id #s
    for (j in 1:attr(commute.sn, 'n')) {
        loc <- which(commute.sn$cd.work == as.vector(unique(reside))[j])
        if (any(loc)) {
            commute.sn$cd.work[loc] <- rep(j, length(loc))
        }
    }

    # Make neighbors list
    commute.listw <- sn2listw(commute.sn)
    # Make neighbors weights matrix
    commute.mat <- listw2mat(commute.listw)

    # construct symmetric weights matrix
    if (symm) {
        commute.mat <- pmax(t(commute.mat), commute.mat)
        # make all the spatial neighbor objects based on symmetrized matrix
        listw <- mat2listw(commute.mat, attr(commute.sn, 'region.id'))
        mat <- listw2mat(listw)
        sn <- listw2sn(listw)
    } else {
        listw <- commute.listw
        mat <- commute.mat
        sn <- commute.sn
    }

    output <- list(listw=listw, mat=mat, sn=sn)
    return(output)
}

#----------------------------------------------------------------------------#
## Constructing neighbors based on commuter flow rate
data_loc <- '/home/mtwest2718/Documents/research/brexit_spatial/data_sets'
# ONS code mapping from file
msoa_codes <- msoa_to_lad(data_loc)
# Get commuting numbers on a LAD->LAD basis
lad.counts <- lad_commuters_2011(data_loc, msoa_codes)
# compute percentage flow rates and filter list
nb.pcts <- reduce_neighbors(lad.counts)
# construct spatial objects with binary weights via spdep
nb.bin <- define_neighbors(nb.pcts, symm=TRUE, w_type='binary', thresh=0.015)
