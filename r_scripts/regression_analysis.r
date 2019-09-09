####----------------------------------------------------------------------####
#                                 Preamble                                   #
####----------------------------------------------------------------------####

## Load libraries
package_names <- c('purrr', 'dplyr', 'stringr')
for (pkg in package_names) {
    library(pkg, character.only=TRUE, quietly=TRUE, verbose=FALSE)
}

## Set working path for this script by its current location
rel_path <- getSrcDirectory(function(x) {x})
path <- ifelse(
    rel_path != '',
    paste(normalizePath(rel_path), 'r_scripts', sep='/'),
    dirname(rstudioapi::getActiveDocumentContext()$path)
)
setwd(path)

## Reference functions
source('ons_codes.r')
source('elections.r')
source('demographics.r')
source('economics.r')

# Where the data files are located
data_loc <- '/home/mtwest2718/Documents/research/brexit_spatial/data_sets'

####----------------------------------------------------------------------####
#                       Load covariate data from files                       #
####----------------------------------------------------------------------####

## English LAD Codes ------------------------------------------------------ ##
english_lads <- lad_codes(data_loc) %>%
    filter(str_detect(LAD17CD, '^E0')) %>%
    rename(ladcd = LAD17CD, ladnm = LAD17NM)

## Referendum Vote -------------------------------------------------------- ##
votes <- referendum_votes(data_loc)  %>%
    select(-c(lad16nm)) %>%
    rename(ladcd = lad16cd)

## Spatial Neighbors ------------------------------------------------------ ##
lad.counts <- lad_commuters_2011(data_loc)
# compute percentage flow rates and filter list
nb.pcts <- reduce_neighbors(lad.counts)
# percentages for commuters who work in the district they reside
local_worker_pct <- nb.pcts %>%
    filter(cd.work == cd.reside) %>%
    rename(ladcd == cd.work) %>%
    select(ladcd, r_pct, w_pct)

# construct spatial objects with binary weights via spdep
nb.obj <- define_neighbors(nb.pcts, symm=TRUE, w_type='pct', thresh=0.015)
# Get boundary polygons for english districts
english_bnds <- boundary_polygons(data_loc) %>%
    filter(str_detect(lad16cd, '^E0')) %>%
    select(-c(lad16nm)) %>%
    rename(ladcd = lad16cd)

## Austerity Costs -------------------------------------------------------- ##
# Benefit cuts in pounds per year per person
benefit_cuts <- welfare_cuts(data_loc) %>%
    rename(ladcd = lad15cd)
# Cuts in local authority spending per year per person
spending_cuts <- council_cuts(data_loc) %>%
    select(-c(LAD17NM)) %>%
    rename(ladcd = LAD17CD)

## LAD Avg Deprivation Stats ---------------------------------------------- ##
depriv_stats <- deprivation_avgs(data_loc) %>%
    rename(ladcd = lad13cd)

## Price of housing ------------------------------------------------------- ##
housing <- housing_tenure(data_loc) %>%
    rename(ladcd = lad11cd)

## Employment measures ---------------------------------------------------- ##
unemployment <- unemployment_lm(data_loc) %>%
    rename(ladcd = lad15cd)

####----------------------------------------------------------------------####
#                    Combine data into a single structure                    #
####----------------------------------------------------------------------####
data <- list(
    english_lads, votes, nb.pcts, local_worker_pct, english_bnds,
    benefit_cuts, spending_cuts, depriv_stats, housing, unemployment
) %>%
    purrr::reduce(inner_join, by='ladcd')
