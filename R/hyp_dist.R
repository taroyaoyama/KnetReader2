
rot_mat <- function(N, E) {

    phi <- N * pi / 180
    lambda <- E * pi / 180

    M <- matrix(c( cos(phi), sin(phi)*sin(lambda), sin(phi)*cos(lambda),
                   0,          cos(lambda),         -sin(lambda),
                   -sin(phi), cos(phi)*sin(lambda), cos(phi)*cos(lambda)), 3, 3)

    return(M)
}

#' Calculating hypocentral distance between source and station.
#'
#' @param depth the depth of hypocenter.
#' @param epi_N latitude of epicenter.
#' @param epi_E longitude of epicenter.
#' @param sta_N latitude of station.
#' @param sta_E longitude of station.
#' @param r radius of the earth (default = 6371).
#' @export

hyp_dist <- function(depth, epi_N, epi_E, sta_N, sta_E, r = 6371) {

    MF <- rot_mat(epi_N, epi_E) # rotation mat for source
    MP <- rot_mat(sta_N, sta_E) # rotation mat for site

    F0 <- c(0, 0, r - depth)    # location of source

    ## transformation to xyz-coordinate and calculation of distance
    P  <- solve(MF) %*% MP %*% c(0, 0, r)
    VP <- P - F0             # vector; source to site
    D  <- sqrt(t(VP) %*% VP) # calculation of norm

    return(D[1])
}
