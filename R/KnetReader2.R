
#' Reading K-NET/KiK-net ASCII format files
#'
#' @importFrom geosphere distGeo
#' @importFrom readr read_fwf
#' @param fname filename (or pathname) of K-NET/KiK-net ASCII format file
#' @export

KnetReader2 <- function (fname) {

    # read ASCII data
    prps <- read.fwf(file = fname, n = 16, widths = c(18, 100))
    dats <- read.fwf(file = fname, skip = 17, widths = c(8, 9, 9, 9, 9, 9, 9, 9))

    # get scale factor
    prp14 <- as.character(prps[14, 2])
    ss_prp14 <- as.numeric(strsplit(x = prp14, split = '\\(gal\\)\\/')[[1]])
    SF <- ss_prp14[1]/ss_prp14[2]

    # Sampling Freq. to integer
    SampFreq <- as.integer(strsplit(as.character(prps[11, 2]), 'Hz')[[1]])

    # format data
    vec_dats <- as.vector(t(dats))
    dlen <- sum(1 - is.na(vec_dats))  # get the num. of time steps
    SF_vec_dats <- SF*vec_dats[1:dlen]  # multiply scale factor
    acc <- SF_vec_dats - mean(SF_vec_dats)  # to be zero-mean

    # list of properties
    properties <- list(

        OriginTime      =            as.character(prps[ 1, 2]),
        OriginLat       = as.numeric(as.character(prps[ 2, 2])),
        OriginLong      = as.numeric(as.character(prps[ 3, 2])),
        Depth           = as.numeric(as.character(prps[ 4, 2])),
        Magnitude       = as.numeric(as.character(prps[ 5, 2])),
        StationCode     =            as.character(prps[ 6, 2]),
        StationLat      = as.numeric(as.character(prps[ 7, 2])),
        StationLong     = as.numeric(as.character(prps[ 8, 2])),
        StationHeight   = as.numeric(as.character(prps[ 9, 2])),
        RecordTime      =            as.character(prps[10, 2]),
        SamplingFreq    = SampFreq,
        DurationTime    = as.numeric(as.character(prps[12, 2])),
        Direction       =            as.character(prps[13, 2]),
        ScaleFactor     =            as.character(prps[14, 2]),
        MaxAcc          = as.numeric(as.character(prps[15, 2])),
        LastCorrection  =            as.character(prps[16, 2])
    )

    properties$HypocentralDist <- hyp_dist(properties$Depth,
                                           properties$OriginLat , properties$OriginLong ,
                                           properties$StationLat, properties$StationLong)
    properties$EpicentralDist  <- 10^(-3) * distGeo(c(properties$OriginLong , properties$OriginLat ),
                                                    c(properties$StationLong, properties$StationLat))

    return(list(properties = properties, data = acc))
}

KnetReader22 <- function (fname) {

    fname <- 'C:/data/_asc/20090811050700/AIC0010908110507.EW'

    # read properties
    prps <- fname %>% read_fwf(fwf_widths(c(18, 100)), n_max = 16, cols(X1 = col_character(), X2 = col_character()))

    # read ASCII data
    prps <- as.data.frame(read_fwf(file = fname, n_max = 16, fwf_widths(c(18, 100))))
    dats <- as.data.frame(read_fwf(file = fname, skip = 17, fwf_widths(c(8, 9, 9, 9, 9, 9, 9, 9))))

    # get scale factor
    prp14 <- as.character(prps[14, 2])
    ss_prp14 <- as.numeric(strsplit(x = prp14, split = '\\(gal\\)\\/')[[1]])
    SF <- ss_prp14[1]/ss_prp14[2]

    # Sampling Freq. to integer
    SampFreq <- as.integer(strsplit(as.character(prps[11, 2]), 'Hz')[[1]])

    # format data
    vec_dats <- as.vector(t(dats))
    dlen <- sum(1 - is.na(vec_dats))  # get the num. of time steps
    SF_vec_dats <- SF*vec_dats[1:dlen]  # multiply scale factor
    acc <- SF_vec_dats - mean(SF_vec_dats)  # to be zero-mean

    # list of properties
    properties <- list(

        OriginTime      =            as.character(prps[ 1, 2]),
        OriginLat       = as.numeric(as.character(prps[ 2, 2])),
        OriginLong      = as.numeric(as.character(prps[ 3, 2])),
        Depth           = as.numeric(as.character(prps[ 4, 2])),
        Magnitude       = as.numeric(as.character(prps[ 5, 2])),
        StationCode     =            as.character(prps[ 6, 2]),
        StationLat      = as.numeric(as.character(prps[ 7, 2])),
        StationLong     = as.numeric(as.character(prps[ 8, 2])),
        StationHeight   = as.numeric(as.character(prps[ 9, 2])),
        RecordTime      =            as.character(prps[10, 2]),
        SamplingFreq    = SampFreq,
        DurationTime    = as.numeric(as.character(prps[12, 2])),
        Direction       =            as.character(prps[13, 2]),
        ScaleFactor     =            as.character(prps[14, 2]),
        MaxAcc          = as.numeric(as.character(prps[15, 2])),
        LastCorrection  =            as.character(prps[16, 2])
    )

    properties$HypocentralDist <- hyp_dist(properties$Depth,
                                           properties$OriginLat , properties$OriginLong ,
                                           properties$StationLat, properties$StationLong)
    properties$EpicentralDist  <- 10^(-3) * distGeo(c(properties$OriginLong , properties$OriginLat ),
                                                    c(properties$StationLong, properties$StationLat))

    return(list(properties = properties, data = acc))
}
