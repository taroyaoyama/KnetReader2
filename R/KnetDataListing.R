
#' Listing the properties of K-NET/KiK-net ASCII format files
#'
#' @importFrom geosphere distGeo
#' @param flist list of pathnames of K-NET/KiK-net ASCII format file
#' @export

KnetDataListing <- function (flist) {

    # extract ascii format files
    flist <- flist[c(grep('\\.EW$' , flist), grep('\\.NS$' , flist), grep('\\.UD$' , flist),
                     grep('\\.EW1$', flist), grep('\\.NS1$', flist), grep('\\.UD1$', flist),
                     grep('\\.EW2$', flist), grep('\\.NS2$', flist), grep('\\.UD2$', flist))]

    # num. of ASCII
    N_files <- length(flist)

    # box for properties
    propbox <- array(NA, dim = c(N_files, 19))

    colnames(propbox) <- c(
        'FileName',
        'OriginTime', 'OriginLat', 'OriginLong', 'Depth', 'Magnitude',
        'StationCode', 'StationLat', 'StationLong', 'StationHeight',
        'RecordTime', 'SamplingFreq', 'DurationTime', 'Direction',
        'ScaleFactor', 'MaxAcc', 'LastCorrection',
        'HypocentralDist', 'EpicentralDist'
    )

    propbox <- as.data.frame(propbox)

    # read
    for (i in 1:N_files) {

        suppressWarnings(
            prps <- flist[i] %>%
                read_fwf(n_max = 16, fwf_widths(c(18, 100)), cols(col_character(), col_character())) %>%
                as.data.frame()
        )

        SampFreq <- as.integer(strsplit(as.character(prps[11, 2]), 'Hz')[[1]])

        propbox[i,  1] <- basename(flist[i])
        propbox[i,  2] <-            as.character(prps[ 1, 2])
        propbox[i,  3] <- as.numeric(as.character(prps[ 2, 2]))
        propbox[i,  4] <- as.numeric(as.character(prps[ 3, 2]))
        propbox[i,  5] <- as.numeric(as.character(prps[ 4, 2]))
        propbox[i,  6] <- as.numeric(as.character(prps[ 5, 2]))
        propbox[i,  7] <-            as.character(prps[ 6, 2])
        propbox[i,  8] <- as.numeric(as.character(prps[ 7, 2]))
        propbox[i,  9] <- as.numeric(as.character(prps[ 8, 2]))
        propbox[i, 10] <- as.numeric(as.character(prps[ 9, 2]))
        propbox[i, 11] <-            as.character(prps[10, 2])
        propbox[i, 12] <- SampFreq
        propbox[i, 13] <- as.numeric(as.character(prps[12, 2]))
        propbox[i, 14] <-            as.character(prps[13, 2])
        propbox[i, 15] <-            as.character(prps[14, 2])
        propbox[i, 16] <- as.numeric(as.character(prps[15, 2]))
        propbox[i, 17] <-            as.character(prps[16, 2])
        propbox[i, 18] <- hyp_dist(propbox$Depth[i],
                                   propbox$OriginLat [i], propbox$OriginLong [i],
                                   propbox$StationLat[i], propbox$StationLong[i])
        propbox[i, 19] <- 10^(-3) * distGeo(c(propbox$OriginLong [i], propbox$OriginLat [i]),
                                            c(propbox$StationLong[i], propbox$StationLat[i]))
    }

    return (propbox)
}
