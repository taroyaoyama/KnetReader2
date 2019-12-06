
#' Searching K-NET/KiK-net ASCII format files by given conditions
#'
#' @param flist list of pathnames of K-NET/KiK-net ASCII format file
#' @export

KnetDataSearch <- function (flist,
                            olat  = c(    0,   90),
                            olong = c(    0,  180),
                            depth = c(    0, 10^4),
                            mag   = c(    0,   10),
                            slat  = c(    0,   90),
                            slong = c(    0,  180),
                            shgt  = c(-10^8, 10^8),
                            sfreq = c(  100,  200),
                            dur   = c(    0, 10^4),
                            macc  = c(    0, 10^5),
                            scode = c(),
                            direc = c('N-S', 'E-W', 'U-D', 1:6),
                            epid  = c(0, 10^5),
                            hypd  = c(0, 10^5),
                            otime = as.POSIXlt(c('1900-01-01 00:00:00 JST', '2100-01-01 00:00:00 JST')),
                            rtime = as.POSIXlt(c('1900-01-01 00:00:00 JST', '2100-01-01 00:00:00 JST')),
                            ctime = as.POSIXlt(c('1900-01-01 00:00:00 JST', '2100-01-01 00:00:00 JST'))) {

    ## listing
    KDL <- KnetDataListing(flist)

    ## format to POSIXlt
    otime <- as.POSIXlt(otime)
    rtime <- as.POSIXlt(rtime)
    ctime <- as.POSIXlt(ctime)

    ## switch
    swc <- as.POSIXlt(KDL$OriginTime)     >= otime[1] & as.POSIXlt(KDL$OriginTime)     <= otime[2] &
           as.POSIXlt(KDL$RecordTime)     >= rtime[1] & as.POSIXlt(KDL$RecordTime)     <= rtime[2] &
           as.POSIXlt(KDL$LastCorrection) >= ctime[1] & as.POSIXlt(KDL$LastCorrection) <= ctime[2] &
                      KDL$OriginLat       >=  olat[1] &            KDL$OriginLat       <=  olat[2] &
                      KDL$OriginLong      >= olong[1] &            KDL$OriginLong      <= olong[2] &
                      KDL$Depth           >= depth[1] &            KDL$Depth           <= depth[2] &
                      KDL$Magnitude       >=   mag[1] &            KDL$Magnitude       <=   mag[2] &
                      KDL$StationLat      >=  slat[1] &            KDL$StationLat      <=  slat[2] &
                      KDL$StationLong     >= slong[1] &            KDL$StationLong     <= slong[2] &
                      KDL$StationHeight   >=  shgt[1] &            KDL$StationHeight   <=  shgt[2] &
                      KDL$DurationTime    >=   dur[1] &            KDL$DurationTime    <=   dur[2] &
                      KDL$MaxAcc          >=  macc[1] &            KDL$MaxAcc          <=  macc[2] &
                      KDL$HypocentralDist >=  hypd[1] &            KDL$HypocentralDist <=  hypd[2] &
                      KDL$EpicentralDist  >=  epid[1] &            KDL$EpicentralDist  <=  epid[2] &
                      KDL$Direction       %in%  direc &            KDL$SamplingFreq    %in%  sfreq &
                     (KDL$StationCode     %in%  scode |            length(scode)       ==        0 )

    return(KDL[swc,])
}

