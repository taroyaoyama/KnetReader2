
#' Formatting K-NET/KiK-net ASCII to csv file
#'
#' @param flist list of pathnames of K-NET/KiK-net ASCII format file
#' @param exdir directory for output
#' @importFrom readr write_csv
#' @export

ASCIItocsv <- function (flist, exdir, unified = TRUE) {

    if (!file.exists(exdir)) dir.create(exdir)

    # extract ascii format files
    flist <- flist[c(grep('\\.EW$' , flist), grep('\\.NS$' , flist), grep('\\.UD$' , flist),
                     grep('\\.EW1$', flist), grep('\\.NS1$', flist), grep('\\.UD1$', flist),
                     grep('\\.EW2$', flist), grep('\\.NS2$', flist), grep('\\.UD2$', flist))]


    # properties
    properties <- KnetDataListing(flist)
    write_csv(properties, paste0(exdir, '/_properties.csv'))

    if (unified) {

        # df
        df <- data.frame(file = flist, station = substr(basename(flist), 1, 16))
        st <- unique(df[,2])

        for (i in 1:length(st)) {

            cmps <- as.character(df[df$station == st[i], 1])
            prps <- properties[df$station == st[i],]

            # data length
            dlen <- prps$SamplingFreq[1] * prps$DurationTime[1]

            # get code and extension
            ss <- strsplit(basename(cmps[1]), '\\.')[[1]]

            # knt
            if (nchar(ss[2]) == 2) {

                out <- array(NA, dim = c(dlen, 4))
                colnames(out) <- c('Time', 'NS', 'EW', 'UD')

                out[,1] <- 1:dlen/prps$SamplingFreq[1]

                if (length(grep('\\.NS$', cmps))) {

                    out[,2] <- KnetReader2(cmps[grep('\\.NS$', cmps)])$data
                }
                if (length(grep('\\.EW$', cmps))) {

                    out[,3] <- KnetReader2(cmps[grep('\\.EW$', cmps)])$data
                }
                if (length(grep('\\.UD$', cmps))) {

                    out[,4] <- KnetReader2(cmps[grep('\\.UD$', cmps)])$data
                }

                write_csv(as.data.frame(out), paste0(exdir, '/', ss[1], '.csv'))
            }

            # kik
            if (nchar(ss[2]) == 3) {

                out <- array(NA, dim = c(dlen, 7))
                colnames(out) <- c('Time', 'NS1', 'NS2', 'EW1', 'EW2', 'UD1', 'UD2')

                out[,1] <- 1:dlen/prps$SamplingFreq[1]

                if (length(grep('\\.NS1$', cmps))) {

                    out[,2] <- KnetReader2(cmps[grep('\\.NS1$', cmps)])$data
                }
                if (length(grep('\\.NS2$', cmps))) {

                    out[,3] <- KnetReader2(cmps[grep('\\.NS2$', cmps)])$data
                }
                if (length(grep('\\.EW1$', cmps))) {

                    out[,4] <- KnetReader2(cmps[grep('\\.EW1$', cmps)])$data
                }
                if (length(grep('\\.EW2$', cmps))) {

                    out[,5] <- KnetReader2(cmps[grep('\\.EW2$', cmps)])$data
                }
                if (length(grep('\\.UD1$', cmps))) {

                    out[,6] <- KnetReader2(cmps[grep('\\.UD1$', cmps)])$data
                }
                if (length(grep('\\.UD2$', cmps))) {

                    out[,7] <- KnetReader2(cmps[grep('\\.UD2$', cmps)])$data
                }

                write_csv(as.data.frame(out), paste0(exdir, '/', ss[1], '.csv'))
            }
        }

    } else {

        for (i in 1:length(flist)) {

            KR2 <- KnetReader2(flist[i])
            acc <- KR2$data

            # def. filename for output
            ss <- strsplit(basename(flist[i]), '\\.')[[1]]
            outname <- paste0(exdir,'/',ss[1],'_',ss[2],'.csv')

            # output to csv
            write_csv(as.data.frame(acc), outname)
        }
    }
}

