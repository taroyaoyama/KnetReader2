
#' Extracting K-NET/KiK-net ASCII format files from compressed folder
#'
#' @param tarfile compressed .tar.gz folder
#' @param exdir directory for output
#' @export

extractASCII <- function (tarfile, exdir) {

    # create directory for output
    if(!file.exists(exdir)) dir.create(exdir)

    # decompression
    untar(tarfile = tarfile, exdir = exdir)

    # search '.tar' file
    lf <- list.files(exdir)
    lftar <- lf[grep('\\.tar', lf)]

    # untar
    while (length(lftar) != 0) {

        for (i in 1:length(lftar)) {

            untar(paste0(exdir, '/', lftar[i]), exdir = exdir)
            file.remove(paste0(exdir, '/', lftar[i]))
        }

        lf <- list.files(exdir)
        lftar <- lf[grep('\\.tar', lf)]
    }

    # remove 'trash'
    file.remove(paste0(exdir, '/', lf[grep('\\.gz$', lf)]))
}
