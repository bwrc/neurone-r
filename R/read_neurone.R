#' Read data from a Mega Electronics neurOne device.
#'
#' @param datadir The directory containing the data.
#' @param session.number The session number. Default is 1.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#' @param channels A list containing channel names to be read.
#' @param read.events Should events be read. Boolean. Default is TRUE.
#' @param header.only Should only the header information, but no signal data, be read. Boolean. Default is FALSE.
#'
#' @return A recording structure (a list) containing the data.
#'
#' @export
read.neurone <- function(datadir, session.number = 1, start = 0, data.length = NULL, channels = NULL, read.events = TRUE, header.only = FALSE) {

    ## --------------------------------------------------
    ## Read header information
    ## --------------------------------------------------

    ## get protocol and session files from the datadir
    file.protocol <- "DataSetProtocol.xml"
    file.session  <- "DataSetSession.xml"

    path.protocol <- file.path(datadir, file.protocol)
    path.session  <- file.path(datadir, file.session)

    ## check that these files exist, if OK read and parse
    if (! file.exists(path.protocol))
        stop("Protocol file not found!")

    if (! file.exists(path.session))
        stop("Session file not found!")

    protocol <- xml_to_list(path.protocol)
    session  <- xml_to_list(path.session)

    channel_list  <- get_channels(protocol)
    protocol_list <- get_protocol(protocol)

    ## Get sampling rate
    samplingrate  <- as.numeric(protocol$TableProtocol$SamplingFrequency)

    ## Get number of sessions
    n.sessions <- get_number_of_sessions(session)

    ## --------------------------------------------------
    ## Create recording
    ## --------------------------------------------------
    recording <- new_recording()

    time_tmp <- get_session_start_and_end(session, session_number = session.number)

    recording$properties$time.start.raw <- time_tmp$start_time_raw
    recording$properties$time.start     <- time_tmp$start_time

    recording$properties$time.stop.raw  <- time_tmp$stop_time_raw
    recording$properties$time.stop      <- time_tmp$stop_time

    ## Set subject and casename information
    recording$properties$subject        <- session$TablePerson$PersonId

    ## Information on the data format
    recording$properties$format         <- session$TableInfo$Revision
    recording$properties$format.long    <- paste0("Software revision ", session$TableInfo$Revision)
    recording$properties$device.type    <- "neurOne"
    recording$properties$device.version <- session$TableInfo$NeurOneVersion

    recording$properties$header$protocol   <- protocol_list
    recording$properties$header$channel    <- channel_list
    recording$properties$header$session    <- session
    recording$properties$header$n_sessions <- n.sessions

    ## --------------------------------------------------
    ## Channels and session number to be read
    ## --------------------------------------------------
    if (! header.only) {
        if (session.number > n.sessions)
            stop("Given session number exceeds the number of available sessions in this recording.")
        
        ## Store channel data in the recording
        recording$signal <- read_session_data(datadir, channel_list, channels, samplingrate, start, data.length, session.number)

        ## The length of the recording in seconds
        recording$properties$length <- length(recording$signal[[1]]$data) / samplingrate

        if (! is.null(data.length)) {
            recording$properties$time.stop.raw  <- NA
            recording$properties$time.stop      <- recording$properties$time.start + data.length
        }
    }

    ## --------------------------------------------------
    ## Read events
    ## --------------------------------------------------
    if (read.events) {
        recording$events <- read_events(datadir, session.number = session.number, samplingrate = samplingrate)
    }

    ## --------------------------------------------------
    ## Return recording
    ## --------------------------------------------------

    recording
}
