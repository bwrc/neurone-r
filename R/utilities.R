#' Return a list of event files related to the given session number.
#'
#' @param datadir The directory containing the neurOne data
#' @param session.number The session number. Default is 1.
#'
#' @return The event files as a list.
#'
#' @keywords internal
get_event_files <- function(datadir, session.number = 1) {

    datapath <- file.path(datadir, as.character(session.number))

    event.files <- list()

    event.files["events"]            <- list.files(datapath, pattern = "^(events.bin)", full.names = TRUE)
    event.files["eventdata"]         <- list.files(datapath, pattern = "^(eventData.bin)", full.names = TRUE)
    event.files["eventdescriptions"] <- list.files(datapath, pattern = "^(eventDescriptions.bin)", full.names = TRUE)

    event.files
}


#' Return a list of data files related to the given session number.
#'
#' @param datadir The directory containing the neurOne data
#' @param session.number The session number. Default is 1.
#'
#' @return The data files as a list.
#'
#' @keywords internal
get_data_files <- function(datadir, session.number = 1) {
    datapath     <- file.path(datadir, as.character(session.number))
    list.files(datapath, pattern = "^[0-9]+\\.bin", full.names = TRUE)
}


#' #' Read an XML file and retun the result as a list.
#'
#' Read an XML file and retun the result as a list.
#'
#' @param filename The path to the XML file.
#' @return The XML file as a list.
#'
#' @keywords internal
xml_to_list <- function(filename) {
    xmlToList(xmlParse(filename))
}


#' Read protocol information.
#'
#' Read the protocol information from a list structure containing the protocol
#'
#' @param protocol The protocol as a list.
#' @return A list with with the protocol (inputs, vurves and monitors are removed from the input list).
#'
#' @keywords internal
get_protocol <- function(protocol) {
    ind  <- which(!(names(protocol) %in% c("TableProtocolInput", "TableProtocolCurve", "TableMonitor")))
    protocol[ind]
}


#' Return the number of sessions in the recording
#'
#' Return the number of sessions in the recording
#'
#' @param session The session as a list.
#' @return The number of sessions in the recordiing.
#'
#' @keywords internal
get_number_of_sessions <- function(session) {
    length(names(session) == "TableSessionPhase")
}


#' Read channel information
#'
#' Read information on the channels.
#'
#' @param protocol The protocol as a list.
#' @return A list with with the channel information (only TableProtocolInputs are kept in the input list).
#'
#' @keywords internal
get_channels <- function(protocol){
    ind      <- which(names(protocol) == "TableProtocolInput")
    channels <- protocol[ind]
    ip       <- vector(mode = "numeric", length = length(ind))

    for (i in seq.int(length(ind))) {
        names(channels)[i] <- channels[[i]]$Name
        ip[i]              <- as.numeric(channels[[i]]$InputNumber)
    }

    channels[order(ip)]
}


#' Read binary neurOne data from all channels.
#'
#' Read binary neurOne data from a bin-file. All channels are read.
#'
#' @param binfile The name of the file containing the data.
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#' @param samplingrate The sampling rate.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#'
#' @return A matrix with the channel data.
#'
#' @seealso get_channels
#'
#' @keywords internal
read_data <- function(binfile, channel.list, samplingrate, start = 0, data.length = NULL) {
    ## open the data file for reading
    f <- file(binfile, "rb")

    ## the number of channels
    n_channels <- length(channel.list)

    ## get file information
    f_info                <- file.info(binfile)
    n_samples_per_channel <- (f_info$size / n_channels / 4)
    t_total               <- n_samples_per_channel / samplingrate

    ## skip to a starting point
    sample_start <- start * samplingrate
    bytes_start <- ceiling(4 * n_channels * sample_start)
    seek(f, where = bytes_start, origin = "start")

    ## calculate amount of data to read
    if (is.null(data.length))
        nd <- ceiling(n_channels * ((t_total - start) * samplingrate))
    else
        nd <- ceiling(n_channels * (data.length * samplingrate))

    ## read the data
    data <- readBin(f, "integer", n = nd, 4, signed  = TRUE)
    close(f)

    data <- matrix(data, nrow = nd / n_channels, ncol = n_channels, byrow = TRUE)
    colnames(data) <- names(channel.list)

    data
}


#' Translate between channel name and number.
#'
#' Given a channel name or a channel number, find the corresponding number or name.
#'
#' @param channel The number or name of a channel.
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#'
#' @return A list with the name and number of the channel.
#'
#' @seealso get_channels
#'
#' @keywords internal
channel_name_number <- function(channel, channel.list) {
    if (class(channel) == "character") {
        channel.name <- channel
        channel      <- which(names(channel.list) == channel.name)

        if (length(channel) == 0)
            stop(paste("Channel '", channel.name, "' not found.", sep = ""))
    }

    if (class(channel) == "numeric") {
        channel.name   <- names(channel.list)[channel]

        if ((channel < 1) | (channel > length(channel.list)))
            stop("Channel number invalid")
    }

    list("name" = channel.name, "number" = channel)
}


#' Read binary neurOne data from one channel.
#'
#' Read binary neurOne data from a bin-file. Only data from one channel is read.
#'
#' @param binfile The name of the file containing the data.
#' @param channel The number or name of the channel to be read.
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#' @param samplingrate The sampling rate.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#'
#' @return A matrix with the channel data.
#'
#' @seealso get_channels
#'
#' @keywords internal
read_channel <- function(binfile, channel, channel.list, samplingrate, start = 0, data.length = NULL) {
    ## open the data file for reading
    f <- file(binfile, "rb")

    ## the number of channels
    n_channels <- length(channel.list)

    ## get file information
    f_info                <- file.info(binfile)
    n_samples_per_channel <- (f_info$size / n_channels / 4)
    t_total               <- n_samples_per_channel / samplingrate

    ## skip to a starting point
    sample_start <- start * samplingrate
    bytes_start <- ceiling(4 * n_channels * sample_start)
    seek(f, where = bytes_start, origin = "start")

    ## get channel name and number
    tmp          <- channel_name_number(channel, channel.list)
    channel.name <- tmp$name
    channel      <- tmp$number

    ## seek to the correct channel
    seek(f, where = (4 * channel) - 4, origin = "current")

    ## calculate number of samples to read
    if (is.null(data.length))
        Ns <- ceiling(n_samples_per_channel)
    else
        Ns <- ceiling(data.length * samplingrate)

    ## read the data
    data <- vector(mode = "numeric", length = Ns)

    for (i in seq.int(Ns)) {
        data[i] <- readBin(f, "integer", n = 1, 4, signed  = TRUE)
        seek(f, where = 4 * (n_channels - 1), origin = "current")
    }

    close(f)
    names(data) <- channel.name
    data
}


#' Read binary neurOne data from multiple channels.
#'
#' Read binary neurOne data from a bin-file. Data from multiple channels is read.
#'
#' @param binfile The name of the file containing the data.
#' @param channels The number or name of the channels to be read.
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#' @param samplingrate The sampling rate.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#'
#' @return A matrix with the channel data.
#'
#' @seealso get_channels
#'
#' @keywords internal
read_channels <- function(binfile, channels, channel.list, samplingrate, start = 0, data.length = NULL) {
    ## the number of channels
    n.channels <- length(channel.list)

    ## get file information
    f.info                <- file.info(binfile)
    n.samples.per.channel <- (f.info$size / n.channels / 4)
    t.total               <- n.samples.per.channel / samplingrate

    ## calculate number of samples to read
    if (is.null(data.length))
        Ns <- ceiling(n.samples.per.channel)
    else
        Ns <- ceiling(data.length * samplingrate)

    ## allocate for data
    data <- matrix(nrow = Ns, ncol = length(channels))
    colnames(data) <- rep("", length(channels))

    for (i in seq.int(length(channels))) {
        data[,i] <- read_channel(binfile, channel = channels[i], channel.list = channel.list, samplingrate = samplingrate, start = start, data.length = data.length)
        colnames(data)[i] <- channel_name_number(channels[i], channel.list)$name
    }

    data
}


#' Read all of the channel data associated with a particular session.
#'
#' @param datadir The directory containing the neurOne data.
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#' @param samplingrate The sampling rate.
#' @param start The offset in seconds from the beginning of the file to start reading data.
#' @param data.length The amount of data in seconds to be read.
#' @param session.number The session number. Default is 1.
#'
#' @return A matrix with the channel data.
#'
#' @seealso get_channels
#'
#' @keywords internal
read_session_data <- function(datadir, channel.list, channels, samplingrate, start, data.length, session.number = 1) {
    ## Get data files
    files.data  <- get_data_files(datadir, session.number = session.number)

    ## Number of channels to read
    if (is.null(channels)) {
        n.channels <- length(channel.list)
        read.mode <- "all"
    } else {
        n.channels <- length(channels)
        read.mode <- "selected"
    }

    ## Create a structure for the data
    data <- matrix(nrow = 0, ncol = n.channels)
    
    ## Read data associated with the given session
    for (f in files.data) {
        if (read.mode == "all")
            data <- rbind(data, read_data(f, channel.list, samplingrate = samplingrate, start = start, data.length = data.length))
        if (read.mode == "selected")
            data <- rbind(data, read_channels(f, channels, channel.list, samplingrate = samplingrate, start = start, data.length = data.length))
    }

    ## Calibrate the channel data
    cn             <- colnames(data)
    data           <- sapply(seq.int(ncol(data)), function(i) calibrate_channel(colnames(data)[i], data[,i], channel.list))
    colnames(data) <- cn

    ## Pack the data into a list
    data <- data_matrix_to_list(data, channel.list, samplingrate)
    
    data
}


#' Read events from a binary neurOne event file.
#' 
#' @param datadir The directory containing the neurOne data.
#' @param session.number The session number. Default is 1.
#' @param samplingrate The sampling rate.
#'
#' @return A matrix with the channel data.
#'
#' @seealso get_channels
#'
#' @keywords internal
read_events <- function(datadir, session.number = 1, samplingrate = NULL) {
    ## get the event files
    files.events <- get_event_files(datadir, session.number = session.number)

    ## open the data file for reading
    f <- file(files.events[["events"]], "rb")

    ## get file information
    f.info    <- file.info(files.events[["events"]])
    n.events  <- f.info$size / 88

    ## read the event data
    events <- matrix(nrow = n.events, ncol = 16)
    event.structure <- get_event_structure()

    for (i in seq.int(n.events))
        events[i,] <- parse_event(readBin(f, "raw", size = 1, n = 88, signed = FALSE, endian = "little"), event.structure)

    close(f)

    colnames(events) <- names(event.structure)
    events           <- as.data.frame(events)

    ## set the source port
    events$SourcePort <- ordered(sapply(events$SourcePort, set_source_port), levels = c("Unknown", "A", "B", "EightBit"))

    ## set the event type
    for (i in seq.int(nrow(events)))
        events[i,] <- set_code_event_type(events[i,], files.events)

    ## Add start and stop times of events if the sampling rate is given
    if (! is.null(samplingrate)) {
        events$StartTime <- events$StartSampleIndex / samplingrate
        events$StopTime  <- events$StopSampleIndex / samplingrate
    }

    ## do not include event fields reserved for future use (RFU)
    ind    <- grep("RFU?", names(events))
    events <- events[, -ind]

    events
}


#' Define the structure of a neurOne event.
#'
#' @return A list describing the structure of the event.
#'
#' @keywords internal
get_event_structure <- function() {
    list("Revision"          = list(n.bytes = 4, type = "int32"),
         "RFU1"              = list(n.bytes = 4, type = "int32"),
         "Type"              = list(n.bytes = 4, type = "int32"),
         "SourcePort"        = list(n.bytes = 4, type = "int32"),
         "ChannelNumber"     = list(n.bytes = 4, type = "int32"),
         "Code"              = list(n.bytes = 4, type = "int32"),
         "StartSampleIndex"  = list(n.bytes = 8, type = "uint64"),
         "StopSampleIndex"   = list(n.bytes = 8, type = "uint64"),
         "DescriptionLength" = list(n.bytes = 8, type = "uint64"),
         "DescriptionOffset" = list(n.bytes = 8, type = "uint64"),
         "DataLength"        = list(n.bytes = 8, type = "uint64"),
         "DataOffset"        = list(n.bytes = 8, type = "uint64"),
         "RFU2"              = list(n.bytes = 4, type = "int32"),
         "RFU3"              = list(n.bytes = 4, type = "int32"),
         "RFU4"              = list(n.bytes = 4, type = "int32"),
         "RFU5"              = list(n.bytes = 4, type = "int32"))
}


#' Parse a neurOne event.
#'
#' @param data A list of 88 bytes containing the event data.
#' @param event.structure The structure of the event. See \code{\link{get_event_structure}}
#'
#' @return The source port as a string.
#'
#' @keywords internal
parse_event <- function(data, event.structure) {
    event <- matrix(nrow = 1, ncol = 16)

    bytes.start <- 1

    i <- 1
    for (event.field in names(event.structure)) {
        n.bytes     <- event.structure[[event.field]]$n.bytes

        event[1, i] <- readBin(data[bytes.start:(bytes.start+n.bytes - 1)], "integer", size = n.bytes, n = 1, signed = TRUE)

        bytes.start <- bytes.start + n.bytes
        i <- i + 1
    }

    event

}


#' Map a numeric source port to a string representation.
#'
#' @param x A numeric describing the source port.
#'
#' @return The source port as a string.
#'
#' @keywords internal
set_source_port <- function(x) {
    list("0" = "Unknown",
         "1" = "A",
         "2" = "B",
         "3" = "EightBit")[[as.character(x)]]
}


#' Map the numeric event type to a string and set the event code.
#'
#' Also read user-defined events from the file eventData.bin and set
#' the contents as the event type.
#'
#' @param event An event (as a one-row data frame)
#' @param files.events A list containing the event files. See \code{\link{get_event_files}}
#'
#' @return The event with the type and code modified.
#'
#' @keywords internal
set_code_event_type <- function(event, files.events) {

    if (event[["Type"]] == 0) {
        event[["Type"]] <- paste(event[["SourcePort"]], "Unknown", sep = " - ")
        event[["Code"]] <- 0
    }
    else if (event[["Type"]] == 1) {
        event[["Type"]] <- paste(event[["SourcePort"]], "Stimulation", sep = " - ")
        event[["Code"]] <- 256
    }
    else if (event[["Type"]] == 2) {
        event[["Type"]] <- paste(event[["SourcePort"]], "Video", sep = " - ")
        event[["Code"]] <- 257
    }
    else if (event[["Type"]] == 3) {
        event[["Type"]] <- paste(event[["SourcePort"]], "Mute", sep = " - ")
        event[["Code"]] <- 258
    }
    else if (event[["Type"]] == 4) {
        event[["Type"]] <- as.character(event[["Code"]])
    }
    else if (event[["Type"]] == 5) {
        event[["Type"]] <- paste(event[["SourcePort"]], "Out", sep = " - ")
        event[["Code"]] <- 259
    }
    else if (event[["Type"]] == 6) {
        f <- file(files.events[["eventdata"]], "rb")

        seek(f, where = event[["DataOffset"]] / 2, origin = "start")

        event[["Type"]] <- readBin(f, what = character(), n = 1, size = (event[["DataLength"]] / 2))
        event[["Code"]] <- 260
        close(f)
    }

    event
}


#' Calibrate channel data
#'
#' @param channel.name The name of the channel
#' @param data The channel data as a vector
#' @param channel.list A list containing channel information (from the function \code{get_channels}).
#'
#' @return The calibrated channel data.
#'
#' @keywords internal
calibrate_channel <- function(channel.name, data, channel.list) {

    raw.min <- as.numeric(channel.list[[channel.name]]$RangeMinimum)
    raw.max <- as.numeric(channel.list[[channel.name]]$RangeMaximum)
    cal.min <- as.numeric(channel.list[[channel.name]]$RangeAsCalibratedMinimum)
    cal.max <- as.numeric(channel.list[[channel.name]]$RangeAsCalibratedMaximum)

    cal.min + ((data - raw.min) / ((raw.max - raw.min) * (cal.max - cal.min)))
}


#' Convert a data matrix to a list
#'
#' @param data A matrix containing the channel data in the columns
#' @param data
#'
#' @return A list with the data channels.
#'
#' @keywords internal
data_matrix_to_list <- function(data, channel_list, samplingrate) {
    out <- vector(mode = "list", length = ncol(data))

    time_vector   <- seq.int(0, (nrow(data) - 1)) / samplingrate

    for (i in seq.int(ncol(data))) {
        channel_name          <- colnames(data)[i]
        out[[i]]$data         <- data[,i]
        out[[i]]$t            <- time_vector
        out[[i]]$samplingrate <- samplingrate
        out[[i]]$unit         <- channel_list[[channel_name]]$Unit
        out[[i]]$signaltype   <- channel_list[[channel_name]]$SignalType
    }

    names(out) <- colnames(data)
    out
}


#' Return and initialise an empty recording structure.
#' The recording structure is a list.
#'
#' @return An empty recording structure.
#' 
#' @export
new_recording <- function() {
    ## Create containers
    recording                   <- list()
    recording$properties        <- list()
    recording$signal            <- list()
    recording$events            <- list()
    recording$properties$header <- list()
    
    recording$properties$time.start.raw <- NA
    recording$properties$time.start     <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$time.stop      <- NA

    ## Set subject and casename information
    recording$properties$subject        <- NA

    ## Information on the data format
    recording$properties$format         <- NA
    recording$properties$format.long    <- NA
    recording$properties$device.type    <- NA
    recording$properties$device.version <- NA

    ## The length of the recording in seconds
    recording$properties$length         <- NA

    recording
}


#' Convert a string to a timestamp.
#'
#' @return A timestamp representing the given string.
#'
#' @export
parse_timestamp <- function(ts, format = "%Y-%m-%dT%H:%M:%S") {
    as.POSIXct(strptime(ts, format = format))
}
