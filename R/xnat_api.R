if(getRversion() >= "2.15.1")  utils::globalVariables(c("jsid"))

subject_search_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<xdat:search allow-diff-columns="0" secure="false"
xmlns:xdat="http://nrg.wustl.edu/security" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<xdat:root_element_name>xnat:subjectData</xdat:root_element_name>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>PROJECT</xdat:field_ID>
<xdat:sequence>0</xdat:sequence>
<xdat:header>Subject label</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>ID</xdat:field_ID>
<xdat:sequence>1</xdat:sequence>
<xdat:header>Subject label</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>LABEL</xdat:field_ID>
<xdat:sequence>1</xdat:sequence>
<xdat:header>Subject label</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>GENDER_TEXT</xdat:field_ID>
<xdat:sequence>2</xdat:sequence>
<xdat:header>Gender</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>HANDEDNESS_TEXT</xdat:field_ID>
<xdat:sequence>3</xdat:sequence>
<xdat:header>Handedness</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>DOB</xdat:field_ID>
<xdat:sequence>4</xdat:sequence>
<xdat:header>YOB</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>EDUC</xdat:field_ID>
<xdat:sequence>5</xdat:sequence>
<xdat:header>Education</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>SES</xdat:field_ID>
<xdat:sequence>6</xdat:sequence>
<xdat:header>SES</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>SUB_GROUP</xdat:field_ID>
<xdat:sequence>7</xdat:sequence>
<xdat:header>Group</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>RACE</xdat:field_ID>
<xdat:sequence>8</xdat:sequence>
<xdat:header>Race</xdat:header>
</xdat:search_field>
<xdat:search_field>
<xdat:element_name>xnat:subjectData</xdat:element_name>
<xdat:field_ID>ETHNICITY</xdat:field_ID>
<xdat:sequence>9</xdat:sequence>
<xdat:header>Ethnicity</xdat:header>
</xdat:search_field>
</xdat:search>'

get_experiment_search_xml <- function(type)
{

  xml <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
                 <xdat:search ID="" allow-diff-columns="0" secure="false" brief-description="MR Sessions"
                 xmlns:xdat="http://nrg.wustl.edu/security" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                 <xdat:root_element_name>%s</xdat:root_element_name>
                 <xdat:search_field>
                 <xdat:element_name>%s</xdat:element_name>
                 <xdat:field_ID>LABEL</xdat:field_ID>
                 <xdat:sequence>0</xdat:sequence>
                 <xdat:type>string</xdat:type>
                 <xdat:header>one</xdat:header>
                 </xdat:search_field>
                 <xdat:search_field>
                 <xdat:element_name>%s</xdat:element_name>
                 <xdat:field_ID>SUBJECT_ID</xdat:field_ID>
                 <xdat:sequence>2</xdat:sequence>
                 <xdat:type>string</xdat:type>
                 <xdat:header>Subject</xdat:header>
                 </xdat:search_field>
                 <xdat:search_field>
                 <xdat:element_name>%s</xdat:element_name>
                 <xdat:field_ID>AGE</xdat:field_ID>
                 <xdat:sequence>3</xdat:sequence>
                 <xdat:type>integer</xdat:type>
                 <xdat:header>Age</xdat:header>
                 </xdat:search_field>
                 </xdat:search>', type, type, type, type, type)
  return(xml)
}

scan_params_xnat_list <- c('subject_ID' = 'xnat:mrSessionData.SUBJECT_ID', 'project' = 'xnat:mrSessionData.PROJECT', 'age' = 'xnat:mrSessionData.AGE', 'experiment_ID' = 'xnat:mrScanData.IMAGE_SESSION_ID', 'type' = 'xnat:mrScanData.TYPE' )

get_scan_parameters_search_xml <- function(subject_ID = NULL,
                                           project = NULL,
                                           age = NULL,
                                           experiment_ID = NULL,
                                           type = NULL,
                                           TR = NULL,
                                           TE = NULL,
                                           TI = NULL,
                                           flip = NULL,
                                           voxel_res = NULL,
                                           voxel_res_X = NULL,
                                           voxel_res_Y = NULL,
                                           voxel_res_Z = NULL,
                                           orientation = NULL) {

  scan_search_xml = '<?xml version="1.0" encoding="UTF-8"?>
      <xdat:search ID="" allow-diff-columns="0" secure="false" brief-description="MR Sessions"
      xmlns:xdat="http://nrg.wustl.edu/security" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <xdat:root_element_name>xnat:mrSessionData</xdat:root_element_name>
      <xdat:search_field>
      <xdat:element_name>xnat:mrSessionData</xdat:element_name>
        <xdat:field_ID>SUBJECT_ID</xdat:field_ID>
        <xdat:sequence>0</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>SUBJECT_ID</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrSessionData</xdat:element_name>
        <xdat:field_ID>PROJECT</xdat:field_ID>
        <xdat:sequence>1</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>PROJECT</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrSessionData</xdat:element_name>
        <xdat:field_ID>AGE</xdat:field_ID>
        <xdat:sequence>2</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>AGE</xdat:header>
        </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>IMAGE_SESSION_ID</xdat:field_ID>
        <xdat:sequence>3</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>Session ID</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>TYPE</xdat:field_ID>
        <xdat:sequence>4</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>Type</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_TR</xdat:field_ID>
        <xdat:sequence>5</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>TR</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_TE</xdat:field_ID>
        <xdat:sequence>6</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>TE</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_TI</xdat:field_ID>
        <xdat:sequence>7</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>TI</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_FLIP</xdat:field_ID>
        <xdat:sequence>8</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>FLIP</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_VOXELRES_UNITS</xdat:field_ID>
        <xdat:sequence>9</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>VOXELRES_UNITS</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
      <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_VOXELRES_X</xdat:field_ID>
        <xdat:sequence>10</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>VOXELRES_X</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_VOXELRES_Y</xdat:field_ID>
        <xdat:sequence>11</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>VOXELRES_Y</xdat:header>
        </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_VOXELRES_Z</xdat:field_ID>
        <xdat:sequence>12</xdat:sequence>
        <xdat:type>float</xdat:type>
        <xdat:header>VOXELRES_Z</xdat:header>
      </xdat:search_field>
      <xdat:search_field>
        <xdat:element_name>xnat:mrScanData</xdat:element_name>
        <xdat:field_ID>PARAMETERS_ORIENTATION</xdat:field_ID>
        <xdat:sequence>13</xdat:sequence>
        <xdat:type>string</xdat:type>
        <xdat:header>Orientation</xdat:header>
      </xdat:search_field>
      '
   # if(!is.null(subject_ID)) {
  #    scan_search_xml <- paste0(scan_search_xml,'<xdat:search_where method="AND">
   #     <xdat:criteria override_value_formatting="0">
    #      <xdat:schema_field>xnat:mrSessionData.SUBJECT_ID</xdat:schema_field>
    #      <xdat:comparison_type>=</xdat:comparison_type>
    #      <xdat:value>',subject_ID,'</xdat:value>
    #      </xdat:criteria>
    #    </xdat:search_where>')
    #}

    query_xml <- NULL

    this_args <- match.call()

    for(i in names(this_args)) {
      if(!is.null(this_args[[`i`]])){
        query_xml <- paste0(query_xml,sprintf('<xdat:criteria override_value_formatting="0">
                                      <xdat:schema_field>%s</xdat:schema_field>
                                      <xdat:comparison_type>=</xdat:comparison_type>
                                      <xdat:value>%s</xdat:value>
                                      </xdat:criteria>',scan_params_xnat_list[[`i`]],this_args[[`i`]]))



      }
    }

    if(!is.null(query_xml)){
      query_xml <- paste0('<xdat:search_where method="AND">',query_xml,'</xdat:search_where>')
      scan_search_xml <- paste0(scan_search_xml,query_xml)
    }

    scan_search_xml <- paste0(scan_search_xml,'</xdat:search>')
    return(scan_search_xml)
    #return(query_xml)
}

#' @title Make a connection to an XNAT server
#' @description Make the XNAT connection and provides
#' functions to retrive the list of projects, experiments
#' and subjects.
#' @name xnat_connection
#' @param base_url the URL for the XNAT server
#' @param username user name to use for connection. If \code{NULL} this
#' can be provided through the \code{xxx_WEB_USER} system variable where
#' xxx is provided by the \code{xnat_name} parameter
#' @param password user name to use for connection. If \code{NULL} this
#' can be provided through the \code{xxx_WEB_PASS} system variable where
#' xxx is provided by the \code{xnat_name} parameter
#' @param xnat_name prefix to use for retrieving the proper username/pass
#' system variables for a specific XNAT server
#' @examples
#' ## Connect to the NITRC.ORG database
#' \dontrun{xnat_connect('https://nitrc.org/ir', xnat_name='NITRC')}
#'
#' ## Connect to the XNAT.CENTRAL database
#' \dontrun{xnat_connect('https://central.xnat.org', xnat_name="CENTRAL")}
#'
#' @return \code{projects}
#' @importFrom RCurl basicTextGatherer curlPerform parseHTTPHeader
#' @importFrom httr set_cookies timeout
#' @export
xnat_connect <- function(base_url, username=NULL, password=NULL, xnat_name=NULL)
{
  xnat_call <- function(request, customrequest = 'GET', data='') {
    if(is.null(jsid)) {
      stop('not connected')
    }
    reader <- basicTextGatherer()
    header <- basicTextGatherer()
    if(nchar(data) > 0) {
      customrequest = 'POST'
    }
    curlPerform(url = paste(base_url, request, sep = ''),
                writefunction = reader$update,
                headerfunction = header$update,
                customrequest = customrequest,
                postfields = data,
                ssl.verifypeer = FALSE,
                cookie = paste('JSESSIONID=', jsid, sep = ''))
    if(parseHTTPHeader(header$value())['status'] != 200) {
      stop('error during HTTP request')
    }
    return(reader$value())
  }

  close <- function() {
    if(!is.null(jsid)) {
      data <- xnat_call('/data/JSESSION', customrequest = 'DELETE')
      jsid <<- NULL
    }
  }

  projects <- function() {
    if(is.null(.projects)) {
      data <- xnat_call('/data/projects?format=csv')
      csv <- string2csv(data)
      csv <- csv[with(csv, order(csv[,1])),]
      rownames(csv) <- 1:nrow(csv)
      .projects <<- csv
    }
    return(.projects)
  }

  scans <- function(...) {
    data <- xnat_call('/data/search?format=csv',
                      data = get_scan_parameters_search_xml(...))
    csv <- string2csv(data)
    names(csv) <- c('subject_ID',
                    'Project',
                    'Age',
                    'experiment_ID',
                    'Type',
                    'TR',
                    'TE',
                    'TI',
                    'Flip',
                    'Voxel_res',
                    'Voxel_res_X',
                    'Voxel_res_Y',
                    'Voxel_res_Z',
                    'Orientation',
                    'quarantine_status')
    return(csv)
  }

  subjects <- function(project = NULL) {
    if(is.null(.subjects)) {
      data <- xnat_call('/data/search?format=csv',
                         data = subject_search_xml)
      csv <- string2csv(data)
      names(csv) <- c('project',
                      'ID',
                      'subjectid',
                      'label',
                      'gender',
                      'handedness',
                      'yob',
                      'education',
                      'ses',
                      'group',
                      'race',
                      'ethnicity',
                      'quarantine_status')
      csv["subjectid"] <- NULL
      csv["quarantine_status"] <- NULL
      csv <- csv[with(csv, order(csv[,1], csv[,3])),]
      rownames(csv) <- 1:nrow(csv)
      .subjects <<- csv
    }
    if(!is.null(project)) {
      if(!project %in% projects()$ID) {
        stop(sprintf('unknown project "%s"', project))
      }
      rv <- .subjects[.subjects$project==project,]
      rownames(rv) <- 1:nrow(rv)
      return(rv)
    }
    return(.subjects)
  }

  get_experiment_types <- function(project = NULL, subject = NULL) {
    if(is.null(.experiment.types)) {
      data <- xnat_call('/data/search/elements?format=csv')
      csv <- string2csv(data)
      et <- grep('^xnat:.*SessionData$', csv$ELEMENT_NAME, value = TRUE)
      if(length(et) == 0) {
        warning('error getting experiment types; falling back on CT, MR, PET, US')
        .experiment.types <<- c('xnat:ctSessionData',
                                'xnat:mrSessionData',
                                'xnat:petSessionData',
                                'xnat:usSessionData')
      } else {
        .experiment.types <<- et
      }
    }
    return(.experiment.types)
  }

  experiments <- function(e_project = NULL, e_subject = NULL) {
    if(is.null(.experiments)) {
      experiments <- NULL
      for(type in get_experiment_types()) {
        in_data <- get_experiment_search_xml(type)
        out_data <- xnat_call('/data/search?format=csv',
                               data = in_data)
        csv <- string2csv(out_data)
        if(nrow(csv) > 0) {
          if(type == 'xnat:mrSessionData') {
            csv <- subset(csv, select = c('subject_id',
                                          'session_id',
                                          'label',
                                          'age'))
          } else {
            csv <- subset(csv, select = c('subject_id',
                                          'expt_id',
                                          'label',
                                          'age'))
          }
          names(csv) <- c('subject_id', 'ID', 'label', 'age')
          csv$type <- rep(type, nrow(csv))
          experiments <- rbind(experiments, csv)
        }
      }
    if(is.null(experiments)) {
      .experiments <<- data.frame()
    }
    else {
      ss <- subset(subjects(), select = c('ID', 'label', 'project'))

      experiments <- merge(experiments,
                           ss,
                           by.x = 'subject_id',
                           by.y = 'ID')
      experiments <- subset(experiments, select = c('project',
                                                    'label.y',
                                                    'ID',
                                                    'type',
                                                    'label.x',
                                                    'age'))
      names(experiments) <- c('project',
                              'subject',
                              'ID',
                              'type',
                              'label',
                              'age')
      experiments <- experiments[with(experiments,
                                      order(experiments[,1],experiments[,2],experiments[,5])),]
      rownames(experiments) <- 1:nrow(experiments)
      .experiments <<- experiments
    }
  }
    if(!is.null(e_project)) {
      if(!e_project %in% projects()$ID) {
        stop(sprintf('unknown project "%s"', e_project))
      }
      if(!is.null(e_subject)) {
        if(!e_subject %in% subjects(e_project)$label) {
          stop(sprintf('no subject "%s" in project %s', e_subject, e_project))
          rv <- .experiments[.experiments$project==e_project&&.experiments$subject==e_subject,]
          rownames(rv) <- 1:nrow(rv)
          return(rv)
        }
      }
        rv <- .experiments[.experiments$project==e_project,]
        rownames(rv) <- 1:nrow(rv)
        return(rv)
    }
    return(.experiments)
  }

  get_xnat_experiment_resources <- function(experiment_ID) {
    data <- xnat_call(paste0('/data/experiments/',experiment_ID,'/scans/ALL/files?format=csv'))
    csv <- string2csv(data)

    return(csv)
  }

  download_file <- function(file_path,
                            destfile = NULL,
                            prefix = NULL,
                            verbose = FALSE,
                            error = FALSE
                            ) {
    if (is.null(destfile)) {
      if(!is.null(prefix)) {
        prefix <- paste0(prefix,"_")
      }
      destfile = file.path(tempdir(),paste0(prefix,basename(file_path)))
    }
    args = list(
      url = paste0(base_url,file_path),
      write_disk(path = destfile,
                 overwrite = TRUE),
      set_cookies(JSESSIONID = jsid)
    )
    if (verbose) {
      args = c(args, list(progress()))
    }
    ret <- do.call("GET", args)

    if (error) {
      stop_for_status(ret)
    }
    if(ret$status_code == "200") {
      return(destfile)
    }
    else {
      message("File not found")
      return(NULL)
    }
  }

  download_dir <- function(experiment_ID,
                           scan_type = NULL,
                           zipped = TRUE,
                           verbose = FALSE,
                           error = FALSE){

    if(zipped) {
      url_address <- paste0(base_url,"/data/experiments/",experiment_ID,"/scans/")
      if(is.null(scan_type)) {
        url_address <- paste0(url_address,"ALL")
      }
      else {
        url_address <- paste0(url_address,scan_type)
      }
      url_address <- paste0(url_address,"/files?format=zip")
      destfile = file.path(tempdir(),paste0(experiment_ID,".zip"))
      message(url_address)
      message(destfile)
      args = list(
        url = url_address,
        write_disk(path = destfile,
                   overwrite = TRUE),
        set_cookies(JSESSIONID = jsid),
        timeout(200)
      )
      if (verbose) {
        args = c(args, list(progress()))
      }
      ret <- do.call("GET", args)
      
      if (error) {
        stop_for_status(ret)
      }
      if(ret$status_code == "200") {
        return(destfile)
      }
      else {
        message("No resources found")
        return(NULL)
      }
    } 
    else {
      # to be added next
    }
  }
  
  reader <- basicTextGatherer()
  header <- basicTextGatherer()

  if(is.null(username) && !is.null(xnat_name)) {
    env_username = Sys.getenv(paste0(toupper(xnat_name),"_RXNAT_USER"), unset=NA)
    if(!is.na(env_username)){
      username = env_username
    }
  }
  else if(!is.null(xnat_name)){
    args = list(username)
    names(args) = paste0(toupper(xnat_name),"_RXNAT_USER")
    do.call(Sys.setenv, args)
  }

  if(is.null(password) && !is.null(xnat_name)) {
    env_password = Sys.getenv(paste0(toupper(xnat_name),"_RXNAT_PASS"), unset=NA)
    if(!is.na(env_password)) {
      password = env_password
    }
  }
  else if(!is.null(xnat_name)){
    args = list(password)
    names(args) = paste0(xnat_name,"_RXNAT_PASS")
    do.call(Sys.setenv, args)
  }

  if(is.null(username)) {
    curlPerform(url = paste(base_url, '/', sep = ''),
                writefunction = reader$update,
                headerfunction = header$update,
                ssl.verifypeer = FALSE)
    jsid <<- NULL
    for(h in strsplit(header$value(), '\n')[[1]]) {
      if(substring(h, 1, 23) == 'Set-Cookie: JSESSIONID=') {
        jsid <<- strsplit(substring(h, 24), ';')[[1]][[1]]
      }
    }
    if(is.null(jsid)) {
      stop('error starting session')
    }
  } else {

    curlPerform(url = paste(base_url, '/data/JSESSION', sep = ''),
                writefunction = reader$update,
                headerfunction = header$update,
                ssl.verifypeer = FALSE,
                userpwd = paste(username, password, sep = ':'),
                httpauth=1L)
    status = parseHTTPHeader(header$value())['status']
    if(status == 401) {
      stop('bad username/password')
    } else if(status != 200) {
      stop('error authenticating')
    }
    jsid <<- reader$value()
  }

  is.connected <- function() {
    if(is.null(jsid))
      return(FALSE)
    return(TRUE)
  }

  .projects <- NULL
  .subjects <- NULL
  .experiments <- NULL
  .experiment.types <- NULL
  .xnat_name <- xnat_name

  rv = list(base_url = base_url,
            close = close,
            is.connected = is.connected,
            projects = projects,
            subjects = subjects,
            experiments = experiments,
            xnat_name = xnat_name,
            get_xnat_experiment_resources = get_xnat_experiment_resources,
            download_file = download_file,
            download_dir = download_dir,
            scans = scans)

  class(rv) <- 'RXNATConnection'

  return(rv)
}


#' @title Convert string to csv
#' @description Convert a string to csv format
#' @param string input string
#' @importFrom utils read.csv
string2csv <- function(string) {
  c <- textConnection(string)
  csv <- read.csv(c, as.is = TRUE)
  close(c)

  return(csv)
}

#' @title Get scan resources for a specific experiment ID
#' @description Get a full list of available resources for a specific experiment ID
#' @param conn The XNAT connection returned by a \code{xnat_connect} call
#' @param ... experiment_ID the experiment ID identifier, unique for each individual subject
#' @examples
#' ## Connect to XNAT CENTRAL
#' xnat_central_conn <- xnat_connect('https://central.xnat.org', xnat_name="CENTRAL")
#' get_scan_resources(xnat_central_conn,'CENTRAL_E00760')
#' @export
get_scan_resources = function(conn, ...){
  conn$get_xnat_experiment_resources(...)
}

#' @title Download XNAT file
#' @description Download a single file from XNAT
#' @param conn The XNAT connection returned by a \code{xnat_connect} call
#' @param ... file_path Path to the file to be dowloaded
#'  destfile Destination filename
#'  prefix Prefix the file name with this (prevents
#'  overwritting of same name files in case function is
#'  used to download multiple scan types at once)
#'  verbose Should progress be added to download?
#'  error Should function error if download failed?
#'
#' @return Display path to the downloaded file
#' @importFrom httr stop_for_status write_disk progress GET
#' @examples
#' ## file_path is retrieved using the get_scan_resources() function
#' \dontrun{xnat_central_conn <- xnat_connect('https://central.xnat.org', xnat_name="CENTRAL")}
#' \dontrun{r <- get_scan_resources(xnat_central_conn,'CENTRAL_E00760')}
#' \dontrun{download_file(xnat_connect,r$URI[1])}
#' @export
download_xnat_file = function(conn, ...){
  conn$download_file(...)
}


#' @title Download XNAT directory
#' @description Download a full directory of data
#' @param conn The XNAT connection returned by a \code{xnat_connect} call
#' @param ... experiment_ID the experiment Id for which we need to download data
#'   scan_type type of image scan
#'   zipped zip the downloaded result
#'   verbose Should progress be added to download?
#'   error Should function error if download failed?
#' 
#' @return Display path to the downloaded file
#' @importFrom httr stop_for_status write_disk progress GET
#' @examples
#' \dontrun{download_xnat_dir(hcp, experiment_ID='ConnectomeDB_E03657',scan_type='T2w')}
#' @export
download_xnat_dir = function(conn, ...){
  conn$download_dir(...)
}

#' @title Query all XNAT scan resources
#' @description Query all scan resources to match
#' specific query parameters and return a list of all
#' mathching rows
#' @param conn The XNAT connection returned by a \code{xnat_connect} call
#' @param ... Select query parameters: subject_ID, project, age, experiment_ID, type
#'   TR, TE, TI, flip, voxel_res, voxel_res_X, voxel_res_Y, voxel_res_Z,
#'   orientation 
#' 
#' @return A data.frame containing all matching rows. XNAT does not do 
#' sql join joins so only one row is returned per match. However each of the 
#' experiment_IDs returned will have at least one row matching the user
#' query (even if the displayed results show something else). This function
#' should be used just to retrieve matching experiment IDs for downloading
#' the queried data.
#' @examples 
#' \dontrun{hcp <-xnat_connect('https://db.humanconnectome.org', xnat_name = "hcp")}
#' \dontrun{query_scan_resources(hcp,age='26', project='HCP_500')}
#' @export
query_scan_resources = function(conn, ...){
  conn$scans(...)
}
