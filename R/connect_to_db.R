#' Establishing a connection to the aikia database
#'
#' @param database The database name
#' @param user Your MariaDB Username
#' @param password Your password
#'
#' @return A DBI connection object
#' @export
#'
#' @examples \dontrun{
#' connect_to_db(user = "testuser", password = "password")
#' }
connect_to_db <- function(database = "fin_data", user=NULL, password=NULL){

  ip <- unlist(pingr::nsl("aikia.org")$answer$data)


  if(Sys.info()[1]=="Windows"){
    mydb <- DBI::dbConnect(RMariaDB::MariaDB(),
                           host = ip,
                           user = user,
                           password = password,
                           group = database,
                           default.file = "C:/Users/Admin/Documents/Git Projects/aikia/my.cnf")
  } else {

    mydb <- DBI::dbConnect(RMariaDB::MariaDB(),
                           host = ip,
                           user = user,
                           password = password,
                           group = database,
                           default.file = paste0(here::here(),"/my.cnf"))
  }
}


