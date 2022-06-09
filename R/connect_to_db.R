#' Establishing a connection to the aikia database
#'
#' @param mydb A mydb connection from DBI::dbConnect
#' @param database The database name
#' @param user Your MariaDB Username
#' @param password Your password
#'
#' @return A DBI connection object
#' @export
#'
#' @examples \dontrun{
#' connect_to_DB(user = "testuser", password = "password")
#' }
connect_to_db <- function(mydb, database = "fin_data", user, password){

  ip <- unlist(pingr::nsl("aikia.org")$answer$data)

  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){

      mydb <- DBI::dbConnect(RMariaDB::MariaDB(),
                             host = ip,
                             user = user,
                             password = password,
                             port = 3306,
                             dbname = database)
  }
}


