
db.wordnet <- function(database_name = "wordnet", username = "root", usrpassword = "root"){
  require(RMySQL)
  
  conn_ = dbConnect(
    MySQL(),
    user=username,
    password=usrpassword,
    dbname=database_name
  )
  
  get.tables <- function(){
    return(dbListTables(conn_))
  }
  
  get.attributes <- function(table_name){
    dbListFields(conn_, table_name)
  }
  
  query <- function(query){
    rs <- dbSendQuery(conn_, query)
    df <- fetch(rs, n=-1)
    return(df)
  }
  
  list(tables = get.tables, attributesByTable = get.attributes, query = query)
}