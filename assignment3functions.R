query = # makes it so that you don't have to type dbGetQuery a bunch
  # db is the default
  function(myQuery, dbms = db){
    
    dbGetQuery(db, myQuery)
    
  }