-- Logic Programming

-- look-up a query in the list of clauses
lookup1 h [] = (False,[])
lookup1 h ((p,t):r) = if h==p then (True,t) else lookup1 h r

--  takes a list of queries and a list of clauses, and returns True if the queries are satisfied by the program clauses
solution [] prog = True
solution (h:t) prog = 
    let (a,b) = (lookup1 h prog) in
    if a then solution (b++t) prog else False