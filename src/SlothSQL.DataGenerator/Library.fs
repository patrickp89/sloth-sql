namespace SlothSQL.DataGenerator

module TestDataGenerator =

    open TSQL
    open TSQL.Tokens


    type SqlSelectStatement = {
        SelectClause: TSQLToken list option
        FromClause: TSQLToken list option
        WhereClause: TSQLToken list option
    }


    let parseQuery query: TSQLToken list =
        List.ofSeq (TSQLTokenizer.ParseTokens query)


    let extractSqlClause (sqlTokens: TSQLToken list) (startToken: string) (endToken: string) =
        let clause =
            sqlTokens
            |> List.skipWhile (fun t -> not(t.Type.Equals TSQLTokenType.Keyword && t.Text.Equals startToken))
            |> List.takeWhile (fun t -> not(t.Type.Equals TSQLTokenType.Keyword && t.Text.Equals endToken))
        printfn "\n%s clause:" startToken
        for tn in (clause |> List.map (fun t -> t.Text)) do
            printfn "%s" tn
        clause


    let extractSqlClauses (sqlTokens: TSQLToken list): SqlSelectStatement = {
        SelectClause = Some (extractSqlClause sqlTokens "SELECT" "FROM")
        FromClause = Some (extractSqlClause sqlTokens "FROM" "WHERE")
        WhereClause = Some (extractSqlClause sqlTokens "WHERE" "ORDER") // TODO: could be "GROUP", too!
    }


    let generateTestData query: string list =
        // parse the given SQL query:
        let sqlTokens = parseQuery query
        for tn in sqlTokens do
            printfn "%s      %A" tn.Text tn.Type

        // the parser returns a list, we therefore need to
        // extract the different SQL clauses manually:
        let clauses = extractSqlClauses sqlTokens
        [
            "INSERT INTO customer (id, name) VALUES (id, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
