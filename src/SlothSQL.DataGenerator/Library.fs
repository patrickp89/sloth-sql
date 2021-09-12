namespace SlothSQL.DataGenerator

module TestDataGenerator =

    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax

    type TestDataRow = {
        Table: string;
        KeysAndValues: (string * string * bool) list
    }


    let toTableName (tableExpr: TableExpr) =
        match tableExpr with
        | NonAliasTableExpr (t) -> t
        | TableAliasExpr (t, _) -> t


    let extractTableNames (fromClause: FromClause) =
        match fromClause with
        | Tables (tl) ->
            tl
            |> List.map toTableName


    let columnExprToColumnName ce =
        match ce with
        | NonQualifiedColumnExpr (cn) -> "" + cn // TODO: concat the (single) "global" table name!
        | QualifiedColumnExpr (tn, cn) -> tn + "." + cn


    let filterExprToColumnNames fe =
        match fe with
        | EqualsColumnExpr (c1, c2) -> seq { (columnExprToColumnName c1); (columnExprToColumnName c2) }
        | EqualsIntExpr (c, _) -> seq { (columnExprToColumnName c) }


    /// Extracts a list of all distinct column names from a
    /// given "WHERE ..." clause.
    let extractWhereColumns (wc: WhereClause) =
        match wc with
        | Filters (fltrs) ->
            fltrs
            |> Seq.ofList
            |> Seq.collect filterExprToColumnNames
            |> Seq.sort
            |> Seq.distinct
            |> Seq.toList


    /// Extracts a list of all distinct column names from a
    /// given "SELECT ..." clause.
    let extractSelectColumns (sc: SelectClause) =
        match sc with
        | Wildcard -> []
        | Columns (cols) ->
            cols
            |> Seq.ofList
            |> Seq.map columnExprToColumnName
            |> Seq.sort
            |> Seq.distinct
            |> Seq.toList


    /// Maps a given table name to a list of triplets where each
    /// triplet corresponds to a key-value pair (plus a type flag) that
    /// will be added to the corresponding INSERT statement for this 
    /// pecific table, e.g.
    ///   "customer" -> [("id", "123", false); ("name", "Jane Doe", true)]
    /// which will result in a statement
    ///   "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
    let buildKeysAndValues () =
        // TODO!
        [
            ("customer", [("id", "123", false); ("name", "Jane Doe", true)]);
            ("contract", [("due_date", "2021-08-15", true); ("fk_customer", "123", false)])
        ] |> Map.ofList


    /// Creates a single test data row from a table (name).
    let toTestDataRow (tableToCols: Map<string, (string * string * bool) list>) (tableName: string) =
        let keysAndValues =
            tableToCols.TryFind tableName
            |> Option.defaultValue []
        {
            Table = tableName;
            KeysAndValues = keysAndValues
        }


    let extractColumnDetails (q: SqlSelectQuery) =
        let selectClause =
            match q with
            | SelectFrom (sc, _) -> sc
            | SelectFromWhere (sc, _, _) -> sc
        
        let fromClause =
            match q with
            | SelectFrom (_, fc) -> fc
            | SelectFromWhere (_, fc, _) -> fc

        let whereClause =
            match q with
            | SelectFrom (_) -> None
            | SelectFromWhere (_, _, wc) -> Some wc

        // figure out all table names:        
        let fromClauseTableNames = extractTableNames fromClause
        let joinClausesTableNames = [] // TODO!
        let distinctTableNames = List.append fromClauseTableNames joinClausesTableNames
        printf "distinct table names: %A\n" distinctTableNames

        // figure out all column names:
        let selectClauseCols = extractSelectColumns selectClause

        let whereClauseCols =
            whereClause
            |> Option.map extractWhereColumns
            |> Option.defaultValue []
        
        let allCols = List.append selectClauseCols whereClauseCols
        printf "all column names: %A\n" allCols

        // now build a single test data row for each table:
        let tableToCols = buildKeysAndValues ()
        distinctTableNames
        |> List.map (toTestDataRow tableToCols)


    /// Concats either all keys or all values into a single list of
    /// keys (or values respectively), e.g. "(a, b, c, d)".
    let private concat f (keyValueList: (string * string * bool) list) =
        let l =
            keyValueList
            |> List.map f
            |> List.fold (fun acc elem -> (acc + elem)) "("
        
        // get rid of the last ", " and add a ")":
        let i = l.Length - 3
        l.[0..i] + ")"


    /// Assembles a single "INSERT INTO ..." statement.
    let toInsertStatement (tdr: TestDataRow) =
        let {Table=t; KeysAndValues=keyValueList} = tdr
        let columns = keyValueList |> concat (fun (k, _, _) -> (k + ", "))
        let values = keyValueList |> concat (fun (_, v, b) -> if b then ("'" + v + "', ") else (v + ", "))
        "INSERT INTO " + t + " " + columns + " VALUES " + values + ";"


    let private buildInsertStatementList (testDataRows: TestDataRow list) =
        testDataRows
        |> List.map toInsertStatement


    /// Parses a given SQL SELECT query and returns a list
    /// of INSERT statements.
    let generateTestData query: Result<string list, string> =
        parse query
            |> Result.map extractColumnDetails
            |> Result.map buildInsertStatementList 
