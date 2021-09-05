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


    let columnExprToColumnName (ce: ColumnExpr) =
        match ce with
        | NonQualifiedColumnExpr (cn) -> "" + cn // TODO: concat the (single) "global" table name!
        | QualifiedColumnExpr (tn, cn) -> tn + "." + cn


    let filterExprToColumnNames fe =
        match fe with
        | EqualsColumnExpr (c1, c2) -> seq { (columnExprToColumnName c1); (columnExprToColumnName c2) }
        | EqualsIntExpr (c, _) -> seq { (columnExprToColumnName c) }


    let extractCols (wc: WhereClause) =
        match wc with
        | Filters (fltrs) ->
            fltrs
            |> Seq.ofList
            |> Seq.collect filterExprToColumnNames
            |> Seq.distinct
            |> Seq.toList
            |> List.sort


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
        
        let fromClauseTableNames = fromClause |> extractTableNames
        let joinClausesTableNames = [] // TODO!
        let distinctTableNames = List.append fromClauseTableNames joinClausesTableNames

        let whereClauseCols =
            whereClause
            |> Option.map extractCols
            |> Option.defaultValue []
        
        // map the column aliases to their proper names:
        let allCols = List.append [] whereClauseCols // TODO: append select cols

        // default result, just while developing:
        let row1 = {
            Table = "customer";
            KeysAndValues = [("id", "123", false); ("name", "Jane Doe", true)]
        }

        let row2 = {
            Table = "contract";
            KeysAndValues = [
                ("id", "987", false); ("due_date", "2021-08-15", true);
                ("status", "0", true); ("fk_customer", "123", false)] 
        }
        [row1; row2]


    let private concat f (keyValueList: (string * string * bool) list) =
        let l =
            keyValueList
            |> List.map f
            |> List.fold (fun acc elem -> (acc + elem)) "("
        
        // get rid of the last ", " and add a ")":
        let i = l.Length - 3
        l.[0..i] + ")"


    let toInsertStatement (tdr: TestDataRow) =
        let {Table=t; KeysAndValues=keyValueList} = tdr
        let columns = keyValueList |> concat (fun (k, _, _) -> (k + ", "))
        let values = keyValueList |> concat (fun (_, v, b) -> if b then ("'" + v + "', ") else (v + ", "))
        "INSERT INTO " + t + " " + columns + " VALUES " + values + ";"


    let buildInsertStatementList (testDataRows: TestDataRow list) =
        testDataRows
        |> List.map toInsertStatement


    let generateTestData query: Result<string list, string> =
        parse query
            |> Result.map extractColumnDetails
            |> Result.map buildInsertStatementList 
