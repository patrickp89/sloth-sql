namespace SlothSQL.DataGenerator

module TestDataGenerator =

    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax

    type TestDataRow = {
        Table: string;
        KeysAndValues: (string * string * bool) list
    }

    type Column = {
        TableNameOrAlias: string option; // TODO: use a proper sum type instead of a mere string!
        ColName: string
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


    let columnExprToColumn (tableName: string option) ce =
         // TODO: if tableName = None and the column name is distinct => look up its table!
        match ce with
        | NonQualifiedColumnExpr (cn) -> {TableNameOrAlias=tableName; ColName=cn}
        | QualifiedColumnExpr (tn, cn) -> {TableNameOrAlias=Some tn; ColName=cn}


    let filterExprToColumnNames fe =
        // there is no "global" table name in a WHERE clause:
        let tableName: string option = None

        // extract the column(s) name(s):
        match fe with
        | EqualsColumnExpr (c1, c2) -> seq {
                (columnExprToColumn tableName c1);
                (columnExprToColumn tableName c2)
            }
        | EqualsIntExpr (c, _) -> seq { (columnExprToColumn tableName c) }


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
        // there is no "global" table name in a SELECT clause:
        let tableName: string option = None

        // extract the column(s) name(s):
        match sc with
        | Wildcard -> []
        | Columns (cols) ->
            cols
            |> Seq.ofList
            |> Seq.map (columnExprToColumn tableName)
            |> Seq.sort
            |> Seq.distinct
            |> Seq.toList


    /// Checks whether a given column belongs to a given table.
    let isMatchingCol (tableName: string) (aliasesToTables: Map<string, string>) (c: Column) =
        let matchesByName = c.TableNameOrAlias.Value = tableName
        let alias = aliasesToTables.TryFind c.TableNameOrAlias.Value
        let matchesByAlias =
            alias
            |> Option.map (fun a -> a = tableName)
            |> Option.defaultValue false
        matchesByName || matchesByAlias


    let addTestValue (col: Column) =
        // TODO: look up generated test values from a dict!
        match col.ColName with
        | "due_date" -> ("due_date", "2021-08-15", true)
        | "fk_customer" -> ("fk_customer", "123", false)
        | "id" -> ("id", "123", false)
        | "name" -> ("name", "Jane Doe", true)
        | _ -> raise (System.ArgumentException("Could not recognize column!"))


    let tableNameToCols (tableName: string) (aliasesToTables: Map<string, string>) (allCols: Column list) =
        allCols
        |> List.filter (fun c -> c.TableNameOrAlias.IsSome)
        |> List.filter (fun c -> isMatchingCol tableName aliasesToTables c)
        |> List.sortBy (fun c -> c.ColName)
        |> List.map addTestValue


    /// Builds a map from table names to lists of triplets where each
    /// triplet corresponds to a key-value pair (plus a type flag) that
    /// will be added to the corresponding INSERT statement for this 
    /// pecific table, e.g.
    ///   "customer" -> [("id", "123", false); ("name", "Jane Doe", true)]
    /// which will finally result in an
    ///   "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
    /// statement.
    let buildTableToColsMap distinctTableNames aliasesToTables allCols =
        distinctTableNames
        |> List.map (fun t -> (t, tableNameToCols t aliasesToTables allCols))
        |> Map.ofList


    /// Creates a single test data row for a given table name.
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

        // figure out all column names:
        let selectClauseCols = extractSelectColumns selectClause

        let whereClauseCols =
            whereClause
            |> Option.map extractWhereColumns
            |> Option.defaultValue []
        
        let allCols = List.append selectClauseCols whereClauseCols

        let aliasesToTables = // TODO!
            [
                ("o", "contract");
                ("c", "customer")
            ] |> Map.ofList

        // associate each table name with its columns:
        let tableToCols = buildTableToColsMap distinctTableNames aliasesToTables allCols

        // now build a single test data row for each table:
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
