namespace SlothSQL.DataGenerator

module TestDataGenerator =

    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax

    type TestDataRow = {
        Table: string;
        KeysAndValues: (string * string * bool) list
    }


    let extractColumnDetails (q: SqlSelectQuery) =
        //for tn in sqlTokens do
        //    printfn "%s      %A" tn.Text tn.Type

        let selectClause =
            match q with
            | SelectFrom (sc, _) -> sc
            | SelectFromWhere (sc, _, _) -> sc
        // TODO: ...

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
