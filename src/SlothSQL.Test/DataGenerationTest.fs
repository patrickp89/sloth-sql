namespace SlothSQL.Test

module DataGenerationTest =

    open NUnit.Framework
    open SlothSQL.DataGenerator.TestDataGenerator
    open SlothSQL.Parser.Syntax


    let testDataRow1 = {
            Table = "customer";
            KeysAndValues = [
                ("id", "123", false);
                ("name", "Jane Doe", true)
            ]
        }
    
    let testDataRow2 = {
            Table = "contract";
            KeysAndValues = [
                ("id", "987", false);
                ("due_date", "2021-08-15", true);
                ("status", "0", true);
                ("fk_customer", "123", false)
            ]
        }


    [<SetUp>]
    let Setup () =
        ()


    [<Test>]
    let TestInsertStatementGeneration () =
        let expInsertStatement = "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
        Assert.That(toInsertStatement testDataRow1, Is.EqualTo expInsertStatement)
        
        let expInsertStatement2 =
            "INSERT INTO contract (id, due_date, status, fk_customer)"
            + " VALUES (987, '2021-08-15', '0', 123);"
        Assert.That(toInsertStatement testDataRow2, Is.EqualTo expInsertStatement2)


    [<Test>]
    let TestWhereClauseColumnExtraction () =
        let testWhereClause =
            Filters [
                    EqualsColumnExpr
                        (QualifiedColumnExpr ("o", "fk_customer"),
                        QualifiedColumnExpr ("c", "id"));
                    EqualsIntExpr
                        (QualifiedColumnExpr ("c", "id"),
                        123)
            ]
        let cols = extractWhereColumns testWhereClause
        let expCols = ["c.id"; "o.fk_customer"]
        Assert.That(cols, Is.EquivalentTo expCols)


    [<Test>]
    let TestSelectClauseColumnExtraction () =
        let testSelectClause =
            Columns [
                QualifiedColumnExpr ("o", "fk_customer");
                QualifiedColumnExpr ("c", "id")
            ]
        let cols = extractSelectColumns testSelectClause
        let expCols = ["c.id"; "o.fk_customer"]
        Assert.That(cols, Is.EquivalentTo expCols)


    [<Test>]
    let TestTableNameExtraction () =
        let testFromClause =
            Tables [
                    TableAliasExpr ("customer", "c");
                    TableAliasExpr ("contract", "o")
            ]
        let namez = extractTableNames testFromClause
        let expTableNames = ["customer"; "contract"]
        Assert.That(namez, Is.EquivalentTo expTableNames)


    [<Test>]
    let TestDataGeneration () =
        let testQuery =
            "SELECT c.name, o.due_date FROM customer c, contract o WHERE o.fk_customer = c.id;"
        let testData: Result<string list, string> = generateTestData testQuery
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
            "INSERT INTO contract (due_date, fk_customer) VALUES ('2021-08-15', 123);"
        ]
        match testData with
        | Ok t -> Assert.That(t, Is.EquivalentTo expTestData)
        | Error e -> Assert.Fail(e)


(*
    [<Test>]
    let TestDataGeneration2 () =
        let testQuery =
            "SELECT c.name, o.due_date FROM customer c, contract o WHERE o.fk_customer = c.id;"
        //    + " AND c.id = 123 AND o.status = 'o';" // with an additional "status" column
        let testData: Result<string list, string> = generateTestData testQuery
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
        match testData with
        | Ok t -> Assert.That(t, Is.EquivalentTo expTestData)
        | Error e -> Assert.Fail(e)
*)
