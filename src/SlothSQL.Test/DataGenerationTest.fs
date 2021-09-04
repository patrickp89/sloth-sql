namespace SlothSQL.Test

module DataGenerationTest =

    open NUnit.Framework
    open SlothSQL.DataGenerator.TestDataGenerator


    let testDataRow1 = {
            Table = "customer";
            KeysAndValues = [("id", "123", false); ("name", "Jane Doe", true)]
        }
    
    let testDataRow2 = {
            Table = "contract";
            KeysAndValues = [
                ("id", "987", false); ("due_date", "2021-08-15", true);
                ("status", "0", true); ("fk_customer", "123", false)] 
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
    let TestInsertStatementsGeneration () =
        let testData = buildInsertStatementList [testDataRow1; testDataRow2]
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
        Assert.That(testData, Is.EquivalentTo expTestData)


    [<Test>]
    let TestDataGeneration () =
        let testQuery02 =
            "SELECT c.name, o.due_date FROM customer c, contract o;"
        let testData: Result<string list, string> = generateTestData testQuery02
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
        match testData with
        | Ok t -> Assert.That(t, Is.EquivalentTo expTestData)
        | Error e -> Assert.Fail(e)

(*
    [<Test>]
    let TestDataGeneration2 () =
        let testQuery =
            "SELECT c.name, o.due_date FROM customer c, contract o "
            + "WHERE o.fk_customer = c.id AND c.id = 123 AND o.status = 'o';"
        let testData: Result<string list, string> = generateTestData testQuery01
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (123, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
        match testData with
        | Ok t -> Assert.That(t, Is.EquivalentTo expTestData)
        | Error e -> Assert.Fail(e)
*)
