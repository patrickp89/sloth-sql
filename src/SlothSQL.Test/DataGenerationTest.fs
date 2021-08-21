namespace SlothSQL.Test

module DataGenerationTest =

    open NUnit.Framework
    open SlothSQL.DataGenerator.TestDataGenerator
    open TSQL.Tokens


    let testQuery01 =
        "SELECT c.name, o.due_date\nFROM customer c, contract o\nWHERE
        o.fk_customer = c.id AND c.id = 123 AND o.status = 'o';"
        

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let TestQueryParsing () =
        let sqlTokens: TSQLToken list = parseQuery testQuery01
        let expTokenCount = 8 + 6 + 21
        Assert.That(sqlTokens.Length, Is.EqualTo expTokenCount)
        Assert.That(sqlTokens.Item(0).Type, Is.EqualTo TSQLTokenType.Keyword)
        Assert.That(sqlTokens.Item(0).Text, Is.EqualTo "SELECT")
        Assert.That(sqlTokens.Item(1).Type, Is.EqualTo TSQLTokenType.Identifier)
        Assert.That(sqlTokens.Item(1).Text, Is.EqualTo "c")


    [<Test>]
    let TestStatemtClausesExtraction () =
        let sqlTokens = parseQuery testQuery01
        let clauses: SqlSelectStatement = extractSqlClauses sqlTokens
        Assert.That(clauses.SelectClause.IsSome, Is.EqualTo true)
        Assert.That(clauses.SelectClause.Value.Length, Is.EqualTo 8)
        Assert.That(clauses.FromClause.IsSome, Is.EqualTo true)
        Assert.That(clauses.FromClause.Value.Length, Is.EqualTo 6)
        Assert.That(clauses.WhereClause.IsSome, Is.EqualTo true)
        Assert.That(clauses.WhereClause.Value.Length, Is.EqualTo 21)

    [<Test>]
    let TestDataGeneration () =
        let testData: string list = generateTestData testQuery01
        let expTestData = [
            "INSERT INTO customer (id, name) VALUES (id, 'Jane Doe');"
            "INSERT INTO contract (id, due_date, status, fk_customer) VALUES (987, '2021-08-15', '0', 123);"
        ]
        Assert.That(testData, Is.EquivalentTo expTestData)
