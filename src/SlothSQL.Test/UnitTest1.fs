module SlothSQL.Test

open NUnit.Framework
open SlothSQL.DataGenerator.TestDataGenerator
open TSQL.Tokens

[<SetUp>]
let Setup () =
    ()

[<Test>]
let TestQueryParsing () =
    let q = "SELECT c.name, o.due_date\nFROM customer c, contract o\nWHERE c.id = 123 AND o.status = 'o';"
    let parseResult = parseQuery q
    Assert.That(parseResult.Count, Is.GreaterThan 0)
    Assert.That(parseResult.Item(0).Type, Is.EqualTo TSQLTokenType.Keyword)
    Assert.That(parseResult.Item(0).Text, Is.EqualTo "SELECT")
    Assert.That(parseResult.Item(1).Type, Is.EqualTo TSQLTokenType.Identifier)
    Assert.That(parseResult.Item(1).Text, Is.EqualTo "c")


[<Test>]
let TestDataGeneration () =
    let q = "SELECT c.name, o.due_date\nFROM customer c, contract o\nWHERE c.id = 123 AND o.status = 'o';"
    let testData = generateTestData q
    Assert.That(testData, Is.EqualTo "INSERT INTO bla")
