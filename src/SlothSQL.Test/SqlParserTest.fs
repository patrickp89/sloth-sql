namespace SlothSQL.Test

module SqlParserTest =

    open NUnit.Framework
    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax

    let testSimpleQuery01 =
        "SELECT c.name FROM customer"
        
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let TestSimpleQueryParsing () =
        let exp: SqlSelectQuery =
            SelectFrom (
                QualifiedColumnExpr ("c", "name"),
                ("customer")
            )
        let sqlSelectQuery: Result<SqlSelectQuery,string> = parse testSimpleQuery01
        match sqlSelectQuery with
        | Ok q -> Assert.That(q, Is.EqualTo exp)
        | Error e -> Assert.Fail(e)
