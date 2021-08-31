namespace SlothSQL.Test

module SqlParserTest =

    open NUnit.Framework
    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax


    let testSimpleQuery01 =
        "SELECT c.name FROM customer;"
    
    let testQuery02 =
        "SELECT c.name, o.due_date FROM customer c, contract o;"
    
    let testQuery03 =
        "SELECT c.id, c.name, o.due_date FROM customer c, contract o;"
    

    [<SetUp>]
    let Setup () =
        ()


    [<Test>]
    let TestSimpleQueryParsing () =
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [QualifiedColumnExpr ("c", "name")],
                ["customer"]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse testSimpleQuery01
        match sqlSelectQuery with
        | Ok q -> Assert.That(q, Is.EqualTo exp)
        | Error e -> Assert.Fail(e)


    [<Test>]
    let Test2QualifTablesParsing () =
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                ["customer"; "contract"]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse testQuery02
        match sqlSelectQuery with
        | Ok q -> Assert.That(q, Is.EqualTo exp)
        | Error e -> Assert.Fail(e)


    [<Test>]
    let Test3QualifTablesParsing () =
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [
                    QualifiedColumnExpr ("c", "id");
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                ["customer"; "contract"]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse testQuery03
        match sqlSelectQuery with
        | Ok q -> Assert.That(q, Is.EqualTo exp)
        | Error e -> Assert.Fail(e)
