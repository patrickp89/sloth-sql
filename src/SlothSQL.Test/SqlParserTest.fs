namespace SlothSQL.Test

module SqlParserTest =

    open NUnit.Framework
    open SlothSQL.Parser.SqlParser
    open SlothSQL.Parser.Syntax


    let assertParsedQueryOk (sqlSelectQuery: Result<SqlSelectQuery, string>) (exp: SqlSelectQuery) =
        match sqlSelectQuery with
        | Ok q -> Assert.That(q, Is.EqualTo exp)
        | Error e -> Assert.Fail(e)


    [<SetUp>]
    let Setup () =
        ()


    [<Test>]
    let TestSimpleQueryParsing () =
        let simpleTestQuery01 =
            "SELECT name FROM customer;"
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [NonQualifiedColumnExpr "name"],
                Tables [NonAliasTableExpr "customer"]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse simpleTestQuery01
        assertParsedQueryOk sqlSelectQuery exp


    [<Test>]
    let TestQualifiedColumnNamesParsing () =
        let qualifiedColumnNamesTestQuery01 =
            "SELECT c.name, o.due_date FROM customer c, contract o;"
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                Tables [
                    TableAliasExpr ("customer", "c");
                    TableAliasExpr ("contract", "o")
                ]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse qualifiedColumnNamesTestQuery01
        assertParsedQueryOk sqlSelectQuery exp


    [<Test>]
    let TestMultipleQualifColumnNamesParsing () =
        let qualifiedColumnNamesTestQuery02 =
            "SELECT c.id, c.name, o.due_date FROM customer c, contract o;"
        let exp: SqlSelectQuery =
            SelectFrom (
                Columns [
                    QualifiedColumnExpr ("c", "id");
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                Tables [
                    TableAliasExpr ("customer", "c");
                    TableAliasExpr ("contract", "o")
                ]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse qualifiedColumnNamesTestQuery02
        assertParsedQueryOk sqlSelectQuery exp


    [<Test>]
    let TestSimpleWhereClauseParsing () =
        let whereClauseTestQuery01 =
            "SELECT c.id, c.name, o.due_date FROM customer c, contract o WHERE o.fk_customer = c.id;"
        let exp: SqlSelectQuery =
            SelectFromWhere (
                Columns [
                    QualifiedColumnExpr ("c", "id");
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                Tables [
                    TableAliasExpr ("customer", "c");
                    TableAliasExpr ("contract", "o")
                ],
                Filters [
                    EqualsColumnExpr
                        (QualifiedColumnExpr ("o", "fk_customer"),
                        QualifiedColumnExpr ("c", "id"))
                ]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse whereClauseTestQuery01
        assertParsedQueryOk sqlSelectQuery exp


    [<Test>]
    let TesteWhereClauseWithConstantParsing () =
        let whereClauseTestQuery01 =
            "SELECT c.name, o.due_date FROM customer c, contract o "
            + "WHERE o.fk_customer = c.id AND c.id = 123;"
        let exp: SqlSelectQuery =
            SelectFromWhere (
                Columns [
                    QualifiedColumnExpr ("c", "name");
                    QualifiedColumnExpr ("o", "due_date")
                ],
                Tables [
                    TableAliasExpr ("customer", "c");
                    TableAliasExpr ("contract", "o")
                ],
                Filters [
                    EqualsColumnExpr
                        (QualifiedColumnExpr ("o", "fk_customer"),
                        QualifiedColumnExpr ("c", "id"));
                    EqualsIntExpr
                        (QualifiedColumnExpr ("c", "id"),
                        123)
                ]
            )
        let sqlSelectQuery: Result<SqlSelectQuery, string> = parse whereClauseTestQuery01
        assertParsedQueryOk sqlSelectQuery exp
