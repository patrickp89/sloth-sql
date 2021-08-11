module SlothSQL.Client.Main

open Elmish
open Bolero
open Bolero.Html
open SlothSQL.DataGenerator.TestDataGenerator

type Model = {
        SqlQuery: string
        TestDataStatements: string list
    }

let initModel = {
        // default SQL query:
        SqlQuery = "SELECT c.name, o.due_date\nFROM customer c, contract o\nWHERE c.id = 123 AND o.status = 'o';"
        TestDataStatements = []
    }

type Message =
    | SetSqlQuery of string
    | GenerateTestData


let testDataFor (model: Model) =
    let q = model.SqlQuery
    printfn "SQL query is: %s" q
    printfn "Parsing query..."
    let parseResult = generateTestData q
    printfn "Parse result is: %s" parseResult
    [parseResult]


let update message model =
    match message with
    | SetSqlQuery(q) -> { model with SqlQuery = q }
    | GenerateTestData -> { model with TestDataStatements = testDataFor model }


let view model dispatch =
    let defaultSqlQuery = "bla"
    let testDataStatements = model.TestDataStatements
    div [] [
        h1 [] [text "SlothSQL"]
        p  [] [text "Comfortably generate test data for your SQL queries."]

        textarea [
            bind.input.string model.SqlQuery (dispatch << SetSqlQuery)
        ] [text defaultSqlQuery]
        br []
        button [on.click (fun _ -> dispatch GenerateTestData)] [text "Generate test data!"]

        br []
        textarea [] [
            forEach testDataStatements <| fun testStmnt ->
                        (text (testStmnt + "\n"))
        ]   
    ]
    

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
