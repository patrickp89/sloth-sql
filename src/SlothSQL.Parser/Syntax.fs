namespace SlothSQL.Parser

module Syntax =

    type ColumnExpr =
    | NonQualifiedColumnExpr of string // e.g. "age"
    | QualifiedColumnExpr of string * string // e.g. "customer.age"

    type SelectClause = ColumnExpr // TODO: ColumnExpr list

    type FromClause = string

    type WhereClause = string

    type SqlSelectQuery =
    | SelectFrom of SelectClause * FromClause
    | SelectFromWhere of SelectClause * FromClause * WhereClause
