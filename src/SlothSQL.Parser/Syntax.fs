namespace SlothSQL.Parser

module Syntax =

    type ColumnExpr =
    | NonQualifiedColumnExpr of string // e.g. "age"
    | QualifiedColumnExpr of string * string // e.g. "customer.age"

    type SelectClause =
    | Columns of ColumnExpr list
    | Wildcard

    type FromClause = string list // TODO: use a type TableExpr instead of string!

    type FilterExpr =
    | EqualsColumnExpr of ColumnExpr * ColumnExpr // e.g. "WHERE a.xyz = b.qwe"
    | EqualsIntExpr of ColumnExpr * int // e.g. "WHERE a.xyz = 123"

    type WhereClause =
    | Filters of FilterExpr list

    type SqlSelectQuery =
    | SelectFrom of SelectClause * FromClause
    | SelectFromWhere of SelectClause * FromClause * WhereClause
