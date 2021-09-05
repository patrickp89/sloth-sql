namespace SlothSQL.Parser

module Syntax =

    type ColumnExpr =
    | NonQualifiedColumnExpr of string // e.g. "age"
    | QualifiedColumnExpr of string * string // e.g. "customer.age"


    type SelectClause =
    | Columns of ColumnExpr list
    | Wildcard


    type TableExpr =
    | NonAliasTableExpr of string // e.g. "FROM Account"
    | TableAliasExpr of string * string // e.g. "FROM Account a"


    type FromClause =
    | Tables of TableExpr list


    type FilterExpr =
    | EqualsColumnExpr of ColumnExpr * ColumnExpr // e.g. "WHERE a.xyz = b.qwe"
    | EqualsIntExpr of ColumnExpr * int // e.g. "WHERE a.xyz = 123"


    type WhereClause =
    | Filters of FilterExpr list


    type SqlSelectQuery =
    | SelectFrom of SelectClause * FromClause
    | SelectFromWhere of SelectClause * FromClause * WhereClause
