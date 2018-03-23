# Todo

## Symbols

 Parser | Printer | Symbol
--------|---------|------------------------------
   [x]  |   [x]   | start_symbol
   [x]  |   [x]   | program
   [x]  |   [x]   | procedure_declaration_part
   [ ]  |   [ ]   | statement
   [ ]  |   [ ]   | empty_statement
   [ ]  |   [ ]   | assignment_statement
   [ ]  |   [ ]   | procedure_statement
   [ ]  |   [ ]   | actual_parameter_list
   [ ]  |   [ ]   | compound_statement
   [ ]  |   [ ]   | statement_sequence
   [ ]  |   [ ]   | if_statement
   [ ]  |   [ ]   | while_statement
   [ ]  |   [ ]   | for_statement
   [ ]  |   [ ]   | expression
   [ ]  |   [ ]   | relational_operator
   [ ]  |   [ ]   | simple_expression
   [ ]  |   [ ]   | adding_operator
   [ ]  |   [ ]   | term
   [ ]  |   [ ]   | multiplying_operator
   [ ]  |   [ ]   | factor
   [x]  |   [x]   | unsigned_constant
   [x]  |   [x]   | unsigned_number
   [ ]  |   [ ]   | variable_access
   [ ]  |   [ ]   | indexed_variable
   [x]  |   [x]   | procedure_declaration
   [x]  |   [x]   | formal_parameter_list
   [x]  |   [x]   | formal_parameter_section
   [x]  |   [x]   | identifier_list
   [x]  |   [x]   | variable_declaration_part
   [x]  |   [x]   | variable_declaration
   [x]  |   [x]   | type_denoter
   [x]  |   [x]   | type_identifier
   [x]  |   [x]   | array_type
   [x]  |   [x]   | subrange_type
   [x]  |   [x]   | constant
   [x]  |   [x]   | sign


## Tokens

 Printer | Token
---------|------------------------------
   [ ]   | start_symbol
   [ ]   | WHITESPACE
   [ ]   | COMMENT
   [x]   | LEFT_BRACE
   [x]   | RIGHT_BRACE
   [ ]   | lexical_token
   [x]   | LEFT_PARENTHESIS
   [x]   | RIGHT_PARENTHESIS
   [x]   | TIMES
   [x]   | PLUS
   [x]   | COMMA
   [x]   | MINUS
   [x]   | ELLIPSIS
   [x]   | DOT
   [x]   | DIVIDE_BY
   [x]   | ASSIGN
   [x]   | COLON
   [x]   | SEMICOLON
   [x]   | LESS_THAN_OR_EQUAL
   [x]   | NOT_EQUAL
   [x]   | LESS_THAN
   [x]   | EQUAL
   [x]   | GREATER_THAN_OR_EQUAL
   [x]   | GREATER_THAN
   [x]   | LEFT_BRACKET
   [x]   | RIGHT_BRACKET
   [ ]   | character_string
   [ ]   | SINGLE_QUOTE
   [ ]   | string_element
   [x]   | identifier
   [x]   | unsigned_integer
   [x]   | digit_sequence
   [x]   | unsigned_real
   [x]   | E
   [x]   | scale_factor
   [x]   | sign
   [x]   | AND
   [x]   | ARRAY
   [x]   | BEGIN
   [x]   | BOOLEAN
   [x]   | DIV
   [x]   | DO
   [x]   | DOWN_TO
   [x]   | ELSE
   [x]   | END
   [x]   | FOR
   [x]   | FUNCTION
   [x]   | IF
   [x]   | INTEGER
   [x]   | NOT
   [x]   | OF
   [x]   | OR
   [x]   | PROCEDURE
   [x]   | PROGRAM
   [x]   | REAL
   [x]   | THEN
   [x]   | TO
   [x]   | VAR
   [x]   | WHILE