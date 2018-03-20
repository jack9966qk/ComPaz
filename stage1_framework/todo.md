# Todo

## Symbols

 Parser | Printer | Symbol
--------|---------|------------------------------
   [x]  |   [x]   | start_symbol
   [x]  |   [x]   | program
   [x]  |   [ ]   | procedure_declaration_part
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
   [ ]  |   [ ]   | unsigned_constant
   [ ]  |   [ ]   | unsigned_number
   [ ]  |   [ ]   | variable_access
   [ ]  |   [ ]   | indexed_variable
   [x]  |   [ ]   | procedure_declaration
   [x]  |   [ ]   | formal_parameter_list
   [x]  |   [ ]   | formal_parameter_section
   [x]  |   [ ]   | identifier_list
   [x]  |   [x]   | variable_declaration_part
   [x]  |   [ ]   | variable_declaration
   [x]  |   [ ]   | type_denoter
   [x]  |   [ ]   | type_identifier
   [x]  |   [ ]   | array_type
   [x]  |   [ ]   | subrange_type
   [x]  |   [ ]   | constant
   [x]  |   [ ]   | sign


## Tokens

 Printer | Token
---------|------------------------------
   [ ]   | start_symbol
   [ ]   | WHITESPACE
   [ ]   | COMMENT
   [ ]   | LEFT_BRACE
   [ ]   | RIGHT_BRACE
   [ ]   | lexical_token
   [ ]   | LEFT_PARENTHESIS
   [ ]   | RIGHT_PARENTHESIS
   [ ]   | TIMES
   [ ]   | PLUS
   [ ]   | COMMA
   [ ]   | MINUS
   [ ]   | ELLIPSIS
   [x]   | DOT
   [ ]   | DIVIDE_BY
   [ ]   | ASSIGN
   [ ]   | COLON
   [x]   | SEMICOLON
   [ ]   | LESS_THAN_OR_EQUAL
   [ ]   | NOT_EQUAL
   [ ]   | LESS_THAN
   [ ]   | EQUAL
   [ ]   | GREATER_THAN_OR_EQUAL
   [ ]   | GREATER_THAN
   [ ]   | LEFT_BRACKET
   [ ]   | RIGHT_BRACKET
   [ ]   | character_string
   [ ]   | SINGLE_QUOTE
   [ ]   | string_element
   [ ]   | identifier
   [ ]   | unsigned_integer
   [ ]   | digit_sequence
   [ ]   | unsigned_real
   [ ]   | E
   [ ]   | scale_factor
   [ ]   | sign
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