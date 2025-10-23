/**
 * Tree-sitter grammar for the Shady language
 */

const PREC = {
  OR: 1,
  AND: 2,
  COMPARISON: 3,
  ADD: 4,
  MULTIPLY: 5,
  CUSTOM_INFIX: 6,
  POWER: 7,
  PREFIX: 8,
  CALL: 9,
};

module.exports = grammar({
  name: 'shady',

  extras: $ => [
    /\s/,
  ],

  word: $ => $.token,

  conflicts: $ => [
    // Resolve ambiguity between unquoted_str_arg and fn_name
    [$.unquoted_str_arg, $.fn_name],
  ],

  rules: {
    program: $ => repeat(choice(
      seq($.fn_definition, ';'),
      $.comment,
    )),

    // Comments
    comment: $ => choice(
      $.single_line_comment,
      $.multiline_comment,
    ),

    single_line_comment: $ => token(seq(
      '#',
      /.*/
    )),

    multiline_comment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/',
    )),

    // Tokens and identifiers
    token: $ => /[a-zA-Z_][-a-zA-Z0-9_]*/,

    fn_name: $ => $.token,

    variable: $ => seq('$', $.token),

    // Types
    typ: $ => choice(
      $.type_int,
      $.type_str,
      $.type_bool,
      $.type_proc,
      $.type_list,
      $.type_fn,
    ),

    type_int: $ => 'int',
    type_str: $ => 'str',
    type_bool: $ => 'bool',
    type_proc: $ => 'proc',
    type_list: $ => seq('[', field('element_type', $.typ), ']'),
    type_fn: $ => seq(
      'fn',
      '(',
      optional(seq(
        field('param_type', $.typ),
        repeat(seq(',', field('param_type', $.typ))),
      )),
      ')',
      '->',
      field('return_type', $.typ),
    ),

    // Parameters
    parameter: $ => seq(
      field('name', $.variable),
      optional(seq(':', field('type', $.typ))),
      optional(seq('(', field('spec', $.param_spec), ')')),
    ),

    param_spec: $ => seq(
      field('default_value', $.value),
      repeat($.param_option),
    ),

    param_option: $ => seq(
      ',',
      field('option_name', $.token),
      optional(seq('=', field('option_value', $.value))),
    ),

    // Function definitions
    fn_definition: $ => seq(
      optional(field('public', 'public')),
      choice(
        // Non-infix function
        seq(
          field('name', $.fn_name),
          field('parameters', repeat($.parameter)),
        ),
        // Infix function (requires exactly 2 parameters)
        seq(
          field('infix', 'infix'),
          field('name', $.fn_name),
          field('parameters', seq($.parameter, $.parameter)),
        ),
      ),
      optional(seq('->', field('return_type', $.typ))),
      '=',
      field('body', $.expr),
    ),

    // Values
    value: $ => choice(
      $.int,
      $.str,
      $.bool,
    ),

    int: $ => /[0-9]+/,

    str: $ => seq(
      '"',
      repeat(choice(
        $.string_content,
        $.escape_sequence,
        $.interpolation,
      )),
      '"',
    ),

    string_content: $ => token.immediate(prec(1, /[^"\\{]+/)),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /["\\/bfnrt{}]/,
        seq('u', /[0-9a-fA-F]{4}/),
      ),
    )),

    interpolation: $ => seq(
      '{',
      field('expr', $.expr),
      '}',
    ),

    bool: $ => choice('true', 'false'),

    // Lists
    list: $ => seq(
      '[',
      optional(seq(
        repeat(seq($.expr, ';')),
        optional($.expr),
      )),
      ']',
    ),

    // Expressions
    expr: $ => choice(
      $.if_expr,
      $.binary_expr,
      $.prefix_expr,
      $.primary_expr,
    ),

    primary_expr: $ => choice(
      $.value,
      $.fn_call,
      $.variable,
      $.list,
      $.block_expr,
      $.lambda_expr,
      seq('(', $.expr, ')'),
    ),

    // Block expressions: { expr; expr; expr } or { expr; expr; expr; }
    block_expr: $ => seq(
      '{',
      optional(seq(
        repeat(seq(field('expr', $.expr), ';')),
        field('last_expr', $.expr),
        optional(';'),  // Allow trailing semicolon
      )),
      '}',
    ),

    // If expressions
    if_expr: $ => seq(
      'if',
      '(',
      field('condition', $.expr),
      ')',
      field('when_true', $.expr),
      optional(';'),
      'else',
      field('when_false', $.expr),
    ),

    // Function calls
    fn_call: $ => prec(PREC.CALL, seq(
      field('name', $.fn_name),
      repeat(field('argument', $.fn_arg)),
    )),

    fn_arg: $ => choice(
      $.value,
      $.variable,
      $.list,
      $.unquoted_str_arg,
      seq('(', $.expr, ')'),
    ),

    unquoted_str_arg: $ => /([a-zA-Z][a-zA-Z0-9\-\/.~]*|[.~\/\-][a-zA-Z0-9\-\/.~]+)/,

    // Prefix operators
    prefix_expr: $ => prec(PREC.PREFIX, seq(
      field('operator', choice('!', '-')),
      field('operand', $.expr),
    )),

    // Binary expressions with precedence
    binary_expr: $ => choice(
      // Logical OR
      prec.left(PREC.OR, seq(
        field('left', $.expr),
        field('operator', '||'),
        field('right', $.expr),
      )),

      // Logical AND
      prec.left(PREC.AND, seq(
        field('left', $.expr),
        field('operator', '&&'),
        field('right', $.expr),
      )),

      // Comparison operators
      prec.left(PREC.COMPARISON, seq(
        field('left', $.expr),
        field('operator', choice('==', '!=', '<', '>', '<=', '>=')),
        field('right', $.expr),
      )),

      // Addition and subtraction
      prec.left(PREC.ADD, seq(
        field('left', $.expr),
        field('operator', choice('+', '-')),
        field('right', $.expr),
      )),

      // Multiplication and division
      prec.left(PREC.MULTIPLY, seq(
        field('left', $.expr),
        field('operator', choice('*', '/', '%')),
        field('right', $.expr),
      )),

      // Custom infix operators (backtick syntax)
      prec.left(PREC.CUSTOM_INFIX, seq(
        field('left', $.expr),
        field('operator', $.custom_infix_op),
        field('right', $.expr),
      )),

      // Power (right-associative)
      prec.right(PREC.POWER, seq(
        field('left', $.expr),
        field('operator', '^'),
        field('right', $.expr),
      )),
    ),

    custom_infix_op: $ => seq('`', $.token, '`'),

    // Lambda expressions: lambda $x $y -> body or lambda $x: int $y: str -> body
    lambda_expr: $ => seq(
      'lambda',
      repeat1(field('parameter', $.parameter)),
      '->',
      optional(seq(field('return_type', $.typ), '=')),
      field('body', $.expr),
    ),
  },
});
