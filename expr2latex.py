# expr2latex.py

import re
import sys
import ply.lex as lex
import ply.yacc as yacc

# Token definitions
tokens = (
    'FLOAT_CONST', 'INT_CONST',
    'ID',
    'LPAREN', 'RPAREN',
    'LBRACKET', 'RBRACKET',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'LDIVIDE',
    'TRANSPOSE', 'CCTRANSPOSE',
    'EXPONENT', 'FACTORIAL', 'LT', 'LTE', 'GT', 'GTE', 'EQ', 'NEQ', 'NEG', 'COMMA',
    'SEMICOLON', 'COLON', 'AND', 'ANDAND', 'EQUALS', 'ELLIPSIS',
    'ABS',
)

# Operator tokens and literals
t_PLUS = r'\+'
t_MINUS = r'-'
t_NEG = r'~'
t_EQUALS = r'='
t_TIMES = r'\*|\.?\*'
t_DIVIDE = r'/|\.?/'
t_LDIVIDE = r'\\|\.?\\'
t_EXPONENT = r'\^|\.?\^'
t_TRANSPOSE = r"\.'"
t_CCTRANSPOSE = r"'"
t_FACTORIAL = r'!'
t_COLON = r':'
t_COMMA = r','
t_SEMICOLON = r';'
t_LT = r'<'
t_LTE = r'<='
t_GT = r'>'
t_GTE = r'>='
t_EQ = r'=='
t_NEQ = r'(~=|!=)'
t_AND = r'&'
t_ANDAND = r'&&'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_ELLIPSIS = r'\.\.\.'
t_ABS = r'\|'

# Ignored characters
t_ignore = " \t\n\r"

# Function and operator names mapping to LaTeX
function_names = {
    'sqrt': r'\sqrt',
    'nthroot': r'\sqrt',
    'log': r'\log',
    'ln': r'\ln',
    'log10': r'\log_{10}',
    'exp': r'e^{ {expr} }',
    'sin': r'\sin',
    'cos': r'\cos',
    'tan': r'\tan',
    'cot': r'\cot',
    'sec': r'\sec',
    'csc': r'\csc',
    'sinh': r'\sinh',
    'cosh': r'\cosh',
    'tanh': r'\tanh',
    'coth': r'\coth',
    'asin': r'\arcsin',
    'acos': r'\arccos',
    'atan': r'\arctan',
    'acot': r'\arccot',
    'asec': r'\arcsec',
    'acsc': r'\arccsc',
    'abs': r'\left| {expr} \right|',
    'floor': r'\left\lfloor {expr} \right\rfloor',
    'ceil': r'\left\lceil {expr} \right\rceil',
    'round': r'\operatorname{round}',
    'min': r'\min',
    'max': r'\max',
    'det': r'\det',
    'trace': r'\operatorname{tr}',
    'rank': r'\operatorname{rank}',
    'inv': r'^{-1}',
    'diff': r'\frac{\partial^{order} {expr}}{\partial {vars}}',
    'int': r'\int',
    'sum': r'\sum',
    'prod': r'\prod',
    'lim': r'\lim',
    'nabla': r'\nabla',
    'partial': r'\partial',
    'pi': r'\pi',
    'e': r'e',
    'i': r'i',
    'j': r'j',
    'NaN': r'\text{NaN}',
    'Inf': r'\infty',
    'inf': r'\infty',
    'gamma': r'\gamma',
    'delta': r'\delta',
    'theta': r'\theta',
    'lambda': r'\lambda',
    'mu': r'\mu',
    'sigma': r'\sigma',
    'phi': r'\phi',
    'omega': r'\omega',
    'epsilon': r'\epsilon',
    'mu0': r'\mu_{0}',
    'epsilon0': r'\epsilon_{0}',
    'factorial': r'!',
    'nchoosek': r'{n \choose k}',
    'div': r'\nabla \cdot',
    'curl': r'\nabla \times',
    'infty': r'\infty',
    'int_V': r'\int_{V}',
    'int_S': r'\int_{S}',
    'zeta': r'\zeta',
    'alpha': r'\alpha',
    # Add more Greek letters and functions as needed
}

# Regular expression for identifiers
identifier = r'[a-zA-Z_][0-9a-zA-Z_]*'

# Regular expressions for constants
decimal_constant = r'(0|[1-9][0-9]*)'
floating_constant = r'([0-9]*\.[0-9]+([eE][-+]?[0-9]+)?)|([0-9]+\.[0-9]*([eE][-+]?[0-9]+)?)'

@lex.TOKEN(floating_constant)
def t_FLOAT_CONST(t):
    return t

@lex.TOKEN(decimal_constant)
def t_INT_CONST(t):
    return t

@lex.TOKEN(identifier)
def t_ID(t):
    t.type = 'ID'
    return t

# Error handling for illegal characters
def t_error(t):
    sys.stderr.write(f"Illegal character '{t.value[0]}'\n")
    t.lexer.skip(1)
    raise Exception()

# Build the lexer
lexer = lex.lex()

# Precedence rules for parsing
precedence = (
    ('left', 'COMMA', 'SEMICOLON'),
    ('left', 'ANDAND'),
    ('left', 'AND'),
    ('nonassoc', 'EQ', 'NEQ', 'LT', 'LTE', 'GT', 'GTE'),
    ('left', 'COLON'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'LDIVIDE'),
    ('right', 'EXPONENT'),
    ('right', 'FACTORIAL'),
    ('right', 'UMINUS', 'NEG'),
    ('left', 'TRANSPOSE', 'CCTRANSPOSE'),
)

# Set the start symbol
start = 'statement'

# Parser production rules

def p_statement(t):
    '''statement : assignment
                 | equation
                 | expression'''
    t[0] = t[1]

def p_assignment(t):
    'assignment : ID EQUALS expression'
    t[0] = ['assign', t[1], t[3]]

def p_equation(t):
    'equation : expression EQUALS expression'
    t[0] = ['equals', t[1], t[3]]

def p_expression_binop(t):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression LDIVIDE expression
                  | expression EXPONENT expression
                  | expression COLON expression
                  | expression LT expression
                  | expression LTE expression
                  | expression GT expression
                  | expression GTE expression
                  | expression EQ expression
                  | expression NEQ expression
                  | expression AND expression
                  | expression ANDAND expression'''
    op = t[2]
    if op in ['.*', './', '.\\', '.^']:
        op = op[1]
    if op == '\\':
        t[0] = ['ldivide', t[1], t[3]]
    else:
        t[0] = [op, t[1], t[3]]

def p_expression_transpose(t):
    '''expression : expression TRANSPOSE
                  | expression CCTRANSPOSE'''
    t[0] = ['transpose', t[1]] if t[2] == ".'" else ['cctranspose', t[1]]

def p_expression_factorial(t):
    '''expression : expression FACTORIAL'''
    t[0] = ['factorial', t[1]]

def p_expression_uminus(t):
    '''expression : MINUS expression %prec UMINUS'''
    t[0] = ['uminus', t[2]]

def p_expression_negation(t):
    '''expression : NEG expression'''
    t[0] = ['negation', t[2]]

def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0] = ['group', t[2]]

def p_expression_abs(t):
    'expression : ABS expression ABS'
    t[0] = ['abs', t[2]]

def p_expression_function(t):
    '''expression : ID LPAREN arg_list RPAREN
                  | ID LPAREN RPAREN'''
    args = t[3] if len(t) == 5 else []
    t[0] = ['call', t[1], args]

def p_arg_list(t):
    '''arg_list : arg_list COMMA expression
                | expression'''
    if len(t) == 4:
        t[0] = t[1] + [t[3]]
    else:
        t[0] = [t[1]]

def p_expression_array(t):
    '''expression : LBRACKET arr_expression RBRACKET'''
    t[0] = ['array', t[2]]

def p_arr_expression(t):
    '''arr_expression : arr_expression SEMICOLON row_expression
                      | row_expression'''
    if len(t) == 4:
        t[0] = t[1] + [t[3]]
    else:
        t[0] = [t[1]]

def p_row_expression(t):
    '''row_expression : row_expression COMMA expression
                      | expression'''
    if len(t) == 4:
        if isinstance(t[1], list):
            t[0] = t[1] + [t[3]]
        else:
            t[0] = [t[1], t[3]]
    else:
        t[0] = [t[1]]

def p_expression_terminal(t):
    '''expression : FLOAT_CONST
                  | INT_CONST
                  | ID'''
    t[0] = t[1]

def p_expression_ellipsis(t):
    'expression : ELLIPSIS'
    t[0] = '...'

def p_error(p):
    if p:
        sys.stderr.write(f"Syntax error at '{p.value}'\n")
    else:
        sys.stderr.write("Syntax error at EOF\n")
    raise Exception()

# Build the parser
parser = yacc.yacc()

# Function to escape special LaTeX characters
def escape_latex(s):
    replacements = {
        '\\': r'\textbackslash ',
        '&': r'\&',
        '%': r'\%',
        '$': r'\$',
        '#': r'\#',
        '_': r'_',
        '{': r'\{',
        '}': r'\}',
        '~': r'\textasciitilde ',
        '^': r'\^{}',
    }
    return ''.join(replacements.get(c, c) for c in s)

# Constants for LaTeX rendering
LATEX_LPAREN = r'\left( '
LATEX_RPAREN = r' \right)'
LATEX_LBRACE = r'\left\{ '
LATEX_RBRACE = r' \right\}'
LATEX_LBRACKET = r'\left[ '
LATEX_RBRACKET = r' \right]'

# Convert s-expression to LaTeX string
def sexpr_to_latex(s):
    if isinstance(s, list):
        op = s[0]
        if op == 'assign' or op == 'equals':
            left = sexpr_to_latex(s[1])
            right = sexpr_to_latex(s[2])
            return left + ' = ' + right
        elif op in ('+', '-', '*', '/', '\\', 'ldivide', '^', '<', '>', '==', '!=', '~=', '&', '&&', '<=', '>=', 'COLON'):
            left = sexpr_to_latex(s[1])
            right = sexpr_to_latex(s[2])
            if needs_parentheses(s[1], op):
                left = LATEX_LPAREN + left + LATEX_RPAREN
            if needs_parentheses(s[2], op):
                right = LATEX_LPAREN + right + LATEX_RPAREN
            if op == '^':
                return left + '^{' + right + '}'
            elif op == '/':
                return r'\frac{' + left + '}{' + right + '}'
            elif op == 'ldivide':
                return r'\frac{' + right + '}{' + left + '}'
            elif op == '*':
                return left + r' \cdot ' + right
            elif op == 'COLON':
                return left + ' : ' + right
            else:
                op_latex = {
                    '+': '+',
                    '-': '-',
                    '=': '=',
                    '<': '<',
                    '>': '>',
                    '==': '=',
                    '!=': r'\ne ',
                    '~=': r'\approx ',
                    '<=': r'\leq ',
                    '>=': r'\geq ',
                    '&': r'\&',
                    '&&': r'\land',
                }.get(op, op)
                return left + ' ' + op_latex + ' ' + right
        elif op == 'group':
            return LATEX_LPAREN + sexpr_to_latex(s[1]) + LATEX_RPAREN
        elif op == 'call':
            func_name = s[1]
            args = s[2]
            if func_name in function_names:
                latex_func = function_names[func_name]
                if func_name == 'sqrt':
                    return r'\sqrt{' + sexpr_to_latex(args[0]) + '}'
                elif func_name == 'nthroot':
                    if len(args) == 2:
                        radicand = sexpr_to_latex(args[0])
                        index = sexpr_to_latex(args[1])
                        return r'\sqrt[' + index + ']{' + radicand + '}'
                    else:
                        return r'\sqrt{' + sexpr_to_latex(args[0]) + '}'
                elif func_name == 'exp':
                    expr = sexpr_to_latex(args[0])
                    return 'e^{' + expr + '}'
                elif func_name in ['abs', 'floor', 'ceil']:
                    return latex_func.replace('{expr}', ' ' + sexpr_to_latex(args[0]) + ' ')
                elif func_name in ['det', 'trace', 'rank', 'round', 'min', 'max']:
                    return latex_func + r'\left( ' + sexpr_to_latex(args[0]) + r' \right)'
                elif func_name == 'inv':
                    return sexpr_to_latex(args[0]) + '^{-1}'
                elif func_name == 'diff':
                    expr = sexpr_to_latex(args[0])
                    vars = [sexpr_to_latex(v) for v in args[1:]]
                    order = len(vars)
                    if order == 1:
                        return r'\frac{\partial ' + expr + '}{\partial ' + vars[0] + '}'
                    else:
                        return r'\frac{\partial^{' + str(order) + '} ' + expr + '}{' + ''.join([r'\partial ' + v for v in vars]) + '}'
                elif func_name == 'int':
                    if len(args) == 4:
                        integrand = sexpr_to_latex(args[0])
                        var = sexpr_to_latex(args[1])
                        lower = sexpr_to_latex(args[2])
                        upper = sexpr_to_latex(args[3])
                        return r'\int_{' + lower + '}^{' + upper + '} ' + integrand + r' \, d' + var
                    elif len(args) == 3:
                        integrand = sexpr_to_latex(args[0])
                        lower = sexpr_to_latex(args[1])
                        upper = sexpr_to_latex(args[2])
                        var = 'x'
                        return r'\int_{' + lower + '}^{' + upper + '} ' + integrand + r' \, d' + var
                    elif len(args) == 2:
                        integrand = sexpr_to_latex(args[0])
                        var = sexpr_to_latex(args[1])
                        return r'\int ' + integrand + r' \, d' + var
                    else:
                        integrand = sexpr_to_latex(args[0])
                        return r'\int ' + integrand + r' \, dx'
                elif func_name == 'sum':
                    if len(args) == 4:
                        expr = sexpr_to_latex(args[0])
                        var = sexpr_to_latex(args[1])
                        lower = sexpr_to_latex(args[2])
                        upper = sexpr_to_latex(args[3])
                        return r'\sum_{' + var + '=' + lower + '}^{' + upper + '} ' + expr
                    else:
                        expr = sexpr_to_latex(args[0])
                        return r'\sum ' + expr
                elif func_name == 'prod':
                    if len(args) == 4:
                        expr = sexpr_to_latex(args[0])
                        var = sexpr_to_latex(args[1])
                        lower = sexpr_to_latex(args[2])
                        upper = sexpr_to_latex(args[3])
                        return r'\prod_{' + var + '=' + lower + '}^{' + upper + '} ' + expr
                    else:
                        expr = sexpr_to_latex(args[0])
                        return r'\prod ' + expr
                elif func_name == 'lim':
                    if len(args) == 3:
                        var = sexpr_to_latex(args[0])
                        approach = sexpr_to_latex(args[1])
                        expr = sexpr_to_latex(args[2])
                        return r'\lim_{ ' + var + r' \to ' + approach + ' } ' + expr
                    else:
                        return 'Invalid limit expression'
                elif func_name == 'nabla':
                    return r'\nabla\left( ' + sexpr_to_latex(args[0]) + r' \right)'
                elif func_name == 'nchoosek':
                    return r'{' + sexpr_to_latex(args[0]) + r' \choose ' + sexpr_to_latex(args[1]) + '}'
                elif func_name == 'div':
                    return r'\nabla \cdot ' + r'\left( ' + sexpr_to_latex(args[0]) + r' \right)'
                elif func_name == 'curl':
                    return r'\nabla \times ' + r'\left( ' + sexpr_to_latex(args[0]) + r' \right)'
                elif func_name == 'int_V':
                    return r'\int_{V} ' + sexpr_to_latex(args[0]) + r' \, dV'
                elif func_name == 'int_S':
                    return r'\int_{S} ' + sexpr_to_latex(args[0]) + r' \, dS'
                elif func_name in [r'\sin', r'\cos', r'\tan', r'\cot', r'\sec', r'\csc',
                                   r'\arcsin', r'\arccos', r'\arctan', r'\arccot', r'\arccsc', r'\arcsec',
                                   r'\sinh', r'\cosh', r'\tanh', r'\coth', r'\log', r'\ln', r'\zeta']:
                    return latex_func + r'\left( ' + sexpr_to_latex(args[0]) + r' \right)'
                else:
                    return latex_func + r'\left( ' + ', '.join([sexpr_to_latex(arg) for arg in args]) + r' \right)'
            else:
                # Handle unknown functions
                return escape_latex(func_name) + r'\left( ' + ', '.join([sexpr_to_latex(arg) for arg in args]) + r' \right)'
        elif op == 'uminus':
            operand = sexpr_to_latex(s[1])
            return '-' + operand
        elif op == 'negation':
            return r'\neg ' + sexpr_to_latex(s[1])
        elif op == 'transpose':
            return sexpr_to_latex(s[1]) + '^{T}'
        elif op == 'cctranspose':
            return sexpr_to_latex(s[1]) + '^{H}'
        elif op == 'array':
            rows = s[1]
            latex_rows = []
            for row in rows:
                if isinstance(row, list):
                    latex_row = ' & '.join([sexpr_to_latex(elem) for elem in row])
                else:
                    latex_row = sexpr_to_latex(row)
                latex_rows.append(latex_row)
            array_content = r' \\ '.join(latex_rows)
            num_columns = max(len(row) if isinstance(row, list) else 1 for row in rows)
            return r'\left[ \begin{array}{' + 'c' * num_columns + '} ' + array_content + r' \end{array} \right]'
        elif op == '...':
            return r'\ldots'
        elif op == 'factorial':
            operand = sexpr_to_latex(s[1])
            return operand + '!'
        elif op == 'abs':
            expr = sexpr_to_latex(s[1])
            return r'\left| ' + expr + r' \right|'
        else:
            return ''
    else:
        # s is a string (identifier or constant)
        if s in function_names:
            return function_names[s]
        elif re.match(r'^[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?$', s):
            # Check for scientific notation
            match = re.match(r'([0-9]+\.[0-9]+)[eE]([-+]?[0-9]+)', s)
            if match:
                # Convert scientific notation to LaTeX
                base = match.group(1)
                exponent = match.group(2)
                return f'{base} \\times 10^{{{exponent}}}'
            return s  # Regular float number
        elif re.match(r'^[a-zA-Z_][0-9a-zA-Z_]*$', s):
            return escape_latex(s)
        else:
            return escape_latex(s)

# Operator precedence for eliminating parentheses
def get_precedence():
    return {
        '+': 10,
        '-': 10,
        '*': 20,
        '/': 20,
        'ldivide': 20,
        '^': 30,
        'uminus': 40,
        'negation': 40,
        'group': 50,
        # Add other operators as needed
    }

def needs_parentheses(child, parent_op):
    if not isinstance(child, list):
        return False
    child_op = child[0]
    precedence = get_precedence()
    child_prec = precedence.get(child_op, 0)
    parent_prec = precedence.get(parent_op, 0)
    if child_prec < parent_prec:
        return True
    if child_prec == parent_prec and parent_op == '^' and child_op == '^':
        return False  # Exponentiation is right-associative
    if child_prec == parent_prec and parent_op in ['+', '*', '-', '/']:
        return False  # Left-associative operators
    return False

# Eliminate unnecessary parentheses
def eliminate_parens(s, parent_op=None):
    if isinstance(s, list):
        op = s[0]
        operands = s[1:]
        new_operands = [eliminate_parens(child, op) for child in operands]
        if op == 'group':
            child = new_operands[0]
            if needs_parentheses(child, parent_op):
                return ['group', child]
            else:
                return child
        else:
            return [op] + new_operands
    else:
        return s

# Expression to LaTeX converter
def expr2latex(s):
    s = s.rstrip().strip(';')
    try:
        parsed = parser.parse(s, lexer=lexer)
        simplified = eliminate_parens(parsed)
        return sexpr_to_latex(simplified)
    except Exception as e:
        return "Invalid expression. Please check your input."

# Test runner
def runTests(verbose=False):
    tests = [
        # Basic Arithmetic Operations
        ("a + b", "a + b"),
        ("a - b", "a - b"),
        ("a * b", r'a \cdot b'),
        ("a / b", r'\frac{a}{b}'),
        ("a ^ b", r'a^{b}'),
        ("-a", "-a"),
        ("(a + b) * c", r'\left( a + b \right) \cdot c'),

        # Mathematical Functions
        ("sqrt(a)", r'\sqrt{a}'),
        ("nthroot(a, n)", r'\sqrt[n]{a}'),
        ("abs(a)", r'\left|  a  \right|'),
        ("log(a)", r'\log\left( a \right)'),
        ("ln(a)", r'\ln\left( a \right)'),
        ("exp(a)", r'e^{a}'),
        ("sin(a)", r'\sin\left( a \right)'),
        ("cos(a)", r'\cos\left( a \right)'),
        ("tan(a)", r'\tan\left( a \right)'),
        ("asin(a)", r'\arcsin\left( a \right)'),
        ("sinh(a)", r'\sinh\left( a \right)'),
        ("cosh(a)", r'\cosh\left( a \right)'),
        ("tanh(a)", r'\tanh\left( a \right)'),

        # Complex Numbers and Constants
        ("a + i*b", r'a + i \cdot b'),
        ("e^(i*pi)", r'e^{ i \cdot \pi }'),
        ("pi", r'\pi'),
        ("i^2", r'i^{2}'),
        ("N = N0 * exp(-lambda * t)", r'N = N0 \cdot e^{ -\lambda \cdot t }'),

        # Matrices and Vectors
        ("[a; b; c]", r'\left[ \begin{array}{c} a \\ b \\ c \end{array} \right]'),
        ("[a, b; c, d]", r'\left[ \begin{array}{cc} a & b \\ c & d \end{array} \right]'),
        ("det(A)", r'\det\left( A \right)'),
        ("trace(A)", r'\operatorname{tr}\left( A \right)'),
        ("inv(A)", r'A^{-1}'),

        # Calculus and Series
        ("diff(y, x)", r'\frac{\partial y}{\partial x}'),
        ("int(f(x), x)", r'\int f\left( x \right) \, dx'),
        ("int(f(x), a, b)", r'\int_{ a }^{ b } f\left( x \right) \, dx'),
        ("sum(k^2, k, 1, n)", r'\sum_{ k=1 }^{ n } k^{2}'),
        ("prod(k, k, 1, n)", r'\prod_{ k=1 }^{ n } k'),

        # Advanced Expressions
        ("x = (-b + sqrt(b^2 - 4*a*c)) / (2*a)", r'x = \frac{ -b + \sqrt{ b^{2} - 4 \cdot a \cdot c } }{ 2 \cdot a }'),

        # Euler-Lagrange Equation
        ("diff(diff(L, diff(x, t)), t) - diff(L, x) = 0", r'\frac{\partial }{\partial t} \left( \frac{\partial L}{\partial \left( \frac{\partial x}{\partial t} \right)} \right) - \frac{\partial L}{\partial x} = 0'),

        # Fourier Transform
        ("F(w) = int(f(t) * exp(-i * w * t), t, -inf, inf)", r'F\left( w \right) = \int_{ -\infty }^{ \infty } f\left( t \right) \cdot e^{ -i \cdot w \cdot t } \, dt'),

        # Taylor Series
        ("f(x) = sum( (diff(f, x, n) / factorial(n)) * (x - a)^n , n, 0, inf)", r'f\left( x \right) = \sum_{ n=0 }^{ \infty } \frac{ \frac{ \partial^{ n } f }{ \partial x^{ n } } }{ n! } \cdot \left( x - a \right)^{ n }'),

        # Adjusted Maxwell's Equations
        ("div(E) = rho / epsilon0", r'\nabla \cdot \left( E \right) = \frac{ \rho }{ \epsilon_{0} }'),
        ("div(B) = 0", r'\nabla \cdot \left( B \right) = 0'),
        ("curl(E) = -diff(B, t)", r'\nabla \times \left( E \right) = -\frac{\partial B}{\partial t}'),
        ("curl(B) = mu0 * J + mu0 * epsilon0 * diff(E, t)", r'\nabla \times \left( B \right) = \mu_{0} \cdot J + \mu_{0} \cdot \epsilon_{0} \cdot \frac{\partial E}{\partial t}'),

        # Laplace Transform
        ("L(f(t)) = int(f(t) * exp(-s * t), t, 0, inf)", r'L\left( f\left( t \right) \right) = \int_{ 0 }^{ \infty } f\left( t \right) \cdot e^{ -s \cdot t } \, dt'),

        # Probability Density Function
        ("P(X = x) = (1 / (sigma * sqrt(2 * pi))) * exp( - (x - mu)^2 / (2 * sigma^2) )", r'P\left( X = x \right) = \frac{ 1 }{ \sigma \cdot \sqrt{ 2 \cdot \pi } } \cdot e^{ -\frac{ \left( x - \mu \right)^{2} }{ 2 \cdot \sigma^{2} } }'),

        # Binomial Theorem
        ("(a + b)^n = sum( nchoosek(n, k) * a^(n - k) * b^k , k, 0, n)", r'\left( a + b \right)^{ n } = \sum_{ k=0 }^{ n } { n \choose k } \cdot a^{ n - k } \cdot b^{ k }'),

        # Complex Exponential
        ("e^(i * theta) = cos(theta) + i * sin(theta)", r'e^{ i \cdot \theta } = \cos\left( \theta \right) + i \cdot \sin\left( \theta \right)'),

        # Cauchy-Schwarz Inequality
        ("( sum(a_i * b_i, i, 1, n) )^2 <= sum( a_i^2 , i, 1, n ) * sum( b_i^2 , i, 1, n )", r'\left( \sum_{ i=1 }^{ n } a_i \cdot b_i \right)^{2} \leq \left( \sum_{ i=1 }^{ n } a_i^{2} \right) \cdot \left( \sum_{ i=1 }^{ n } b_i^{2} \right)'),

        # Partial Differential Equation
        ("diff(u, x, x) + diff(u, y, y) = 0", r'\frac{ \partial^{2} u }{ \partial x^{2} } + \frac{ \partial^{2} u }{ \partial y^{2} } = 0'),

        # Limit Definition
        ("lim( x, 0, sin(x)/x ) = 1", r'\lim_{ x \to 0 } \frac{ \sin\left( x \right) }{ x } = 1'),

        # Gradient Operator
        ("nabla(f) = [ diff(f, x), diff(f, y), diff(f, z) ]", r'\nabla\left( f \right) = \left[ \frac{ \partial f }{ \partial x } , \frac{ \partial f }{ \partial y } , \frac{ \partial f }{ \partial z } \right]'),

        # Divergence Theorem
        ("int_V( div(F) ) = int_S( F * n )", r'\int_{ V } \nabla \cdot \left( F \right) \, dV = \int_{ S } \left( F \cdot n \right) \, dS'),

        # Laplacian in Spherical Coordinates
        ("nabla^2(f) = (1 / r^2) * diff( r^2 * diff(f, r), r ) + ...", r'\nabla^{2}\left( f \right) = \frac{ 1 }{ r^{2} } \cdot \frac{ \partial }{ \partial r } \left( r^{2} \cdot \frac{ \partial f }{ \partial r } \right) + \ldots'),

        # Hyperbolic Functions
        ("cosh(x)", r'\cosh\left( x \right)'),
        ("tanh(x)", r'\tanh\left( x \right)'),

        # Logarithms and Exponentials
        ("log10(x)", r'\log_{10}\left( x \right)'),
        ("exp( i * pi ) + 1 = 0", r'e^{ i \cdot \pi } + 1 = 0'),

        # Factorials
        ("n!", r'n!'),
        ("factorial(n)", r'n!'),

        # Summations and Products
        ("sum( 1 / k^2 , k, 1, inf )", r'\sum_{ k=1 }^{ \infty } \frac{ 1 }{ k^{2} }'),

        # Limits
        ("lim( h, 0, ( f( x + h ) - f( x ) ) / h )", r'\lim_{ h \to 0 } \frac{ f\left( x + h \right) - f\left( x \right) }{ h }'),

        # Integrals
        ("int( sin(x)^2 , x, 0, pi )", r'\int_{ 0 }^{ \pi } \sin^{2}\left( x \right) \, dx'),

        # Derivatives
        ("diff( f(x) * g(x) , x )", r'\frac{ \partial }{ \partial x } \left( f\left( x \right) \cdot g\left( x \right) \right)'),

        # Complex Numbers
        ("| z | = sqrt( x^2 + y^2 )", r'\left| z \right| = \sqrt{ x^{2} + y^{2} }'),

        # Trigonometric Identities
        ("sin^2( x ) + cos^2( x ) = 1", r'\sin^{2}\left( x \right) + \cos^{2}\left( x \right) = 1'),

        # Special Functions
        ("gamma( n ) = ( n - 1 )!", r'\gamma\left( n \right) = \left( n - 1 \right)!'),

        # Zeta Function
        ("zeta( s ) = sum( 1 / n^s , n, 1, inf )", r'\zeta\left( s \right) = \sum_{ n=1 }^{ \infty } \frac{ 1 }{ n^{ s } }'),
    ]

    all_passed = True
    total_tests = len(tests)
    passed_tests = 0
    failed_tests = []
    for idx, (test_input, expected_output) in enumerate(tests, 1):
        actual_output = expr2latex(test_input)
        # Normalize spaces and underscores for comparison
        expected_output_norm = re.sub(r'\s+', ' ', expected_output.strip())
        actual_output_norm = re.sub(r'\s+', ' ', actual_output.strip())
        expected_output_norm = expected_output_norm.replace(r'\_', '_')
        actual_output_norm = actual_output_norm.replace(r'\_', '_')
        if actual_output_norm == expected_output_norm:
            passed_tests += 1
            if verbose:
                print(f"Test {idx}: PASS")
        else:
            all_passed = False
            failed_tests.append(idx)
            if verbose:
                print(f"Test {idx}: FAIL")
                print(f"Input: {test_input}")
                print(f"Expected Output: {expected_output}")
                print(f"Actual Output:   {actual_output}\n")
    print(f"\nSummary: Passed {passed_tests} out of {total_tests} tests.")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        runTests(verbose=True)
    else:
        s = ' '.join(sys.argv[1:])
        print(expr2latex(s))