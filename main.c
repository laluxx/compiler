#include <ctype.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// [x] DONE Automatically return 0 from main() if main() -> i32
// and no explicit return is found

// [ ] TODO don't generate the .ssa file when we use the -s flag
// we could also plot the scope in real time as we parse with this kind of system

// [ ] TODO a single function where we keep all types like u8 f32..

// [ ] TODO Step trough parsing and ir generation in Emacs
// since we have a point we could use the current value
// of the point in emacs and check for errors ? like flycheck ?

#define MAX_IDENT_LEN 64
#define MAX_TOKEN_LEN 256
#define INITIAL_CAPACITY 16

typedef enum {
    TOKEN_EOF,
    TOKEN_IDENT,
    TOKEN_FN,
    TOKEN_ARROW,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_SEMICOLON,
    TOKEN_RETURN,
    TOKEN_NUMBER,
    TOKEN_I32,
    TOKEN_COMMA,
    TOKEN_DEFER
} TokenType;

typedef enum {
    STMT_RETURN,
    STMT_EXPR,
    STMT_FUNCTION_CALL,
    STMT_DEFER
} StatementType;

typedef struct Token {
    TokenType type;
    char value[MAX_TOKEN_LEN];
    size_t line;
    size_t column;
} Token;

typedef struct Cursor {
    size_t point;
    char *buffer;
    size_t buffer_size;
    size_t line;
    size_t column;
} Cursor;

typedef struct Statement Statement;
typedef struct Scope Scope;
typedef struct Function Function;

struct Statement {
    StatementType type;
    union {
        char *return_value;
        char *function_call;
        Statement *defer_stmt;
    };
    Statement *next;
    Scope *parent_scope;
};

struct Scope {
    int level;
    Scope *parent;
    Scope **children;
    size_t num_children;
    size_t children_capacity;
    Statement *statements;
    Statement *defer_stmts;
    Function *owning_function;
};

struct Function {
    char name[MAX_IDENT_LEN];
    char return_type[MAX_IDENT_LEN];
    int is_main;
    Scope *body_scope;
};

typedef struct {
    Function *items;
    size_t capacity;
    size_t length;
} FunctionList;

typedef struct Compiler {
    FunctionList functions;
    FILE *output;
    Cursor cursor;
    Token current_token;
    Token previous_token;
    Scope *current_scope;
    int scope_level;
    char *source_file;
    int debug_mode;
    size_t debug_point;
} Compiler;

// Function declarations
Compiler *create_compiler(const char *output_filename);
void destroy_compiler(Compiler *c);
void init_cursor(Cursor *cursor, const char *filename);
void advance_cursor(Compiler *c);
char peek_char(Compiler *c);
void next_token(Compiler *c);
void error(const Compiler *c, const char *message);
void expect(Compiler *c, TokenType type);
void parse_function(Compiler *c);
void parse_function_body(Compiler *c);
void generate_qbe(Compiler *c);
Scope *create_scope(Scope *parent, int level, Function *owning_function);
void destroy_scope(Scope *scope);
void add_statement_to_scope(Scope *scope, Statement *stmt);
void add_child_scope(Scope *parent, Scope *child);
void process_defer_statements(FILE *output, Scope *scope);
void generate_scope_qbe(FILE *output, Scope *scope, const char *function_name,
                        int *has_return);

char *get_output_name(const char *input_file, const char *ext);

// Utility functions
static void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    return ptr;
}

static char *safe_strdup(const char *str) {
    char *dup = strdup(str);
    if (!dup) {
        fprintf(stderr, "String duplication failed\n");
        exit(1);
    }
    return dup;
}

char *get_output_name(const char *input_file, const char *ext) {
    char *base = safe_strdup(input_file);
    char *filename = basename(base);
    char *dot = strrchr(filename, '.');
    if (dot)
        *dot = '\0';

    size_t len = strlen(filename) + strlen(ext) + 2;
    char *output = safe_malloc(len);
    snprintf(output, len, "%s.%s", filename, ext);

    free(base);
    return output;
}

void init_cursor(Cursor *cursor, const char *filename) {
    cursor->point = 0;
    cursor->line = 1;
    cursor->column = 0;

    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Cannot open file: %s\n", filename);
        exit(1);
    }

    fseek(file, 0, SEEK_END);
    cursor->buffer_size = (size_t)ftell(file);
    fseek(file, 0, SEEK_SET);

    cursor->buffer = safe_malloc(cursor->buffer_size + 1);
    size_t read_size = fread(cursor->buffer, 1, cursor->buffer_size, file);
    cursor->buffer[read_size] = '\0';
    cursor->buffer_size = read_size;

    fclose(file);
}

Compiler *create_compiler(const char *output_filename) {
    Compiler *c = safe_malloc(sizeof(Compiler));

    c->functions.items = safe_malloc(INITIAL_CAPACITY * sizeof(Function));
    c->functions.capacity = INITIAL_CAPACITY;
    c->functions.length = 0;

    c->output = fopen(output_filename, "w");
    if (!c->output) {
        fprintf(stderr, "Cannot create output file: %s\n", output_filename);
        free(c->functions.items);
        free(c);
        exit(1);
    }

    c->scope_level = 0;
    c->current_scope = create_scope(NULL, 0, NULL);
    c->debug_mode = 0;
    c->debug_point = 0;

    return c;
}

Scope *create_scope(Scope *parent, int level, Function *owning_function) {
    Scope *scope = safe_malloc(sizeof(Scope));

    scope->level = level;
    scope->parent = parent;
    scope->children = safe_malloc(INITIAL_CAPACITY * sizeof(Scope *));
    scope->num_children = 0;
    scope->children_capacity = INITIAL_CAPACITY;
    scope->statements = NULL;
    scope->defer_stmts = NULL;
    scope->owning_function = owning_function;

    return scope;
}

void add_child_scope(Scope *parent, Scope *child) {
    if (parent->num_children >= parent->children_capacity) {
        size_t new_capacity = parent->children_capacity * 2;
        Scope **new_children =
            realloc(parent->children, new_capacity * sizeof(Scope *));
        if (!new_children) {
            fprintf(stderr, "Memory reallocation failed for scope children\n");
            exit(1);
        }
        parent->children = new_children;
        parent->children_capacity = new_capacity;
    }
    parent->children[parent->num_children++] = child;
}

void add_statement_to_scope(Scope *scope, Statement *stmt) {
    stmt->parent_scope = scope;

    if (stmt->type == STMT_DEFER) {
        if (!scope->defer_stmts) {
            scope->defer_stmts = stmt;
        } else {
            Statement *current = scope->defer_stmts;
            while (current->next) {
                current = current->next;
            }
            current->next = stmt;
        }
    } else {
        if (!scope->statements) {
            scope->statements = stmt;
        } else {
            Statement *current = scope->statements;
            while (current->next) {
                current = current->next;
            }
            current->next = stmt;
        }
    }
}

void error(const Compiler *c, const char *message) {
    fprintf(stderr, "%s:%zu:%zu: error: %s\n", c->source_file,
            c->current_token.line, c->current_token.column, message);
    exit(1);
}

void advance_cursor(Compiler *c) {
    if (c->cursor.point < c->cursor.buffer_size) {
        if (c->cursor.buffer[c->cursor.point] == '\n') {
            c->cursor.line++;
            c->cursor.column = 0;
        } else {
            c->cursor.column++;
        }
        c->cursor.point++;
    }
}

char peek_char(Compiler *c) {
    return (c->cursor.point < c->cursor.buffer_size)
        ? c->cursor.buffer[c->cursor.point]
        : EOF;
}

static void skip_whitespace(Compiler *c) {
    while (isspace(peek_char(c))) {
        advance_cursor(c);
    }
}

void next_token(Compiler *c) {
    skip_whitespace(c);

    c->previous_token = c->current_token;
    c->current_token.line = c->cursor.line;
    c->current_token.column = c->cursor.column;

    char ch = peek_char(c);
    if (ch == EOF) {
        c->current_token.type = TOKEN_EOF;
    } else if (isalpha(ch) || ch == '_') {
        size_t i = 0;
        while (isalnum(peek_char(c)) || peek_char(c) == '_') {
            c->current_token.value[i++] = peek_char(c);
            advance_cursor(c);
            if (i >= MAX_TOKEN_LEN - 1)
                break;
        }
        c->current_token.value[i] = '\0';

        if (strcmp(c->current_token.value, "fn") == 0)
            c->current_token.type = TOKEN_FN;
        else if (strcmp(c->current_token.value, "return") == 0)
            c->current_token.type = TOKEN_RETURN;
        else if (strcmp(c->current_token.value, "i32") == 0)
            c->current_token.type = TOKEN_I32;
        else if (strcmp(c->current_token.value, "defer") == 0)
            c->current_token.type = TOKEN_DEFER;
        else
            c->current_token.type = TOKEN_IDENT;
    } else if (isdigit(ch)) {
        size_t i = 0;
        while (isdigit(peek_char(c))) {
            c->current_token.value[i++] = peek_char(c);
            advance_cursor(c);
            if (i >= MAX_TOKEN_LEN - 1)
                break;
        }
        c->current_token.value[i] = '\0';
        c->current_token.type = TOKEN_NUMBER;
    } else {
        switch (ch) {
        case '-':
            advance_cursor(c);
            if (peek_char(c) == '>') {
                c->current_token.type = TOKEN_ARROW;
                advance_cursor(c);
            } else {
                error(c, "Expected '>' after '-'");
            }
            break;
        case '(':
            c->current_token.type = TOKEN_LPAREN;
            advance_cursor(c);
            break;
        case ')':
            c->current_token.type = TOKEN_RPAREN;
            advance_cursor(c);
            break;
        case '{':
            c->current_token.type = TOKEN_LBRACE;
            c->scope_level++;
            advance_cursor(c);
            break;
        case '}':
            c->current_token.type = TOKEN_RBRACE;
            c->scope_level--;
            advance_cursor(c);
            break;
        case ';':
            c->current_token.type = TOKEN_SEMICOLON;
            advance_cursor(c);
            break;
        case ',':
            c->current_token.type = TOKEN_COMMA;
            advance_cursor(c);
            break;
        default:
            error(c, "Unknown character");
        }
    }

    if (c->debug_mode) {
        printf("Point: %zu Token: %d Scope: %d\n", c->cursor.point,
               c->current_token.type, c->scope_level);
    }

    if (c->debug_mode && c->cursor.point >= c->debug_point) {
        printf("Scope level at point %zu: %d\n", c->debug_point, c->scope_level);
        exit(0);
    }
}

void expect(Compiler *c, TokenType type) {
    if (c->current_token.type != type) {
        char message[256];
        snprintf(message, sizeof(message), "Expected token type %d, got %d", type,
                 c->current_token.type);
        error(c, message);
    }
    next_token(c);
}

void parse_function(Compiler *c) {
    expect(c, TOKEN_FN);

    if (c->current_token.type != TOKEN_IDENT) {
        error(c, "Expected function name");
    }

    Function func = {0};
    strncpy(func.name, c->current_token.value, MAX_IDENT_LEN - 1);
    func.is_main = (strcmp(c->current_token.value, "main") == 0);

    next_token(c);
    expect(c, TOKEN_LPAREN);
    expect(c, TOKEN_RPAREN);
    expect(c, TOKEN_ARROW);

    if (c->current_token.type == TOKEN_I32) {
        strncpy(func.return_type, "i32", MAX_IDENT_LEN - 1);
        next_token(c);
    } else {
        strncpy(func.return_type, "void", MAX_IDENT_LEN - 1);
    }

    Scope *function_scope = create_scope(c->current_scope, c->scope_level, &func);
    add_child_scope(c->current_scope, function_scope);
    c->current_scope = function_scope;

    parse_function_body(c);

    c->current_scope = c->current_scope->parent;

    if (c->functions.length >= c->functions.capacity) {
        size_t new_capacity = c->functions.capacity * 2;
        Function *new_items =
            realloc(c->functions.items, new_capacity * sizeof(Function));
        if (!new_items) {
            error(c, "Memory reallocation failed for function list");
        }
        c->functions.items = new_items;
        c->functions.capacity = new_capacity;
    }
    c->functions.items[c->functions.length++] = func;
}

void parse_function_body(Compiler *c) {
    expect(c, TOKEN_LBRACE);

    while (c->current_token.type != TOKEN_EOF &&
           c->current_token.type != TOKEN_RBRACE) {
        switch (c->current_token.type) {
        case TOKEN_RETURN: {
            Statement *stmt = safe_malloc(sizeof(Statement));
            stmt->type = STMT_RETURN;
            stmt->next = NULL;
            next_token(c);
            if (c->current_token.type == TOKEN_NUMBER) {
                stmt->return_value = safe_strdup(c->current_token.value);
                next_token(c);
            } else {
                stmt->return_value = NULL;
            }
            expect(c, TOKEN_SEMICOLON);
            add_statement_to_scope(c->current_scope, stmt);
            break;
        }
        case TOKEN_IDENT: {
            Statement *stmt = safe_malloc(sizeof(Statement));
            stmt->type = STMT_FUNCTION_CALL;
            stmt->next = NULL;
            stmt->function_call = safe_strdup(c->current_token.value);
            next_token(c);
            expect(c, TOKEN_LPAREN);
            expect(c, TOKEN_RPAREN);
            expect(c, TOKEN_SEMICOLON);
            add_statement_to_scope(c->current_scope, stmt);
            break;
        }
        case TOKEN_DEFER: {
            Statement *stmt = safe_malloc(sizeof(Statement));
            stmt->type = STMT_DEFER;
            stmt->next = NULL;
            next_token(c);
            if (c->current_token.type != TOKEN_IDENT) {
                free(stmt);
                error(c, "Expected function call after defer");
            }
            stmt->function_call = safe_strdup(c->current_token.value);
            next_token(c);
            expect(c, TOKEN_LPAREN);
            expect(c, TOKEN_RPAREN);
            expect(c, TOKEN_SEMICOLON);
            add_statement_to_scope(c->current_scope, stmt);
            break;
        }
        case TOKEN_LBRACE: {
            Scope *new_scope = create_scope(c->current_scope, c->scope_level,
                                            c->current_scope->owning_function);
            add_child_scope(c->current_scope, new_scope);
            c->current_scope = new_scope;
            parse_function_body(c);
            c->current_scope = c->current_scope->parent;
            break;
        }
        default:
            error(c, "Unexpected token in function body");
        }
    }

    expect(c, TOKEN_RBRACE);
}

void process_defer_statements(FILE *output, Scope *scope) {
    if (!scope->defer_stmts)
        return;

    size_t count = 0;
    Statement *curr = scope->defer_stmts;
    while (curr) {
        count++;
        curr = curr->next;
    }

    Statement **defer_array = safe_malloc(count * sizeof(Statement *));
    curr = scope->defer_stmts;
    for (size_t i = 0; i < count; i++) {
        defer_array[i] = curr;
        curr = curr->next;
    }

    for (size_t i = count; i > 0; i--) {
        Statement *stmt = defer_array[i - 1];
        if (stmt->type == STMT_FUNCTION_CALL) {
            fprintf(output, "  call $%s()\n", stmt->function_call);
        }
    }

    free(defer_array);
}

void generate_scope_qbe(FILE *output, Scope *scope, const char *function_name,
                        int *has_return) {
    if (!scope)
        return;

    Statement *stmt = scope->statements;
    while (stmt) {
        switch (stmt->type) {
        case STMT_FUNCTION_CALL:
            fprintf(output, "  call $%s()\n", stmt->function_call);
            break;
        case STMT_RETURN:
            if (stmt->return_value) {
                fprintf(output, "  ret %s\n", stmt->return_value);
            } else {
                fprintf(output, "  ret\n");
            }
            *has_return = 1;
            return;
        default:
            break;
        }
        stmt = stmt->next;
    }

    for (size_t i = 0; i < scope->num_children; i++) {
        generate_scope_qbe(output, scope->children[i], function_name, has_return);
    }

    process_defer_statements(output, scope);
}

void generate_qbe(Compiler *c) {
    for (size_t i = 0; i < c->functions.length; i++) {
        const Function *func = &c->functions.items[i];
        int has_return = 0;

        if (func->is_main) {
            fprintf(c->output, "export ");
        }

        fprintf(c->output, "function w $%s() {\n", func->name);
        fprintf(c->output, "  @start\n");
        generate_scope_qbe(c->output, func->body_scope, func->name, &has_return);

        if (!has_return) {
            if (strcmp(func->return_type, "i32") == 0 && func->is_main) {
                fprintf(c->output, "  ret 0\n");
            } else {
                fprintf(c->output, "  ret\n");
            }
        }

        fprintf(c->output, "}\n\n");
    }
}

void destroy_scope(Scope *scope) {
    if (!scope)
        return;

    // Free statements
    Statement *stmt = scope->statements;
    while (stmt) {
        Statement *next = stmt->next;
        if (stmt->type == STMT_RETURN && stmt->return_value) {
            free(stmt->return_value);
        } else if (stmt->type == STMT_FUNCTION_CALL && stmt->function_call) {
            free(stmt->function_call);
        }
        free(stmt);
        stmt = next;
    }

    // Free defer statements
    stmt = scope->defer_stmts;
    while (stmt) {
        Statement *next = stmt->next;
        if (stmt->type == STMT_FUNCTION_CALL && stmt->function_call) {
            free(stmt->function_call);
        }
        free(stmt);
        stmt = next;
    }

    // Free child scopes
    for (size_t i = 0; i < scope->num_children; i++) {
        destroy_scope(scope->children[i]);
    }

    free(scope->children);
    free(scope);
}

void destroy_compiler(Compiler *c) {
    if (!c)
        return;

    free(c->cursor.buffer);
    fclose(c->output);
    free(c->functions.items);
    destroy_scope(c->current_scope);
    free(c);
}

void compile_file(const char *input_file, const char *output_file,
                  int debug_mode, size_t debug_point) {
    Compiler *c = create_compiler(output_file);
    c->source_file = safe_strdup(input_file);
    c->debug_mode = debug_mode;
    c->debug_point = debug_point;
    init_cursor(&c->cursor, input_file);

    next_token(c);

    while (c->current_token.type != TOKEN_EOF) {
        parse_function(c);
    }

    generate_qbe(c);

    free(c->source_file);
    destroy_compiler(c);
}

int main(int argc, char *argv[]) {
    const char *input_file = NULL;
    const char *output_file = NULL;
    int ir_only = 0;
    int verbose = 0;
    int debug_mode = 0;
    size_t debug_point = 0;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0) {
            printf("Usage: %s [options] <input_file>\n", argv[0]);
            printf("Options:\n");
            printf("  --help        Display this information\n");
            printf("  -o <file>     Place the output into <file>\n");
            printf("  -S            Only generate QBE IR, don't assemble\n");
            printf("  -v            Print commands being executed\n");
            printf("  -s <point>    Enable scope debugging at specified point\n");
            return 0;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (++i >= argc) {
                fprintf(stderr, "Error: -o requires an argument\n");
                return 1;
            }
            output_file = argv[i];
        } else if (strcmp(argv[i], "-s") == 0) {
            if (++i >= argc) {
                fprintf(stderr, "Error: -s requires a point number\n");
                return 1;
            }
            debug_mode = 1;
            debug_point = atoi(argv[i]);
        } else if (strcmp(argv[i], "-S") == 0) {
            ir_only = 1;
        } else if (strcmp(argv[i], "-v") == 0) {
            verbose = 1;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else {
            if (input_file) {
                fprintf(stderr, "Error: multiple input files not supported\n");
                return 1;
            }
            input_file = argv[i];
        }
    }

    if (!input_file) {
        fprintf(stderr, "Error: no input file\n");
        return 1;
    }

    char *ir_file = output_file && ir_only ? safe_strdup(output_file)
        : get_output_name(input_file, "ssa");

    compile_file(input_file, ir_file, debug_mode, debug_point);

    if (verbose) {
        printf("Generated QBE IR: %s\n", ir_file);
    }

    if (!ir_only) {
        char *asm_file = get_output_name(input_file, "s");
        char *exec_file = output_file ? safe_strdup(output_file)
            : get_output_name(input_file, "out");

        char qbe_cmd[512];
        snprintf(qbe_cmd, sizeof(qbe_cmd), "qbe %s -o %s", ir_file, asm_file);
        if (system(qbe_cmd) != 0) {
            fprintf(stderr, "QBE command failed\n");
            return 1;
        }

        char gcc_cmd[512];
        snprintf(gcc_cmd, sizeof(gcc_cmd), "gcc %s -o %s", asm_file, exec_file);
        if (system(gcc_cmd) != 0) {
            fprintf(stderr, "GCC command failed\n");
            return 1;
        }

        if (verbose) {
            printf("Generated executable: %s\n", exec_file);
        }

        free(asm_file);
        free(exec_file);
    }

    free(ir_file);

    return 0;
}
