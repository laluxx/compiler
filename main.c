#define _DEFAULT_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <libgen.h>

// TODO Step trough parsing and ir generation in a TUI
// TODO Emacs integratgion since we have a point
// we could use the current value of the point in emacs
// and check for errors ? like flycheck ?

// Constants
#define MAX_IDENT_LEN 64
#define MAX_TOKEN_LEN 256
#define INITIAL_CAPACITY 16

// Forward declarations of all structures
typedef struct Token Token;
typedef struct Cursor Cursor;
typedef struct Statement Statement;
typedef struct Scope Scope;
typedef struct Function Function;
typedef struct FunctionList FunctionList;
typedef struct Compiler Compiler;

// Enums
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
    TOKEN_DEFER  // New token for defer statements
} TokenType;

typedef enum {
    STMT_RETURN,
    STMT_EXPR,
    STMT_FUNCTION_CALL,
    STMT_DEFER  // New statement type for defer
} StatementType;

// Core structures
struct Token {
    TokenType type;
    char value[MAX_TOKEN_LEN];
    size_t line;    // Track line number for better error reporting
    size_t column;  // Track column number for better error reporting
};

struct Cursor {
    size_t point;
    char* buffer;
    size_t buffer_size;
    size_t line;
    size_t column;
};

struct Statement {
    StatementType type;
    union {
        char* return_value;     // For return statements
        char* function_call;    // For function calls
        Statement* defer_stmt;  // For deferred statements
    };
    Statement* next;
    Scope* parent_scope;       // Reference to containing scope
};

struct Scope {
    int level;                  // Nesting level (0 = global)
    Scope* parent;             // Parent scope
    Scope** children;          // Array of child scopes
    size_t num_children;
    size_t children_capacity;
    Statement* statements;      // Statements in this scope
    Statement* defer_stmts;    // Deferred statements for this scope
    Function* owning_function; // Function that owns this scope (NULL for global)
};

struct Function {
    char name[MAX_IDENT_LEN];
    char return_type[MAX_IDENT_LEN];
    int is_main;
    Scope* body_scope;        // Root scope for function body
};

struct FunctionList {
    Function* items;
    size_t capacity;
    size_t length;
};

struct Compiler {
    FunctionList functions;
    FILE* output;
    Cursor cursor;
    Token current_token;
    Token previous_token;    // Keep track of previous token for better error messages
    Scope* current_scope;
    int scope_level;
    char* source_file;      // Store source filename for error reporting
};

// Function declarations
Compiler* create_compiler(const char* output_filename);
void destroy_compiler(Compiler* c);
void init_cursor(Cursor* cursor, const char* filename);
void advance_cursor(Compiler* c);
char peek_char(Compiler* c);
void next_token(Compiler* c);
void error(const Compiler* c, const char* message) __attribute__((noreturn));
void expect(Compiler* c, TokenType type);
void parse_function(Compiler* c);
void parse_function_body(Compiler* c);
void generate_qbe(Compiler* c);

// Scope management functions
Scope* create_scope(Scope* parent, int level, Function* owning_function);
void destroy_scope(Scope* scope);
void add_statement_to_scope(Scope* scope, Statement* stmt);
void add_child_scope(Scope* parent, Scope* child);
void process_defer_statements(FILE* output, Scope* scope);
/* void generate_scope_qbe(FILE *output, Scope *scope, const char *function_name); */

void generate_scope_qbe(FILE *output, Scope *scope, const char *function_name,
                        int *has_return);

// Memory management functions
static void* safe_malloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    return ptr;
}

static char* safe_strdup(const char* str) {
    char* dup = strdup(str);
    if (!dup) {
        fprintf(stderr, "String duplication failed\n");
        exit(1);
    }
    return dup;
}

// Initialize cursor with file content
void init_cursor(Cursor* cursor, const char* filename) {
    cursor->point = 0;
    cursor->line = 1;
    cursor->column = 0;
    
    FILE* file = fopen(filename, "r");
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

// Create and initialize compiler
Compiler* create_compiler(const char* output_filename) {
    Compiler* c = safe_malloc(sizeof(Compiler));
    
    // Initialize function list
    c->functions.items = safe_malloc(INITIAL_CAPACITY * sizeof(Function));
    c->functions.capacity = INITIAL_CAPACITY;
    c->functions.length = 0;
    
    // Open output file
    c->output = fopen(output_filename, "w");
    if (!c->output) {
        fprintf(stderr, "Cannot create output file: %s\n", output_filename);
        free(c->functions.items);
        free(c);
        exit(1);
    }
    
    // Initialize scope tracking
    c->scope_level = 0;
    c->current_scope = create_scope(NULL, 0, NULL);  // Create global scope
    
    return c;
}

// Create a new scope
Scope* create_scope(Scope* parent, int level, Function* owning_function) {
    Scope* scope = safe_malloc(sizeof(Scope));
    
    scope->level = level;
    scope->parent = parent;
    scope->children = safe_malloc(INITIAL_CAPACITY * sizeof(Scope*));
    scope->num_children = 0;
    scope->children_capacity = INITIAL_CAPACITY;
    scope->statements = NULL;
    scope->defer_stmts = NULL;
    scope->owning_function = owning_function;
    
    return scope;
}

// Add a child scope
void add_child_scope(Scope* parent, Scope* child) {
    if (parent->num_children >= parent->children_capacity) {
        size_t new_capacity = parent->children_capacity * 2;
        Scope** new_children = realloc(parent->children, 
                                       new_capacity * sizeof(Scope*));
        if (!new_children) {
            fprintf(stderr, "Memory reallocation failed for scope children\n");
            exit(1);
        }
        parent->children = new_children;
        parent->children_capacity = new_capacity;
    }
    parent->children[parent->num_children++] = child;
}

// Add a statement to a scope
void add_statement_to_scope(Scope* scope, Statement* stmt) {
    stmt->parent_scope = scope;
    
    if (stmt->type == STMT_DEFER) {
        // Add to defer list
        if (!scope->defer_stmts) {
            scope->defer_stmts = stmt;
        } else {
            Statement* current = scope->defer_stmts;
            while (current->next) {
                current = current->next;
            }
            current->next = stmt;
        }
    } else {
        // Add to regular statements
        if (!scope->statements) {
            scope->statements = stmt;
        } else {
            Statement* current = scope->statements;
            while (current->next) {
                current = current->next;
            }
            current->next = stmt;
        }
    }
}

// Error handling
void error(const Compiler* c, const char* message) {
    fprintf(stderr, "%s:%zu:%zu: error: %s\n", 
            c->source_file,
            c->current_token.line,
            c->current_token.column,
            message);
    exit(1);
}

// Lexer functions
void advance_cursor(Compiler* c) {
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

char peek_char(Compiler* c) {
    return (c->cursor.point < c->cursor.buffer_size) 
        ? c->cursor.buffer[c->cursor.point] 
        : EOF;
}

static void skip_whitespace(Compiler* c) {
    while (isspace(peek_char(c))) {
        advance_cursor(c);
    }
}

// Token handling
void next_token(Compiler* c) {
    skip_whitespace(c);
    
    // Save current token as previous
    c->previous_token = c->current_token;
    
    // Save current position for error reporting
    c->current_token.line = c->cursor.line;
    c->current_token.column = c->cursor.column;
    
    char ch = peek_char(c);
    if (ch == EOF) {
        c->current_token.type = TOKEN_EOF;
        return;
    }
    
    if (isalpha(ch) || ch == '_') {
        size_t i = 0;
        while (isalnum(peek_char(c)) || peek_char(c) == '_') {
            c->current_token.value[i++] = peek_char(c);
            advance_cursor(c);
            if (i >= MAX_TOKEN_LEN - 1) break;
        }
        c->current_token.value[i] = '\0';
        
        // Check keywords
        if (strcmp(c->current_token.value, "fn") == 0) {
            c->current_token.type = TOKEN_FN;
        } else if (strcmp(c->current_token.value, "return") == 0) {
            c->current_token.type = TOKEN_RETURN;
        } else if (strcmp(c->current_token.value, "i32") == 0) {
            c->current_token.type = TOKEN_I32;
        } else if (strcmp(c->current_token.value, "defer") == 0) {
            c->current_token.type = TOKEN_DEFER;
        } else {
            c->current_token.type = TOKEN_IDENT;
        }
        return;
    }
    
    if (isdigit(ch)) {
        size_t i = 0;
        while (isdigit(peek_char(c))) {
            c->current_token.value[i++] = peek_char(c);
            advance_cursor(c);
            if (i >= MAX_TOKEN_LEN - 1) break;
        }
        c->current_token.value[i] = '\0';
        c->current_token.type = TOKEN_NUMBER;
        return;
    }
    
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
    case '(': c->current_token.type = TOKEN_LPAREN; advance_cursor(c); break;
    case ')': c->current_token.type = TOKEN_RPAREN; advance_cursor(c); break;
    case '{': c->current_token.type = TOKEN_LBRACE; advance_cursor(c); break;
    case '}': c->current_token.type = TOKEN_RBRACE; advance_cursor(c); break;
    case ';': c->current_token.type = TOKEN_SEMICOLON; advance_cursor(c); break;
    case ',': c->current_token.type = TOKEN_COMMA; advance_cursor(c); break;
    default: error(c, "Unknown character");
    }
}

void expect(Compiler* c, TokenType type) {
    if (c->current_token.type != type) {
        char message[256];
        snprintf(message, sizeof(message), 
                 "Expected token type %d, got %d", 
                 type, c->current_token.type);
        error(c, message);
    }
    next_token(c);
}

// Parse function declaration
void parse_function(Compiler* c) {
    expect(c, TOKEN_FN);
    
    if (c->current_token.type != TOKEN_IDENT) {
        error(c, "Expected function name");
    }
    
    Function func = {0};  // Zero initialize
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
    
    // Create function scope
    func.body_scope = create_scope(c->current_scope, c->scope_level + 1, &func);
    
    // Add function to list
    if (c->functions.length >= c->functions.capacity) {
        size_t new_capacity = c->functions.capacity * 2;
        Function* new_items = realloc(c->functions.items, 
                                      new_capacity * sizeof(Function));
        if (!new_items) {
            error(c, "Memory reallocation failed for function list");
        }
        c->functions.items = new_items;
        c->functions.capacity = new_capacity;
    }
    c->functions.items[c->functions.length++] = func;
    
    // Parse function body
    c->current_scope = func.body_scope;
    c->scope_level++;
    parse_function_body(c);
    c->scope_level--;
    c->current_scope = c->current_scope->parent;
}

// Parse function body
void parse_function_body(Compiler* c) {
    expect(c, TOKEN_LBRACE);
    
    while (c->current_token.type != TOKEN_RBRACE) {
        if (c->current_token.type == TOKEN_LBRACE) {
            // New nested scope
            Scope* nested_scope = create_scope(c->current_scope, 
                                               c->scope_level + 1,
                                               c->current_scope->owning_function);
            add_child_scope(c->current_scope, nested_scope);
            c->current_scope = nested_scope;
            c->scope_level++;
            next_token(c);
            continue;
        }
        
        if (c->current_token.type == TOKEN_RBRACE) {
            // Process defer statements before leaving scope
            process_defer_statements(c->output, c->current_scope);
            c->current_scope = c->current_scope->parent;
            c->scope_level--;
            next_token(c);
            continue;
        }
        
        // Handle statements
        Statement* stmt = safe_malloc(sizeof(Statement));
        stmt->next = NULL;
        
        if (c->current_token.type == TOKEN_DEFER) {
            next_token(c);
            stmt->type = STMT_DEFER;
            if (c->current_token.type != TOKEN_IDENT) {
                error(c, "Expected function call after defer");
            }
            stmt->function_call = safe_strdup(c->current_token.value);
            next_token(c);
            expect(c, TOKEN_LPAREN);
            expect(c, TOKEN_RPAREN);
            expect(c, TOKEN_SEMICOLON);
        } else if (c->current_token.type == TOKEN_IDENT) {
            // Function call
            stmt->type = STMT_FUNCTION_CALL;
            stmt->function_call = safe_strdup(c->current_token.value);
            next_token(c);
            expect(c, TOKEN_LPAREN);
            expect(c, TOKEN_RPAREN);
            expect(c, TOKEN_SEMICOLON);
        } else if (c->current_token.type == TOKEN_RETURN) {
            // Return statement
            stmt->type = STMT_RETURN;
            next_token(c);
            if (c->current_token.type == TOKEN_NUMBER) {
                stmt->return_value = safe_strdup(c->current_token.value);
                next_token(c);
            } else {
                stmt->return_value = NULL;
            }
            expect(c, TOKEN_SEMICOLON);
            
            // Process defer statements before return
            process_defer_statements(c->output, c->current_scope);
        } else {
            error(c, "Unexpected token in function body");
        }
        
        add_statement_to_scope(c->current_scope, stmt);
    }
    
    expect(c, TOKEN_RBRACE);
}

// Process deferred statements in reverse order
void process_defer_statements(FILE* output, Scope* scope) {
    if (!scope->defer_stmts) return;
    
    // Create array of deferred statements
    size_t count = 0;
    Statement* curr = scope->defer_stmts;
    while (curr) {
        count++;
        curr = curr->next;
    }
    
    Statement** defer_array = safe_malloc(count * sizeof(Statement*));
    curr = scope->defer_stmts;
    for (size_t i = 0; i < count; i++) {
        defer_array[i] = curr;
        curr = curr->next;
    }
    
    // Process in reverse order
    for (size_t i = count; i > 0; i--) {
        Statement* stmt = defer_array[i - 1];
        if (stmt->type == STMT_FUNCTION_CALL) {
            fprintf(output, "  call $%s()\n", stmt->function_call);
        }
    }
    
    free(defer_array);
}

// Generate scope-aware QBE output
void generate_scope_qbe(FILE* output, Scope* scope, const char* function_name, int* has_return) {
    if (!scope) return;
    
    // Generate code for statements in this scope
    Statement* stmt = scope->statements;
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
            return; // Exit after return
        default:
            break;
        }
        stmt = stmt->next;
    }
    
    // Generate code for child scopes
    for (size_t i = 0; i < scope->num_children; i++) {
        generate_scope_qbe(output, scope->children[i], function_name, has_return);
    }
}



// Generate final QBE output

void generate_qbe(Compiler* c) {
    // First pass: non-main functions
    for (size_t i = 0; i < c->functions.length; i++) {
        const Function* func = &c->functions.items[i];
        if (!func->is_main) {
            int has_return = 0;
            fprintf(c->output, "function w $%s() {\n", func->name);
            fprintf(c->output, "  @start\n");
            generate_scope_qbe(c->output, func->body_scope, func->name, &has_return);
            // Add default return if none was provided
            if (!has_return) {
                fprintf(c->output, "  ret\n");
            }
            fprintf(c->output, "}\n\n");
        }
    }
    
    // Second pass: main function
    for (size_t i = 0; i < c->functions.length; i++) {
        const Function* func = &c->functions.items[i];
        if (func->is_main) {
            int has_return = 0;
            fprintf(c->output, "export function w $%s() {\n", func->name);
            fprintf(c->output, "  @start\n");
            generate_scope_qbe(c->output, func->body_scope, func->name, &has_return);
            // Add default return if none was provided
            if (!has_return) {
                if (strcmp(func->return_type, "i32") == 0) {
                    fprintf(c->output, "  ret 0\n");
                } else {
                    fprintf(c->output, "  ret\n");
                }
            }
            fprintf(c->output, "}\n");
        }
    }
}

// Cleanup functions
void destroy_scope(Scope* scope) {
    if (!scope) return;
    
    // Free statements in this scope
    Statement* stmt = scope->statements;
    while (stmt) {
        Statement* next = stmt->next;
        free(stmt->function_call);
        free(stmt->return_value);
        free(stmt);
        stmt = next;
    }
    
    // Free deferred statements
    stmt = scope->defer_stmts;
    while (stmt) {
        Statement* next = stmt->next;
        free(stmt->function_call);
        free(stmt);
        stmt = next;
    }
    
    // Recursively destroy child scopes
    for (size_t i = 0; i < scope->num_children; i++) {
        destroy_scope(scope->children[i]);
    }
    
    free(scope->children);
    free(scope);
}

void destroy_compiler(Compiler* c) {
    if (!c) return;
    
    free(c->cursor.buffer);
    fclose(c->output);
    
    // Clean up function list
    free(c->functions.items);
    
    // Clean up scopes
    destroy_scope(c->current_scope);
    
    free(c);
}

// Main compilation function
static void compile_file(const char* input_file, const char* output_file) {
    Compiler* c = create_compiler(output_file);
    c->source_file = safe_strdup(input_file);
    init_cursor(&c->cursor, input_file);
    
    next_token(c);
    
    while (c->current_token.type != TOKEN_EOF) {
        parse_function(c);
    }
    
    generate_qbe(c);
    
    free(c->source_file);
    destroy_compiler(c);
}

// Utility functions for command line handling
static void print_usage(const char* program_name) {
    fprintf(stderr, "Usage: %s [options] <input_file>\n", program_name);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --help        Display this information\n");
    fprintf(stderr, "  -o <file>     Place the output into <file>\n");
    fprintf(stderr, "  -S            Only generate QBE IR, don't assemble\n");
    fprintf(stderr, "  -v            Print commands being executed\n");
}

static char* get_output_name(const char* input_file, const char* ext) {
    char* base = safe_strdup(input_file);
    char* filename = basename(base);
    char* dot = strrchr(filename, '.');
    if (dot) *dot = '\0';
    
    size_t len = strlen(filename) + strlen(ext) + 2;
    char* output = safe_malloc(len);
    snprintf(output, len, "%s.%s", filename, ext);
    
    free(base);
    return output;
}

static void execute_command(const char* cmd, int verbose) {
    if (verbose) {
        printf("%s\n", cmd);
    }
    if (system(cmd) != 0) {
        fprintf(stderr, "Command failed: %s\n", cmd);
        exit(1);
    }
}

int main(int argc, char* argv[]) {
    const char* input_file = NULL;
    const char* output_file = NULL;
    int ir_only = 0;
    int verbose = 0;
    
    // Parse command line arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (++i >= argc) {
                fprintf(stderr, "Error: -o requires an argument\n");
                return 1;
            }
            output_file = argv[i];
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
        print_usage(argv[0]);
        return 1;
    }
    
    // Generate output filenames
    char* ir_file = output_file && ir_only ? 
        safe_strdup(output_file) : 
        get_output_name(input_file, "ssa");
    
    char* exec_file = output_file && !ir_only ? 
        safe_strdup(output_file) : 
        get_output_name(input_file, "out");
    
    // Compile to QBE IR
    compile_file(input_file, ir_file);
    if (verbose) {
        printf("Generated QBE IR: %s\n", ir_file);
    }
    
    if (!ir_only) {
        // Generate assembly
        char qbe_cmd[512];
        char* asm_file = get_output_name(input_file, "s");
        snprintf(qbe_cmd, sizeof(qbe_cmd), "qbe %s -o %s", ir_file, asm_file);
        execute_command(qbe_cmd, verbose);
        
        // Generate executable
        char gcc_cmd[512];
        snprintf(gcc_cmd, sizeof(gcc_cmd), "gcc %s -o %s", asm_file, exec_file);
        execute_command(gcc_cmd, verbose);
        
        if (verbose) {
            printf("Generated executable: %s\n", exec_file);
        }
        
        free(asm_file);
    }
    
    free(ir_file);
    free(exec_file);
    
    return 0;
}
