use rustyline::DefaultEditor;
use std::collections::HashMap;

fn main() {
    println!("Nost Basic");
    let mut engine = Engine::new();
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        let code = rl.readline("> ").unwrap();
        if code.is_empty() {
            continue;
        }

        if code == "run" {
            engine.run_program();
            println!("Okay")
        } else if {
            let code = code.split_once(" ").unwrap_or_default();
            code.0.parse::<usize>().is_ok() && !code.1.is_empty()
        } {
            if let Some(ast) = parse_program(code.clone()) {
                engine.set_program(ast);
            } else {
                if let Some(expr) = parse_expr(code) {
                    if let Some(result) = expr.eval(&mut engine.scope) {
                        println!(" {}", result.display(&mut engine.scope));
                    } else {
                        println!("i");
                    }
                } else {
                    println!("Error");
                }
            }
        } else {
            if let Some(ast) = parse_opecode(code.clone()) {
                if engine.run_opecode(ast).is_some() {
                    println!("Okay");
                } else {
                    println!("Error");
                }
            } else {
                if let Some(expr) = parse_expr(code) {
                    if let Some(result) = expr.eval(&mut engine.scope) {
                        println!(" {}", result.display(&mut engine.scope));
                    } else {
                        println!("Error");
                    }
                } else {
                    println!("Error");
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Engine {
    pc: usize,
    program: Program,
    bytecode: Vec<(usize, Statement)>,
    scope: Scope,
}

impl Engine {
    fn new() -> Engine {
        Engine {
            pc: 0,
            program: HashMap::new(),
            bytecode: vec![],
            scope: HashMap::new(),
        }
    }

    fn set_program(&mut self, program: Program) {
        self.program.extend(program);
    }

    fn run_program(&mut self) -> Option<()> {
        self.bytecode = {
            let mut vec: Vec<_> = self.program.clone().into_iter().collect();
            vec.sort_by(|a, b| a.0.cmp(&b.0));
            vec
        };
        while self.bytecode.len() > self.pc {
            let code = self.bytecode[self.pc].1.clone();
            if self.run_opecode(code)? {
                self.pc += 1;
            }
        }
        self.pc = 0;
        Some(())
    }

    fn run_opecode(&mut self, code: Statement) -> Option<bool> {
        match code {
            Statement::Print(expr) => println!(" {}", expr.eval(&mut self.scope)?.get_string()),
            Statement::Let(name, expr) => {
                self.scope.insert(name, expr.eval(&mut self.scope.clone())?);
            }
            Statement::If(expr, then, elses) => {
                if expr.eval(&mut self.scope)?.get_bool() {
                    if !self.run_opecode(*then)? {
                        return Some(false);
                    }
                } else {
                    if let Some(elses) = elses {
                        if !self.run_opecode(*elses)? {
                            return Some(false);
                        }
                    }
                }
            }
            Statement::Goto(addr) => {
                let addr = addr.eval(&mut self.scope)?.get_number() as usize;
                self.pc = self.bytecode.iter().position(|x| x.0 == addr)?;
                return Some(false);
            }
            Statement::End => std::process::exit(0),
        }
        Some(true)
    }
}

type Scope = HashMap<String, Type>;
type Program = HashMap<usize, Statement>;
#[derive(Debug, Clone)]
enum Statement {
    Print(Expr),
    Let(String, Expr),
    Goto(Expr),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    End,
}

#[derive(Debug, Clone)]
enum Type {
    Number(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expr>),
    Symbol(String),
    Null,
}

impl Type {
    fn get_number(&self) -> f64 {
        match self {
            Type::Number(n) => n.to_owned(),
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(0.0),
            Type::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            _ => 0.0,
        }
    }

    fn get_bool(&self) -> bool {
        match self {
            Type::Number(n) => *n != 0.0,
            Type::String(s) | Type::Symbol(s) => s.trim().parse().unwrap_or(false),
            Type::Bool(b) => *b,
            _ => false,
        }
    }

    fn get_string(&self) -> String {
        match self {
            Type::String(s) | Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string().to_uppercase(),
            _ => String::new(),
        }
    }

    fn get_array(&self) -> Vec<Expr> {
        match self {
            Type::Array(s) => s.to_owned(),
            Type::String(s) => s
                .chars()
                .map(|x| Expr::Value(Type::String(x.to_string())))
                .collect(),
            other => vec![Expr::Value(other.to_owned())],
        }
    }
    fn display(&self, scope: &mut HashMap<String, Type>) -> String {
        match self {
            Type::String(s) => format!("\"{}\"", s),
            Type::Symbol(s) => s.to_string(),
            Type::Number(n) => n.to_string(),
            Type::Bool(b) => b.to_string(),
            Type::Array(a) => format!(
                "[{}]",
                a.iter()
                    .map(|x| x.eval(scope).unwrap().display(scope))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Null => "null".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Infix(Box<Infix>),
    Prefix(Box<Prefix>),
    Value(Type),
}

impl Expr {
    fn eval(&self, scope: &mut Scope) -> Option<Type> {
        Some(match self {
            Expr::Prefix(prefix) => (*prefix).eval(scope)?,
            Expr::Infix(infix) => (*infix).eval(scope)?,
            Expr::Value(value) => {
                if let Type::Symbol(name) = value {
                    if let Some(refer) = scope.get(name.as_str()).cloned() {
                        refer
                    } else {
                        value.clone()
                    }
                } else {
                    value.clone()
                }
            }
        })
    }
}

#[derive(Debug, Clone)]
struct Infix {
    operator: Operator,
    values: (Expr, Expr),
}

#[derive(Debug, Clone)]
struct Prefix {
    operator: Operator,
    values: Expr,
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Equal,
    LessThan,
    GreaterThan,
    And,
    Or,
    Not,
}

impl Infix {
    fn eval(&self, scope: &mut Scope) -> Option<Type> {
        let left = self.values.0.eval(scope)?;
        let right = self.values.1.eval(scope)?;
        Some(match self.operator {
            Operator::Add => {
                if let Type::Number(left) = left {
                    Type::Number(left + right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left + &right.get_string())
                } else if let Type::Array(left) = left {
                    Type::Array([left, right.get_array()].concat())
                } else {
                    Type::Null
                }
            }
            Operator::Sub => {
                if let Type::Number(left) = left {
                    Type::Number(left - right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left.replace(&right.get_string(), ""))
                } else {
                    Type::Null
                }
            }
            Operator::Mul => {
                if let Type::Number(left) = left {
                    Type::Number(left * right.get_number())
                } else if let Type::String(left) = left {
                    Type::String(left.repeat(right.get_number() as usize))
                } else {
                    Type::Null
                }
            }
            Operator::Div => Type::Number(left.get_number() / right.get_number()),
            Operator::Mod => Type::Number(left.get_number() % right.get_number()),
            Operator::Pow => Type::Number(left.get_number().powf(right.get_number())),
            Operator::Equal => Type::Bool(left.get_string() == right.get_string()),
            Operator::LessThan => Type::Bool(left.get_number() < right.get_number()),
            Operator::GreaterThan => Type::Bool(left.get_number() > right.get_number()),
            Operator::And => Type::Bool(left.get_bool() && right.get_bool()),
            Operator::Or => Type::Bool(left.get_bool() || right.get_bool()),
            _ => todo!(),
        })
    }
}

impl Prefix {
    fn eval(&self, scope: &mut Scope) -> Option<Type> {
        let value = self.values.eval(scope)?;
        Some(match self.operator {
            Operator::Sub => Type::Number(-value.get_number()),
            Operator::Not => Type::Bool(!value.get_bool()),
            _ => todo!(),
        })
    }
}

fn parse_expr(soruce: String) -> Option<Expr> {
    let tokens: Vec<String> = tokenize_expr(soruce)?;
    let left = tokens.last()?.trim().to_string();
    let left = if let Ok(n) = left.parse::<f64>() {
        Expr::Value(Type::Number(n))
    } else if let Ok(b) = left.parse::<bool>() {
        Expr::Value(Type::Bool(b))
    } else if left.starts_with("!") {
        let mut left = left.clone();
        left.remove(0);
        Expr::Prefix(Box::new(Prefix {
            operator: Operator::Not,
            values: parse_expr(left.to_string())?,
        }))
    } else if left.starts_with("-") {
        let mut left = left.clone();
        left.remove(0);
        Expr::Prefix(Box::new(Prefix {
            operator: Operator::Sub,
            values: parse_expr(left.to_string())?,
        }))
    } else if left.starts_with('"') && left.ends_with('"') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        Expr::Value(Type::String(left.to_string()))
    } else if left.starts_with('(') && left.ends_with(')') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        parse_expr(left)?
    } else if left.starts_with('[') && left.ends_with(']') {
        let left = {
            let mut left = left.clone();
            left.remove(0);
            left.remove(left.len() - 1);
            left
        };
        Expr::Value(Type::Array(
            tokenize_args(left)?
                .iter()
                .map(|x| parse_expr(x.trim().to_string()).unwrap())
                .collect(),
        ))
    } else {
        Expr::Value(Type::Symbol(left))
    };

    if let Some(operator) = {
        let mut tokens = tokens.clone();
        tokens.reverse();
        tokens
    }
    .get(1)
    {
        let operator = match operator.as_str() {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            "^" => Operator::Pow,
            "=" => Operator::Equal,
            "<" => Operator::LessThan,
            ">" => Operator::GreaterThan,
            "&" => Operator::And,
            "|" => Operator::Or,
            _ => return None,
        };
        Some(Expr::Infix(Box::new(Infix {
            operator,
            values: (
                parse_expr(tokens.get(..tokens.len() - 2)?.to_vec().join(" "))?,
                left,
            ),
        })))
    } else {
        return Some(left);
    }
}

fn parse_program(source: String) -> Option<Program> {
    let mut program: Program = HashMap::new();
    for line in source.lines() {
        let line = line.trim();
        let (ln, code) = line.split_once(" ")?;
        let ln: usize = ln.trim().parse().unwrap();
        program.insert(ln, parse_opecode(code.trim().to_string())?);
    }
    Some(program)
}

fn parse_opecode(code: String) -> Option<Statement> {
    let code = code.trim();
    Some(if code.starts_with("print") {
        Statement::Print(parse_expr(code["print".len()..].to_string())?)
    } else if code.starts_with("goto") {
        Statement::Goto(parse_expr(code["goto".len()..].to_string())?)
    } else if code.starts_with("if") {
        let code = code["if".len()..].to_string();
        let (cond, code) = code.split_once("then")?;
        if let Some((then, elses)) = code.split_once("else") {
            Statement::If(
                parse_expr(cond.to_string())?,
                Box::new(parse_opecode(then.to_string())?),
                Some(Box::new(parse_opecode(elses.to_string())?)),
            )
        } else {
            Statement::If(
                parse_expr(cond.to_string())?,
                Box::new(parse_opecode(code.to_string())?),
                None,
            )
        }
    } else if code.starts_with("let") {
        let code = code["let".len()..].to_string();
        let (name, code) = code.split_once("=")?;
        Statement::Let(name.trim().to_string(), parse_expr(code.to_string())?)
    } else if code == "end" {
        Statement::End
    } else {
        return None;
    })
}

fn tokenize_expr(input: String) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: isize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '{' | '[' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | '}' | ']' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    return None;
                }
            }
            ' ' | '　' | '\t' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Syntax error check
    if in_quote {
        return None;
    }
    if in_parentheses != 0 {
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }

    Some(tokens)
}

fn tokenize_args(input: String) -> Option<Vec<String>> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: isize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' | '[' | '{' if !in_quote => {
                current_token.push(c);
                in_parentheses += 1;
            }
            ')' | ']' | '}' if !in_quote => {
                current_token.push(c);
                if in_parentheses > 0 {
                    in_parentheses -= 1;
                } else {
                    return None;
                }
            }
            ',' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            '"' => {
                in_quote = !in_quote;
                current_token.push(c);
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    // Syntax error check
    if in_quote {
        return None;
    }
    if in_parentheses != 0 {
        return None;
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Some(tokens)
}
