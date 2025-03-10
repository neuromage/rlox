#![allow(dead_code)]

use clap::Parser;
use miette::IntoDiagnostic;
use miette::Result;
use miette::WrapErr;
use std::fs;

mod frontend;

use frontend::scanner::scan;

/// A Lox Interpreter
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Source filename.
    #[clap(short, long, value_parser)]
    source: Option<String>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.source {
        // FIXME: do it
        Some(filename) => run_file(&filename),
        None => run_prompt(),
    }
}

fn run_file(filename: &str) -> Result<()> {
    println!("Running file `{}`...", filename);

    let code = fs::read_to_string(filename)
        .into_diagnostic()
        .wrap_err(format!("failed to read file `{}`", filename))?;

    run(&code)
}

fn run_prompt() -> Result<()> {
    println!("Running prompt...");
    Ok(())
}

fn run(code: &str) -> Result<()> {
    println!("Running code:\n=========={}\n==========", code);
    let _ = scan(code).wrap_err("unable to parse Lox source code")?;
    Ok(())
}
