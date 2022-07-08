#![allow(dead_code)]

use anyhow::Context;
use anyhow::Error;
use anyhow::Result;
use clap::Parser;
use std::fs;

pub mod scanner;

/// A Lox Interpreter
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Source filename.
    #[clap(short, long, value_parser)]
    source: Option<String>,
}

fn main() {
    let args = Args::parse();
    let result = match args.source {
        Some(filename) => run_file(&filename),
        None => run_prompt(),
    };

    if let Err(err) = result {
        eprint!("Error: {:?}\n", err);
        std::process::exit(1);
    }
}

fn run_file(filename: &str) -> Result<()> {
    println!("Running file `{}`...", filename);

    let code =
        fs::read_to_string(filename).context(format!("failed to read file `{}`", filename))?;

    run(&code)
}

fn run_prompt() -> Result<()> {
    println!("Running prompt...");
    Ok(())
}

fn run(code: &str) -> Result<()> {
    println!("Running code:\n\n{}", code);
    Ok(())
}
