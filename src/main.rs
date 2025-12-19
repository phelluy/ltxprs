use ltxprs::LtxNode;

use clap::Parser;

#[derive(Parser, Debug)]
struct Cli {
  #[clap(short, long, default_value = "test/simple_fr.tex")]
    //#[clap(short, long, default_value = "test/axigen.tex")]
    file_input: String,
    // #[clap(short, long, default_value = "test/simple_en.tex")]
    // output_file: String,
}

fn main() {
    let input_file = Cli::parse().file_input;
    let str = match std::fs::read_to_string(&input_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", input_file, e);
            std::process::exit(1);
        }
    };
    // remove text before \begin{document} and after \end{document}
    let vstr = str.split(r"\begin{document}").collect::<Vec<&str>>();
    let str = vstr.last().map(|s| *s).unwrap_or(str.as_str());
    let str = str.split(r"\end{document}").next().unwrap_or(str);
    // split at %done (if present) and take the last part
    let strs = str.split("%done").collect::<Vec<&str>>();
    let len = strs.len();
    //println!("len: {}", len);
    let str = if len == 0 { str } else { strs[len - 1] };

    let latex = LtxNode::new(str);
    println!("{:?}", latex);
    //assert!(1==2);
    let length = 200;
    let level = 0;
    let s = String::new();
    let s = latex.print_split(level, s, length);
    println!("{}", s);
    let cmds = latex.extracts_commands();
    println!("commands: {:?}", cmds);
    let labels = latex.extracts_labels();
    println!("labels: {:?}", labels);
    let refs = latex.extracts_references();
    println!("references: {:?}", refs);
    let cites = latex.extracts_citations();
    println!("citations: {:?}", cites);
    println!("{}", latex.to_ebnf());
}
