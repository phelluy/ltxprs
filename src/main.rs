use ltxprs::LtxNode;

use clap::Parser;

#[derive(Parser, Debug)]
struct Cli {
    #[clap(short, long, default_value = "test/simple_fr.tex")]
    input_file: String,
    // #[clap(short, long, default_value = "test/simple_en.tex")]
    // output_file: String,
}

fn main() {
    let input_file = Cli::parse().input_file;
    let str = std::fs::read_to_string(input_file).unwrap();
    // remove text before \begin{document} and after \end{document}
    let str = str.split(r"\begin{document}").collect::<Vec<&str>>()[1];
    let str = str.split(r"\end{document}").collect::<Vec<&str>>()[0];
    // split at %done (if present) and take the last part
    let strs = str.split("%done").collect::<Vec<&str>>();
    let len = strs.len();
    //println!("len: {}", len);
    let str = if len == 0 { str } else { &strs[len - 1] };

    let latex = LtxNode::new(&str);
    //println!("{:?}", latex);
    let length = 4000;
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
    println!("{}", latex.to_ebnf());
}
