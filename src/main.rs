use ltxprs::LtxNode;

use clap::Parser;
use clap::Arg;

#[derive(Parser, Debug)]
struct Cli {
    #[clap(short, long, default_value = "test/simple_fr.tex")]
    input_file: String,
    // #[clap(short, long, default_value = "test/simple_en.tex")]
    // output_file: String,
}

fn main() {
    let str = r#"
% comment
\ref{toto}        
\item a \\
\{ oh \$ \& \}
$ \frac{a}{b} $
\label{toto}
\item {\blue {\b \ref{!tata} \label{titi}}}
    "#;
    // read the "test/simple_fr.tex" file
    // let str = std::fs::read_to_string("test/simple_fr.tex").unwrap();
    // let str = std::fs::read_to_string("test/thermo_torch_fr.tex").unwrap();
    // let str = std::fs::read_to_string("test/kin_diapos_wuerzburg.tex").unwrap();
    // let args = Cli::parse();
    // let input_file = args.input_file.as_str();
    let input_file = Cli::parse().input_file;
    let str = std::fs::read_to_string(input_file).unwrap();
    // remove text before \begin{document} and after \end{document}
    //let str = str.split(r"\begin{document}").collect::<Vec<&str>>()[1];
    let str = str.split(r"\end{document}").collect::<Vec<&str>>()[0];

    let latex = LtxNode::new(&str);
    let cmds = latex.extracts_commands();
    println!("commands: {:?}", cmds);
    let labels = latex.extracts_labels();
    println!("labels: {:?}", labels);
    let refs = latex.extracts_references();
    println!("references: {:?}", refs);
    println!("{}", latex.to_ebnf());
}
