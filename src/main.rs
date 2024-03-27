use ltxprs::LtxNode;

fn main() {
    let str = r#"
% comment
\ref{toto}        
\item a \\
\{ oh \$ \& \}
$ \frac{a}{b} $
\label{toto}
\item {\blue {\b \ref{tata} \label{titi}}}
    "#;
    // read the "test/simple_fr.tex" file
    let str = std::fs::read_to_string("test/simple_fr.tex").unwrap();
    let latex = LtxNode::new(&str);
    let cmds = latex.extracts_commands();
    println!("commands: {:?}", cmds);
    let labels = latex.extracts_labels();
    println!("labels: {:?}", labels);
    let refs = latex.extracts_references();
    println!("references: {:?}", refs);
    println!("{}", latex.to_ebnf());
}
