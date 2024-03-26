# ltxprs
Minimal LaTeX parser

This is a minimal LaTeX parser. Its purpose is to perform very simple checks for a subset of the LaTeX syntax. It then extract a simple BNF grammer with all the parsed commands

Example of use

```rust
use ltxprs::LtxNode;

fn main() {
    let str = r#"
% comment
\ref{toto}        
\item a \\
$ \frac{a}{b} $
\label{toto}
\item {\blue {\b \ref{tata} \label{titi}}}
"#;
    let latex = LtxNode::new(str);
    let cmds = latex.extracts_commands();
    println!("commands: {:?}", cmds);
    let labels = latex.extracts_labels();
    println!("labels: {:?}", labels);
    let refs = latex.extracts_references();
    println!("references: {:?}", refs);
    assert_eq!(refs[1] , "\\ref{tata}".to_string());
    println!("{}",latex.to_ebnf());
}
```