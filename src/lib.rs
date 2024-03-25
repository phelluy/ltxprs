// library for parsing a LateX chunk
// the grammar of the chunk is (more or less) as follows
// in the ANTL format
#[allow(dead_code)]
const GRAMAR: &str = r#"
grammar latex;
latex: stuff;
stuff: (atom | construct)* ;
atom: command | comment | text ;  /* sans majuscules */
construct:  dmath | tmath | group | env;
dmath: '$$' stuff '$$' | '\\[' stuff '\\]' ;
tmath: '$' stuff '$' | '\\(' stuff '\\)' ;
group: '{' stuff '}' ;
env: Begin stuff End ;
Begin: '\\begin{'[a-zA-Z]+'*'?'}' ;
End: '\\end{'[a-zA-Z]+'*'?'}' ;
comment: Comment ;
Comment: '%'~[\n]*'\n' ;
command: Command;
Command: '\\'[\\&a-zA-Z]+ ;
text: Text ;
Text: (~[\\{}$%])+ ;
"#;
// however there is an error in this definition because
// the Begin and End in the env construct are not
// necessarily matching
// that's why we do it in Rust with nom and a recursive parser

// import exit function for debugging (sometimes)
#[allow(unused_imports)]
use std::process::exit;

#[allow(unused_imports)]
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, multispace0, none_of},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    IResult,
};

// The recursive structure that contains the whole AST
#[derive(Debug, PartialEq, Clone)]
enum LtxNode {
    Text(String),              // a text without any special character (no \{}$%)
    Comment(String),           // a comment starting with a % and ending with a \n
    Command(String),           // a command starting with a \ and followed by [a-zA-Z]+ or [\&{}[]]
    Group(Vec<LtxNode>),       // a group of nodes between { and }
    Math(Vec<LtxNode>),        // a math environment between $ and $ or \( and \)
    DisplayMath(Vec<LtxNode>), // a display math environment between $$ and $$ or \[ and \]
}


// implement the constructor for LtxNode
impl LtxNode {
    fn new(s: &str) -> LtxNode {
        let s = s.trim();
        // construct the string { s }
        let s = format!("{{{}}}", s);
        println!("new: {}", s);
        group_node(&s).unwrap().1
    }

    // iter in the ltxnode and extracts all the command names
    fn extracts_commands(&self) -> Vec<String> {
        let mut cmd_list = vec!();
        match self {
            LtxNode::Text(_) => (),
            LtxNode::Comment(_) => (),
            LtxNode::Command(s) => cmd_list.push(s.clone()),
            LtxNode::Group(v) => {
                for n in v {
                    cmd_list.append(&mut n.extracts_commands());
                }
            }
            LtxNode::Math(v) => {
                for n in v {
                    cmd_list.append(&mut n.extracts_commands());
                }
            }
            LtxNode::DisplayMath(v) => {
                for n in v {
                    cmd_list.append(&mut n.extracts_commands());
                }
            }
        }
        cmd_list
    }   
}

// parse a text until one of these character is encountered: \{}$%
fn text(input: &str) -> nom::IResult<&str, &str> {
    recognize(many1(none_of("\\{}$%")))(input)
}

// parse a text and produce a LtxNode::Text
fn text_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(text, |s: &str| LtxNode::Text(s.to_string()))(input)
}

// parse an ascii text preceded by a backslash
fn ascii_cmd(input: &str) -> nom::IResult<&str, &str> {
    preceded(tag("\\"), alpha1)(input)
}

// parse a double backslash
fn double_backslash(input: &str) -> nom::IResult<&str, &str> {
    tag("\\\\")(input)
}

// parse an ascii_cmd or a double_backslash
fn command(input: &str) -> nom::IResult<&str, &str> {
    alt((ascii_cmd, double_backslash))(input)
}

fn command_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(command, |s: &str| {
        // add "\\" at the beginning of the command
        // if the string is not already a double backslash
        let cs = if s == "\\\\" { s.to_string() } else { format!("\\{}", s) };
        LtxNode::Command(cs.to_string())
    })(input)
}

// parse until end of line
fn end_of_line(input: &str) -> nom::IResult<&str, &str> {
    recognize(many0(none_of("\n")))(input)
}

// parse a comment: anything between a % and a \n
fn comment(input: &str) -> nom::IResult<&str, &str> {
    preceded(tag("%"), end_of_line)(input)
}

fn comment_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(comment, |s: &str| LtxNode::Comment(s.to_string()))(input)
}

// parse an atom, which is a command, a comment or a text
fn atom_node(input: &str) -> nom::IResult<&str, LtxNode> {
    alt((command_node, comment_node, text_node))(input)
}

// parse a group of nodes recursively
fn group_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(
        delimited(char('{'), many0(alt((atom_node, group_node))), char('}')),
        |v| LtxNode::Group(v),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    // cargo command for running a test <nametest>
    // with results displayed
    // cargo test <nametest> -- --nocapture
    #[test]
    fn parse_text() {
        let str = "oulaOula";
        let res = text(str);
        assert_eq!(res, Ok(("", "oulaOula")));
        let str = "oulaOula%";
        let res = text_node(str);
        assert_eq!(res, Ok(("%", LtxNode::Text("oulaOula".to_string()))));
        assert_eq!(text("oulaOula%"), Ok(("%", "oulaOula")));
        assert_eq!(text("oula\\Oula"), Ok(("\\Oula", "oula")));
    }

    #[test]
    fn parse_ascii_cmd() {
        let str = "\\oula";
        let res = ascii_cmd(str);
        println!("{:?}", res);
        assert_eq!(res, Ok(("", "oula")));
        assert_eq!(ascii_cmd("\\oulaé%"), Ok(("é%", "oula")));
    }

    #[test]
    fn parse_end_of_line() {
        let str = "oulaOula\n";
        let res = end_of_line(str);
        assert_eq!(res, Ok(("\n", "oulaOula")));
        assert_eq!(end_of_line("oulaOula%"), Ok(("", "oulaOula%")));
        assert_eq!(end_of_line("oulaOula\\Oula"), Ok(("", "oulaOula\\Oula")));
    }

    #[test]
    fn parse_command() {
        let str = "\\oula";
        let res = command(str);
        println!("{:?}", res);
        assert_eq!(res, Ok(("", "oula")));
        assert_eq!(command("\\oulaé%"), Ok(("é%", "oula")));
        assert_eq!(command("\\\\oulaé%"), Ok(("oulaé%", "\\\\")));
    }

    #[test]
    fn parse_comment() {
        // let str = "aaaa%oula\n";
        // // assert comment(str) generates an error
        //assert_eq!(comment(str), Ok(("", str)));
        let str = "%oula\n";
        assert_eq!(comment(str), Ok(("\n", "oula")));
    }

    #[test]
    fn parse_atom() {
        let str = "aaaa%oula\n";
        assert_eq!(
            atom_node(str),
            Ok(("%oula\n", LtxNode::Text("aaaa".to_string())))
        );
        let str = "%oula\n\\toto";
        assert_eq!(
            atom_node(str),
            Ok(("\n\\toto", LtxNode::Comment("oula".to_string())))
        );
        let str = "\\oulaé";
        assert_eq!(
            atom_node(str),
            Ok(("é", LtxNode::Command("oula".to_string())))
        );
    }

    #[test]
    fn parse_group_node() {
        let str = "{\\item salut ça va ? % ouf tout va bien\n}";
        let grp = group_node(str);
        println!("{:?}", grp);
        assert_eq!(
            grp,
            Ok((
                "",
                LtxNode::Group(vec![
                    LtxNode::Command("item".to_string()),
                    LtxNode::Text(" salut ça va ? ".to_string()),
                    LtxNode::Comment(" ouf tout va bien".to_string()),
                    LtxNode::Text("\n".to_string()),
                ])
            ))
        );
    }

    #[test]
    fn recursive_test() {
        let str = r#"{
\item a
% rien
\item {\blue b}
}
        "#;
        let grp = group_node(str);
        println!("{:?}", grp);
    }

    #[test]
    fn new_ltx_test() {
        let str = r#"
           
\item a \\
% rien 
\item {\blue {\b}}
              
              "#;
        let latex = LtxNode::new(str);
        println!("{:?}", latex);
        let cmds = latex.extracts_commands();
        println!("{:?}", cmds);
    }
}
