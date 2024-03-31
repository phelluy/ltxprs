//! This library is a parser for a subset of LateX.
//! It uses the nom library.
//! It can extract a W3C EBNF grammar of the latex parsed chunk.
//! # Examples
//! ```
//! use ltxprs::LtxNode;
//!let str = r#"
//!% comment
//!\ref{toto}        
//!\item a \\
//!$ \frac{a}{b} $
//!\label{toto}
//!\item {\blue {\b \ref{tata} \label{titi}}}
//! "#;
//!let latex = LtxNode::new(str);
//!println!("{:?}", latex);
//!let cmds = latex.extracts_commands();
//!println!("commands: {:?}", cmds);
//!let labels = latex.extracts_labels();
//!println!("labels: {:?}", labels);
//!let refs = latex.extracts_references();
//!println!("references: {:?}", refs);
//! println!("{}",latex.to_ebnf());
//!```

//the grammar of the chunk is as follows
// the list of possible commands is added afterward
#[allow(dead_code)]
const GRAMAR: &str = r#"
# W3C EBNF grammar of the Latex chunk
root ::= "\\begin{trsltx}" stuff "\\end{trsltx}"
stuff ::= (atom | construct)*
atom ::= command | text
construct ::= group | math
text ::= [^\\{}$%]+
group ::= "{" stuff "}"
math ::= ("$" stuff "$") | ("$$" stuff "$$")"#;
// example of a missing command line that will be added by the grammar
//command ::= "\\item"  | "\\begin" | "\\frac" | "\\label{eq:formule}" |
// "\\end" | "\\ref//{eq:autre_formule}" | "\\section" | "\\sqrt"

// import exit function for debugging (sometimes)
#[allow(unused_imports)]
use std::process::exit;
use std::{collections::HashSet, vec};

//use clap::Command;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, none_of},
    combinator::{map, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
};

use nom_locate::{position, LocatedSpan};


///The recursive structure that contains the whole AST
/// Remark: the Text node may contain \begin{} ... \end{} environments
/// including maths. Only the $...$ and $$...$$ are checked for now.
#[derive(Debug, PartialEq, Clone)]
pub enum LtxNode {
    Text(String),              // a text without any special character (no \{}$%)
    Comment(String),           // a comment starting with a % and ending with a \n
    Label(String),             // a label starting with \label{ and ending with }
    Reference(String),         // a reference starting with \ref{ and ending with }
    Cite(String),              // a citation starting with \cite{ and ending with }
    Command(String),           // a command starting with a \ and followed by [a-zA-Z]+ or [\&{}[]]
    Group(Vec<LtxNode>),       // a group of nodes between { and }
    Math(Vec<LtxNode>),        // a math environment between $ and $ or \( and \)
    DisplayMath(Vec<LtxNode>), // a display math environment between $$ and $$ or \[ and \]
    Problem(String),                      // when the syntaxic analysis fails
}
// take two non empty and sorted vectors of strings and count the number of different elements between them
// for this convert the vectors into multisets compute the difference and count the elements
fn distance(v1: &[String], v2: &[String]) -> usize {
    if v1.is_empty() {
        return v2.len();
    } else if v2.is_empty() {
        return v1.len();
    }
    // assert the vectors are sorted
    v1.iter().zip(v1.iter().skip(1)).for_each(|(s1, s2)| {
        assert!(s1 <= s2);
    });
    v2.iter().zip(v2.iter().skip(1)).for_each(|(s1, s2)| {
        assert!(s1 <= s2);
    });
    let mut count = 1;
    let mut v1count = vec![(1, &v1[0])];
    for i in 1..v1.len() {
        if v1[i] == v1[i - 1] {
            count += 1;
            v1count.push((count, &v1[i]));
        } else {
            count = 1;
            v1count.push((count, &v1[i]));
        }
    }

    count = 1;
    let mut v2count = vec![(1, &v2[0])];
    for i in 1..v2.len() {
        if v2[i] == v2[i - 1] {
            count += 1;
            v2count.push((count, &v2[i]));
        } else {
            count = 1;
            v2count.push((count, &v2[i]));
        }
    }

    // println!("v1count: {:?}", v1count);
    // println!("v2count: {:?}", v2count);
    let v1set: HashSet<(usize, &String)> = HashSet::from_iter(v1count);
    let v2set: HashSet<(usize, &String)> = HashSet::from_iter(v2count);
    // println!("v1set: {:?}", v1set);
    // println!("v2set: {:?}", v2set);
    let vdiff1 = v1set.difference(&v2set);
    let vdiff2 = v2set.difference(&v1set);
    // println!("vdiff1: {:?}", vdiff1);
    // println!("vdiff2: {:?}", vdiff2);
    vdiff1.count() + vdiff2.count()
}

impl LtxNode {
    pub fn new(s: &str) -> LtxNode {
        let s = s.trim();
        // construct the string {s} so that the head Node is a group.
        // the \n's are important for parsing initial or closing %'s
        let s = format!("{{\n{}\n}}", s);
        //println!("new: {}", s);
        let grpn = group_node(&s);
        match grpn {
            Ok((_, grpn)) => grpn,
            Err(err) => {
                println!("Syntax error of type: {:?}", err);
                //println!("Returning an empty analysis");
                //println!("The error occurs at: {}\n", s);
                println!("It is generally caused by non matching delimiters (worst case) or empty string (can be ignored)");
                println!("Note: pdflatex does not necessarily detect unbalanced delimiters");
                let serr = format!("{}\n", err);
                LtxNode::Problem(serr)
            }
        }
    }

    // distance between two LtxNode: it counts the number of different command, citations,
    // labels and references
    pub fn distance(&self, other: &LtxNode) -> usize {
        let cmds1 = self.extracts_commands();
        let cmds2 = other.extracts_commands();
        let dist1 = distance(&cmds1, &cmds2);
        let labels1 = self.extracts_labels();
        let labels2 = other.extracts_labels();
        let dist2 = distance(&labels1, &labels2);
        let refs1 = self.extracts_references();
        let refs2 = other.extracts_references();
        let dist3 = distance(&refs1, &refs2);
        let cites1 = self.extracts_citations();
        let cites2 = other.extracts_citations();
        let dist4 = distance(&cites1, &cites2);
        dist1 + dist2 + dist3 + dist4
    }

    // Print the LtxNode by iterating recursively on the nodes and printing the
    // corresponding string, if relevant
    pub fn print(&self) {
        match self {
            LtxNode::Problem(s) => print!("{}", s),
            LtxNode::Text(s) => print!("{}", s),
            LtxNode::Comment(s) => print!("{}", s),
            LtxNode::Label(s) => print!("{}", s),
            LtxNode::Reference(s) => print!("{}", s),
            LtxNode::Cite(s) => print!("{}", s),
            LtxNode::Command(s) => print!("{}", s),
            LtxNode::Group(v) => {
                print!("{{");
                for n in v {
                    n.print();
                }
                print!("}}");
            }
            LtxNode::Math(v) => {
                print!(" \\(");
                for n in v {
                    n.print();
                }
                print!("\\) ");
            }
            LtxNode::DisplayMath(v) => {
                print!(" \\[");
                for n in v {
                    n.print();
                }
                print!("\\] ");
            }
        }
    }

    // Print the LtxNode in a string by iterating recursively on the nodes
    // put a marker "%trsltx-split" when more than length characters reached
    // (the length is measured betwen the last "%trsltx-split" and the current node)
    // but without breaking a syntaxic unit
    // the string cannot be split if the recursing level is different from 1
    pub fn print_split(&self, level: usize, mut s_inout: String, length: usize) -> String {
        //print!("level: {} ", level);
        let split_level = 1;
        let mut split = true;

        match self {
            LtxNode::Problem(s) => s_inout.push_str(s),
            LtxNode::Text(s) => s_inout.push_str(s),
            LtxNode::Comment(s) => s_inout.push_str(s),
            LtxNode::Label(s) => {
                s_inout.push_str(s);
                split = false;
            }
            LtxNode::Reference(s) => {
                s_inout.push_str(s);
                split = false;
            }
            LtxNode::Cite(s) => {
                s_inout.push_str(s);
                split = false;
            }
            LtxNode::Command(s) => {
                s_inout.push_str(s);
                split = false;
            }
            LtxNode::Group(v) => {
                s_inout.push('{');
                for n in v {
                    s_inout = n.print_split(level + 1, s_inout, length);
                }
                s_inout.push('}');
            }
            LtxNode::Math(v) => {
                s_inout.push('$');
                for n in v {
                    s_inout = n.print_split(level + 1, s_inout, length);
                }
                s_inout.push('$');
            }
            LtxNode::DisplayMath(v) => {
                s_inout.push_str("$$");
                for n in v {
                    s_inout = n.print_split(level + 1, s_inout, length);
                }
                s_inout.push_str("$$");
            }
        }

        // extract the last part of the string after the last "%trsltx-split"
        let mut parts = s_inout.split("%trsltx-split");
        let nbparts = parts.clone().count();
        let lastpart = parts.nth(nbparts - 1).unwrap_or("");
        //println!("lastpart: {}", lastpart);

        //get the length since the last " %trsltx-split" or the beginning of s_inout
        let len = lastpart.len();

        if len < length {
            split = false;
        }

        split = split && level == split_level;

        // only cut if the last character in s_inout is a \n
        // and the split is allowed
        split = split && s_inout.ends_with("\n");

        if split {
            // count the number of \begin ... \end in lastpart
            // we use the syntax analysis for avoiding counting
            // the commented \begin and \end
            let ltxnode = LtxNode::new(lastpart);
            let cmds = ltxnode.extracts_commands_multi();
            let nbbegin = cmds.iter().filter(|&x| x.contains("\\begin")).count();
            let nbend = cmds.iter().filter(|&x| x.contains("\\end")).count();
            println!("nbbegin={}, nbend={}", nbbegin, nbend);
            split = nbbegin == nbend;
            // if !split {
            //     println!("No split here: nbbegin={}, nbend={}", nbbegin, nbend);
            // }
            // count the number of trsltx-begin-ignore
            let nbignore = lastpart.matches("%trsltx-begin-ignore").count();
            // count the number of trsltx-end-ignore
            let nbendignore = lastpart.matches("%trsltx-end-ignore").count();
            split = split && nbignore == nbendignore;
            // if nbignore != nbendignore {
            //     println!(
            //         "No split here: nbignore={}, nbendignore={}",
            //         nbignore, nbendignore
            //     );
            // }
        }

        if split {
            println!("reach {} characters, thus split", len);
            s_inout.push_str("\n%trsltx-split\n");
        }

        s_inout
    }

    // add the preamble and postamble to the split string
    // fn generate_split_latex(&self) -> String {
    //     let mut s = String::new();
    //     s= self.print_split(0, s, 4000);
    //     s
    // }

    ///Iters in the ltxnode and extracts all the command names
    /// possibly duplicated
    pub fn extracts_commands_multi(&self) -> Vec<String> {
        let mut cmd_list = vec![];
        match self {
            LtxNode::Problem(_) => (),
            LtxNode::Text(_) => (),
            LtxNode::Comment(_) => (),
            LtxNode::Label(_) => (),
            LtxNode::Reference(_) => (),
            LtxNode::Cite(_) => (),
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

    /// same as above but with no duplicate
    pub fn extracts_commands(&self) -> Vec<String> {
        let mut cmds = self.extracts_commands_multi();
        cmds.sort();
        cmds.dedup();
        cmds
    }

    ///Iters in the ltxnode and extracts all the labels with duplicates
    pub fn extracts_labels_multi(&self) -> Vec<String> {
        let mut label_list = vec![];
        match self {
            LtxNode::Problem(_) => (),
            LtxNode::Text(_) => (),
            LtxNode::Comment(_) => (),
            LtxNode::Command(_) => (),
            LtxNode::Reference(_) => (),
            LtxNode::Cite(_) => (),
            LtxNode::Label(s) => label_list.push(s.clone()),
            LtxNode::Group(v) => {
                for n in v {
                    label_list.append(&mut n.extracts_labels());
                }
            }
            LtxNode::Math(v) => {
                for n in v {
                    label_list.append(&mut n.extracts_labels());
                }
            }
            LtxNode::DisplayMath(v) => {
                for n in v {
                    label_list.append(&mut n.extracts_labels());
                }
            }
        }
        label_list
    }

    // same as above but with no duplicate
    pub fn extracts_labels(&self) -> Vec<String> {
        let mut labels = self.extracts_labels_multi();
        labels.sort();
        labels.dedup();
        labels
    }

    ///Iters in the ltxnode and extracts all the references with duplicates
    pub fn extracts_references_multi(&self) -> Vec<String> {
        let mut ref_list = vec![];
        match self {
            LtxNode::Problem(_) => (),
            LtxNode::Text(_) => (),
            LtxNode::Comment(_) => (),
            LtxNode::Command(_) => (),
            LtxNode::Label(_) => (),
            LtxNode::Reference(s) => ref_list.push(s.clone()),
            LtxNode::Cite(_) => (),
            LtxNode::Group(v) => {
                for n in v {
                    ref_list.append(&mut n.extracts_references());
                }
            }
            LtxNode::Math(v) => {
                for n in v {
                    ref_list.append(&mut n.extracts_references());
                }
            }
            LtxNode::DisplayMath(v) => {
                for n in v {
                    ref_list.append(&mut n.extracts_references());
                }
            }
        }
        ref_list
    }

    /// same as above but with no duplicate
    pub fn extracts_references(&self) -> Vec<String> {
        let mut refs = self.extracts_references_multi();
        refs.sort();
        refs.dedup();
        refs
    }

    ///Iters in the ltxnode and extracts all the citations with duplicates
    pub fn extracts_citations_multi(&self) -> Vec<String> {
        let mut cite_list = vec![];
        match self {
            LtxNode::Problem(_) => (),
            LtxNode::Text(_) => (),
            LtxNode::Comment(_) => (),
            LtxNode::Command(_) => (),
            LtxNode::Label(_) => (),
            LtxNode::Reference(_) => (),
            LtxNode::Cite(s) => cite_list.push(s.clone()),
            LtxNode::Group(v) => {
                for n in v {
                    cite_list.append(&mut n.extracts_citations());
                }
            }
            LtxNode::Math(v) => {
                for n in v {
                    cite_list.append(&mut n.extracts_citations());
                }
            }
            LtxNode::DisplayMath(v) => {
                for n in v {
                    cite_list.append(&mut n.extracts_citations());
                }
            }
        }
        cite_list
    }

    /// same as above but with no duplicate
    pub fn extracts_citations(&self) -> Vec<String> {
        let mut cites = self.extracts_citations_multi();
        cites.sort();
        cites.dedup();
        cites
    }

    /// Generate the W3C EBNF grammar of the latex chunk
    pub fn to_ebnf(&self) -> String {
        let s0 = GRAMAR.to_string();
        // append to s all the labels and command separated by "|"
        // on a single line, do it so that the backslashes are not removed
        let labels = self.extracts_labels();
        let refs = self.extracts_references();
        let cites = self.extracts_citations();
        let cmds = self.extracts_commands();
        // add a fake command for avoiding an empty list of commands
        let mut s = "\ncommand ::= \"\\commandevide\" | ".to_string();
        for l in labels {
            s = s + "\"" + l.clone().as_str() + "\"" + " | ";
        }
        for r in refs {
            s = s + "\"" + r.clone().as_str() + "\"" + " | ";
        }
        for r in cites {
            s = s + "\"" + r.clone().as_str() + "\"" + " | ";
        }
        for c in cmds {
            s = s + "\"" + c.clone().as_str() + "\"" + " | ";
        }
        // remove the trailing " | "
        s = s.trim_end_matches(" | ").to_string();
        // replace all the "\" by "\\"
        s = s.replace('\\', "\\\\");
        //let s = s0 + &s;
        //println!("{}", s);
        s0 + &s
    }
}


// nom_locate test: parse until a "a" is found
// little example for using nom locate
// maybe for later development
type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
struct Token<'a> {
    pub position: Span<'a>,
    pub s: &'a str,
}

fn parse_a(s: Span) -> nom::IResult<Span, Token> {
    let (s, _) = take_until("a")(s)?;
    let (s, pos) = position(s)?;
    let (s,a) = tag("a")(s)?;
    Ok((s, Token { position: pos, s: a.fragment() }))
}

fn parse_delimited(s: Span) -> nom::IResult<Span, Token> {
    let (s,_) = take_until("{")(s)?;
    let (s,pos) = position(s)?;
    let (_,_) = take_until("}")(s)?;
    let (s, res) = delimited(char('{'), many0(alpha1), char('}'))(s)?;
    let token = Token { position: pos, s: res.last().unwrap() };
    println!("{:?}", res);
    Ok((s, token))
}

///parse a text until one of these character is encountered: \{}$%
/// returns a String
/// if it ends with \n's a \n is appended to the string
/// if it starts with \n's a \n is prepended to the string
fn text(input: &str) -> nom::IResult<&str, String> {
    alt((
        // text that is a end of line after a special character
        map(
            terminated(recognize(many0(none_of("\\{}$%\n"))), many1(tag("\n"))),
            |s: &str| {
                let sn = format!("{}\n", s);
                //let sn = format!("type1: {}\n", s);
                sn.to_string()
            },
        ),
        // text that is a beginning of line before a special character
        map(
            preceded(many1(tag("\n")), recognize(many0(none_of("\\{}$%\n")))),
            |s: &str| {
                let sn = format!("\n{}", s);
                //let sn = format!("type2: \n{}", s);
                sn.to_string()
            },
        ),
        // text in a single line between two special characters
        map(recognize(many1(none_of("\\{}$%"))), |s: &str| {
            //let sn = format!("type 3: {}", s);
            let sn = format!("{}", s);
            sn.to_string()
        }),
    ))(input)
}

///parse a text and produce a LtxNode::Text
fn text_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(text, |s: String| LtxNode::Text(s))(input)
}

// ///parse a string that is neither  "ref" nor "label"
// fn no_ref_label_str(input: &str) -> nom::IResult<&str, &str> {
//     permutation((recognize(many1(none_of("label"))), recognize(many1(none_of("ref")))))(input)
// }

// // parse a string that is not "ref" and not "label" using the previous parser
// fn not_ref_label_str(input: &str) -> nom::IResult<&str, &str> {

// }

// parse an ascii command: a backslash followed by a string of letters
fn ascii_cmd(input: &str) -> nom::IResult<&str, &str> {
    preceded(char('\\'), alpha1)(input)
}

//parse an alphatext with this possible character: :-_
// fn label_text(input: &str) -> nom::IResult<&str, &str> {
//     recognize(many1(alt((alpha1, tag(":"), tag("-"), tag("_")))))(input)
// }
// label_text parser same as text parser
fn label_text(input: &str) -> nom::IResult<&str, &str> {
    recognize(many1(none_of("\\{}$%")))(input)
}

///parse a label_text enclosed in braces
fn label_braces(input: &str) -> nom::IResult<&str, &str> {
    delimited(char('{'), label_text, char('}'))(input)
}

///parse a label: a label_text with a \label prefix
fn label(input: &str) -> nom::IResult<&str, &str> {
    preceded(tag("\\label"), label_braces)(input)
}

///LtxNode version of the label parser
fn label_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(label, |s: &str| {
        // prepend \label{ and append }
        let cs = format!("\\label{{{}}}", s);
        LtxNode::Label(cs.to_string())
    })(input)
}

///parse a ref: a label_text with a \ref or \eqref prefix
fn ltxref(input: &str) -> nom::IResult<&str, &str> {
    preceded(alt((tag("\\eqref"), tag("\\ref"))), label_braces)(input)
    //preceded(tag("\\ref"), label_braces)(input)
}

///LtxNode version of the previous function
fn ltxref_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(ltxref, |s: &str| {
        // prepend \ref{ and append }
        let cs = format!("\\ref{{{}}}", s);
        //let cs = format!("\\eqref{{{}}}", s);
        LtxNode::Reference(cs.to_string())
    })(input)
}

///parse a cite: a label_text with a \cite prefix
fn cite(input: &str) -> nom::IResult<&str, &str> {
    preceded(tag("\\cite"), label_braces)(input)
}

///LtxNode version of the previous function
fn cite_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(cite, |s: &str| {
        // prepend \cite{ and append }
        let cs = format!("\\cite{{{}}}", s);
        LtxNode::Cite(cs.to_string())
    })(input)
}

///Parse a backslash followed by a special character: \{}()[]$&,;%@:-
/// and the accented characters: '`^"~
fn backslash_special(input: &str) -> nom::IResult<&str, &str> {
    alt((
        tag("\\\\"),
        tag("\\{"),
        tag("\\}"),
        // tag("\\("),  // BUG here --> math not detected TODO fix \left(  and \right)
        // tag("\\)"), // BUG here --> math not detected
        // tag("\\["),  // BUG here --> math not detected
        // tag("\\]"),  // BUG here --> math not detected
        tag("\\$"),
        tag("\\&"),
        tag("\\,"),
        tag("\\;"),
        tag("\\%"),
        tag("\\@"),
        tag("\\:"),
        tag("\\-"),
        tag("\\'"),
        tag("\\`"),
        tag("\\^"),
        tag("\\\""),
        tag("\\~"),
    ))(input)
    //tag("\\\\")(input)
}
// fn backslash_special(input: &str) -> nom::IResult<&str, &str> {
//     alt((tag("\\\\"), tag("\\$"), tag("\\&")))(input)
// }

///parse an ascii_cmd or a backslash_special
fn command(input: &str) -> nom::IResult<&str, &str> {
    alt((ascii_cmd, backslash_special))(input)
}

///parse a command and produce a LtxNode::Command
fn command_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(command, |s: &str| {
        // add "\\" at the beginning of the command
        // if the string is not already a backslash_special
        let cs = if s.starts_with('\\') {
            s.to_string()
        } else {
            format!("\\{}", s)
        };
        LtxNode::Command(cs.to_string())
    })(input)
}

///parse until end of line
fn end_of_line(input: &str) -> nom::IResult<&str, &str> {
    recognize(many0(none_of("\n")))(input)
}

///parse a comment: anything between a % and a \n
fn comment(input: &str) -> nom::IResult<&str, &str> {
    preceded(tag("%"), end_of_line)(input)
}

///parse a comment and produce a LtxNode::Comment
fn comment_node(input: &str) -> nom::IResult<&str, LtxNode> {
    map(comment, |s: &str| {
        //println!("comment");
        LtxNode::Comment(format!("%{}\n", s).to_string())
    })(input)
}

///parse a math node delimited by $ .. $ or \( .. \)
fn math_node(input: &str) -> nom::IResult<&str, LtxNode> {
    //println!("math");
    alt((
        map(
            delimited(tag("$"), many0(alt((atom_node, group_node))), tag("$")),
            LtxNode::Math,
        ),
        map(
            delimited(tag("\\("), many0(alt((atom_node, group_node))), tag("\\)")),
            LtxNode::Math,
        ),
    ))(input)
}

///parse a display math node delimited by $$ .. $$ or \[ .. \]
fn display_math_node(input: &str) -> nom::IResult<&str, LtxNode> {
    //println!("display math");
    alt((
        map(
            delimited(tag("$$"), many0(alt((atom_node, group_node))), tag("$$")),
            LtxNode::DisplayMath,
        ),
        map(
            delimited(tag("\\["), many0(alt((atom_node, group_node))), tag("\\]")),
            LtxNode::DisplayMath,
        ),
    ))(input)
}

///parse an atom, which is a command, a comment or a text or a math env
/// some remarks: math envs cannot be nested
fn atom_node(input: &str) -> nom::IResult<&str, LtxNode> {
    alt((
        comment_node, // the order is important
        text_node,
        ltxref_node,
        label_node,
        cite_node,
        command_node,
    ))(input)
}

///parse a group of nodes recursively
fn group_node(input: &str) -> nom::IResult<&str, LtxNode> {
    //println!("recursing");
    let res = map(
        delimited(
            char('{'),
            many0(alt((math_node, display_math_node, atom_node, group_node))),
            char('}'),
        ),
        |s| { LtxNode::Group(s) },
//        LtxNode::Group,
    )(input);
    match res {
        Ok(_) => {
            //println!("Ok: {:?}", &res);
            res
        }
        Err(_) => {
            //println!("Err: {:?}", &res);
            res
        }
    }
    // println!("input:\n{}\nTree:{:?}", input, res);
    // Ok(res)
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
        assert_eq!(res, Ok(("", "oulaOula".to_string())));
        let str = "oulaOula%";
        let res = text_node(str);
        assert_eq!(res, Ok(("%", LtxNode::Text("oulaOula".to_string()))));
        assert_eq!(text("oulaOula%"), Ok(("%", "oulaOula".to_string())));
        assert_eq!(text("oula\\Oula"), Ok(("\\Oula", "oula".to_string())));
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
            Ok(("\n\\toto", LtxNode::Comment("%oula\n".to_string())))
        );
        let str = "\\oulaé";
        assert_eq!(
            atom_node(str),
            Ok(("é", LtxNode::Command("\\oula".to_string())))
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
                    LtxNode::Command("\\item".to_string()),
                    LtxNode::Text(" salut ça va ? ".to_string()),
                    LtxNode::Comment("% ouf tout va bien\n".to_string()),
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
\ref{toto}        
\item a \\
% rien 
\label{toto}
\cite{tutu}
\item {\blue {\b \ref{tata} \label{titi}}}
              
              "#;
        let latex = LtxNode::new(str);
        println!("{:?}", latex);
        let cmds = latex.extracts_commands();
        println!("commands: {:?}", cmds);
        let labels = latex.extracts_labels();
        println!("labels: {:?}", labels);
        let refs = latex.extracts_references();
        println!("references: {:?}", refs);
    }

    #[test]
    fn test_math() {
        let str = r#"
\toto
\[ \int_0^1 f(x) dx \]
$ \frac{1}{2}$
"#;
        let latex = LtxNode::new(str);
        println!("{:?}", latex);
        let cmds = latex.extracts_commands();
        println!("commands: {:?}", cmds);
        let labels = latex.extracts_labels();
        println!("labels: {:?}", labels);
        let refs = latex.extracts_references();
        println!("references: {:?}", refs);
    }

    #[test]
    fn test_full() {
        let str = r#"
% comment
\ref{toto} 
\item a \\
$ \frac{a}{b} $
\label{toto}
\item {\blue {\b \ref{tata} \label{titi}}}
 "#;
        let latex = LtxNode::new(str);
        println!("{:?}", latex);
        let cmds = latex.extracts_commands();
        println!("commands: {:?}", cmds);
        let labels = latex.extracts_labels();
        println!("labels: {:?}", labels);
        let refs = latex.extracts_references();
        println!("references: {:?}", refs);
        println!("{}", latex.to_ebnf());
    }

    #[test]
    fn test_latex_split() {
        let str = r#"
\documentclass{article}
\begin{document}
\section{Introduction}
\label{sec:intro}
This is the introduction.
\section{Method}
\label{sec:method}
This is the method.
\end{document}
 "#;
        let s = String::new();
        let latex = LtxNode::new(str);
        let s = latex.print_split(1, s, 40);
        println!("{}", s);
    }

    #[test]
    fn test_distance() {
        let v1: Vec<String> = vec!["a", "a", "b", "c", "d"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let v2: Vec<String> = vec!["a", "b", "b", "c", "d"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        let d = distance(&v1, &v2);
        assert_eq!(d, 2);
        let v2: Vec<String> = vec![];
        let d = distance(&v1, &v2);
        assert_eq!(d, 5);
        let d = distance(&v2, &v1);
        assert_eq!(d, 5);
        //let v2: Vec<String> = vec!["bb".to_string(),"ba".to_string()];
        // must fail because the vectors are not sorted
        //let d = distance(&v1, &v2);
    }

    #[test]
    fn test_distance_ltxnode() {
        let str1 = r#"
        \begin{document}
        \section{Introduction}
        \cite{tutu}
        $y=cos(x)$
        \end{document}
        "#;

        let str2 = r#"
        \begin{document}
        \section{Introduction}
        \cite{toto}
        $y=sin(x)$
        \noindent
        \end{document}
        "#;

        let ltx1 = LtxNode::new(str1);
        let ltx2 = LtxNode::new(str2);
        let d = ltx1.distance(&ltx2);
        assert_eq!(d, 3);
    }

    #[test]
    fn test_locate() {
        let str = Span::new("123");
        let res = parse_a(str);
        println!("{:?}", res);
    }

    #[test]
    fn test_delimited() {
        let str = Span::new("\n\n{abcd}");
        let res = parse_delimited(str);
        println!("{:?}", res);
    }


}
