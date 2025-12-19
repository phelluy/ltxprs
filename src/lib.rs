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
math ::= ("$$" stuff "$$") | ("$" stuff "$") | ("\\(" stuff "\\)") | ("\\[" stuff "\\]")"#;
// example of a missing command line that will be added by the grammar
//command ::= "\\item"  | "\\begin" | "\\frac" | "\\label{eq:formule}" |
// "\\end" | "\\ref//{eq:autre_formule}" | "\\section" | "\\sqrt"

// import exit function for debugging (sometimes)
#[allow(unused_imports)]
use std::process::exit;
use std::{collections::HashSet, vec};

//use clap::Command;
#[allow(unused_imports)]
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, none_of},
    combinator::{map, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    Parser,
};

// get a slice of the nth first elements of a char str or the whole string
// if it is empty or too short
fn str_start(s: &str, n: usize) -> String {
    let s = s.to_string();
    if s.is_empty() {
        return s;
    }
    if s.len() < n {
        return s;
    }
    s.chars().take(n).collect()
}

use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

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
    Problem(String),           // when the syntaxic analysis fails
}
// take two non empty and sorted vectors of strings and count the number of different elements between them
// for this convert the vectors into multisets compute the difference and count the elements
fn distance(v1: &[String], v2: &[String]) -> usize {
    if v1.is_empty() {
        return v2.len();
    } else if v2.is_empty() {
        return v1.len();
    }
    // Clone and sort to safely ensure order without panicking
    let mut v1 = v1.to_vec();
    v1.sort();
    let mut v2 = v2.to_vec();
    v2.sort();

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
        // The \n's are important for parsing initial or closing %'s
        let s = format!("\n{}\n", s);
        let span = Span::new(s.as_str());
        
        // Use a root parser logic that doesn't expect braces
        let res = many0(alt((display_math_node, math_node, atom_node, group_node))).parse(span);

        match res {
            Ok((s, children)) => {
                if !s.fragment().trim().is_empty() {
                    // This happens if parsing stopped prematurely (should not happen with many0 unless error)
                    let scut = str_start(s.fragment(), 50);
                    println!("Warning: parsing stopped with remaining content: \"{}...\"", scut);
                };
                LtxNode::Group(children)
            }
            Err(err) => {
                let (input, code) = match err {
                    nom::Err::Incomplete(_) => return LtxNode::Problem("Incomplete input".to_string()),
                    nom::Err::Error(e) => (e.input, e.code),
                    nom::Err::Failure(e) => (e.input, e.code),
                };

                let line_num = input.location_line();
                let col_num = input.get_column();
                
                // Extract the specific line from the original string `s` for context
                let line_content = s.lines().nth(line_num as usize - 1).unwrap_or("");
                
                // Create a pointer string like "      ^"
                let pointer: String = std::iter::repeat(' ').take(col_num - 1).chain(std::iter::once('^')).collect();

                // Custom message mapping
                let msg = match code {
                    nom::error::ErrorKind::Char => "Unexpected character or missing closing delimiter (e.g. '}')",
                    nom::error::ErrorKind::Eof => "Unexpected end of file",
                    _ => "Parse error",
                };

                let serr = format!(
                    "Error at line {}, col {}: {}\n{}\n{}\n{:?}",
                    line_num, col_num, msg, line_content, pointer, code
                );
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
                print!("\\(");
                for n in v {
                    n.print();
                }
                print!("\\)");
            }
            LtxNode::DisplayMath(v) => {
                print!("\\[");
                for n in v {
                    n.print();
                }
                print!("\\]");
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
                s_inout.push_str("\\(");
                for n in v {
                    s_inout = n.print_split(level + 1, s_inout, length);
                }
                s_inout.push_str("\\)");
            }
            LtxNode::DisplayMath(v) => {
                s_inout.push_str("\\[");
                for n in v {
                    s_inout = n.print_split(level + 1, s_inout, length);
                }
                s_inout.push_str("\\]");
            }
        }

        // extract the last part of the string after the last "%trsltx-split"
        let mut parts = s_inout.split("%trsltx-split");
        let nbparts = parts.clone().count();
        let lastpart = parts.nth(nbparts - 1).unwrap_or("");
        //println!("lastpart: {}", lastpart);

        //get the length since the last " %trsltx-split" or the beginning of s_inout
        let len = lastpart.len();

        let mut length_reached = len >= length;
        
        if !length_reached {
            split = false;
        }

        // IMPORTANT: Only split at the ROOT level (level 0)
        // This ensures we are not splitting inside ANY LaTeX structure (group, math, environment, etc.)
        // recursive calls naturally increment 'level', so 'level == 0' means we are at the top level of the document body.
        split = split && level == 1;

        if split {
            let lastpart = lastpart.strip_prefix('{').unwrap_or(lastpart);
            let nbignore = lastpart.matches("%trsltx-begin-ignore").count();
            let nbendignore = lastpart.matches("%trsltx-end-ignore").count();
            
            // Do not split inside an ignored region
            if nbignore != nbendignore {
                split = false;
            }

            if split {
                let ltxnode = LtxNode::new(lastpart);
                let cmds = ltxnode.extracts_commands_multi();
                let nbbegin = cmds.iter().filter(|&x| x.contains("\\begin")).count();
                let nbend = cmds.iter().filter(|&x| x.contains("\\end")).count();
                
                // Do not split inside an environment like \begin{} \end{}
                // This is a backup check, as level==0 should already handle most cases, 
                // but some environments might be parsed as a sequence of nodes at root level if not enclosed in a single group
                if nbbegin != nbend {
                    split = false;
                }
            }
            
            if split {
                // Heuristic: check if we are at a good semantic break
                // 1. Check for empty line (paragraph break)
                let is_paragraph_break = s_inout.ends_with("\n\n") || s_inout.ends_with("\r\n\r\n");
                
                // 2. Check for sectioning command in the current node
                let is_section_command = match self {
                    LtxNode::Command(s) => {
                         s.contains("\\section") || s.contains("\\chapter") || s.contains("\\subsection") || s.contains("\\item")
                    },
                    _ => false
                };

                // 3. Check for sentence-ending punctuation inside math or text
                // We check if the LAST node processed (prior to this potential split point) ended with punctuation.
                // However, 'self' here is the CURRENT node being processed *appended* to s_inout.
                // Actually, 'self' is the node we just appended or are about to append? 
                // Looking at the code: s_inout.push_str(s) happens BEFORE this check for simple nodes like Text/Command.
                // For recursive nodes (Group/Math), recursive print_split is called.
                // So checking 's_inout' content is more reliable for "ending with punctuation".
                
                let s_trimmed = s_inout.trim_end();
                let ends_with_punctuation = s_trimmed.ends_with('.') || s_trimmed.ends_with('?') || s_trimmed.ends_with('!');

                // If we are over the length limit, ONLY split if we found a valid break point
                if !is_paragraph_break && !is_section_command && !ends_with_punctuation {
                    split = false;
                }
            }
        }

        if split {
            println!("Split triggered at length {} (boundary found)\nLast chars: {:?}", len, &s_inout[s_inout.len().saturating_sub(20)..]);
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
// not used: maybe for later developments...
// type Span<'a> = LocatedSpan<&'a str>;

// #[derive(Debug)]
// struct Token<'a> {
//     pub position: Span<'a>,
//     pub s: &'a str,
// }

// fn parse_a(s: Span) -> nom::IResult<Span, Token> {
//     let (s, _) = take_until("a")(s)?;
//     let (s, pos) = position(s)?;
//     let (s, a) = tag("a")(s)?;
//     Ok((
//         s,
//         Token {
//             position: pos,
//             s: a.fragment(),
//         },
//     ))
// }

// fn parse_delimited(s: Span) -> nom::IResult<Span, Token> {
//     let (s, _) = take_until("{")(s)?;
//     let (s, pos) = position(s)?;
//     let (_, _) = take_until("}")(s)?;
//     let (s, res) = delimited(char('{'), many0(alpha1), char('}'))(s)?;
//     let token = Token {
//         position: pos,
//         s: res.last().unwrap(),
//     };
//     println!("{:?}", res);
//     Ok((s, token))
// }

///parse a text until one of these character is encountered: \{}$%
/// returns a String
/// if it ends with \n's a \n is appended to the string
/// if it starts with \n's a \n is prepended to the string
fn text(input: Span) -> nom::IResult<Span, String> {
    alt((
        // text that is a end of line after a special character
        map(
            terminated(recognize(many0(none_of("\\{}$%\n"))), many1(tag("\n"))),
            |s: Span| {
                let sn = format!("{}\n", s);
                //let sn = format!("type1: {}\n", s);
                sn.to_string()
            },
        ),
        // text that is a beginning of line before a special character
        map(
            preceded(many1(tag("\n")), recognize(many0(none_of("\\{}$%\n")))),
            |s: Span| {
                let sn = format!("\n{}", s);
                sn.to_string()
            },
        ),
        // text in a single line between two special characters
        map(recognize(many1(none_of("\\{}$%"))), |s: Span| {
            //let sn = format!("type 3: {}", s);
            s.to_string()
        }),
    )).parse(input)
}

///parse a text and produce a LtxNode::Text
fn text_node(input: Span) -> nom::IResult<Span, LtxNode> {
    //println!("text: {}\n((((((((((((((((((((", input);
    // let res = 
    map(text, |s: String| LtxNode::Text(s)).parse(input)//;
    // match res {
    //     Ok(ref resu) => println!("text ok, reste:{}\n))))))))))))))))", resu.0),
    //     Err(_) => println!("text fail\n)))))))))))))))))"),
    // };
    //res
}

// ///parse a string that is neither  "ref" nor "label"
// fn no_ref_label_str(input: &str) -> nom::IResult<&str, &str> {
//     permutation((recognize(many1(none_of("label"))), recognize(many1(none_of("ref")))))(input)
// }

// // parse a string that is not "ref" and not "label" using the previous parser
// fn not_ref_label_str(input: &str) -> nom::IResult<&str, &str> {

// }

// parse an ascii command: a backslash followed by a string of letters
fn ascii_cmd(input: Span) -> nom::IResult<Span, Span> {
    preceded(char('\\'), alpha1).parse(input)
}

//parse an alphatext with this possible character: :-_
// fn label_text(input: &str) -> nom::IResult<&str, &str> {
//     recognize(many1(alt((alpha1, tag(":"), tag("-"), tag("_")))))(input)
// }
// label_text parser same as text parser
fn label_text(input: Span) -> nom::IResult<Span, Span> {
    recognize(many1(none_of("\\{}$%"))).parse(input)
}

///parse a label_text enclosed in braces
fn label_braces(input: Span) -> nom::IResult<Span, Span> {
    delimited(char('{'), label_text, char('}'))
    .parse(input)
}

///parse a label: a label_text with a \label prefix
fn label(input: Span) -> nom::IResult<Span, Span> {
    preceded(tag("\\label"), label_braces).parse(input)
}

///LtxNode version of the label parser
fn label_node(input: Span) -> nom::IResult<Span, LtxNode> {
    map(label, |s: Span| {
        // prepend \label{ and append }
        let cs = format!("\\label{{{}}}", s.to_string());
        LtxNode::Label(cs.to_string())
    }).parse(input)
}

///parse a ref: a label_text with a \ref or \eqref or \Cref prefix
fn ltxref(input: Span) -> nom::IResult<Span, Span> {
    preceded(
        alt((tag("\\eqref"), tag("\\ref"), tag("\\Cref"))),
        label_braces,
    ).parse(input)
    //preceded(tag("\\ref"), label_braces)(input)
}

///LtxNode version of the previous function
fn ltxref_node(input: Span) -> nom::IResult<Span, LtxNode> {
    map(ltxref, |s: Span| {
        // prepend \ref{ and append }
        let cs = format!("\\ref{{{}}}", s.to_string());
        //let cs = format!("\\eqref{{{}}}", s);
        LtxNode::Reference(cs.to_string())
    }).parse(input)
}

///parse a cite: a label_text with a \cite prefix
fn cite(input: Span) -> nom::IResult<Span, Span> {
    preceded(tag("\\cite"), label_braces).parse(input)
}

///LtxNode version of the previous function
fn cite_node(input: Span) -> nom::IResult<Span, LtxNode> {
    map(cite, |s: Span| {
        // prepend \cite{ and append }
        let cs = format!("\\cite{{{}}}", s.to_string());
        LtxNode::Cite(cs.to_string())
    }).parse(input)
}

///Parse a backslash followed by a special character: \{}()[]$&,;%@:-
/// and the accented characters: '`^"~
fn backslash_special(input: Span) -> nom::IResult<Span, Span> {
    alt((
        // not to be detected because of math mode !
        // tag("\\("),  begin math
        // tag("\\)"),  end math
        // tag("\\["),  begin display math
        // tag("\\]"),  end display math
        //////////////////////////////
        // special symbols
        tag("\\\\"),
        tag("\\{"),
        tag("\\}"),
        tag("\\$"),
        tag("\\&"),
        tag("\\%"),
        tag("\\@"),
        tag("\\-"),
        tag("\\|"),
        tag("\\#"),
        // accents
        tag("\\'"),
        tag("\\`"),
        tag("\\^"),
        tag("\\\""),
        tag("\\~"),
        // spaces
        tag("\\ "),
        tag("\\,"),
        tag("\\:"),
        tag("\\;"),
        tag("\\!"),
        tag("\\!"),
    )).parse(input)
    //tag("\\\\")(input)
}
// fn backslash_special(input: &str) -> nom::IResult<&str, &str> {
//     alt((tag("\\\\"), tag("\\$"), tag("\\&")))(input)
// }

///parse an ascii_cmd or a backslash_special
fn command(input: Span) -> nom::IResult<Span, Span> {
    alt((ascii_cmd, backslash_special)).parse(input)
}

///parse a command and produce a LtxNode::Command
fn command_node(input: Span) -> nom::IResult<Span, LtxNode> {
    map(command, |s: Span| {
        // add "\\" at the beginning of the command
        // if the string is not already a backslash_special
        let cs = if s.starts_with('\\') {
            s.to_string()
        } else {
            format!("\\{}", s.to_string())
        };
        LtxNode::Command(cs.to_string())
    }).parse(input)
}

///parse until end of line
fn end_of_line(input: Span) -> nom::IResult<Span, Span> {
    recognize(many0(none_of("\n"))).parse(input)
}

///parse a comment: anything between a % and a \n
fn comment(input: Span) -> nom::IResult<Span, Span> {
    preceded(tag("%"), end_of_line).parse(input)
}

///parse a comment and produce a LtxNode::Comment
fn comment_node(input: Span) -> nom::IResult<Span, LtxNode> {
    map(comment, |s: Span| {
        //println!("comment");
        LtxNode::Comment(format!("%{}\n", s.to_string()).to_string())
    }).parse(input)
}

///parse a math node delimited by $ .. $ or \( .. \)
fn math_node(input: Span) -> nom::IResult<Span, LtxNode> {
    //println!("math");
    alt((
        map(
            // appreciate the many1 instead of many0 !
            delimited(tag("$"), many1(alt((atom_node, group_node))), tag("$")),
            LtxNode::Math,
        ),
        map(
            delimited(tag("\\("), many0(alt((atom_node, group_node))), tag("\\)")),
            LtxNode::Math,
        ),
    )).parse(input)
}

///parse a display math node delimited by $$ .. $$ or \[ .. \]
fn display_math_node(input: Span) -> nom::IResult<Span, LtxNode> {
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
    )).parse(input)
}

///parse an atom, which is a command, a comment or a text or a math env
/// some remarks: math envs cannot be nested
fn atom_node(input: Span) -> nom::IResult<Span, LtxNode> {
    alt((
        comment_node, // the order is important
        text_node,
        ltxref_node,
        label_node,
        cite_node,
        command_node,
    )).parse(input)
}

///parse a group of nodes recursively
fn group_node(input: Span) -> nom::IResult<Span, LtxNode> {
    // Manually parse to capture the start position of the group
    let (s, start_pos) = nom_locate::position(input)?;
    let (s, _) = char('{')(s)?;
    
    // Parse content (potentially empty)
    let (s, children) = many0(alt((display_math_node, math_node, atom_node, group_node))).parse(s)?;
    
    // Parse closing brace with error handling
    match char::<Span, nom::error::Error<Span>>('}')(s) {
        Ok((s, _)) => Ok((s, LtxNode::Group(children))),
        Err(_) => {
            // If the closing brace is missing, we report the error AT THE OPENING BRACE
            // We construct a Failure error pointing to start_pos
            Err(nom::Err::Failure(nom::error::Error::new(start_pos, nom::error::ErrorKind::Char)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // cargo command for running a test <nametest>
    // with results displayed
    // cargo test <nametest> -- --nocapture
    fn check<T: std::fmt::Debug + PartialEq>(res: nom::IResult<Span, T>, expected_rem: &str, expected_val: T) {
        let (rem, val) = res.expect("Parser failed");
        assert_eq!(*rem.fragment(), expected_rem, "Remaining input mismatch");
        assert_eq!(val, expected_val, "Value mismatch");
    }

    fn check_span(res: nom::IResult<Span, Span>, expected_rem: &str, expected_val: &str) {
        let (rem, val) = res.expect("Parser failed");
        assert_eq!(*rem.fragment(), expected_rem, "Remaining input mismatch");
        assert_eq!(*val.fragment(), expected_val, "Value mismatch");
    }

    #[test]
    fn parse_text() {
        let str = "oulaOula";
        let res = text(Span::new(str));
        check(res, "", "oulaOula".to_string());
        
        let str = "oulaOula%";
        let res = text_node(Span::new(str));
        check(res, "%", LtxNode::Text("oulaOula".to_string()));

        check(text(Span::new("oulaOula%")), "%", "oulaOula".to_string());
        check(text(Span::new("oula\\Oula")), "\\Oula", "oula".to_string());
    }

    #[test]
    fn parse_bb() {
        let str = "{}";
        let res = group_node(Span::new(str));
        println!("void group:{:?}", res);
        check(res, "", LtxNode::Group([].to_vec()));

        let str = "oulaOula%";
        let res = text_node(Span::new(str));
        check(res, "%", LtxNode::Text("oulaOula".to_string()));

        check(text(Span::new("oulaOula%")), "%", "oulaOula".to_string());
        check(text(Span::new("oula\\Oula")), "\\Oula", "oula".to_string());
    }

    #[test]
    fn parse_group_text() {
        let str = "{\\\n1{2{2}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        let res = LtxNode::new(str); // calls new which handles Span
        println!("{:?}", res);
    }

    #[test]
    fn parse_ascii_cmd() {
        let str = "\\oula";
        let res = ascii_cmd(Span::new(str));
        println!("{:?}", res);
        check_span(res, "", "oula");
        
        let res = ascii_cmd(Span::new("\\oulaé%"));
        check_span(res, "é%", "oula");
    }

    #[test]
    fn parse_end_of_line() {
        let str = "oulaOula\n";
        let res = end_of_line(Span::new(str));
        check_span(res, "\n", "oulaOula");
        
        check_span(end_of_line(Span::new("oulaOula%")), "", "oulaOula%");
        check_span(end_of_line(Span::new("oulaOula\\Oula")), "", "oulaOula\\Oula");
    }

    #[test]
    fn parse_command() {
        let str = "\\oula";
        let res = command(Span::new(str));
        println!("{:?}", res);
        check_span(res, "", "oula");
        
        check_span(command(Span::new("\\oulaé%")), "é%", "oula");
        check_span(command(Span::new("\\\\oulaé%")), "oulaé%", "\\\\");
    }

    #[test]
    fn parse_comment() {
        let str = "%oula\n";
        check_span(comment(Span::new(str)), "\n", "oula");
    }

    #[test]
    fn parse_atom() {
        let str = "aaaa%oula\n";
        check(
            atom_node(Span::new(str)),
            "%oula\n", 
            LtxNode::Text("aaaa".to_string())
        );

        let str = "%oula\n\\toto";
        check(
            atom_node(Span::new(str)),
            "\n\\toto",
            LtxNode::Comment("%oula\n".to_string())
        );

        let str = "\\oulaé";
        check(
            atom_node(Span::new(str)),
            "é",
            LtxNode::Command("\\oula".to_string())
        );
    }

    #[test]
    fn parse_group_node() {
        let str = "{\\item salut ça va ? % ouf tout va bien\n}";
        let grp = group_node(Span::new(str));
        println!("{:?}", grp);
        check(
            grp,
            "",
            LtxNode::Group(vec![
                LtxNode::Command("\\item".to_string()),
                LtxNode::Text(" salut ça va ? ".to_string()),
                LtxNode::Comment("% ouf tout va bien\n".to_string()),
                LtxNode::Text("\n".to_string()),
            ])
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
        let grp = group_node(Span::new(str));
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

    // #[test]
    // fn test_locate() {
    //     let str = Span::new("123");
    //     let res = parse_a(str);
    //     println!("{:?}", res);
    // }

    // #[test]
    // fn test_delimited() {
    //     let str = Span::new("\n\n{abcd}");
    //     let res = parse_delimited(str);
    //     println!("{:?}", res);
    // }
}
