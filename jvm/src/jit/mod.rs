use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::jit::scc::{enumerate_graph, StronglyConnectedComponents};
use std::borrow::BorrowMut;
use std::collections::{BTreeMap, HashMap};

mod scc;

pub type Index = usize;

#[derive(Clone, Debug)]
pub struct Node {
    pub edges: Vec<usize>,
    pub scc: usize,
    pub predecessors: Vec<usize>,
    pub idx: usize,
}

impl Node {
    pub fn new(instruction: &Instruction, mapping: &HashMap<u32, u32>, last_index: u32) -> Self {
        let edges = match instruction.bytecode {
            Bytecode::Goto_w(offset) => vec![
                (*mapping
                    .get(&((offset + instruction.bytes_index as i32) as u32))
                    .unwrap()) as usize,
            ],
            Bytecode::Goto(offset) => vec![
                (*mapping
                    .get(&(((offset as i32) + instruction.bytes_index as i32) as u32))
                    .unwrap()) as usize,
            ],
            Bytecode::If_acmpeq(offset)
            | Bytecode::If_acmpne(offset)
            | Bytecode::If_icmpeq(offset)
            | Bytecode::If_icmpne(offset)
            | Bytecode::If_icmplt(offset)
            | Bytecode::If_icmpge(offset)
            | Bytecode::If_icmpgt(offset)
            | Bytecode::If_icmple(offset)
            | Bytecode::Ifeq(offset)
            | Bytecode::Ifne(offset)
            | Bytecode::Iflt(offset)
            | Bytecode::Ifge(offset)
            | Bytecode::Ifgt(offset)
            | Bytecode::Ifle(offset)
            | Bytecode::Ifnonnull(offset)
            | Bytecode::Ifnull(offset) => {
                let jump = (*mapping
                    .get(&((offset as i32 + instruction.bytes_index as i32) as u32))
                    .unwrap()) as usize;

                if instruction.bytecode_index < last_index {
                    vec![jump, instruction.bytecode_index as usize + 1]
                } else {
                    vec![jump]
                }
            }
            Bytecode::Lookupswitch(_, _) => unimplemented!(),
            Bytecode::Tableswitch => unimplemented!(),
            Bytecode::Wide(_) => unimplemented!(),
            _ => {
                if instruction.bytecode_index < last_index {
                    vec![instruction.bytecode_index as usize + 1]
                } else {
                    vec![]
                }
            }
        };

        Self {
            edges,
            scc: 0,
            predecessors: vec![],
            idx: instruction.bytecode_index as usize,
        }
    }

    pub fn find_predecessors(&mut self, nodes: &[Node]) {
        nodes.iter().for_each(|node| {
            if node.edges.contains(&self.idx) {
                self.predecessors.push(node.idx);
            }
        })
    }
}

pub fn detect_irreducible_cfg(instructions: &[Instruction]) {
    let byte_index_to_instruction_index: HashMap<u32, u32> = instructions
        .iter()
        .map(|instruction| (instruction.bytes_index, instruction.bytecode_index))
        .collect();

    let mut scc = StronglyConnectedComponents::new(instructions.len());

    let mut nodes: Vec<Node> = instructions
        .iter()
        .map(|instruction| {
            Node::new(
                instruction,
                &byte_index_to_instruction_index,
                (instructions.len() - 1) as u32,
            )
        })
        .collect();
    let nodes_copy = nodes.clone();

    nodes
        .iter_mut()
        .for_each(|node| node.find_predecessors(&nodes_copy[..]));

    let tree = nodes
        .iter()
        .enumerate()
        .map(|(idx, node)| (idx, node.edges.clone()))
        .collect();

    let graph = enumerate_graph(&tree);

    scc.find_components(&graph);

    let mut scc_s = HashMap::new();

    scc.component
        .iter()
        .filter(|scc| **scc != 0)
        .enumerate()
        .for_each(|(node_index, scc)| {
            nodes[node_index].scc = *scc;
            scc_s.insert(*scc, vec![]);
        });

    nodes.iter().for_each(|node| {
        scc_s.get_mut(&node.scc).unwrap().push(node.idx);
    });

    let multiple_entry_loops: HashMap<usize, Vec<Node>> = scc_s
        .iter()
        .filter(|(scc, nodes)| nodes.len() > 1)
        .map(|(scc, node_idxs)| {
            let loop_headers: Vec<&Node> = node_idxs
                .iter()
                .filter_map(|idx| {
                    let node = &nodes[*idx];

                    let is_header = node.predecessors.iter().any(|pre| {
                        let predecessor = &nodes[*pre];
                        node.scc != predecessor.scc
                    });

                    if is_header {
                        Some(node)
                    } else {
                        None
                    }
                })
                .collect();

            (*scc, loop_headers)
        }).collect();

    println!("{:#?}", multiple_entry_loops);
}
