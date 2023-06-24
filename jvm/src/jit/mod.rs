use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::jit::scc::{enumerate_graph, StronglyConnectedComponents};
use std::borrow::BorrowMut;
use std::collections::{BTreeMap, HashMap};
use std::time::Instant;

mod scc;

pub type Index = usize;

#[derive(Clone, Debug)]
pub struct ComponentNode {
    pub edges: Vec<usize>,
    pub scc: usize,
    pub predecessors: Vec<usize>,
    pub idx: usize,
}

impl ComponentNode {
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

    pub fn find_predecessors(&mut self, nodes: &[ComponentNode]) {
        nodes.iter().for_each(|node| {
            if node.edges.contains(&self.idx) {
                self.predecessors.push(node.idx);
            }
        })
    }
}

#[derive(Debug)]
pub struct Loop {
    pub scc: usize,
    pub nodes: Vec<ComponentNode>,
    pub headers: Vec<ComponentNode>,
    pub invokers: Vec<ComponentNode>
}

pub fn find_loops(instructions: &[Instruction]) -> (HashMap<usize, Loop>, Vec<ComponentNode>, HashMap<usize, Vec<usize>>) {
    let byte_index_to_instruction_index: HashMap<u32, u32> = instructions
        .iter()
        .map(|instruction| (instruction.bytes_index, instruction.bytecode_index))
        .collect();

    let mut scc = StronglyConnectedComponents::new(instructions.len());

    let mut nodes: Vec<ComponentNode> = instructions
        .iter()
        .map(|instruction| {
            ComponentNode::new(
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

    (scc_s
        .iter()
        .filter(|(scc, nodes)| nodes.len() > 1)
        .map(|(scc, node_idxs)| {
            let (loop_headers, invokers): (Vec<ComponentNode> ,Vec<Vec<usize>>) = node_idxs
                .iter()
                .filter_map(|idx| {
                    let node = &nodes[*idx];

                    let predecessors: Vec<usize> = node.predecessors.iter().filter(|&pre| {
                        let predecessor = &nodes[*pre];
                        node.scc != predecessor.scc
                    }).copied().collect();

                    if predecessors.len() > 0 {
                        Some((node.clone(), predecessors))
                    } else {
                        None
                    }
                })
                .unzip();

            let mut invokers: Vec<usize> = invokers.into_iter().flatten().collect();
            invokers.dedup();

            (*scc, Loop {
                scc: *scc,
                nodes: scc_s[scc].iter().map(|idx| nodes[*idx].clone()).collect(),
                headers: loop_headers,
                invokers: invokers.into_iter().map(|idx| nodes[idx].clone()).collect(),
            })
        }).collect(), nodes, scc_s)
}

pub fn label_nodes(loops: &HashMap<usize, Loop>, nodes: &[ComponentNode], scc_s: &HashMap<usize, Vec<usize>>) -> Vec<LabeledNode> {

    //Loops with multiple entries must be fixed, as well as loops with a single entry but said entry
    // is not the earliest instruction in the loop, e.g.
    // A -> C
    // B -> C
    // C -> B
    // C is a loop entry but B is the first instruction in the loop

    // let now = Instant::now();

    let irreducible: HashMap<_, _> = loops.iter().filter(|(scc_idx, loop_)| {
        if loop_.headers.len() > 1 {
            return true;
        }

        let scc = scc_s.get(scc_idx).unwrap();

        for node in loop_.headers.iter() {
            if node.idx != scc[0] {
                return true;
            }
        }

        false
    }).collect();

    let mut edges: Vec<usize> = nodes.iter().map(|node| {
        node.edges.iter().filter(|&&edge| edge != node.idx + 1)
    }).flatten().copied().collect();

    edges.sort();

    let labeled: Vec<LabeledNode> = nodes.iter().map(|node| {

        let node_scc = &scc_s[&node.scc];

        if node_scc.len() > 1 && node_scc[0] == node.idx && irreducible.contains_key(&node.scc) { //This node belongs to a loop and it's the first node in said loop and is irreducible
            
            return LabeledNode::LoopController {
                node: node.clone(),
                targets: vec![], //this is figured out in another pass
            };
            
        }

        for &edge in node.edges.iter() {
            let target = &nodes[edge];
            let target_scc = &scc_s[&target.scc];

            if node.idx == target.idx - 1 { //not a jump, just a normal progression of instructions
                continue;
            }

            if target.scc != node.scc { //This instruction is a jump, and said jump isn't backwards (a loop)

                //This instruction jumps into the middle of a loop
                return if target_scc.len() > 1 && target_scc[0] != target.idx {
                    LabeledNode::LabeledControlFlow {
                        node: node.clone(),
                        label: target.scc,
                        target: edge,
                    }
                } else { //Doesn't jump into a loop, or if it does, it only jumps right before the loop header, which is fine. This is a block
                    //This will never be a backwards jump (a loop) because otherwise they would be in the same group of strongly connected components
                    LabeledNode::BlockJump {
                        node: node.clone(),
                        target: edge,
                    }
                }

            } else if target.scc == node.scc {
                return if edge < node.idx { //This jumps backwards within its own scc

                    //Jumps somewhere that isn't the beginning of the loop
                    if target.idx != target_scc[0] {
                        LabeledNode::LabeledControlFlow {
                            node: node.clone(),
                            label: target.scc,
                            target: edge,
                        }
                    } else {
                        //Jumps cleanly to the beginning of the loop, aka a plain old continue statement
                        LabeledNode::LoopContinue {
                            node: node.clone(),
                        }
                    }
                } else { //This jumps forwards within it's own scc
                    LabeledNode::BlockJump {
                        node: node.clone(),
                        target: edge,
                    }
                }

            }
        }

        if edges.binary_search(&node.idx).is_ok() {
            LabeledNode::NonCondensibleNonControlFlow {
                node: node.clone(),
            }
        } else {
            LabeledNode::CondensibleNonControlFlow {
                node: node.clone(),
            }
        }

    }).collect();

    labeled

}

#[derive(Debug)]
pub enum LabeledNode {
    LoopController {
        node: ComponentNode,
        targets: Vec<usize>
    },
    LoopContinue {
        node: ComponentNode,
    },
    LabeledControlFlow {
        node: ComponentNode,
        label: usize, //index of the scc it wants to jump into
        target: usize //the node it wants to reach
    },
    BlockJump {
        node: ComponentNode,
        target: usize
    },
    ReducibleControlFlow {
        node: ComponentNode,
        target: usize
    },
    //Nothing jumps to this instruction
    CondensibleNonControlFlow {
        node: ComponentNode
    },
    //Something jumps to this instruction
    NonCondensibleNonControlFlow {
        node: ComponentNode
    }
}