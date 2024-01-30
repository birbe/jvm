use crate::bytecode::Bytecode;
use crate::classfile::resolved::attribute::Instruction;
use crate::env::wasm::scc::{enumerate_graph, StronglyConnectedComponents};
use std::collections::HashMap;
use std::mem;
use std::ops::Range;

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
    pub invokers: Vec<ComponentNode>,
}

pub fn find_loops(
    instructions: &[Instruction],
) -> (
    HashMap<usize, Loop>,
    Vec<ComponentNode>,
    HashMap<usize, Vec<usize>>,
) {
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

    (
        scc_s
            .iter()
            .filter(|(_scc, nodes)| nodes.len() > 1)
            .map(|(scc, node_idxs)| {
                let (loop_headers, invokers): (Vec<ComponentNode>, Vec<Vec<usize>>) = node_idxs
                    .iter()
                    .filter_map(|idx| {
                        let node = &nodes[*idx];

                        let predecessors: Vec<usize> = node
                            .predecessors
                            .iter()
                            .filter(|&pre| {
                                let predecessor = &nodes[*pre];
                                node.scc != predecessor.scc
                            })
                            .copied()
                            .collect();

                        if predecessors.len() > 0 {
                            Some((node.clone(), predecessors))
                        } else {
                            None
                        }
                    })
                    .unzip();

                let mut invokers: Vec<usize> = invokers.into_iter().flatten().collect();
                invokers.dedup();

                (
                    *scc,
                    Loop {
                        scc: *scc,
                        nodes: scc_s[scc].iter().map(|idx| nodes[*idx].clone()).collect(),
                        headers: loop_headers,
                        invokers: invokers.into_iter().map(|idx| nodes[idx].clone()).collect(),
                    },
                )
            })
            .collect(),
        nodes,
        scc_s,
    )
}

pub fn label_nodes(
    loops: &HashMap<usize, Loop>,
    nodes: &[ComponentNode],
    scc_s: &HashMap<usize, Vec<usize>>,
) -> Vec<LabeledNode> {
    //Loops with multiple entries must be fixed, as well as loops with a single entry but said entry
    // is not the earliest instruction in the loop, e.g.
    // A -> C
    // B -> C
    // C -> B
    // C is a loop entry but B is the first instruction in the loop

    let _irreducible: HashMap<_, _> = loops
        .iter()
        .filter(|(scc_idx, loop_)| {
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
        })
        .collect();

    let mut edges: Vec<usize> = nodes
        .iter()
        .map(|node| node.edges.iter().filter(|&&edge| edge != node.idx + 1))
        .flatten()
        .copied()
        .collect();

    edges.sort();

    let mut labels = vec![];

    let mut labeled: Vec<LabeledNode> = nodes
        .iter()
        .map(|node| {
            let _node_scc = &scc_s[&node.scc];

            for &edge in node.edges.iter() {
                let target = &nodes[edge];
                let target_scc = &scc_s[&target.scc];

                if node.idx == target.idx - 1 {
                    //not a jump, just a normal progression of instructions
                    continue;
                }

                if target.scc != node.scc {
                    //This instruction is a jump, and said jump isn't backwards (a loop)

                    //Jumping to an instruction which is not a loop header but is in a loop
                    return if target_scc.len() > 1 && target_scc[0] != target.idx {
                        labels.push((target.scc, edge, target_scc[0]));

                        LabeledNode::LabeledControlFlow {
                            node: node.clone(),
                            label: target.scc,
                            target: edge,
                        }
                    } else {
                        //Doesn't jump into a loop, or if it does, it only jumps right before the loop header, which is fine. This is a block
                        //This will never be a backwards jump (a loop) because otherwise they would be in the same group of strongly connected components
                        //however, it may jump forwards outside of a loop, in which case the loop scope will need to be enclosed in a block scope to allow the jump

                        assert!(node.idx < target.idx);

                        LabeledNode::BlockJump {
                            node: node.clone(),
                            target: edge,
                        }
                    };
                } else if target.scc == node.scc {
                    return if edge < node.idx {
                        //This jumps backwards within its own scc

                        //Jumps somewhere that isn't the beginning of the loop

                        if target.idx != target_scc[0] {
                            labels.push((target.scc, edge, target_scc[0]));

                            LabeledNode::LabeledControlFlow {
                                node: node.clone(),
                                label: target.scc,
                                target: edge,
                            }
                        } else {
                            //Jumps cleanly to the beginning of the loop, aka a plain old continue statement
                            LabeledNode::LoopContinue { node: node.clone() }
                        }
                    } else {
                        //This jumps forwards within it's own scc
                        LabeledNode::BlockJump {
                            node: node.clone(),
                            target: edge,
                        }
                    };
                }
            }

            if edges.binary_search(&node.idx).is_ok() {
                LabeledNode::NonCondensibleNonControlFlow { node: node.clone() }
            } else {
                LabeledNode::CondensibleNonControlFlow { node: node.clone() }
            }
        })
        .collect();

    for (_scc, target, header) in labels {
        let swap = match labeled.get_mut(header).unwrap() {
            LabeledNode::LoopController { node: _, targets } => {
                targets.push(target);
                continue;
            }
            LabeledNode::LoopContinue { node, .. }
            | LabeledNode::LabeledControlFlow { node, .. }
            | LabeledNode::BlockJump { node, .. }
            | LabeledNode::CondensibleNonControlFlow { node, .. }
            | LabeledNode::NonCondensibleNonControlFlow { node, .. } => {
                LabeledNode::LoopController {
                    node: node.clone(),
                    targets: vec![target],
                }
            }
        };

        *labeled.get_mut(header).unwrap() = swap;
    }

    labeled
}

#[derive(Debug)]
pub struct ScopeMetrics {
    pub loops: Vec<Range<usize>>,
    pub blocks: Vec<Range<usize>>,
}

fn recurse_get_earliest_index(block: usize, blocks: &[Range<usize>]) -> usize {
    let range = &blocks[block];

    let mut lonely_ends: Vec<(usize, &Range<usize>)> = blocks
        .iter()
        .enumerate()
        .filter(|(_idx, other_range)| {
            if other_range.end <= range.end
                && other_range.end >= range.start
                && !range.contains(&other_range.start)
            {
                true
            } else {
                false
            }
        })
        .collect();

    lonely_ends.sort_by_key(|(_, r)| r.start);

    match lonely_ends.get(0) {
        None => range.start,
        Some((idx, _)) => recurse_get_earliest_index(*idx, blocks),
    }
}

pub fn identify_scopes(
    nodes: &[(&ComponentNode, &LabeledNode)],
    scc_s: &HashMap<usize, Vec<usize>>,
) -> ScopeMetrics {
    let mut loops = Vec::new();
    let mut blocks = vec![];

    //Identify all loops. These never overlap
    for (_, nodes) in scc_s.iter() {
        if nodes.len() > 1 {
            loops.push(nodes[0]..nodes[nodes.len() - 1] + 1);
        }
    }

    //Create block ranges
    for (component_node, labeled_node) in nodes {
        match labeled_node {
            LabeledNode::LabeledControlFlow { label, target, .. } => {
                let scc_header = scc_s[label][0];

                //Outside the loop
                if component_node.idx < scc_header {
                    blocks.push(component_node.idx..scc_header);
                    if *target > scc_header {
                        blocks.push(scc_header..*target);
                    }
                } else {
                    //Continue statement that wants to branch down after continue
                    assert!(scc_header < *target);
                    blocks.push(scc_header..*target);
                }
            }
            LabeledNode::BlockJump { target, .. } => {
                assert!(component_node.idx < *target);
                blocks.push(component_node.idx..*target);
            }
            _ => {}
        }
    }

    blocks.dedup();

    for idx in 0..blocks.len() {
        let earliest = recurse_get_earliest_index(idx, &blocks);

        blocks[idx].start = earliest;
    }

    blocks.sort_by(|a, b| {
        if a.start == b.start {
            a.end.cmp(&b.end).reverse()
        } else {
            a.start.cmp(&b.start)
        }
    });

    ScopeMetrics { loops, blocks }
}

pub fn create_scopes<'a, 'b: 'a>(
    block_ranges: &'a mut &'b [Range<usize>],
    loop_ranges: &'a mut &'b [Range<usize>],
    nodes: &[(&ComponentNode, &LabeledNode)],
    range: Range<usize>,
    loop_out: bool,
) -> Block {
    let mut out_nodes = vec![];
    let mut out_blocks = vec![];

    let mut idx = range.start;

    loop {
        assert!(idx <= range.end);
        if idx == range.end {
            break;
        }

        let next_block_range = match block_ranges.get(0) {
            None => None,
            Some(other) => {
                if !range.contains(&other.start) || other.end > range.end {
                    None
                } else {
                    Some(other)
                }
            }
        };

        let next_loop_range = match loop_ranges.get(0) {
            None => None,
            Some(other) => {
                if !range.contains(&other.start) || other.end > range.end {
                    None
                } else {
                    Some(other)
                }
            }
        };

        let scrape_end = match (next_block_range, next_loop_range) {
            (None, None) => range.end,
            (Some(next_block_range), None) => next_block_range.start,
            (None, Some(next_loop_range)) => next_loop_range.start,
            (Some(next_block_range), Some(next_loop_range)) => {
                next_block_range.start.min(next_loop_range.start)
            }
        };

        for scrape_idx in idx..scrape_end {
            let node = &nodes[scrape_idx];
            out_nodes.push((node.0.clone(), node.1.clone()));
        }

        if out_nodes.len() > 0 {
            out_blocks.push(Block::Nodes(mem::replace(&mut out_nodes, vec![])));
        }

        match (
            block_ranges
                .iter()
                .find(|block_range| {
                    range.contains(&block_range.start) && block_range.end <= range.end
                })
                .cloned(),
            loop_ranges
                .iter()
                .find(|block_range| {
                    range.contains(&block_range.start) && block_range.end <= range.end
                })
                .cloned(),
        ) {
            (None, None) => {
                break;
            }
            (Some(next_block_range_), None) => {
                assert!(
                    next_block_range_.start >= idx,
                    "{}\n{:#?}\n{:?}",
                    idx,
                    block_ranges,
                    next_block_range_
                );

                *block_ranges = &block_ranges[1..];

                out_blocks.push(create_scopes(
                    block_ranges,
                    loop_ranges,
                    nodes,
                    next_block_range_.clone(),
                    false,
                ));
                idx = next_block_range_.end;
            }
            (None, Some(next_loop_range_)) => {
                assert!(next_loop_range_.start >= idx);

                *loop_ranges = &loop_ranges[1..];

                out_blocks.push(create_scopes(
                    block_ranges,
                    loop_ranges,
                    nodes,
                    next_loop_range_.clone(),
                    true,
                ));
                idx = next_loop_range_.end;
            }
            (Some(next_block_range_), Some(next_loop_range_)) => {
                if next_block_range_.start < next_loop_range_.start {
                    assert!(next_block_range_.start >= idx);

                    *block_ranges = &block_ranges[1..];

                    out_blocks.push(create_scopes(
                        block_ranges,
                        loop_ranges,
                        nodes,
                        next_block_range_.clone(),
                        false,
                    ));
                    idx = next_block_range_.end;
                } else if next_block_range_.start == next_loop_range_.start {
                    if next_loop_range_.end > next_block_range_.end {
                        assert!(next_block_range_.start >= idx);

                        *loop_ranges = &loop_ranges[1..];

                        out_blocks.push(create_scopes(
                            block_ranges,
                            loop_ranges,
                            nodes,
                            next_loop_range_.clone(),
                            true,
                        ));
                        idx = next_loop_range_.end;
                    } else {
                        *block_ranges = &block_ranges[1..];

                        out_blocks.push(create_scopes(
                            block_ranges,
                            loop_ranges,
                            nodes,
                            next_block_range_.clone(),
                            false,
                        ));
                        idx = next_block_range_.end;
                    }
                } else {
                    assert!(next_block_range_.start >= idx);

                    *loop_ranges = &loop_ranges[1..];

                    out_blocks.push(create_scopes(
                        block_ranges,
                        loop_ranges,
                        nodes,
                        next_loop_range_.clone(),
                        true,
                    ));
                    idx = next_loop_range_.end;
                }
            }
        };
    }

    if loop_out {
        Block::Loop(out_blocks)
    } else {
        Block::Normal(out_blocks)
    }
}

#[derive(Clone, Debug)]
pub enum LabeledNode {
    LoopController {
        node: ComponentNode,
        targets: Vec<usize>,
    },
    LoopContinue {
        node: ComponentNode,
    },
    LabeledControlFlow {
        node: ComponentNode,
        label: usize,  //index of the scc it wants to jump into
        target: usize, //the node it wants to reach
    },
    BlockJump {
        node: ComponentNode,
        target: usize,
    },
    //Nothing jumps to this instruction
    CondensibleNonControlFlow {
        node: ComponentNode,
    },
    //Something jumps to this instruction
    NonCondensibleNonControlFlow {
        node: ComponentNode,
    },
}

#[derive(Debug)]
pub enum Block {
    Loop(Vec<Block>),
    Normal(Vec<Block>),
    Nodes(Vec<(ComponentNode, LabeledNode)>),
}
