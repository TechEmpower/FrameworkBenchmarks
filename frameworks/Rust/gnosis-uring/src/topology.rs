//! Compiled topology representation.
//!
//! A topology is a directed acyclic graph where:
//!   - Nodes are operations (accept, parse, resolve, compress, send)
//!   - Edges are scheduling primitives (FORK, RACE, FOLD, VENT)
//!
//! Betty compiles .gg source into this representation at build time.
//! The executor walks edges and submits io_uring operations.
//!
//! For the static file server topology:
//!
//! ```text
//! (accept)-[:FORK]->(conn)
//! (conn)-[:PROCESS]->(parsed)
//! (parsed)-[:FORK]->(cache, mmap, disk)
//! (cache | mmap | disk)-[:RACE]->(resolved)
//! (resolved)-[:FORK]->(id, gz, br, df)
//! (id | gz | br | df)-[:RACE { take: 'smallest' }]->(compressed)
//! (compressed)-[:PROCESS]->(send)
//! ```

/// Edge type — the four primitives.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    /// Fan-out: submit N operations in parallel.
    /// Maps to io_uring batch submission.
    Fork,

    /// First-to-complete wins, cancel losers.
    /// Maps to io_uring_wait_cqe + io_uring_prep_cancel.
    Race,

    /// Gather all completions, combine.
    /// Maps to collecting all CQEs for a submission group.
    Fold,

    /// Controlled failure / cancellation.
    /// Maps to io_uring_prep_cancel.
    Vent,

    /// Sequential computation (no io_uring, just CPU).
    Process,
}

/// Node type — what the operation does.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeOp {
    /// Accept incoming TCP connections.
    Accept,

    /// Parse HTTP request from raw bytes.
    ParseHttp,

    /// Look up file in memory cache.
    CacheLookup,

    /// Memory-map a file.
    MmapRead,

    /// Read file via io_uring.
    DiskRead,

    /// Compress with a specific codec.
    Compress(Codec),

    /// Build HTTP response headers.
    BuildHeaders,

    /// Send response via io_uring writev.
    Send,

    /// Send response via io_uring + sendfile.
    Sendfile,

    /// Laminar pipeline: chunk + race codecs + send.
    LaminarSend,
}

/// Compression codec.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Codec {
    Identity,
    Gzip,
    Brotli,
    Deflate,
}

/// A node in the topology DAG.
#[derive(Debug)]
pub struct Node {
    pub id: u16,
    pub op: NodeOp,
}

/// A directed edge in the topology DAG.
#[derive(Debug)]
pub struct TopoEdge {
    pub from: u16,
    pub to: &'static [u16],
    pub edge_type: Edge,
}

/// Compiled topology — a static DAG.
///
/// Built at compile time by Betty. The executor walks this
/// structure per-connection with zero allocation.
pub struct Topology {
    pub nodes: &'static [Node],
    pub edges: &'static [TopoEdge],
}

// ═══════════════════════════════════════════════════════════════════
// Static file server topology (the one that competes on TechEmpower)
// ═══════════════════════════════════════════════════════════════════

/// The static file server topology with Laminar codec racing.
///
/// This is what Betty would emit from:
/// ```gg
/// (listener: Accept)-[:FORK]->(conn: Connection)
/// (conn)-[:PROCESS]->(req: ParseHTTP)
/// (req)-[:FORK]->(cache: CacheLookup, mmap: MmapRead, disk: DiskRead)
/// (cache | mmap | disk)-[:RACE]->(file: Resolved)
/// (file)-[:PROCESS]->(laminar: LaminarSend)
/// ```
pub static STATIC_SERVER: Topology = Topology {
    nodes: &[
        Node { id: 0, op: NodeOp::Accept },
        Node { id: 1, op: NodeOp::ParseHttp },
        Node { id: 2, op: NodeOp::CacheLookup },
        Node { id: 3, op: NodeOp::MmapRead },
        Node { id: 4, op: NodeOp::DiskRead },
        Node { id: 5, op: NodeOp::LaminarSend },
    ],
    edges: &[
        // accept -[:FORK]-> connections
        TopoEdge { from: 0, to: &[1], edge_type: Edge::Fork },
        // parse -[:FORK]-> cache, mmap, disk
        TopoEdge { from: 1, to: &[2, 3, 4], edge_type: Edge::Fork },
        // cache | mmap | disk -[:RACE]-> resolved
        TopoEdge { from: 2, to: &[5], edge_type: Edge::Race },
        TopoEdge { from: 3, to: &[5], edge_type: Edge::Race },
        TopoEdge { from: 4, to: &[5], edge_type: Edge::Race },
    ],
};

/// Plaintext topology for TechEmpower (minimal — no file resolution).
///
/// ```gg
/// (listener: Accept)-[:FORK]->(conn: Connection)
/// (conn)-[:PROCESS]->(req: ParseHTTP)
/// (req)-[:PROCESS]->(send: Send)
/// ```
pub static PLAINTEXT_SERVER: Topology = Topology {
    nodes: &[
        Node { id: 0, op: NodeOp::Accept },
        Node { id: 1, op: NodeOp::ParseHttp },
        Node { id: 2, op: NodeOp::Send },
    ],
    edges: &[
        TopoEdge { from: 0, to: &[1], edge_type: Edge::Fork },
        TopoEdge { from: 1, to: &[2], edge_type: Edge::Process },
    ],
};
