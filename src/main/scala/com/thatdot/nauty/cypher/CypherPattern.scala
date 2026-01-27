package com.thatdot.nauty.cypher

import scala.util.matching.Regex
import com.thatdot.nauty.core.Nauty
import com.thatdot.nauty.graph.DenseGraph
import com.thatdot.nauty.group.Permutation

/**
 * Represents a parsed Cypher graph pattern.
 *
 * @param nodes Map from variable name to label (or "_UNLABELED")
 * @param edges List of (fromVar, relType, toVar) tuples
 */
case class CypherPattern(
  nodes: Map[String, String],
  edges: Seq[(String, String, String)]
) {
  /** Check if pattern is empty */
  def isEmpty: Boolean = nodes.isEmpty

  /** Check if pattern has no edges */
  def hasNoEdges: Boolean = edges.isEmpty
}

/**
 * Represents a Cypher pattern converted to nauty format.
 *
 * @param n           Number of vertices (nodes + edge vertices)
 * @param partition   Vertex partition for coloring
 * @param edges       List of directed edges in the nauty graph
 * @param nodeVars    List of node variable names (index = vertex id)
 * @param edgeInfo    List of (vertexId, relType) for edge vertices
 * @param sortedLabels Labels in canonical order
 */
case class NautyGraph(
  n: Int,
  partition: Seq[Seq[Int]],
  edges: Seq[(Int, Int)],
  nodeVars: Seq[String],
  edgeInfo: Seq[(Int, String)],
  sortedLabels: Seq[String]
)

/**
 * Parser and canonicalizer for Cypher graph patterns.
 *
 * Ported from cypher_canonicalize.py and cypher_automorphisms.py
 */
object CypherPattern {
  private val NodePattern: Regex = """\((\w+)(?::(\w+))?\)""".r
  private val OutEdgePattern: Regex = """-\[:(\w+)\]->""".r
  private val InEdgePattern: Regex = """<-\[:(\w+)\]-""".r

  /**
   * Parse a Cypher pattern string into a CypherPattern.
   *
   * Supports patterns like:
   *   (a:Person)-[:KNOWS]->(b:Person)
   *   (a)-[:LIKES]->(b:Movie)
   *   (a:Person)-[:KNOWS]->(b:Person)-[:WORKS_AT]->(c:Company)
   *   (a)<-[:KNOWS]-(b) (incoming edges)
   */
  def parse(pattern: String): CypherPattern = {
    val nodes = scala.collection.mutable.Map[String, String]()
    val edges = scala.collection.mutable.ListBuffer[(String, String, String)]()

    // Find all nodes
    val nodePositions = NodePattern.findAllMatchIn(pattern).toList

    for (m <- nodePositions) {
      val varName = m.group(1)
      val label = Option(m.group(2)).getOrElse("_UNLABELED")
      if (!nodes.contains(varName)) {
        nodes(varName) = label
      } else if (m.group(2) != null) {
        // If this occurrence has a label, use it
        nodes(varName) = label
      }
    }

    // Find relationships between consecutive nodes
    for (i <- 0 until nodePositions.length - 1) {
      val m1 = nodePositions(i)
      val m2 = nodePositions(i + 1)
      val var1 = m1.group(1)
      val var2 = m2.group(1)
      val between = pattern.substring(m1.end, m2.start)

      OutEdgePattern.findFirstMatchIn(between) match {
        case Some(edgeMatch) =>
          edges += ((var1, edgeMatch.group(1), var2))
        case None =>
          InEdgePattern.findFirstMatchIn(between) match {
            case Some(edgeMatch) =>
              // Incoming edge: normalize to outgoing
              edges += ((var2, edgeMatch.group(1), var1))
            case None =>
              // No edge found between these nodes
          }
      }
    }

    CypherPattern(nodes.toMap, edges.toSeq)
  }

  /**
   * Convert a CypherPattern to nauty graph format.
   *
   * Edge labels are encoded as intermediate vertices:
   *   (a)-[:KNOWS]->(b) becomes: a -> _KNOWS_vertex -> b
   *
   * Vertex colors (partitions) encode node labels and edge types.
   */
  def toNautyGraph(cp: CypherPattern): NautyGraph = {
    val nodeVars = cp.nodes.keys.toSeq
    val nodeToId = nodeVars.zipWithIndex.toMap

    var nextId = nodeVars.length
    val edgeVertices = scala.collection.mutable.ListBuffer[(Int, String)]()
    val nautyEdges = scala.collection.mutable.ListBuffer[(Int, Int)]()

    for ((fromVar, relType, toVar) <- cp.edges) {
      val edgeId = nextId
      edgeVertices += ((edgeId, relType))
      nautyEdges += ((nodeToId(fromVar), edgeId))
      nautyEdges += ((edgeId, nodeToId(toVar)))
      nextId += 1
    }

    val n = nextId

    // Build partition by label (colors)
    val labelToVertices = scala.collection.mutable.Map[String, List[Int]]()

    for ((varName, label) <- cp.nodes) {
      val id = nodeToId(varName)
      labelToVertices(label) = id :: labelToVertices.getOrElse(label, Nil)
    }

    for ((edgeId, relType) <- edgeVertices) {
      val labelKey = s"_REL_$relType"
      labelToVertices(labelKey) = edgeId :: labelToVertices.getOrElse(labelKey, Nil)
    }

    val sortedLabels = labelToVertices.keys.toSeq.sorted
    val partition = sortedLabels.map { label =>
      labelToVertices(label).sorted
    }

    NautyGraph(n, partition, nautyEdges.toSeq, nodeVars, edgeVertices.toSeq, sortedLabels)
  }

  /**
   * Get the canonical hash for a Cypher pattern.
   *
   * The hash uniquely identifies the pattern's structure, independent of
   * variable names. Two patterns with the same hash are isomorphic.
   */
  def canonicalHash(pattern: String): String = {
    val cp = parse(pattern)

    if (cp.isEmpty) return "EMPTY"

    if (cp.hasNoEdges) {
      // Single node or disconnected nodes - just hash the labels
      val labels = cp.nodes.values.toSeq.sorted
      return s"NODES:${labels.mkString(",")}"
    }

    val ng = toNautyGraph(cp)
    val g = DenseGraph.fromEdges(ng.n, ng.edges, directed = true)
    val result = Nauty.densenauty(
      g,
      com.thatdot.nauty.util.NautyOptions(
        getCanon = 1,
        digraph = true,
        defaultPtn = false,
        initialPartition = Some(ng.partition)
      )
    )

    val nautyHash = result.canonicalHash
    val labelSig = ng.sortedLabels.mkString("|")
    s"$nautyHash:$labelSig"
  }

  /**
   * Get automorphism generators for a Cypher pattern.
   *
   * Returns list of permutations that map the pattern to itself.
   */
  def automorphismGenerators(pattern: String): List[Permutation] = {
    val cp = parse(pattern)

    if (cp.isEmpty || cp.hasNoEdges) return Nil

    val ng = toNautyGraph(cp)
    val g = DenseGraph.fromEdges(ng.n, ng.edges, directed = true)
    val result = Nauty.densenauty(
      g,
      com.thatdot.nauty.util.NautyOptions(
        getCanon = 0,
        digraph = true,
        defaultPtn = false,
        initialPartition = Some(ng.partition)
      )
    )

    result.generators
  }

  /**
   * Generate all automorphisms (full group, not just generators).
   */
  def allAutomorphisms(pattern: String): Set[Permutation] = {
    val generators = automorphismGenerators(pattern)
    if (generators.isEmpty) {
      val cp = parse(pattern)
      if (cp.isEmpty) Set.empty
      else Set(Permutation.identity(toNautyGraph(cp).n))
    } else {
      Nauty.generateGroup(generators)
    }
  }

  /**
   * Apply a permutation to a pattern and return the new variable mapping.
   *
   * @param cp       The parsed pattern
   * @param ng       The nauty graph
   * @param perm     The permutation to apply
   * @return Map from old variable name to new variable name
   */
  def applyAutomorphism(cp: CypherPattern, ng: NautyGraph, perm: Permutation): Map[String, String] = {
    val numNodes = ng.nodeVars.length
    ng.nodeVars.zipWithIndex.map { case (oldVar, oldIdx) =>
      val newIdx = perm(oldIdx)
      val newVar = if (newIdx < numNodes) ng.nodeVars(newIdx) else oldVar
      oldVar -> newVar
    }.toMap
  }

  /**
   * Reconstruct a Cypher pattern with variables renamed according to mapping.
   */
  def reconstructPattern(originalPattern: String, varMapping: Map[String, String]): String = {
    var result = originalPattern

    // Sort by length descending to avoid partial replacements
    val sortedVars = varMapping.keys.toSeq.sortBy(-_.length)

    // Use placeholders to avoid conflicts
    val placeholders = sortedVars.zipWithIndex.map { case (varName, i) =>
      val placeholder = s"__VAR_${i}__"
      result = result.replaceAll(s"\\(${varName}(?=[:)])", s"($placeholder")
      placeholder -> varMapping(varName)
    }.toMap

    // Replace placeholders with final values
    for ((placeholder, newVar) <- placeholders) {
      result = result.replace(placeholder, newVar)
    }

    result
  }

  /**
   * Generate all automorphic variants of a Cypher pattern.
   *
   * @param pattern                  The Cypher pattern string
   * @param includeOriginal          Whether to include the original pattern
   * @param includeSyntacticVariants Whether to include edge direction notation variants
   *                                 e.g., (a)-[:X]->(b) vs (b)<-[:X]-(a)
   * @return List of equivalent patterns
   */
  def automorphicPatterns(
    pattern: String,
    includeOriginal: Boolean = true,
    includeSyntacticVariants: Boolean = false
  ): Seq[String] = {
    val cp = parse(pattern)

    if (cp.isEmpty) {
      return if (includeOriginal) Seq(pattern) else Seq.empty
    }

    if (cp.hasNoEdges) {
      // For nodes only, return original
      return if (includeOriginal) Seq(pattern) else Seq.empty
    }

    val ng = toNautyGraph(cp)
    val automs = allAutomorphisms(pattern)

    val patterns = scala.collection.mutable.Set[String]()

    for (perm <- automs) {
      val varMapping = applyAutomorphism(cp, ng, perm)

      // Apply variable mapping to nodes and edges
      val mappedNodes = cp.nodes.map { case (v, label) => varMapping.getOrElse(v, v) -> label }
      val mappedEdges = cp.edges.map { case (f, rel, t) =>
        (varMapping.getOrElse(f, f), rel, varMapping.getOrElse(t, t))
      }

      if (includeSyntacticVariants) {
        // Generate all edge direction notation variants
        val variants = generateSyntacticVariants(mappedNodes, mappedEdges)
        patterns ++= variants
      } else {
        val newPattern = reconstructPattern(pattern, varMapping)
        patterns += newPattern
      }
    }

    val result = patterns.toSeq.distinct.sorted
    if (includeOriginal) result else result.filterNot(_ == pattern)
  }

  /**
   * Generate all syntactic variants of a pattern with different edge direction notations.
   *
   * (a)-[:X]->(b) and (b)<-[:X]-(a) represent the same directed edge but with different syntax.
   * This generates all 2^n combinations for n edges.
   */
  def generateSyntacticVariants(
    nodes: Map[String, String],
    edges: Seq[(String, String, String)]
  ): Seq[String] = {
    if (edges.isEmpty) {
      // Just nodes
      val parts = nodes.map { case (varName, label) =>
        if (label != "_UNLABELED") s"($varName:$label)" else s"($varName)"
      }
      return Seq(parts.mkString(","))
    }

    val numEdges = edges.length
    val variants = scala.collection.mutable.Set[String]()

    // Generate all 2^n combinations of edge directions
    for (mask <- 0 until (1 << numEdges)) {
      val edgeSpecs = edges.zipWithIndex.map { case ((from, rel, to), i) =>
        val reverse = (mask & (1 << i)) != 0
        if (reverse) {
          // Reverse notation: (to)<-[:REL]-(from)
          EdgeSpec(to, from, rel, isReversed = true)
        } else {
          // Forward notation: (from)-[:REL]->(to)
          EdgeSpec(from, to, rel, isReversed = false)
        }
      }

      buildPatternFromEdges(nodes, edgeSpecs) match {
        case Some(p) => variants += p
        case None => // Skip invalid combinations
      }
    }

    variants.toSeq
  }

  private case class EdgeSpec(left: String, right: String, rel: String, isReversed: Boolean)

  private def buildPatternFromEdges(nodes: Map[String, String], edgeSpecs: Seq[EdgeSpec]): Option[String] = {
    if (edgeSpecs.isEmpty) return None

    def formatNode(varName: String): String = {
      val label = nodes.getOrElse(varName, "_UNLABELED")
      if (label != "_UNLABELED") s"($varName:$label)" else s"($varName)"
    }

    def formatEdge(spec: EdgeSpec): String = {
      if (spec.isReversed) s"<-[:${spec.rel}]-" else s"-[:${spec.rel}]->"
    }

    // Build paths by chaining edges
    val used = Array.fill(edgeSpecs.length)(false)
    val paths = scala.collection.mutable.ListBuffer[String]()

    while (used.exists(!_)) {
      // Start a new path with first unused edge
      var pathParts = scala.collection.mutable.ListBuffer[String]()
      var currentLeft = ""
      var currentRight = ""

      val firstUnused = used.indexWhere(!_)
      if (firstUnused >= 0) {
        val spec = edgeSpecs(firstUnused)
        used(firstUnused) = true
        pathParts += formatNode(spec.left)
        pathParts += formatEdge(spec)
        pathParts += formatNode(spec.right)
        currentLeft = spec.left
        currentRight = spec.right

        // Try to extend the path
        var changed = true
        while (changed) {
          changed = false
          for (i <- edgeSpecs.indices if !used(i)) {
            val spec = edgeSpecs(i)

            // Can we attach to the right end?
            if (spec.left == currentRight) {
              pathParts += formatEdge(spec)
              pathParts += formatNode(spec.right)
              currentRight = spec.right
              used(i) = true
              changed = true
            } else if (spec.right == currentRight) {
              // Flip this edge's notation
              val flipped = EdgeSpec(spec.right, spec.left, spec.rel, !spec.isReversed)
              pathParts += formatEdge(flipped)
              pathParts += formatNode(flipped.right)
              currentRight = flipped.right
              used(i) = true
              changed = true
            }
            // Can we attach to the left end?
            else if (spec.right == currentLeft) {
              pathParts.prepend(formatEdge(spec))
              pathParts.prepend(formatNode(spec.left))
              currentLeft = spec.left
              used(i) = true
              changed = true
            } else if (spec.left == currentLeft) {
              val flipped = EdgeSpec(spec.right, spec.left, spec.rel, !spec.isReversed)
              pathParts.prepend(formatEdge(flipped))
              pathParts.prepend(formatNode(flipped.left))
              currentLeft = flipped.left
              used(i) = true
              changed = true
            }
          }
        }
      }

      if (pathParts.nonEmpty) {
        paths += pathParts.mkString
      }
    }

    Some(paths.mkString(","))
  }

  /**
   * Check if two Cypher patterns are isomorphic.
   */
  def areIsomorphic(pattern1: String, pattern2: String): Boolean = {
    canonicalHash(pattern1) == canonicalHash(pattern2)
  }

  /**
   * Get automorphism group size for a Cypher pattern.
   */
  def automorphismGroupSize(pattern: String): Int = {
    allAutomorphisms(pattern).size
  }
}
