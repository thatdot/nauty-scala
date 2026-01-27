package com.thatdot.nauty.util

/**
 * Statistics from a nauty computation.
 * Matches the C statsblk structure.
 */
case class NautyStats(
  /** Group size mantissa: size = grpsize1 * 10^grpsize2 */
  grpsize1: Double,
  /** Group size exponent */
  grpsize2: Int,
  /** Number of orbits */
  numOrbits: Int,
  /** Number of generators found */
  numGenerators: Int,
  /** Error status (0 = success) */
  errStatus: Int,
  /** Total number of search tree nodes */
  numNodes: Long,
  /** Number of leaves that didn't contribute */
  numBadLeaves: Long,
  /** Maximum depth of search */
  maxLevel: Int,
  /** Total size of all target cells */
  tcTotal: Long,
  /** Number of canonical labeling updates */
  canUpdates: Long,
  /** Number of invariant applications */
  invApplics: Long,
  /** Number of successful invariant uses */
  invSuccesses: Long,
  /** Least level where invariant worked */
  invarSucLevel: Int
) {
  /**
   * Compute the group size as a BigDecimal.
   * This gives the exact value for groups up to a certain size,
   * then an approximation for very large groups.
   */
  def groupSize: BigDecimal = {
    if (grpsize2 == 0) BigDecimal(grpsize1)
    else BigDecimal(grpsize1) * BigDecimal(10).pow(grpsize2)
  }

  /**
   * Format the group size as a string.
   */
  def groupSizeString: String = {
    if (grpsize2 == 0) {
      f"$grpsize1%.0f"
    } else {
      f"$grpsize1%.6g * 10^$grpsize2"
    }
  }

  def isSuccess: Boolean = errStatus == 0
}

object NautyStats {
  /** Initial empty statistics */
  val empty: NautyStats = NautyStats(
    grpsize1 = 1.0,
    grpsize2 = 0,
    numOrbits = 0,
    numGenerators = 0,
    errStatus = 0,
    numNodes = 0,
    numBadLeaves = 0,
    maxLevel = 0,
    tcTotal = 0,
    canUpdates = 0,
    invApplics = 0,
    invSuccesses = 0,
    invarSucLevel = 0
  )

  // Error codes matching C nauty
  val NTOOBIG = 1      // n > MAXN or n > WORDSIZE*m
  val MTOOBIG = 2      // m > MAXM
  val CANONGNIL = 3    // canong = null but getcanon = true
  val NAUABORTED = 4   // nauty terminated early under program control
  val NAUKILLED = 5    // nauty terminated by signal
}

/**
 * Mutable statistics builder used during computation.
 */
final class StatsBuilder {
  var grpsize1: Double = 1.0
  var grpsize2: Int = 0
  var numOrbits: Int = 0
  var numGenerators: Int = 0
  var errStatus: Int = 0
  var numNodes: Long = 0
  var numBadLeaves: Long = 0
  var maxLevel: Int = 0
  var tcTotal: Long = 0
  var canUpdates: Long = 0
  var invApplics: Long = 0
  var invSuccesses: Long = 0
  var invarSucLevel: Int = 0

  /**
   * Multiply the group size by a factor.
   * Handles overflow into the exponent.
   */
  def multiplyGroupSize(factor: Int): Unit = {
    grpsize1 *= factor
    while (grpsize1 >= 1e10) {
      grpsize1 /= 1e10
      grpsize2 += 10
    }
  }

  /**
   * Build immutable statistics.
   */
  def build(): NautyStats = NautyStats(
    grpsize1 = grpsize1,
    grpsize2 = grpsize2,
    numOrbits = numOrbits,
    numGenerators = numGenerators,
    errStatus = errStatus,
    numNodes = numNodes,
    numBadLeaves = numBadLeaves,
    maxLevel = maxLevel,
    tcTotal = tcTotal,
    canUpdates = canUpdates,
    invApplics = invApplics,
    invSuccesses = invSuccesses,
    invarSucLevel = invarSucLevel
  )

  def reset(): Unit = {
    grpsize1 = 1.0
    grpsize2 = 0
    numOrbits = 0
    numGenerators = 0
    errStatus = 0
    numNodes = 0
    numBadLeaves = 0
    maxLevel = 0
    tcTotal = 0
    canUpdates = 0
    invApplics = 0
    invSuccesses = 0
    invarSucLevel = 0
  }
}

/**
 * Statistics from a Traces computation.
 */
case class TracesStats(
  grpsize1: Double,
  grpsize2: Int,
  numOrbits: Int,
  numGenerators: Int,
  errStatus: Int,
  numNodes: Long,
  numBadNodes: Long,
  maxLevel: Int,
  canupdates: Long,
  peakNodes: Long
) {
  def groupSize: BigDecimal = {
    if (grpsize2 == 0) BigDecimal(grpsize1)
    else BigDecimal(grpsize1) * BigDecimal(10).pow(grpsize2)
  }

  def isSuccess: Boolean = errStatus == 0
}

object TracesStats {
  val empty: TracesStats = TracesStats(
    grpsize1 = 1.0,
    grpsize2 = 0,
    numOrbits = 0,
    numGenerators = 0,
    errStatus = 0,
    numNodes = 0,
    numBadNodes = 0,
    maxLevel = 0,
    canupdates = 0,
    peakNodes = 0
  )
}
