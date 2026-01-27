package com.thatdot.nauty.group

/**
 * Orbit computation and union-find for permutation groups.
 *
 * Orbits are represented by an array where orbits(i) = representative of i's orbit.
 * The representative is always the smallest element in the orbit.
 */
object Orbits {
  /**
   * Initialize orbits to singletons (each vertex in its own orbit).
   */
  def identity(n: Int): Array[Int] = {
    val orbits = new Array[Int](n)
    var i = 0
    while (i < n) {
      orbits(i) = i
      i += 1
    }
    orbits
  }

  /**
   * Find the representative of vertex i's orbit.
   * Also performs path compression.
   */
  @inline def find(orbits: Array[Int], i: Int): Int = {
    var j = orbits(i)
    while (orbits(j) != j) {
      j = orbits(j)
    }
    // Path compression
    var k = i
    while (orbits(k) != j) {
      val next = orbits(k)
      orbits(k) = j
      k = next
    }
    j
  }

  /**
   * Join orbits of i and j, keeping the smaller element as representative.
   *
   * @param orbits The orbit array (modified in place)
   * @param i First vertex
   * @param j Second vertex
   * @return true if orbits were different (and are now merged), false if already same
   */
  def join(orbits: Array[Int], i: Int, j: Int): Boolean = {
    val ri = find(orbits, i)
    val rj = find(orbits, j)
    if (ri == rj) {
      false
    } else if (ri < rj) {
      orbits(rj) = ri
      true
    } else {
      orbits(ri) = rj
      true
    }
  }

  /**
   * Update orbits based on a permutation.
   * For each i, i and perm(i) must be in the same orbit.
   *
   * @param orbits The orbit array (modified in place)
   * @param perm The permutation
   * @param n Number of vertices
   * @return The new number of orbits
   */
  def orbjoin(orbits: Array[Int], perm: Array[Int], n: Int): Int = {
    // First pass: join orbits
    var i = 0
    while (i < n) {
      if (perm(i) != i) {
        var j1 = orbits(i)
        while (orbits(j1) != j1) j1 = orbits(j1)

        var j2 = orbits(perm(i))
        while (orbits(j2) != j2) j2 = orbits(j2)

        if (j1 < j2) orbits(j2) = j1
        else if (j1 > j2) orbits(j1) = j2
      }
      i += 1
    }

    // Second pass: path compression and count
    var count = 0
    i = 0
    while (i < n) {
      val rep = find(orbits, i)
      orbits(i) = rep
      if (rep == i) count += 1
      i += 1
    }
    count
  }

  /**
   * Count the number of orbits.
   */
  def numOrbits(orbits: Array[Int], n: Int): Int = {
    var count = 0
    var i = 0
    while (i < n) {
      if (orbits(i) == i) count += 1
      i += 1
    }
    count
  }

  /**
   * Get all orbits as sets.
   */
  def toSets(orbits: Array[Int], n: Int): Map[Int, Set[Int]] = {
    val result = scala.collection.mutable.Map[Int, Set[Int]]()
    var i = 0
    while (i < n) {
      val rep = find(orbits, i)
      result(rep) = result.getOrElse(rep, Set.empty) + i
      i += 1
    }
    result.toMap
  }

  /**
   * Get the orbit containing vertex v.
   */
  def orbitOf(orbits: Array[Int], n: Int, v: Int): Set[Int] = {
    val rep = find(orbits, v)
    val result = Set.newBuilder[Int]
    var i = 0
    while (i < n) {
      if (find(orbits, i) == rep) {
        result += i
      }
      i += 1
    }
    result.result()
  }

  /**
   * Check if two vertices are in the same orbit.
   */
  def sameOrbit(orbits: Array[Int], i: Int, j: Int): Boolean = {
    find(orbits, i) == find(orbits, j)
  }

  /**
   * Create a copy of the orbits array.
   */
  def copy(orbits: Array[Int]): Array[Int] = orbits.clone()
}
