package com.thatdot.nauty.bits

/**
 * SetWord represents a machine word used for bit-packed set operations.
 * In nauty, a setword is the fundamental unit for representing sets of vertices.
 *
 * We use Long (64-bit) as the underlying type, matching nauty's WORDSIZE=64.
 * Bits are numbered 0..63 from left (high-order) to right (low-order).
 * Bit k represents element k in the set.
 */
object SetWord {
  /** Number of bits in a setword */
  val WordSize: Int = 64

  /** All bits set */
  val AllBits: Long = -1L  // 0xFFFFFFFFFFFFFFFFL

  /** No bits set */
  val NoBits: Long = 0L

  /** Precomputed single-bit masks: bit(i) has bit i set (from high to low) */
  private[nauty] val bit: Array[Long] = {
    val arr = new Array[Long](64)
    var i = 0
    while (i < 64) {
      arr(i) = 1L << (63 - i)
      i += 1
    }
    arr
  }

  /** Precomputed bitmasks: bitmask(i) has bits i+1..63 set */
  private[nauty] val bitmask: Array[Long] = {
    val arr = new Array[Long](64)
    var i = 0
    while (i < 64) {
      arr(i) = if (i == 63) 0L else (1L << (63 - i)) - 1L
      i += 1
    }
    arr
  }

  /**
   * Number of setwords needed to represent n elements.
   * setwordsNeeded(n) = ceil(n / 64)
   */
  @inline def setwordsNeeded(n: Int): Int = {
    if (n <= 0) 0 else ((n - 1) >> 6) + 1
  }

  /** Which setword contains bit position pos */
  @inline def setwd(pos: Int): Int = pos >> 6

  /** Position within setword of bit pos (0..63) */
  @inline def setbt(pos: Int): Int = pos & 0x3F

  /** Multiply by WORDSIZE */
  @inline def timesWordSize(w: Int): Int = w << 6
}

/**
 * Bit operations on setwords (Long values).
 * These are implemented as extension methods for performance.
 */
object BitOps {
  import SetWord._

  /**
   * Population count - number of 1-bits.
   * Uses Java's optimized Long.bitCount which typically compiles to POPCNT.
   */
  @inline def popcount(w: Long): Int = java.lang.Long.bitCount(w)

  /**
   * Find the position of the first (leftmost/highest) 1-bit.
   * Returns 0..63 for non-zero values, 64 for zero.
   */
  @inline def firstBit(w: Long): Int = {
    if (w == 0L) WordSize
    else java.lang.Long.numberOfLeadingZeros(w)
  }

  /**
   * Find the position of the first 1-bit (assumes w != 0).
   * Slightly faster than firstBit when we know w is nonzero.
   */
  @inline def firstBitNZ(w: Long): Int = {
    java.lang.Long.numberOfLeadingZeros(w)
  }

  /**
   * Test if element pos is in the set represented by this setword.
   * Note: For multi-word sets, use the array version.
   */
  @inline def isElement1(w: Long, pos: Int): Boolean = {
    (w & bit(pos)) != 0L
  }

  /**
   * Add element pos to the set.
   */
  @inline def addElement1(w: Long, pos: Int): Long = {
    w | bit(pos)
  }

  /**
   * Delete element pos from the set.
   */
  @inline def delElement1(w: Long, pos: Int): Long = {
    w & ~bit(pos)
  }

  /**
   * Flip element pos in the set.
   */
  @inline def flipElement1(w: Long, pos: Int): Long = {
    w ^ bit(pos)
  }

  /**
   * Create a mask with the first n bits set.
   * allmask(n) has bits 0..n-1 set.
   */
  @inline def allmask(n: Int): Long = {
    if (n <= 0) 0L
    else if (n >= 64) AllBits
    else ~bitmask(n - 1)
  }

  /**
   * Extract and remove the first (leftmost) 1-bit.
   * Returns (bit position, word with that bit cleared).
   */
  @inline def takeBit(w: Long): (Int, Long) = {
    val pos = firstBitNZ(w)
    (pos, w ^ bit(pos))
  }

  /**
   * Get the lowest-order (rightmost) 1-bit as a mask.
   */
  @inline def swhibit(w: Long): Long = w & (-w)

  /**
   * Test if at most one bit is set.
   */
  @inline def atmostOneBit(w: Long): Boolean = {
    (w & (w - 1L)) == 0L
  }

  /**
   * Clear the lowest-order 1-bit.
   */
  @inline def withoutHiBit(w: Long): Long = w & (w - 1L)

  /**
   * Test if word1 has any bits not in word2.
   */
  @inline def notSubset(word1: Long, word2: Long): Boolean = {
    (word1 & ~word2) != 0L
  }
}

/**
 * Operations on multi-word sets (arrays of Long).
 */
object SetOps {
  import SetWord._
  import BitOps._

  /**
   * Test if element pos is in the set.
   */
  @inline def isElement(set: Array[Long], pos: Int): Boolean = {
    val word = pos >> 6
    val bit = pos & 0x3F
    (set(word) & SetWord.bit(bit)) != 0L
  }

  /**
   * Add element pos to the set (mutates the array).
   */
  @inline def addElement(set: Array[Long], pos: Int): Unit = {
    val word = pos >> 6
    val bit = pos & 0x3F
    set(word) |= SetWord.bit(bit)
  }

  /**
   * Delete element pos from the set (mutates the array).
   */
  @inline def delElement(set: Array[Long], pos: Int): Unit = {
    val word = pos >> 6
    val bit = pos & 0x3F
    set(word) &= ~SetWord.bit(bit)
  }

  /**
   * Flip element pos in the set (mutates the array).
   */
  @inline def flipElement(set: Array[Long], pos: Int): Unit = {
    val word = pos >> 6
    val bit = pos & 0x3F
    set(word) ^= SetWord.bit(bit)
  }

  /**
   * Clear all elements in the set (mutates the array).
   */
  @inline def emptySet(set: Array[Long], m: Int): Unit = {
    var i = 0
    while (i < m) {
      set(i) = 0L
      i += 1
    }
  }

  /**
   * Fill the set with elements 0..n-1 (mutates the array).
   */
  def fillSet(set: Array[Long], m: Int, n: Int): Unit = {
    val fullWords = n / WordSize
    var i = 0
    while (i < fullWords) {
      set(i) = AllBits
      i += 1
    }
    val remainder = n - fullWords * WordSize
    if (remainder > 0) {
      set(i) = allmask(remainder)
      i += 1
    }
    while (i < m) {
      set(i) = 0L
      i += 1
    }
  }

  /**
   * Count elements in the set.
   */
  def setSize(set: Array[Long], m: Int): Int = {
    var count = 0
    var i = 0
    while (i < m) {
      count += popcount(set(i))
      i += 1
    }
    count
  }

  /**
   * Find next element in set starting from pos+1.
   * Returns -1 if no more elements.
   */
  def nextElement(set: Array[Long], m: Int, pos: Int): Int = {
    var w: Int = 0
    var x: Long = 0L

    if (pos < 0) {
      w = 0
      x = set(0)
    } else {
      w = setwd(pos)
      x = set(w) & bitmask(setbt(pos))
    }

    while (w < m) {
      if (x != 0L) {
        return timesWordSize(w) + firstBitNZ(x)
      }
      w += 1
      if (w < m) x = set(w)
    }
    -1
  }

  /**
   * Intersect set2 into set1 (set1 &= set2).
   */
  @inline def intersect(set1: Array[Long], set2: Array[Long], m: Int): Unit = {
    var i = 0
    while (i < m) {
      set1(i) &= set2(i)
      i += 1
    }
  }

  /**
   * Union set2 into set1 (set1 |= set2).
   */
  @inline def union(set1: Array[Long], set2: Array[Long], m: Int): Unit = {
    var i = 0
    while (i < m) {
      set1(i) |= set2(i)
      i += 1
    }
  }

  /**
   * Set difference: set1 &= ~set2
   */
  @inline def setDiff(set1: Array[Long], set2: Array[Long], m: Int): Unit = {
    var i = 0
    while (i < m) {
      set1(i) &= ~set2(i)
      i += 1
    }
  }

  /**
   * XOR set2 into set1.
   */
  @inline def xor(set1: Array[Long], set2: Array[Long], m: Int): Unit = {
    var i = 0
    while (i < m) {
      set1(i) ^= set2(i)
      i += 1
    }
  }

  /**
   * Test if set1 is equal to set2.
   */
  def setsEqual(set1: Array[Long], set2: Array[Long], m: Int): Boolean = {
    var i = 0
    while (i < m) {
      if (set1(i) != set2(i)) return false
      i += 1
    }
    true
  }

  /**
   * Test if the set is empty.
   */
  def isEmpty(set: Array[Long], m: Int): Boolean = {
    var i = 0
    while (i < m) {
      if (set(i) != 0L) return false
      i += 1
    }
    true
  }

  /**
   * Copy set2 to set1.
   */
  @inline def copySet(dest: Array[Long], src: Array[Long], m: Int): Unit = {
    System.arraycopy(src, 0, dest, 0, m)
  }
}
