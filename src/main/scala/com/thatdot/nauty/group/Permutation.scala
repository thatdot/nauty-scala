package com.thatdot.nauty.group

/**
 * A permutation of {0, 1, ..., n-1}.
 *
 * Internally stored as an array where perm(i) = the image of i under the permutation.
 * Immutable once constructed.
 *
 * @param images The permutation array: images(i) = image of element i
 */
final class Permutation private(private val images: Array[Int]) extends Equals {
  /** The degree (number of elements being permuted) */
  def n: Int = images.length

  /** Alias for n */
  def size: Int = images.length

  /** Apply the permutation to an element */
  @inline def apply(i: Int): Int = images(i)

  /** Get the image of an element */
  @inline def image(i: Int): Int = images(i)

  /** Get the underlying array (defensive copy) */
  def toArray: Array[Int] = images.clone()

  /** Get the underlying array without copying (for internal use) */
  private[nauty] def unsafeArray: Array[Int] = images

  /**
   * Compose this permutation with another: (this * other)(x) = this(other(x))
   * This means: first apply other, then apply this.
   */
  def compose(other: Permutation): Permutation = {
    require(n == other.n, s"Cannot compose permutations of different degrees: $n vs ${other.n}")
    val result = new Array[Int](n)
    var i = 0
    while (i < n) {
      result(i) = images(other.images(i))
      i += 1
    }
    new Permutation(result)
  }

  /** Alias for compose */
  def *(other: Permutation): Permutation = compose(other)

  /**
   * Compute the inverse permutation.
   */
  def inverse: Permutation = {
    val result = new Array[Int](n)
    var i = 0
    while (i < n) {
      result(images(i)) = i
      i += 1
    }
    new Permutation(result)
  }

  /**
   * Test if this is the identity permutation.
   */
  def isIdentity: Boolean = {
    var i = 0
    while (i < n) {
      if (images(i) != i) return false
      i += 1
    }
    true
  }

  /**
   * Count the number of fixed points.
   */
  def numFixedPoints: Int = {
    var count = 0
    var i = 0
    while (i < n) {
      if (images(i) == i) count += 1
      i += 1
    }
    count
  }

  /**
   * Get the set of fixed points.
   */
  def fixedPoints: Set[Int] = {
    val result = Set.newBuilder[Int]
    var i = 0
    while (i < n) {
      if (images(i) == i) result += i
      i += 1
    }
    result.result()
  }

  /**
   * Get the set of moved points (non-fixed points).
   */
  def movedPoints: Set[Int] = {
    val result = Set.newBuilder[Int]
    var i = 0
    while (i < n) {
      if (images(i) != i) result += i
      i += 1
    }
    result.result()
  }

  /**
   * Compute the cycle structure of this permutation.
   * Returns a list of cycles, where each cycle is a list of elements.
   */
  def cycles: List[List[Int]] = {
    val seen = new Array[Boolean](n)
    val result = List.newBuilder[List[Int]]

    var i = 0
    while (i < n) {
      if (!seen(i)) {
        val cycle = List.newBuilder[Int]
        var j = i
        while (!seen(j)) {
          seen(j) = true
          cycle += j
          j = images(j)
        }
        val cycleList = cycle.result()
        if (cycleList.length > 1) {  // Don't include fixed points as 1-cycles
          result += cycleList
        }
      }
      i += 1
    }
    result.result()
  }

  /**
   * Compute the order of this permutation (smallest k > 0 such that p^k = identity).
   */
  def order: Int = {
    val cycleLengths = cycles.map(_.length)
    if (cycleLengths.isEmpty) 1
    else cycleLengths.reduce(lcm)
  }

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def lcm(a: Int, b: Int): Int = (a / gcd(a, b)) * b

  /**
   * Compute the k-th power of this permutation.
   */
  def pow(k: Int): Permutation = {
    if (k == 0) Permutation.identity(n)
    else if (k == 1) this
    else if (k < 0) inverse.pow(-k)
    else {
      // Use repeated squaring
      var result = Permutation.identity(n)
      var base = this
      var exp = k
      while (exp > 0) {
        if ((exp & 1) == 1) {
          result = result * base
        }
        base = base * base
        exp >>= 1
      }
      result
    }
  }

  /**
   * Format as cycle notation.
   */
  def toCycleString: String = {
    val cs = cycles
    if (cs.isEmpty) "()"
    else cs.map(c => "(" + c.mkString(" ") + ")").mkString
  }

  /**
   * Format as array notation.
   */
  def toArrayString: String = images.mkString("[", ", ", "]")

  override def toString: String = toCycleString

  override def equals(obj: Any): Boolean = obj match {
    case other: Permutation => java.util.Arrays.equals(images, other.images)
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Permutation]

  override def hashCode(): Int = java.util.Arrays.hashCode(images)
}

object Permutation {
  /**
   * Create the identity permutation of degree n.
   */
  def identity(n: Int): Permutation = {
    val arr = new Array[Int](n)
    var i = 0
    while (i < n) {
      arr(i) = i
      i += 1
    }
    new Permutation(arr)
  }

  /**
   * Create a permutation from an array.
   * The array is copied.
   */
  def fromArray(images: Array[Int]): Permutation = {
    validatePermutation(images)
    new Permutation(images.clone())
  }

  /**
   * Create a permutation from an array without copying (for internal use).
   */
  private[nauty] def unsafeFromArray(images: Array[Int]): Permutation = {
    new Permutation(images)
  }

  /**
   * Create a permutation from a sequence of images.
   */
  def apply(images: Int*): Permutation = {
    fromArray(images.toArray)
  }

  /**
   * Create a permutation from cycle notation.
   * Example: fromCycles(5, List(0, 1, 2), List(3, 4)) creates (0 1 2)(3 4)
   */
  def fromCycles(n: Int, cycles: List[Int]*): Permutation = {
    val arr = new Array[Int](n)
    // Initialize to identity
    var i = 0
    while (i < n) {
      arr(i) = i
      i += 1
    }
    // Apply cycles
    for (cycle <- cycles if cycle.nonEmpty) {
      val len = cycle.length
      var j = 0
      while (j < len) {
        arr(cycle(j)) = cycle((j + 1) % len)
        j += 1
      }
    }
    new Permutation(arr)
  }

  /**
   * Create a transposition (swap two elements).
   */
  def transposition(n: Int, i: Int, j: Int): Permutation = {
    require(i >= 0 && i < n && j >= 0 && j < n, s"Invalid transposition ($i, $j) for n=$n")
    val arr = new Array[Int](n)
    var k = 0
    while (k < n) {
      arr(k) = k
      k += 1
    }
    arr(i) = j
    arr(j) = i
    new Permutation(arr)
  }

  /**
   * Create a cyclic permutation (0 -> 1 -> 2 -> ... -> n-1 -> 0).
   */
  def cyclic(n: Int): Permutation = {
    val arr = new Array[Int](n)
    var i = 0
    while (i < n - 1) {
      arr(i) = i + 1
      i += 1
    }
    arr(n - 1) = 0
    new Permutation(arr)
  }

  /**
   * Validate that an array represents a valid permutation.
   */
  private def validatePermutation(arr: Array[Int]): Unit = {
    val n = arr.length
    val seen = new Array[Boolean](n)
    var i = 0
    while (i < n) {
      val img = arr(i)
      require(img >= 0 && img < n, s"Image $img is out of range [0, $n)")
      require(!seen(img), s"Duplicate image: $img appears twice")
      seen(img) = true
      i += 1
    }
  }
}
