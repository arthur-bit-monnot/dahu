package dahu.utils

class BiMap[@specialized(Int) A, @specialized(Int) B] private (forward: debox.Map[A, B],
                                                               backward: debox.Map[B, A]) {
  def size: Int = forward.size

  def add(a: A, b: B): Unit = {
    require(!forward.contains(a))
    require(!backward.contains(b))
    forward += ((a, b))
    backward += ((b, a))
  }

  def get(a: A): B = forward(a)
  def coget(b: B): A = backward(b)

  def contains(a: A): Boolean = forward.contains(a)
  def cocontains(b: B): Boolean = backward.contains(b)

  def iterator: Iterator[(A, B)] = forward.iterator
}

object BiMap {
  def apply[A: ClassTag, B: ClassTag](): BiMap[A, B] =
    new BiMap(debox.Map[A, B](), debox.Map[B, A]())
}
