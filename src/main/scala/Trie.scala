package io.hours.midnight

object Trie {
    def apply[T, F[T] <: Iterable[T]](list: List[F[T]]): TrieNode.Root[T, F] = TrieNode.Root(buildLeafMap(list))

    def from[T, F[T] <: Iterable[T]](list: List[F[T]]): TrieNode.Root[T, F] = apply(list)

    def fromString(s: List[String]): TrieNode.Root[Char, List] = from(s.map(_.toList))

    private def buildLeafMap[T, F[T] <: Iterable[T]](list: List[F[T]]): Map[T, TrieNode[T, F]] = list
        .map(iter => iter.splitAt(1))
        .map { case (headIter, tail) => headIter.headOption -> tail }
        .filter { case (headOption, _) => headOption.isDefined }
        .map { case (headOption, tail) => headOption.get -> tail }
        .groupBy { case (head, _) => head }
        .map { case (key, p) => key -> p.map(_._2) }
        .map { case (key, tail) =>
            key -> TrieNode.Node(key, buildLeafMap(tail.asInstanceOf[List[F[T]]]), tail.exists(_.isEmpty))
        }
}

enum TrieNode[T, F[T] <: Iterable[T]] {
    case Root(leaves: Map[T, TrieNode[T, F]]) extends TrieNode[T, F]
    case Node(character: T, leaves: Map[T, TrieNode[T, F]], accepted: Boolean) extends TrieNode[T, F]
}

extension[T, F[T] <: Iterable[T]] (trie: TrieNode[T, F]) {
    def exist(iter: F[T]): Boolean = trie match {
        case TrieNode.Root(map) => {
            if (iter.isEmpty) return false
            if (!map.contains(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
        }
        case TrieNode.Node(_, map, accepted) => {
            if (iter.isEmpty) return accepted
            if (!map.contains(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
        }
    }
}

extension (stringTrie: TrieNode[Char, List]) {
    def exist(iter: String): Boolean = stringTrie match {
        case TrieNode.Root(map) => {
            if (iter.isEmpty) return false
            if (!map.contains(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
        }
        case TrieNode.Node(_, map, accepted) => {
            if (iter.isEmpty) return accepted
            if (!map.contains(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
        }
    }
}