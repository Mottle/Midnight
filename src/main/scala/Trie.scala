package io.hours.midnight

import javax.swing.tree.TreeNode

object Trie {
    def apply[T, F[T] <: Iterable[T]](seq: F[T]*): Trie[T, F] = TrieNode.Root(buildLeafMap(seq.toList))

    def from[T, F[T] <: Iterable[T]](seq: F[T]*): Trie[T, F] = apply(seq: _*)

    def fromString(s: String*): Trie[Char, List] = from(s.map(_.toList): _*)

    private def buildLeafMap[T, F[T] <: Iterable[T]](list: List[F[T]]): Map[T, TrieTree[T, F]] = list
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

type Trie[T, F[T] <: Iterable[T]] = TrieNode.Root[T, F]
type TrieTree[T, F[T] <: Iterable[T]] = TrieNode[T, F]

enum TrieNode[T, F[T] <: Iterable[T]] {
    case Root(leaves: Map[T, TrieTree[T, F]]) extends TrieNode[T, F]
    case Node(character: T, leaves: Map[T, TrieTree[T, F]], accepted: Boolean) extends TrieNode[T, F]
}

extension [T, F[T] <: Iterable[T]] (trie: TrieTree[T, F]) {
    private def isValidNode: Boolean = trie match {
        case TrieNode.Root(leaves) => leaves.nonEmpty
        case TrieNode.Node(_, leaves, accepted) => leaves.nonEmpty || accepted
    }

    private def existString(iter: F[T]): Boolean = trie.exist(iter)

    private def contain(character: T): Boolean = trie match {
        case TrieNode.Root(leaves) => leaves.contains(character)
        case TrieNode.Node(_, leaves, _) => leaves.contains(character)
    }

    private def leaf(character: T): Option[TrieTree[T, F]] = trie match {
        case TrieNode.Root(leaves) => leaves.get(character)
        case TrieNode.Node(_, leaves, _) => leaves.get(character)
    }

    private def isAccepted(character: T): Boolean = trie match {
        case TrieNode.Root(_) => false
        case TrieNode.Node(ch, _, accepted) => (ch == character) && accepted
    }

    private def _insert(iter: F[T]): TrieTree[T, F] = trie match {
        case TrieNode.Root(leaves) =>
            if(iter.isEmpty) return trie
            if(leaves.contains(iter.head)) TrieNode.Root(leaves.updated(iter.head, leaves(iter.head)._insert(iter.tail.asInstanceOf)))
            else TrieNode.Root(leaves + (iter.head -> TrieNode.Node(iter.head, Map.empty, false)._insert(iter.tail.asInstanceOf)))
        case TrieNode.Node(character, leaves, accepted) =>
            if(iter.isEmpty) return TrieNode.Node(character, leaves, true)
            if(leaves.contains(iter.head)) TrieNode.Node(character, leaves.updated(iter.head, leaves(iter.head)._insert(iter.tail.asInstanceOf)), accepted)
            else TrieNode.Node(character, leaves + (iter.head -> TrieNode.Node(iter.head, Map.empty, false)._insert(iter.tail.asInstanceOf)), accepted)
    }

    private def insertString(s: F[T]): Trie[T, F] = trie.insert(s)
}

extension[T, F[T] <: Iterable[T]] (trie: TrieTree[T, F]) {
    def exist(iter: F[T]): Boolean = trie match {
        case TrieNode.Root(map) =>
            if (iter.isEmpty) return false
            if (!trie.contain(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
        case TrieNode.Node(_, map, accepted) =>
            if (iter.isEmpty) return accepted
            if (!trie.contain(iter.head)) false
            else map(iter.head).exist(iter.tail.asInstanceOf)
    }

    def insert(iter: F[T]): Trie[T, F] = trie._insert(iter).asInstanceOf

    def inserts(seq: F[T]*): Trie[T, F] = seq.foldLeft(trie.asInstanceOf)((t, iter) => t.insert(iter))
}

extension (stringTrie: TrieNode[Char, List]) {
    def exist(string: String): Boolean = stringTrie.existString(string.toList)

    def insert(string: String): Trie[Char, List] = stringTrie.insertString(string.toList)
}