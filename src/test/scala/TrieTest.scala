package io.hours.midnight

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.HashMap
import scala.util.Random

class TrieTest extends AnyFlatSpec {
    "char sequence trie build" should "passed" in {
        val trie = Trie.fromString("123", "122", "2")
        assert(trie == TrieNode.Root(
            HashMap(
                '1' -> TrieNode.Node('1', HashMap(
                    '2' -> TrieNode.Node('2', HashMap(
                        '2' -> TrieNode.Node('2', HashMap.empty, true),
                        '3' -> TrieNode.Node('3', HashMap.empty, true)
                    ), false),
                ), false),
                '2' -> TrieNode.Node('2', HashMap.empty, true)
            )))
    }

    "char sequence trie exist test" should "passed" in {
        val trie = Trie.fromString("1223", "12233", "234")
        assert(trie.exist("1223"))
        assert(trie.exist("12233"))
        assert(!trie.exist("123123"))
    }

    "random char sequence trie exist test" should "passed" in {
        val random = Random(114514 * System.currentTimeMillis() % 1919810)
        val randomList = Range(1, 114514).map(_ => random.nextInt().toString).toList
        val trie = Trie.fromString(randomList: _*)
        for (i <- 0 to 100) {
            val string = randomList(i)
            assert(trie.exist(string))
        }
    }

    "char sequence trie update " should "passed" in {
        val trie = Trie.fromString("ab", "abc", "abcd", "cd")
        assert(trie.insert("abcde") == Trie.fromString("ab", "abc", "abcd", "cd", "abcde"))
    }
}
