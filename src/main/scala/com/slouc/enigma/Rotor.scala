package com.slouc.enigma

import scala.util.Random

/**
 * Rotor for Enigma 
 * Wires right side characters to left side characters, e.g.
 * Q -- A
 * W -- B
 * E -- C
 *
 * @author slouc
 *
 */
class Rotor(left: List[Char], right: List[Char]) {

  private var rotor = makeRotor
  private var counter = 0

  private def makeRotor: Map[Char, Char] = {
    if (left.size != right.size)
      throw new Exception("Invalid rotor config")
    (left zip right) toMap
  }

  def rotate: Boolean = {
    val rotated = for (k <- rotor.keys) yield {
      k match {
        case 'z' => ('a', rotor(k))
        case _ => ((k + 1).toChar, rotor(k))
      }
    }
    rotor = rotated.toMap
    counter = (counter + 1) % left.size
    counter == 0
  }

  def encode(c: Char): Char = rotor(c)

  def encodeInv(c: Char): Char = {
    val rinv = rotor map (_.swap)
    rinv(c)
  }

  def copy = new Rotor(left, right)
}