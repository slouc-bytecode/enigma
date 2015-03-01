package com.slouc.enigma

import scala.collection._
import scala.util.Random

case class Params(
  val plugboard: Board,
  val reflector: Board,
  val rotor1: Rotor,
  val rotor2: Rotor,
  val rotor3: Rotor)

/**
 * Defines method for creating new parameters for an Enigma machine.
 *
 * @author slouc
 *
 */
object Enigma {

  def createParams(): Params = {

    val left = ('a' to 'z').toList
    def right = Random.shuffle(('a' to 'z').toList)

    val plugboard = new Board(right.take(20))
    val reflector = new Board(right.take(26))
    val rotor1, rotor2, rotor3 = new Rotor(left, right)

    Params(plugboard, reflector, rotor1, rotor2, rotor3)
  }
}

/**
 * Builds an Enigma machine using given Params and provides encoding ability.
 *
 * @author slouc
 *
 */
class Enigma(p: Params) {

  private val plugboard = p.plugboard.copy
  private val reflector = p.reflector.copy
  private val rotor1 = p.rotor1.copy
  private val rotor2 = p.rotor2.copy
  private val rotor3 = p.rotor3.copy

  private def encodeRec(fl: List[Char => Char], c: Char): Char = {
    fl match {
      case Nil => c
      case _ => fl.head(encodeRec(fl.tail, c))
    }
  }

  private def encode(c: Char) = {
    val funs = List[Char => Char](
      plugboard.encodeInv _,
      rotor3.encodeInv _,
      rotor2.encodeInv _,
      rotor1.encodeInv _,
      reflector.encode _,
      rotor1.encode _,
      rotor2.encode _,
      rotor3.encode _,
      plugboard.encode _)

    encodeRec(funs, c)
  }

  def encode(s: String): String = {
    for (c <- s) yield {
      if (rotor3.rotate)
        if (rotor2.rotate)
          rotor1.rotate
      encode(c)
    }
  }

  def copy = new Enigma(p)

}
    
    