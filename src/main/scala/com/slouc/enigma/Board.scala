package com.slouc.enigma

/**
 * Board for Enigma (plugboard, reflector).
 * 
 * @author slouc
 *
 */
class Board(list: List[Char]) {

  private val (k, v) = (list.take(list.size / 2), list.drop(list.size / 2))
  private val board = (k ++ v zip v ++ k) toMap

  def encode(c: Char): Char = {
    if (board.contains(c)) board(c)
    else c
  }

  def encodeInv(c: Char): Char = {
    val binv = board map (_.swap)
    if (binv.contains(c)) binv(c)
    else c
  }

  def copy = new Board(list)
}