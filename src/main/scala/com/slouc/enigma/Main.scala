package com.slouc.enigma

/**
 * @author slouc
 *
 */
object Main extends App {

  val input = "geronimo"

  val params = Enigma.createParams

  val enigmaEnc = new Enigma(params)
  val enigmaDec = new Enigma(params)

  val encrypted = enigmaEnc.encode(input)
  val decrypted = enigmaDec.encode(encrypted)

  println(encrypted + "\n" + decrypted)

}