// Copyright (c) 2013-2020 Rob Norris and Contributors
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package doobie.util

trait Colors {
  def BLACK: String
  def RED: String
  def GREEN: String
  def YELLOW: String
  def BLUE: String
  def MAGENTA: String
  def CYAN: String
  def WHITE: String
  def BLACK_B: String
  def RED_B: String
  def GREEN_B: String
  def YELLOW_B: String
  def BLUE_B: String
  def MAGENTA_B: String
  def CYAN_B: String
  def WHITE_B: String
  def RESET: String
  def BOLD: String
  def UNDERLINED: String
  def BLINK: String
  def REVERSED: String
  def INVISIBLE: String
}

object Colors {

  object Ansi extends Colors {
    val BLACK = "\u001b[30m"
    val RED = "\u001b[31m"
    val GREEN = "\u001b[32m"
    val YELLOW = "\u001b[33m"
    val BLUE = "\u001b[34m"
    val MAGENTA = "\u001b[35m"
    val CYAN = "\u001b[36m"
    val WHITE = "\u001b[37m"
    val BLACK_B = "\u001b[40m"
    val RED_B = "\u001b[41m"
    val GREEN_B = "\u001b[42m"
    val YELLOW_B = "\u001b[43m"
    val BLUE_B = "\u001b[44m"
    val MAGENTA_B = "\u001b[45m"
    val CYAN_B = "\u001b[46m"
    val WHITE_B = "\u001b[47m"
    val RESET = "\u001b[0m"
    val BOLD = "\u001b[1m"
    val UNDERLINED = "\u001b[4m"
    val BLINK = "\u001b[5m"
    val REVERSED = "\u001b[7m"
    val INVISIBLE = "\u001b[8m"
  }

  object None extends Colors {
    val BLACK = ""
    val RED = ""
    val GREEN = ""
    val YELLOW = ""
    val BLUE = ""
    val MAGENTA = ""
    val CYAN = ""
    val WHITE = ""
    val BLACK_B = ""
    val RED_B = ""
    val GREEN_B = ""
    val YELLOW_B = ""
    val BLUE_B = ""
    val MAGENTA_B = ""
    val CYAN_B = ""
    val WHITE_B = ""
    val RESET = ""
    val BOLD = ""
    val UNDERLINED = ""
    val BLINK = ""
    val REVERSED = ""
    val INVISIBLE = ""
  }

}
