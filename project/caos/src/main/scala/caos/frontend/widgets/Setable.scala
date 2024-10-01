package caos.src.main.scala.caos.frontend.widgets

trait Setable[-A] {
  /**
   * sets the value of a given widget, e.g., content text.
   * @param value
   */
  def setValue(value:A): Unit
}

