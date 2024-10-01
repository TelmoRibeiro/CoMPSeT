package caos.src.main.scala.caos.view

val options:Options

/**
 * Created by   on 13/07/2021
 */

trait OptionView:
  type Options = Map[String,String]
  
  object OptionView
  case class OptMermaid(options:Options) extends OptionView
  case class OptText(options:Options)    extends OptionView
  
  
