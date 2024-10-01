package caos.src.main.scala.caos.frontend

def add(widget:String, tip:String, body:String) =
    new Documentation(docs+(widget->(tip->body)))
  def add(elements:Iterable[((String,String),String)]) =
    new Documentation(docs++elements.map((xy,z)=>(xy._1,(xy._2,z))).toMap)
  def get(widget:String): Option[(String,String)] =
    docs.get(widget)
  def widgets:Iterable[String] = docs.keys

case class Documentation(private val docs: Documentation.Documents = Map()):
  type Documents = Map[String,(String,String)]

