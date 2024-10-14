package mpst.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS
import caos.sos.SOS.*
import caos.view.*
import mpst.projection.{AsyncProjection, SyncProjection}
import mpst.syntax.{Parser, Protocol}
import mpst.syntax.Type.*
import mpst.utilities.Environment
import mpst.utilities.Multiset
import mpst.wellformedness.*
import mpst.operational_semantic.{MPSTSemanticWrapper, NetworkMultisetWrapper, SyncTraverseWrapper}
import mpst.operational_semantic.MPSTSemanticWrapper.*

object CaosConfigurator extends Configurator[Global]:
  override val name:String = "CoMPSeT - Comparison of Multiparty Session Types"

  override val languageName:String = "Session"

  override val parser:String=>Global = (input:String) => Parser(input)

  //********** SETTINGS DEFINITION **********//
  private def mkInterleavingOption =
    Setting(name = "Interleaving", render = true)
  end mkInterleavingOption

  private def mkCommModelOption =
    val asyncMSChoice = Setting(name = "Async MS", render = true)
    val asyncCSChoice = Setting(name = "Async CS", render = true)
    val syncChoice    = Setting(name = "Sync",     render = true)
    Setting(name = "Comm Model", children = List(asyncMSChoice, asyncCSChoice, syncChoice), render = true)
  end mkCommModelOption

  private val ConfigA = Setting(name = "Config A", children = List(mkInterleavingOption, mkCommModelOption), render = true)
  private val ConfigB = Setting(name = "Config B", children = List(mkInterleavingOption, mkCommModelOption), render = true)

  override val setting: Setting = Setting(name = "Settings", children = List(ConfigA, ConfigB), render = true)
  //********** SETTINGS DEFINITION **********//

  //********** OPTIONS DEFINITION **********//
  private def mkNoInterleavingWidget =
    view(
      viewProg = {
        (global: Global) =>
          def hasInterleaving(protocol: Protocol): Boolean =
            val hasInterleaving = Protocol.hasInterleaving(protocol)
            if hasInterleaving then RuntimeException(s"interleaving construct found in $protocol")
            hasInterleaving
          end hasInterleaving

          val locals = AsyncProjection.projectionWithAgent(global)

          s"Has Interleaving: ${hasInterleaving(global) || locals.exists(local => hasInterleaving(local._2))}"
      },
      typ = Text
    )
  end mkNoInterleavingWidget

  private def mkNoInterleavingCheck =
    check(
      (global: Global) =>
        def hasInterleaving(protocol: Protocol): Seq[String] =
          if Protocol.hasInterleaving(protocol) then
            Seq(s"interleaving construct found in $protocol")
          else
            Seq.empty

        val localsWithParticipant = AsyncProjection.projectionWithAgent(global)
        hasInterleaving(global) ++ localsWithParticipant.flatMap(localWithParticipant => hasInterleaving(localWithParticipant._2)).toSeq
    )
  end mkNoInterleavingCheck

  private def mkAsyncMSWidget =
    steps(
      initialSt = (global: Global) =>
        val locals = AsyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        (locals, Multiset(), localEnv),
      sos = NetworkMultisetWrapper,
      viewSt = (loc: Set[(Participant, Local)], pen: Multiset[Action], env: Map[Participant, Map[Variable, Local]]) =>
        loc.map { case (agent, local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    )
  end mkAsyncMSWidget

  private val AsyncMSOptionA = Option(Map("Settings.Config A.Comm Model.Async MS" -> true), List("Async MS A" -> mkAsyncMSWidget))
  private val AsyncMSOptionB = Option(Map("Settings.Config B.Comm Model.Async MS" -> true), List("Async MS B" -> mkAsyncMSWidget))

  private val NoInterleavingA = Option(Map("Settings.Config A.Interleaving" -> false), List("No Interleaving A" -> mkNoInterleavingWidget))
  private val NoInterleavingB = Option(Map("Settings.Config B.Interleaving" -> false), List("No Interleaving B" -> mkNoInterleavingWidget))

  private val NoInterleavingCheckA = Option(Map("Settings.Config B.Interleaving" -> false), List("No Interleaving B" -> mkNoInterleavingCheck))
  private val NoInterleavingCheckB = Option(Map("Settings.Config B.Interleaving" -> false), List("No Interleaving B" -> mkNoInterleavingCheck))

  override val options: List[Option[Global]] = List(AsyncMSOptionA, AsyncMSOptionB, NoInterleavingCheckA, NoInterleavingCheckB)
  //override val options: List[Option[Global]] = List(AsyncMSOptionA, AsyncMSOptionB, NoInterleavingA, NoInterleavingB)
  //********** OPTIONS DEFINITION **********//

  override val examples:Seq[Example] = List(
    "MasterWorkers"
      -> "m>wA:Work ; m>wB:Work ; (wA>m:Done || wB>m:Done)",

    "SimpleRecursion"
      -> "def X in (m>w:Task ; X)"
  )
  
  override val widgets:Seq[(String,WidgetInfo[Global])] = List(
    "parsed global"
      -> view(
      viewProg = (global:Global) =>
        s"parsed global: ${global.toString}",
      typ = Text),

    "well formedness" // there is a check() caos function I might want to explore
      -> view(
      viewProg = (global:Global) =>
        s"${wellFormedness(global)}",
      typ = Text),

    // @ telmo - async not properly working
    "my spin on \"Choreo Semantics \""
      -> steps(
      initialSt = (global:Global) =>
        global -> Environment.globalEnv(global),
      sos       = MPSTSemanticWrapper,
      viewSt    = (pro:Protocol,env:Map[Variable,Protocol]) => pro.toString,
      typ       = Text,
    ),

    "Composed Local MSNet Semantics - lazy view"
      -> steps(
      initialSt = (global:Global) =>
        val locals   = AsyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        (locals,Multiset(),localEnv),
      sos = NetworkMultisetWrapper,
      viewSt = (loc:Set[(Participant,Local)], pen:Multiset[Action], env:Map[Participant,Map[Variable,Local]]) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    ),

    "Composed Local Sync Semantics - lazy view"
      -> steps(
      initialSt = (global:Global) =>
        val locals   = SyncProjection.projectionWithAgent(global)
        val localEnv = Environment.localEnv(global)
        locals -> localEnv,
      sos = SyncTraverseWrapper,
      viewSt = (loc:Set[(Participant,Local)], env:LocalEnv) =>
        loc.map { case (agent,local) => s"$agent: $local" }.mkString("\n"),
      typ = Text,
    ),

    "Global LTS - with lazy environment"
      -> lts(
      initialSt = (global:Global) =>
        global -> Environment.globalEnv(global),
      sos = MPSTSemanticWrapper,
      viewSt = (global:Global,environment:Map[Variable,Global]) => environment.toString,
    ),

    "Local LTS - with lazy environment"
     -> viewMerms((global:Global) =>
      val result = for agent -> local <- AsyncProjection.projectionWithAgent(global) yield
        agent -> caos.sos.SOS.toMermaid(
          sos      = MPSTSemanticWrapper,
          s        = local -> Environment.singleLocalEnv(local),
          showSt   = (protocol:Protocol,environment:Map[Variable,Protocol]) => environment.toString,
          showAct  = _.toString,
          maxNodes = 100,
        )
      result.toList
      )
  )

  // @ telmo - expand this to better error handling
  // @ telmo - "check" might be useful here
  private def wellFormedness(global:Global):String =
    if DependentlyGuarded(global) && WellBounded(global) && WellBranched(global) && WellChannelled(global) && WellCommunicated(global)
    then "WELL FORMED!"
    else "NOT WELL FORMED!"
  end wellFormedness
end CaosConfigurator