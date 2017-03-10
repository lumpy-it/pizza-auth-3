package moe.pizza.auth.adapters

import moe.pizza.auth.interfaces.PilotGrader
import moe.pizza.auth.models.Pilot
import moe.pizza.auth.models.Pilot.Status
import org.log4s.getLogger

/**
  * Created by Andi on 28/02/2016.
  */
class GraderChain(graders: Seq[PilotGrader]) extends PilotGrader {
  private[this] val log = getLogger
  log.debug(s"created GraderChain with ${graders.length} graders")

  override def grade(p: Pilot): Status.Value = {
    log.debug("grading pilot with grader chain")
    val r = graders.foldLeft(Status.unclassified) { (status, nextGrader) =>
      log.debug(s"inner grader loop ${status}, ${nextGrader}")
      status match {
        case Status.unclassified => nextGrader.grade(p)
        case s => s
      }
    }
    log.debug(s"returning a grade status of ${r} for ${p.uid}")
    r
  }
}
