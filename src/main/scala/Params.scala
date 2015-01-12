import org.lemurproject.galago.utility.Parameters

/**
 * Created by pv on 1/12/15.
 */
object Params {

  val wikiDoc = {
    val p = new Parameters()
    p.set("mu", 96400.0)
    p.set("defaultSmoothingMu", 96400.0)
    p.set("uniw", 0.85)
    p.set("odw", 0.1)
    p.set("uww", 0.05)
    p.set("deltaReady", true)
    p
  }

  val robustTitleDoc = {

    // these parameters from an sjh
    // {"mu": 1269.5795757425599, "od2w": 0.079062895302894176, "uniw": 0.87264448809030171, "uw2w": 0.048292616606804145, "joinScore" : "0.26279", "score" : "0.26409"}
    val p = new Parameters()
    p.set("mu", 1269.0)
    p.set("defaultSmoothingMu", 1269.0)
    p.set("uniw", 0.87264)
    p.set("odw", 0.07906)
    p.set("uww", 0.04829)
    p.set("deltaReady", true)
    p
  }

  val clueb = {
    // these parameters from an sjh
    // {"mu": 4311.383584722118, "od2w": 0.05306743841197932, "uniw": 0.84584290673192286, "uw2w": 0.10108965485609786, "avg2012Score" : "0.13469"}
    val p = new Parameters()
    p.set("mu", 2500.0)
    p.set("defaultSmoothingMu", 2500.0)
    p.set("uniw", 0.85)
    p.set("odw", 0.05)
    p.set("uww", 0.10)
    p.set("deltaReady", true)
    p
  }
}
