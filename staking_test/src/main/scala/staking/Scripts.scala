package staking

object Scripts {

//  val incomeScript: String =
//    s"""
//       |  val bool = true
//       |  sigmaProp(bool)
//       |""".stripMargin

//  lazy val incomeScript: String =
//    s"""
//       |  val incomeMerge =
//       |    allOf(Coll(
//       |      INPUTS.size == 10,
//       |      OUTPUTS.size == 2,
//       |      OUTPUTS(0).propositionBytes == SELF.propositionBytes,
//       |      OUTPUTS(1).value <= maxFee
//       |      // ERG can not burn so more checking is not required
//       |    )) &&
//       |    if(SELF.tokens.size > 0) {
//       |      allOf(Coll(
//       |        OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
//       |        OUTPUTS(0).tokens(0)._2 == INPUTS(0).tokens(0)._2 + INPUTS(1).tokens(0)._2 + INPUTS(2).tokens(0)._2 + INPUTS(3).tokens(0)._2 +
//       |                                INPUTS(4).tokens(0)._2 + INPUTS(5).tokens(0)._2 + INPUTS(6).tokens(0)._2 + INPUTS(7).tokens(0)._2 +
//       |                                INPUTS(8).tokens(0)._2 + INPUTS(9).tokens(0)._2
//       |      ))
//       |    } else {true}
//       |
//       |  // TODO
//       |  val incomeDistribution = false
//       |
//       |  sigmaProp(incomeMerge || incomeDistribution)
//       |""".stripMargin

  lazy val incomeScript: String =
    s"""
       |  if(SELF.tokens.size == 0)
       |  {
       |    val incomeMerge = allOf(Coll(
       |      INPUTS.size == 10,
       |      OUTPUTS.size == 2,
       |      OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |      OUTPUTS(1).value <= maxFee
       |      // ERG can not burn so more checking is not required
       |    ))
       |    sigmaProp(incomeMerge)
       |  }
       |  else{
       |    val incomeMerge = allOf(Coll(
       |      INPUTS.size == 10,
       |      OUTPUTS.size == 2,
       |      OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |      OUTPUTS(1).value <= maxFee,
       |      // ERG can not burn so more checking is not required
       |      OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |      OUTPUTS(0).tokens(0)._2 == INPUTS(0).tokens(0)._2 + INPUTS(1).tokens(0)._2 + INPUTS(2).tokens(0)._2 + INPUTS(3).tokens(0)._2 +
       |                                 INPUTS(4).tokens(0)._2 + INPUTS(5).tokens(0)._2 + INPUTS(6).tokens(0)._2 + INPUTS(7).tokens(0)._2 +
       |                                 INPUTS(8).tokens(0)._2 + INPUTS(9).tokens(0)._2
       |      ))
       |      sigmaProp(incomeMerge)
       |    }
       |
       |""".stripMargin

}
