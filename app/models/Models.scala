package models

import org.ergoplatform.appkit.InputBox
import special.collection.Coll

case class Config(id: String, checkpoint: Long, minErgShare: Long, minTokenShare: Long, ticketCount: Long,
                  stakeCount: Long, fee: Long, minTicketValue: Long, minBoxVal: Long)

case class Distribution(id: String)

object Config{
  def apply(configBox: InputBox): Config ={
    val r4 = configBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
    val checkpoint = r4(0)
    val minErgShare = r4(1)
    val minTokenShare = r4(2)
    val ticketCount = r4(3)
    val stakeCount = r4(4)
    val fee = r4(5)
    val minTicketValue = r4(6)
    val minBoxVal = r4(7)
    new Config(configBox.getId.toString, checkpoint, minErgShare, minTokenShare, ticketCount, stakeCount,
      fee, minTicketValue, minBoxVal)
  }
}
