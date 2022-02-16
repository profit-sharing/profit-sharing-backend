package models

import helpers.Utils
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.{ErgoId, InputBox}
import special.collection.Coll

case class Config(id: String, checkpoint: Long, minErgShare: Long, minTokenShare: Long, ticketCount: Long,
                  stakeCount: Long, fee: Long, minTicketValue: Long, minBoxVal: Long)

case class Distribution(id: String, checkpoint: Long, ergShare: Long, tokenShare: Long, ticketCount: Long,
                        fee: Long)

case class Ticket(id: String, stakeCount: Long, InitialCheckpoint: Long, fee: Long, minBoxVal: Long, checkpoint: Long,
                  recipientAddress: ErgoAddress, reservedTokenId: ErgoId)

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

object Distribution{
  def apply(distributionBox: InputBox): Distribution ={
    val r4 = distributionBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
    val checkpoint = r4(0)
    val ergShare = r4(1)
    val tokenShare = r4(2)
    val fee = r4(3)
    val ticketCount = distributionBox.getRegisters.get(1).getValue.asInstanceOf[Long]
    new Distribution(distributionBox.getId.toString, checkpoint, ergShare, tokenShare, ticketCount, fee)
  }
}

object Ticket{
  def apply(ticketBox: InputBox): Ticket ={
    val stakeCount = ticketBox.getTokens.get(1).getValue
    val ticketAddress = Utils.getAddress(ticketBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]].toArray)
    val ticketR4 = ticketBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
    val initialCheckpoint = ticketR4(0)
    val checkpoint = ticketR4(1)
    val fee = ticketR4(2)
    val minBoxVal = ticketR4(3)
    val reservedToken: ErgoId = new ErgoId(ticketBox.getRegisters.get(2).getValue.asInstanceOf[Coll[Byte]].toArray)
    new Ticket(ticketBox.getId.toString, stakeCount, initialCheckpoint, fee, minBoxVal, checkpoint, ticketAddress, reservedToken)
  }
}
