package ProfitSharing

import org.ergoplatform.appkit.{BlockchainContext, ConstantsBuilder, ErgoContract, ErgoId}
import helpers.{Configs, Utils}
import network.Client

import javax.inject.{Inject, Singleton}

@Singleton
class Contracts @Inject()(client: Client, utils: Utils){
  lazy val income: ErgoContract = generateIncomeContract()
  lazy val distribution: ErgoContract = generateDistributionContract()
  lazy val ticket: ErgoContract = generateTicketContract()
  lazy val config: ErgoContract = generateConfigContract()

  private def generateIncomeContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val income = ctx.compileContract(ConstantsBuilder.create()
        .item("maxFee", Configs.maxFee)
        .item("configNFT", ErgoId.create(Configs.token.configNFT).getBytes)
        .build(), Scripts.incomeScript)
      val address = utils.getContractAddress(income)
      println(s"income address is : \t\t\t$address")
      income
    })
  }

  private def generateDistributionContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val contract = ctx.compileContract(ConstantsBuilder.create()
        .item("configNFT", ErgoId.create(Configs.token.configNFT).getBytes)
        .item("lockingToken", ErgoId.create(Configs.token.locking).getBytes)
        .item("stakingToken", ErgoId.create(Configs.token.staking).getBytes)
        .build(), Scripts.distributionScript)
      val address = utils.getContractAddress(contract)
      println(s"distribution address is : \t\t\t$address")
      contract
    })
  }

  private def generateTicketContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val contract = ctx.compileContract(ConstantsBuilder.create()
        .item("configNFT", ErgoId.create(Configs.token.configNFT).getBytes)
        .item("distributionToken", ErgoId.create(Configs.token.distribution).getBytes)
        .build(), Scripts.ticketScript)
      val address = utils.getContractAddress(contract)
      println(s"ticket address is : \t\t\t$address")
      contract
    })
  }

  private def generateConfigContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val contract = ctx.compileContract(ConstantsBuilder.create()
        .item("distributionToken", ErgoId.create(Configs.token.distribution).getBytes)
        .item("lockingToken", ErgoId.create(Configs.token.locking).getBytes)
        .item("stakingToken", ErgoId.create(Configs.token.staking).getBytes)
        .item("distributionHash", utils.getContractScriptHash(distribution))
        .item("ticketHash", utils.getContractScriptHash(ticket))
        .build(), Scripts.configScript)
      val address = utils.getContractAddress(contract)
      println(s"config address is : \t\t\t$address")
      contract
    })
  }

}

