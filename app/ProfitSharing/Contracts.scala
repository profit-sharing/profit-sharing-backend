package ProfitSharing

import org.ergoplatform.appkit.{Address, ConstantsBuilder, ErgoContract, ErgoId}
import helpers.{Configs, Utils}
import network.Client
import scorex.util.encode.{Base16, Base64}

import javax.inject.{Inject, Singleton}

@Singleton
class Contracts @Inject()(client: Client){
  lazy val income: ErgoContract = generateIncomeContract()
  lazy val distribution: ErgoContract = generateDistributionContract()
  lazy val ticket: ErgoContract = generateTicketContract()
  lazy val config: ErgoContract = generateConfigContract()
  lazy val incomeAddress: Address = Utils.generateAddress(income)
  lazy val configAddress: Address = Utils.generateAddress(config)
  lazy val distributionAddress: Address = Utils.generateAddress(distribution)
  lazy val ticketAddress: Address = Utils.generateAddress(ticket)

  private def generateIncomeContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val script = Scripts.incomeScript
        .replace("MAX_FEE", Configs.incomeMerge.maxFee.toString)
        .replace("MIN_INPUT_MERGE", Configs.incomeMerge.min.toString)
        .replace("MAX_INPUT_MERGE", Configs.incomeMerge.max.toString)
        .replace("CONFIG_NFT", Base64.encode(Base16.decode(Configs.token.configNFT).get))
      ctx.compileContract(ConstantsBuilder.create().build(), script)
    })
  }

  private def generateDistributionContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val script = Scripts.distributionScript
        .replace("CONFIG_NFT", Base64.encode(Base16.decode(Configs.token.configNFT).get))
        .replace("LOCKING_TOKEN", Base64.encode(Base16.decode(Configs.token.locking).get))
        .replace("STAKING_TOKEN", Base64.encode(Base16.decode(Configs.token.staking).get))
      ctx.compileContract(ConstantsBuilder.create().build(), script)
    })
  }

  private def generateTicketContract(): ErgoContract ={
    client.getClient.execute(ctx => {
      val script = Scripts.ticketScript
        .replace("CONFIG_NFT", Base64.encode(Base16.decode(Configs.token.configNFT).get))
        .replace("DISTRIBUTION_TOKEN", Base64.encode(Base16.decode(Configs.token.distribution).get))
      ctx.compileContract(ConstantsBuilder.create().build(), script)
    })
  }

  private def generateConfigContract(): ErgoContract ={
    val script = Scripts.configScript
      .replace("STAKING_TOKEN", Base64.encode(Base16.decode(Configs.token.staking).get))
      .replace("DISTRIBUTION_HASH", Base64.encode(Utils.getContractScriptHash(distribution)))
      .replace("TICKET_HASH", Base64.encode(Utils.getContractScriptHash(ticket)))
    client.getClient.execute(ctx => {
      ctx.compileContract(ConstantsBuilder.create().build(), script)
    })
  }

}

