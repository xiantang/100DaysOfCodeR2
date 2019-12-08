package selection1

/**
 * 提供一个 combine 函数，可以对同一张信用卡的费用合并
 * case  类只有一个主构造器，构造的参数紧跟在类名后面
 * @param cc
 * @param amount
 */
case class Charge(cc:CreditCard,amount:Double) {

  def combine(other:Charge):Charge = {
    if (cc == other.cc){
      Charge(cc,amount + other.amount)
    }else
      throw new Exception("Can't combine charge to different cards")
  }
}
